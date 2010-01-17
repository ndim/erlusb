#include <libusb.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <poll.h>


void transfer_cb(struct libusb_transfer *transfer)
{
  if (transfer) {
    fprintf(stdout, "%s:"
	    " %p"
	    " status=%d"
	    " type=%d"
	    " ep=0x%02x"
	    " act_len=%d=0x%x\n",
	    __FUNCTION__, (void *)transfer,
	    transfer->status,
	    transfer->type,
	    transfer->endpoint,
	    transfer->actual_length, transfer->actual_length);
    fflush(stdout);
  }
}


int main()
{
  libusb_context *ctx = NULL;
  int init = libusb_init(NULL);
  assert(0 == init);
  libusb_set_debug(ctx, 3);

  libusb_device_handle *dev_handle = /* Garmin GPSmap (various models) */
    libusb_open_device_with_vid_pid(NULL, 0x091e, 0003);
  assert(dev_handle != NULL);

  int claim = libusb_claim_interface(dev_handle, 0);
  assert(0 == claim);

  printf("dev_hdl=%p claim=%d\n", (void *)dev_handle, claim);

  char start_session_packet[12] = {
    0,
    0,0,0,
    5,0,
    0,0,
    0,0,0,0
  };

  int transferred = -1;
  int bt = libusb_bulk_transfer(dev_handle,
				0x02,
				start_session_packet,
				sizeof(start_session_packet),
				&transferred,
				3000);
  printf("%d = libusb_bulk_transfer(...)\n", bt);

  /* So far, everything is on track.
   * But we will not receive anything using the sync API.
   */

  char bi_buf[1024];
  struct libusb_transfer *bi_transfer = libusb_alloc_transfer(0);
  assert(bi_transfer);
  libusb_fill_bulk_transfer(bi_transfer, dev_handle,
			    0x83, bi_buf, sizeof(bi_buf),
			    transfer_cb, NULL,
			    2000);

  char ii_buf[1024];
  struct libusb_transfer *ii_transfer = libusb_alloc_transfer(0);
  assert(ii_transfer);
  libusb_fill_interrupt_transfer(ii_transfer, dev_handle,
				 0x81, ii_buf, sizeof(ii_buf),
				 transfer_cb, NULL,
				 2000);

  int bi_sub = libusb_submit_transfer(bi_transfer);
  int ii_sub = libusb_submit_transfer(bi_transfer);

  printf("bi_sub=%d, ii_sub=%d\n", bi_sub, ii_sub);

  const struct libusb_pollfd** pollfds = libusb_get_pollfds(ctx);
  assert(pollfds != NULL);

  while (1 /* user has not requested application exit */ ) {
    fd_set rfds, wfds;
    int max_fd = 0;
    FD_ZERO(&rfds);
    FD_ZERO(&wfds);
    struct timeval lu_tv;
    libusb_get_next_timeout(NULL, &lu_tv);
    for (int i=0; pollfds[i]; i++) {
      const struct libusb_pollfd *pfd = pollfds[i];
      switch (pfd->events) {
      case POLLIN:
	FD_SET(pfd->fd, &rfds);
	max_fd = (max_fd<pfd->fd) ? pfd->fd : max_fd;
	break;
      case POLLOUT:
	FD_SET(pfd->fd, &wfds);
	max_fd = (max_fd<pfd->fd) ? pfd->fd : max_fd;
	break;
      default:
	assert((pfd->events == POLLIN) || (pfd->events == POLLOUT));
	break;
      }
    }

    int sret = select(max_fd+1, &rfds, &wfds, NULL, &lu_tv);
    if (sret == -1) {
      perror("select()");
    } else if (sret) {
      printf("Data is available now.\n");
      /* FIXME: Do the FD_ISSET(foo, &rfds) dance */
      if (1 /* select() indicated activity on libusb file descriptors */)
        libusb_handle_events_timeout(ctx, NULL);
    } else {
      /* timeout */
      if (1 /* time has elapsed to or beyond the libusb timeout */)
        libusb_handle_events_timeout(ctx, NULL);
    }
  }

  int release = libusb_release_interface(dev_handle, 0);
  assert(0 == release);

  printf("release=%d\n", release);

  libusb_close(dev_handle);
  libusb_exit(ctx);
}

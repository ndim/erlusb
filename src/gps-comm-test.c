#include <libusb.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main()
{
  int init = libusb_init(NULL);
  assert(0 == init);
  libusb_set_debug(NULL, 3);

  libusb_device_handle *dev_handle = /* Garmin GPSmap (various models) */
    libusb_open_device_with_vid_pid(NULL, 0x091e, 0003);
  assert(dev_handle != NULL);

  int claim = libusb_claim_interface(dev_handle, 0);
  assert(0 == claim);

  printf("dev_hdl=%p claim=%d\n", (void *)dev_handle, claim);

  int release = libusb_release_interface(dev_handle, 0);
  assert(0 == release);

  printf("release=%d\n", release);

  libusb_close(dev_handle);
  libusb_exit(NULL);
}

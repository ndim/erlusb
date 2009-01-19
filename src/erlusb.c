/* erlusb.c - Erlang interface to libusb
 * Copyright (C) 2006 Hans Ulrich Niedermann <hun@n-dimensional.de>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
 */

#include <string.h>
#include <unistd.h>

#include <usb.h>

#include <erl_interface.h>
#include <ei.h>


int ei_x_encode_uint(ei_x_buff *wb,
		     unsigned int uint)
{
  return ei_x_encode_long(wb, (long)(uint)); /* FIXME: signs etc? */
}


#define CHECK_EI(call) \
	do { \
		int ret = (call); \
		if (ret != 0) { \
			fprintf(log, "error running %s: returned %d\n", \
                                #call, ret); \
			exit(17); \
		} \
	} while (0)


static FILE *log = NULL;

typedef unsigned char byte;

int write_exact(byte *buf, int len);
int read_exact(byte *buf, size_t size, int len);
int read_cmd(byte *buf, size_t size);
int write_cmd(byte *buf, int len);


/** ei_x_encode_usb_string
 */

void
ei_x_encode_usb_string(ei_x_buff *wb,
		       usb_dev_handle *hdl, u_int8_t index)
{
  char buf[256];
  if (index &&
      (0 <= usb_get_string_simple(hdl, index, buf, sizeof(buf)))) {
    CHECK_EI(ei_x_encode_tuple_header(wb, 2));
    CHECK_EI(ei_x_encode_long(wb, (long)index)); /* FIXME: "negative" values? */
    CHECK_EI(ei_x_encode_string(wb, buf));
  } else {
    CHECK_EI(ei_x_encode_long(wb, (long)index)); /* FIXME: "negative" values? */
  }
}


void
ei_x_encode_usb_endpoint(ei_x_buff *wb,
			 struct usb_endpoint_descriptor *epd)
{
  CHECK_EI(ei_x_encode_tuple_header(wb, 9));
  CHECK_EI(ei_x_encode_atom(wb, "usb_endpoint_descriptor"));
  CHECK_EI(ei_x_encode_uint(wb, epd->bLength));
  CHECK_EI(ei_x_encode_uint(wb, epd->bDescriptorType));
  CHECK_EI(ei_x_encode_uint(wb, epd->bEndpointAddress));
  CHECK_EI(ei_x_encode_uint(wb, epd->bmAttributes));
  CHECK_EI(ei_x_encode_uint(wb, epd->wMaxPacketSize));
  CHECK_EI(ei_x_encode_uint(wb, epd->bInterval));
  CHECK_EI(ei_x_encode_uint(wb, epd->bRefresh));
  CHECK_EI(ei_x_encode_uint(wb, epd->bSynchAddress));
}


void
ei_x_encode_usb_endpoint_list(ei_x_buff *wb,
			      struct usb_interface_descriptor *ifd)
{
  unsigned int index;
  CHECK_EI(ei_x_encode_list_header(wb, ifd->bNumEndpoints));
  for (index=0; index<ifd->bNumEndpoints; index++) {
    ei_x_encode_usb_endpoint(wb, &(ifd->endpoint[index]));
  }
}


void
ei_x_encode_usb_interface_descriptor(ei_x_buff *wb,
				     usb_dev_handle *hdl,
				     struct usb_interface_descriptor *ifd)
{
  CHECK_EI(ei_x_encode_tuple_header(wb, 11));
  CHECK_EI(ei_x_encode_atom(wb, "usb_interface_descriptor"));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bLength));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bDescriptorType));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bInterfaceNumber));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bAlternateSetting));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bNumEndpoints));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bInterfaceClass));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bInterfaceSubClass));
  CHECK_EI(ei_x_encode_uint(wb, ifd->bInterfaceProtocol));
  ei_x_encode_usb_string(wb, hdl, ifd->iInterface);
  ei_x_encode_usb_endpoint_list(wb, ifd);
}


void
ei_x_encode_usb_altsettings(ei_x_buff *wb,
			    usb_dev_handle *hdl,
			    struct usb_interface *interface)
{
  int set_index;
  CHECK_EI(ei_x_encode_list_header(wb, interface->num_altsetting));
  for (set_index = 0; set_index < interface->num_altsetting; set_index++) {
    ei_x_encode_usb_interface_descriptor(wb, hdl, &(interface->altsetting[set_index]));
  }
}


void
ei_x_encode_usb_interface(ei_x_buff *wb,
			  usb_dev_handle *hdl,
			  struct usb_interface *interface)
{
  CHECK_EI(ei_x_encode_tuple_header(wb, 2));
  CHECK_EI(ei_x_encode_atom(wb, "usb_interface"));
  ei_x_encode_usb_altsettings(wb, hdl, interface);
}


void
ei_x_encode_usb_interface_tree(ei_x_buff *wb,
			       usb_dev_handle *hdl,
			       struct usb_config_descriptor *config)
{
  int interf_index;
  CHECK_EI(ei_x_encode_list_header(wb, config->bNumInterfaces));
  for (interf_index = 0; interf_index < config->bNumInterfaces; interf_index++) {
    ei_x_encode_usb_interface(wb, hdl, &(config->interface[interf_index]));
  }
}


void
ei_x_encode_usb_configuration(ei_x_buff *wb,
			      usb_dev_handle *hdl, unsigned int config_index,
			      struct usb_config_descriptor *config)
{
  CHECK_EI(ei_x_encode_tuple_header(wb, 4));
  CHECK_EI(ei_x_encode_atom(wb, "usb_config_descriptor"));
  CHECK_EI(ei_x_encode_uint(wb, config_index));
  ei_x_encode_usb_string(wb, hdl, config->iConfiguration);
  ei_x_encode_usb_interface_tree(wb, hdl, config);
}


void
ei_x_encode_usb_configuration_tree(ei_x_buff *wb,
				   struct usb_device *dev,
				   usb_dev_handle *hdl)
{
  unsigned int config_index;
  CHECK_EI(ei_x_encode_list_header(wb, dev->descriptor.bNumConfigurations));
  for (config_index = 0; config_index < dev->descriptor.bNumConfigurations; config_index++) {
    ei_x_encode_usb_configuration(wb, hdl, config_index,
				  &(dev->config[config_index]));
  }
}


void
ei_x_encode_usb_device_descriptor(ei_x_buff *wb,
				  usb_dev_handle *hdl,
				  struct usb_device_descriptor *descriptor)
{
  CHECK_EI(ei_x_encode_tuple_header(wb, 15));
  CHECK_EI(ei_x_encode_atom(wb, "usb_device_descriptor"));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bLength));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bDescriptorType));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bcdUSB));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bDeviceClass));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bDeviceSubClass));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bDeviceProtocol));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bMaxPacketSize0));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->idVendor));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->idProduct));
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bcdDevice));
  ei_x_encode_usb_string(wb, hdl, descriptor->iManufacturer);
  ei_x_encode_usb_string(wb, hdl, descriptor->iProduct);
  ei_x_encode_usb_string(wb, hdl, descriptor->iSerialNumber);
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bNumConfigurations));
}


void
ei_x_encode_usb_device(ei_x_buff *wb,
		       struct usb_device *dev)
{
  char sbuf[20];
  usb_dev_handle *hdl = usb_open(dev);
  sprintf(sbuf, "%04x:%04x",
	  dev->descriptor.idVendor, dev->descriptor.idProduct);
  CHECK_EI(ei_x_encode_tuple_header(wb, 6));
  CHECK_EI(ei_x_encode_atom(wb, "usb_device"));
  CHECK_EI(ei_x_encode_string(wb, dev->filename));
  CHECK_EI(ei_x_encode_string(wb, sbuf));
  ei_x_encode_usb_device_descriptor(wb, hdl, &(dev->descriptor));
  ei_x_encode_usb_configuration_tree(wb, dev, hdl);
  CHECK_EI(ei_x_encode_uint(wb, dev->devnum));
  usb_close(hdl);
}


void
ei_x_encode_usb_device_list(ei_x_buff *wb,
			    struct usb_bus *bus)
{
  unsigned int dev_count = 0;
  struct usb_device *dev;
  for (dev = bus->devices; dev; dev = dev->next) {
    dev_count++;
  }
  if (1) {
    unsigned int dev_index = 0;
    CHECK_EI(ei_x_encode_list_header(wb, dev_count));
    for (dev = bus->devices; dev; dev = dev->next, dev_index++) {
      ei_x_encode_usb_device(wb, dev);
    }
  }
}


void
ei_x_encode_usb_bus(ei_x_buff *wb,
		    struct usb_bus *bus)
{
  CHECK_EI(ei_x_encode_tuple_header(wb, 3));
  CHECK_EI(ei_x_encode_atom(wb, "usb_bus"));
  CHECK_EI(ei_x_encode_string(wb, bus->dirname));
  ei_x_encode_usb_device_list(wb, bus);
}


void
ei_x_encode_usb_bus_list(ei_x_buff *wb)
{
  unsigned int bus_count = 0;
  struct usb_bus *busses;
  struct usb_bus *bus;
  usb_find_busses();
  usb_find_devices();
  busses = usb_get_busses();
  for (bus = busses; bus; bus = bus->next) {
    bus_count++;
  }
  if (1) {
    unsigned int bus_index = 0;
    CHECK_EI(ei_x_encode_list_header(wb, bus_count));
    for (bus = busses; bus; bus = bus->next, bus_index++) {
      ei_x_encode_usb_bus(wb, bus);
    }
  }
}


void dump_data(FILE *fp, const char *data, size_t len)
{
   size_t i;
   for (i=0; i<len; i++) {
     switch (i&15) {
     case 0:
       fprintf(fp, "%08x ", i);
       break;
     case 8:
       fprintf(fp, " ");
       break;
     default:
       break;
     }
     fprintf(fp, " %02x", 0xff&data[i]);
     if (((i&15) == 15) || ((i+1) == len)) {
       fprintf(fp, "\n");
     }
   }
}


int main() {
  byte rbuf[200];

  log = fopen("erlusb.log", "a");
  setvbuf(log, NULL, _IONBF, 0);

  erl_init(NULL, 0);
  usb_init();

  fprintf(log, "erlusb.c init done\n");

  while (read_cmd(rbuf, sizeof(rbuf)) > 0) {
    ei_x_buff write_buffer;
    ei_x_buff *wb = &write_buffer;
    int wb_empty_index;
    int version=-1;
    int index=0;
    int arity=-1;
    int type=-1, size=-1;
    char atom[MAXATOMLEN+1];
    atom[0] = '\0';
    fprintf(log, "read message\n");
    dump_data(log, rbuf, sizeof(rbuf));
    CHECK_EI(ei_decode_version(rbuf, &index, &version));
    fprintf(log, "version: %d=0x%x\n", version, version);
    CHECK_EI(ei_get_type(rbuf, &index, &type, &size));
    fprintf(log, "type of received message: index=%d, type=%d=0x%x='%c', size=%d\n", index, type, type, type, size);
    CHECK_EI(ei_decode_tuple_header(rbuf, &index, &arity));
    fprintf(log, "decoded tuple header: index=%d, arity=%d\n", index, arity);
    CHECK_EI(ei_decode_atom(rbuf, &index, atom));
    fprintf(log, "decoded atom: index=%d, atom=%s\n", index, atom);

    CHECK_EI(ei_x_new_with_version(wb));
    wb_empty_index = wb->index;

    fprintf(log, "checking message: %s\n", atom);
    if (0) {
      /* nothing */
    } else if (strncmp(atom, "aaa", 3) == 0) {
      ei_x_encode_usb_bus_list(wb);
    } else if (strncmp(atom, "xxx", 3) == 0) {
      CHECK_EI(ei_x_encode_string(wb, "Humpf, Mops, Oerks!"));
    } else if (strncmp(atom, "yyy", 3) == 0) {
      CHECK_EI(ei_x_encode_string_len(wb, "Humpf, Mops, Oerks!", 13));
    } else if (strncmp(atom, "fff", 3) == 0) {
      CHECK_EI(ei_x_encode_list_header(wb, 3));
      CHECK_EI(ei_x_encode_string(wb, "Humpf"));
      CHECK_EI(ei_x_encode_string(wb, "Mops"));
      CHECK_EI(ei_x_encode_string(wb, "Oerks"));
    } else if (strncmp(atom, "close", 5) == 0) {
      CHECK_EI(ei_x_encode_atom(wb, "closed"));
    } else {
      CHECK_EI(ei_x_encode_tuple_header(wb, 2));
      CHECK_EI(ei_x_encode_atom(wb, "unknown_function"));
      CHECK_EI(ei_x_encode_atom(wb, atom));
    }

    if (wb->index > wb_empty_index) {
      CHECK_EI(ei_x_encode_tuple_header(wb, 2));
      CHECK_EI(ei_x_encode_atom(wb, "moo"));
      CHECK_EI(ei_x_encode_long(wb, (long) 13));

      fprintf(log, "writing message: wb->buffsz=%d wb->index=%d\n", wb->buffsz, wb->index);
      dump_data(log, wb->buff, wb->index);
      write_cmd(wb->buff, wb->index);
      fprintf(log, "wrote message\n");
    } else {
      fprintf(log, "no message to write back\n");
    }
    CHECK_EI(ei_x_free(wb));
  }

  fprintf(log, "erlusb.c finished.\n");
  fclose(log);
  return 0;
}


int read_cmd(byte *buf, size_t size)
{
  int len;

  if (read_exact(buf, size, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, size, len);
}


int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);

  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}


int read_exact(byte *buf, size_t size, int len)
{
  int i, got=0;

  if (((size_t)len)>size)
    return -1;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}


int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

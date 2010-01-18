/* driver-libusb1.c - libusb1 specific part of Erlang USB interface
 * Copyright (C) 2006,2009 Hans Ulrich Niedermann <hun@n-dimensional.de>
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

#include <assert.h>

#include "driver.h"

#include <libusb.h>

#include <erl_interface.h>
#include <ei.h>

#include "erlusb-ei.h"


static libusb_context *context = NULL;


static
void
driver_init()
{
  libusb_init(&context);
}


static
void
driver_exit()
{
  libusb_exit(context);
  context=NULL;
}


static
void
ei_x_encode_usb_string(ei_x_buff *wb,
		       /* libusb_device_handle *hdl, */
		       uint8_t index)
{
  /*
  char buf[256];

  if (index &&
      (0 <= libusb_get_string_descriptor_ascii(hdl, index,
					       buf, sizeof(buf)))) {
    CHECK_EI(ei_x_encode_tuple_header(wb, 2));
    CHECK_EI(ei_x_encode_uint(wb, index));
    CHECK_EI(ei_x_encode_string(wb, buf));
  } else {
  */
    CHECK_EI(ei_x_encode_uint(wb, index));
    /* } */
}


static
void
ei_x_encode_device(ei_x_buff *wb,
		   libusb_device *dev)
{
  struct libusb_device_descriptor desc;
  struct libusb_device_descriptor *descriptor=&desc;

  /*
  libusb_device_handle *hdl;

  if (0!=libusb_open(dev, &hdl)) {
    ei_x_encode_atom(wb, "cannot_open_libusb_device");
    return;
  }
  */

  if (0!=libusb_get_device_descriptor(dev, descriptor)) {
    ei_x_encode_tuple_header(wb, 2);
    ei_x_encode_atom(wb, "error");
    ei_x_encode_atom(wb, "libusb_get_device_descriptor");
    return;
  }

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
  ei_x_encode_usb_string(wb, /* hdl, */ descriptor->iManufacturer);
  ei_x_encode_usb_string(wb, /* hdl, */ descriptor->iProduct);
  ei_x_encode_usb_string(wb, /* hdl, */ descriptor->iSerialNumber);
  CHECK_EI(ei_x_encode_uint(wb, descriptor->bNumConfigurations));
}


static
void
ei_x_encode_all_devices(ei_x_buff *wb)
{
  libusb_device **list;
  ssize_t cnt = libusb_get_device_list(NULL, &list);
  ssize_t i = 0;
  if (cnt < 0) {
    ei_x_encode_tuple_header(wb, 2);
    ei_x_encode_atom(wb, "error");
    ei_x_encode_atom(wb, "libusb_get_device_list");
  } else {
    for (i = 0; i < cnt; i++) {
      libusb_device *device = list[i];
      ei_x_encode_list_header(wb, 1);
      ei_x_encode_device(wb, device);
    }
    ei_x_encode_empty_list(wb);
    libusb_free_device_list(list, 1);
  }
}


static
void
ei_x_encode_send_packet(ei_x_buff *wb,
                        void *packet,
			long len)
{
  struct libusb_device *device = NULL;
  libusb_device_handle *devhdl = NULL;
  int configuration = 0;
  assert(device);
  assert(0 <= libusb_open(device, &devhdl));
  assert(devhdl);
  assert(0 <= libusb_claim_interface(devhdl, configuration));
  /* FIXME: eventually call libusb_release_interface as well */
  /* FIXME: Actually send the packet */
  packet = packet;
  len = len;
  ei_x_encode_tuple_header(wb, 2);
  ei_x_encode_atom(wb, "not_implemented_yet");
  ei_x_encode_atom(wb, "send_packet");
}


erlusb_driver_t driver = {
  .name = "libusb-1.0",
  .init = driver_init,
  .exit = driver_exit,
  .get_device_list = ei_x_encode_all_devices,
  .send_packet = ei_x_encode_send_packet
};

/* driver-libusb0.c - Erlang interface to libusb
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

#include "driver.h"

#include <usb.h>

#include <erl_interface.h>
#include <ei.h>

#include "erlusb-ei.h"


void
driver_init()
{
  usb_init();
}


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
  CHECK_EI(ei_x_encode_empty_list(wb));
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
    ei_x_encode_usb_interface_descriptor(wb, hdl,
					 &(interface->altsetting[set_index]));
  }
  CHECK_EI(ei_x_encode_empty_list(wb));
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
  CHECK_EI(ei_x_encode_empty_list(wb));
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
  CHECK_EI(ei_x_encode_empty_list(wb));
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
  struct usb_device *dev;
  for (dev = bus->devices; dev; dev = dev->next) {
    ei_x_encode_list_header(wb, 1);
    ei_x_encode_usb_device(wb, dev);
  }
  CHECK_EI(ei_x_encode_empty_list(wb));
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
  struct usb_bus *busses;
  struct usb_bus *bus;
  usb_find_busses();
  usb_find_devices();
  busses = usb_get_busses();
  for (bus = busses; bus; bus = bus->next) {
    ei_x_encode_list_header(wb, 1);
    ei_x_encode_usb_bus(wb, bus);
  }
  ei_x_encode_empty_list(wb);
}

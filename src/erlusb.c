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


static FILE *log = NULL;

typedef unsigned char byte;

int write_exact(byte *buf, int len);
int read_exact(byte *buf, size_t size, int len);
int read_cmd(byte *buf, size_t size);
int write_cmd(byte *buf, int len);

#define ENCODE(term)					       \
  do {							       \
    wbuf_len = erl_term_len(term);			       \
    if (wbuf_len > wbuf_size) {				       \
      erl_free(wbuf);					       \
      wbuf = erl_malloc(2*wbuf_len);			       \
      /* FIXME: Error if wbuf == NULL; */		       \
    }							       \
    erl_encode(term, wbuf);				       \
  } while (0)


#define RETURN_LOG_ETERM(term)			\
  do {						\
    ETERM *result = (term);			\
    erl_print_term(log, result);		\
    fprintf(log, "\n");				\
    return result;				\
  } while (0)


#define RETURN_LOG_CLEANUP_ETERM(term, cleanup)		\
  do {							\
    ETERM *result = (term);				\
    erl_print_term(log, result);			\
    fprintf(log, "\n");					\
    cleanup;						\
    return result;					\
  } while (0)

#define RETURN_CLEANUP_ETERM(term, cleanup)	\
  do {						\
    ETERM *retval = (term);			\
    cleanup;					\
    return retval;				\
  } while (0);


/** erl_mk_usb_string
 *
 * Note: Caller must erl_free_compound() the return value
 */

ETERM *erl_mk_usb_string(usb_dev_handle *hdl, u_int8_t index)
{
  char buf[256];
  if (index && 
      (0 <= usb_get_string_simple(hdl, index, buf, sizeof(buf)))) {
    ETERM *terms[2];
    terms[0] = erl_mk_uint(index);
    terms[1] = erl_mk_string(buf);
    return erl_mk_tuple(terms, 2);
  } else {
    return erl_mk_uint(index);
  }
}


ETERM *erl_mk_usb_endpoint(struct usb_endpoint_descriptor *epd)
{
  unsigned int n = 0;
  ETERM *terms[16];
  terms[n++] = erl_mk_atom("usb_endpoint_descriptor");
  terms[n++] = erl_mk_uint(epd->bLength);
  terms[n++] = erl_mk_uint(epd->bDescriptorType);
  terms[n++] = erl_mk_uint(epd->bEndpointAddress);
  terms[n++] = erl_mk_uint(epd->bmAttributes);
  terms[n++] = erl_mk_uint(epd->wMaxPacketSize);
  terms[n++] = erl_mk_uint(epd->bInterval);
  terms[n++] = erl_mk_uint(epd->bRefresh);
  terms[n++] = erl_mk_uint(epd->bSynchAddress);
  RETURN_LOG_ETERM(erl_mk_tuple(terms, n));
}


ETERM *erl_mk_usb_endpoint_list(struct usb_interface_descriptor *ifd)
{
  ETERM **list_terms = erl_malloc(ifd->bNumEndpoints*sizeof(ETERM *));
  unsigned int index;
  for (index=0; index<ifd->bNumEndpoints; index++) {
    list_terms[index] = erl_mk_usb_endpoint(&(ifd->endpoint[index]));
  }
  RETURN_CLEANUP_ETERM(erl_mk_list(list_terms, index),
		       erl_free(list_terms));
}


ETERM *erl_mk_usb_interface_descriptor(usb_dev_handle *hdl, struct usb_interface_descriptor *ifd)
{
  ETERM *terms[16];
  unsigned int n = 0;
  terms[n++] = erl_mk_atom("usb_interface_descriptor");
  terms[n++] = erl_mk_uint(ifd->bLength);
  terms[n++] = erl_mk_uint(ifd->bDescriptorType);
  terms[n++] = erl_mk_uint(ifd->bInterfaceNumber);
  terms[n++] = erl_mk_uint(ifd->bAlternateSetting);
  terms[n++] = erl_mk_uint(ifd->bNumEndpoints);
  terms[n++] = erl_mk_uint(ifd->bInterfaceClass);
  terms[n++] = erl_mk_uint(ifd->bInterfaceSubClass);
  terms[n++] = erl_mk_uint(ifd->bInterfaceProtocol);
  terms[n++] = erl_mk_usb_string(hdl, ifd->iInterface);
  terms[n++] = erl_mk_usb_endpoint_list(ifd);
  RETURN_LOG_ETERM(erl_mk_tuple(terms, n));
}


ETERM *erl_mk_usb_altsettings(usb_dev_handle *hdl, struct usb_interface *interface)
{
  ETERM **set_terms = erl_malloc(interface->num_altsetting*sizeof(ETERM *));
  int set_index;
  for (set_index = 0; set_index < interface->num_altsetting; set_index++) {
    set_terms[set_index] = erl_mk_usb_interface_descriptor(hdl, &(interface->altsetting[set_index]));
  }  
  RETURN_CLEANUP_ETERM(erl_mk_list(set_terms, set_index), 
		       erl_free(set_terms));
}


ETERM *erl_mk_usb_interface(usb_dev_handle *hdl, struct usb_interface *interface)
{
  unsigned int n = 0;
  ETERM *terms[16];
  terms[n++] = erl_mk_atom("usb_interface");
  terms[n++] = erl_mk_usb_altsettings(hdl, interface);
  RETURN_LOG_ETERM(erl_mk_tuple(terms, n));
}


ETERM *erl_mk_usb_interface_tree(usb_dev_handle *hdl, struct usb_config_descriptor *config)
{
  ETERM **interf_terms = erl_malloc(config->bNumInterfaces*sizeof(ETERM *));
  unsigned int interf_index;
  for (interf_index = 0; interf_index < config->bNumInterfaces; interf_index++) {
    interf_terms[interf_index] = erl_mk_usb_interface(hdl, &(config->interface[interf_index]));
  }
  RETURN_CLEANUP_ETERM(erl_mk_list(interf_terms, interf_index),
		       erl_free(interf_terms));
}


ETERM *erl_mk_usb_configuration(usb_dev_handle *hdl, unsigned int config_index,
				struct usb_config_descriptor *config)
{
  unsigned int n = 0;
  ETERM *terms[16];
  terms[n++] = erl_mk_atom("usb_config_descriptor");
  terms[n++] = erl_mk_uint(config_index);
  terms[n++] = erl_mk_usb_string(hdl, config->iConfiguration);
  terms[n++] = erl_mk_usb_interface_tree(hdl, config);
  RETURN_LOG_ETERM(erl_mk_tuple(terms, n));
}


ETERM *erl_mk_usb_configuration_tree(struct usb_device *dev, usb_dev_handle *hdl)
{
  ETERM **config_terms = erl_malloc(dev->descriptor.bNumConfigurations*sizeof(ETERM *));
  unsigned int config_index;
  for (config_index = 0; config_index < dev->descriptor.bNumConfigurations; config_index++) {
    config_terms[config_index] = erl_mk_usb_configuration(hdl, config_index,
							  &(dev->config[config_index]));
  }
  RETURN_CLEANUP_ETERM(erl_mk_list(config_terms, config_index),
		       erl_free(config_terms));
}


ETERM *erl_mk_usb_device_descriptor(usb_dev_handle *hdl, struct usb_device_descriptor *descriptor)
{
  unsigned int n = 0;
  ETERM *terms[16];
  terms[n++] = erl_mk_atom("usb_device_descriptor");
  terms[n++] = erl_mk_uint(descriptor->bLength);
  terms[n++] = erl_mk_uint(descriptor->bDescriptorType);
  terms[n++] = erl_mk_uint(descriptor->bcdUSB);
  terms[n++] = erl_mk_uint(descriptor->bDeviceClass);
  terms[n++] = erl_mk_uint(descriptor->bDeviceSubClass);
  terms[n++] = erl_mk_uint(descriptor->bDeviceProtocol);
  terms[n++] = erl_mk_uint(descriptor->bMaxPacketSize0);
  terms[n++] = erl_mk_uint(descriptor->idVendor);
  terms[n++] = erl_mk_uint(descriptor->idProduct);
  terms[n++] = erl_mk_uint(descriptor->bcdDevice);
  terms[n++] = erl_mk_usb_string(hdl, descriptor->iManufacturer);
  terms[n++] = erl_mk_usb_string(hdl, descriptor->iProduct);
  terms[n++] = erl_mk_usb_string(hdl, descriptor->iSerialNumber);
  terms[n++] = erl_mk_uint(descriptor->bNumConfigurations);
  RETURN_LOG_ETERM(erl_mk_tuple(terms, n));
}


ETERM *erl_mk_usb_device(struct usb_device *dev)
{
  unsigned int n = 0;
  ETERM *terms[16];
  char sbuf[20];
  usb_dev_handle *hdl = usb_open(dev);
  fprintf(log, "%04x:%04x\n", 
	  dev->descriptor.idVendor, dev->descriptor.idProduct);
  sprintf(sbuf, "%04x:%04x", 
	  dev->descriptor.idVendor, dev->descriptor.idProduct);
  terms[n++] = erl_mk_atom("usb_device");
  terms[n++] = erl_mk_string(dev->filename);
  terms[n++] = erl_mk_string(sbuf);
  terms[n++] = erl_mk_usb_device_descriptor(hdl, &(dev->descriptor));
  terms[n++] = erl_mk_usb_configuration_tree(dev, hdl);
  terms[n++] = erl_mk_uint(dev->devnum);
  fprintf(log, "\n");
  RETURN_LOG_CLEANUP_ETERM(erl_mk_tuple(terms, n), usb_close(hdl));
}


ETERM *erl_mk_usb_device_list(struct usb_bus *bus)
{
  unsigned int dev_count = 0;
  struct usb_device *dev;
  for (dev = bus->devices; dev; dev = dev->next) {
    dev_count++;
  }
  if (1) {
    unsigned int dev_index = 0;
    ETERM **dev_terms = erl_malloc(dev_count*sizeof(ETERM *));
    for (dev = bus->devices; dev; dev = dev->next, dev_index++) {
      dev_terms[dev_index] = erl_mk_usb_device(dev);
    }
    RETURN_CLEANUP_ETERM(erl_mk_list(dev_terms, dev_index),
			 erl_free(dev_terms));
  }  
}


ETERM *erl_mk_usb_bus(struct usb_bus *bus)
{
  unsigned int n = 0;
  ETERM *terms[10];
  terms[n++] = erl_mk_atom("usb_bus");
  terms[n++] = erl_mk_string(bus->dirname);
  terms[n++] = erl_mk_usb_device_list(bus);
  fprintf(log, "\n");
  RETURN_LOG_ETERM(erl_mk_tuple(terms, n));
}


ETERM *erl_mk_usb_bus_list()
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
    ETERM **bus_terms = erl_malloc(bus_count*sizeof(ETERM *));
    for (bus = busses; bus; bus = bus->next, bus_index++) {
      bus_terms[bus_index] = erl_mk_usb_bus(bus);
      fprintf(log, "\n");
    }
    fprintf(log, "\n");
    RETURN_LOG_CLEANUP_ETERM(erl_mk_list(bus_terms, bus_index),
			     erl_free(bus_terms));
  }
}


int main() {
  byte rbuf[200];

  int wbuf_size = 40;
  byte *wbuf = erl_malloc(wbuf_size);
  int wbuf_len = 0;

  log = fopen("erlusb.log", "a");
  setvbuf(log, NULL, _IONBF, 0);

  erl_init(NULL, 0);
  usb_init();

  fprintf(log, "erlusb.c init done\n");

  while (read_cmd(rbuf, sizeof(rbuf)) > 0) {
    ETERM *tuplep;
    ETERM *fnp, *argp;
    fprintf(log, "read message\n");
    tuplep = erl_decode(rbuf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);
    
    fprintf(log, "checking message: %s\n", ERL_ATOM_PTR(fnp));
    if (0) {
      /* nothing */
    } else if (strncmp(ERL_ATOM_PTR(fnp), "aaa", 3) == 0) {
      ETERM *trm = erl_mk_usb_bus_list();
      ENCODE(trm);
      erl_free_compound(trm);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "xxx", 3) == 0) {
      ETERM *str = erl_mk_string("Humpf, Mops, Oerks!");
      ENCODE(str);
      erl_free_term(str);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "yyy", 3) == 0) {
      ETERM *str = erl_mk_estring("Humpf, Mops, Oerks!", 13);
      ENCODE(str);
      erl_free_term(str);
    } else if (strncmp(ERL_ATOM_PTR(fnp), "fff", 3) == 0) {
      unsigned int i = 0;
      ETERM *strs[3];
      ETERM *lst;
      strs[i++] = erl_mk_string("Humpf");
      strs[i++] = erl_mk_string("Mops");
      strs[i++] = erl_mk_string("Oerks");
      lst = erl_mk_list(strs, (sizeof(strs)/sizeof(strs[0])));
      ENCODE(lst);
      for (i=0; i<(sizeof(strs)/sizeof(strs[0])); i++) {
	erl_free_term(strs[i]);
      }
      erl_free_term(lst);
    } else {
      ETERM *trm = erl_mk_atom("function_name_error");
      ENCODE(trm);
      erl_free_term(trm);
    }

    fprintf(log, "writing message\n");
    write_cmd(wbuf, wbuf_len);
    fprintf(log, "wrote message\n");

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp);
  }

  fprintf(log, "erlusb.c finished.\n");
  fclose(log);
  erl_free(wbuf);
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

/* erlusb.c - Erlang interface to libusb
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

#include <string.h>
#include <unistd.h>
#include <assert.h>

#include <erl_interface.h>
#include <ei.h>

#include "driver.h"
#include "erlusb-ei.h"
#include "erlusb-log.h"


typedef unsigned char byte;


int write_exact(byte *buf, int len);
int read_exact(byte *buf, size_t size, int len);
int read_cmd(byte *buf, size_t size);
int write_cmd(byte *buf, int len);


int main() {
  byte rbuf[1<<16];
  int readlen = -1;

  log_init();
  driver_init();

  log_printf("erlusb.c init done\n");

  while ((readlen = read_cmd(rbuf, sizeof(rbuf))) > 0) {
    ei_x_buff write_buffer;
    ei_x_buff *wb = &write_buffer;
    int wb_empty_index;
    int version=-1;
    int index=0;
    int arity=-1;
    int type=-1, size=-1;
    char atom[MAXATOMLEN+1];
    atom[0] = '\0';
    log_printf("read message\n");
    log_data(rbuf, readlen);
    log_buff_term(rbuf);
    CHECK_EI(ei_decode_version(rbuf, &index, &version));
    log_printf("version: %d=0x%x\n", version, version);
    CHECK_EI(ei_get_type(rbuf, &index, &type, &size));
    log_printf("type of received message: "
	       "index=%d, type=%d=0x%x='%c', size=%d\n",
	       index, type, type, type, size);
    CHECK_EI(ei_decode_tuple_header(rbuf, &index, &arity));
    log_printf("decoded tuple header: index=%d, arity=%d\n", index, arity);
    CHECK_EI(ei_decode_atom(rbuf, &index, atom));
    log_printf("decoded atom: index=%d, atom=%s\n", index, atom);

    CHECK_EI(ei_x_new_with_version(wb));
    wb_empty_index = wb->index;

    log_printf("checking message: %s\n", atom);
    if (0) {
      /* nothing */
    } else if (strncmp(atom, "send_packet", 12) == 0) {
      long endpoint = -1;
      int type;
      int size;
      long len;
      void *packet;
      CHECK_EI(ei_decode_long(rbuf, &index, &endpoint));
      CHECK_EI(ei_get_type(rbuf, &index, &type, &size));
      assert(type == ERL_BINARY_EXT);
      packet = malloc(size);
      assert(packet != NULL);
      CHECK_EI(ei_decode_binary(rbuf, &index, packet, &len));
      ei_x_encode_send_packet(wb, packet, len);
      free(packet);
    } else if (strncmp(atom, "usb_bus_list", 13) == 0) {
      ei_x_encode_all_devices(wb);
    } else if (strncmp(atom, "test-1", 7) == 0) {
      CHECK_EI(ei_x_encode_string(wb, "Humpf, Mops, Oerks!"));
    } else if (strncmp(atom, "test-2", 7) == 0) {
      CHECK_EI(ei_x_encode_string_len(wb, "Humpf, Mops, Oerks!", 13));
    } else if (strncmp(atom, "test-3", 7) == 0) {
      CHECK_EI(ei_x_encode_list_header(wb, 3));
      CHECK_EI(ei_x_encode_string(wb, "Humpf"));
      CHECK_EI(ei_x_encode_string(wb, "Mops"));
      CHECK_EI(ei_x_encode_string(wb, "Oerks"));
      CHECK_EI(ei_x_encode_empty_list(wb));
    } else if (strncmp(atom, "close", 6) == 0) {
      CHECK_EI(ei_x_encode_atom(wb, "closed"));
    } else {
      CHECK_EI(ei_x_encode_tuple_header(wb, 2));
      CHECK_EI(ei_x_encode_atom(wb, "unknown_function"));
      CHECK_EI(ei_x_encode_atom(wb, atom));
    }

    if (wb->index > wb_empty_index) {
      /* OK */
    } else {
      log_printf("no message to write back\n");
      CHECK_EI(ei_x_encode_tuple_header(wb, 3));
      CHECK_EI(ei_x_encode_atom(wb, "internal_error"));
      CHECK_EI(ei_x_encode_atom(wb, "no_message_for"));
      CHECK_EI(ei_x_encode_atom(wb, atom));
    }

    log_printf("writing message: wb->buffsz=%d wb->index=%d\n",
	       wb->buffsz, wb->index);
    log_data(wb->buff, wb->index);
    log_buff_term(wb->buff);
    write_cmd(wb->buff, wb->index);
    log_printf("wrote message\n");

    CHECK_EI(ei_x_free(wb));
  }

  driver_exit();
  log_close();
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

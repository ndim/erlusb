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

#include <erl_interface.h>
#include <ei.h>

#include "driver.h"
#include "erlusb-ei.h"


typedef unsigned char byte;

int write_exact(byte *buf, int len);
int read_exact(byte *buf, size_t size, int len);
int read_cmd(byte *buf, size_t size);
int write_cmd(byte *buf, int len);


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

  log_init();

  erl_init(NULL, 0);
  driver_init();

  fprintf(logfile, "erlusb.c init done\n");

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
    fprintf(logfile, "read message\n");
    dump_data(logfile, rbuf, sizeof(rbuf));
    CHECK_EI(ei_decode_version(rbuf, &index, &version));
    fprintf(logfile, "version: %d=0x%x\n", version, version);
    CHECK_EI(ei_get_type(rbuf, &index, &type, &size));
    fprintf(logfile, "type of received message: index=%d, type=%d=0x%x='%c', size=%d\n", index, type, type, type, size);
    CHECK_EI(ei_decode_tuple_header(rbuf, &index, &arity));
    fprintf(logfile, "decoded tuple header: index=%d, arity=%d\n", index, arity);
    CHECK_EI(ei_decode_atom(rbuf, &index, atom));
    fprintf(logfile, "decoded atom: index=%d, atom=%s\n", index, atom);

    CHECK_EI(ei_x_new_with_version(wb));
    wb_empty_index = wb->index;

    fprintf(logfile, "checking message: %s\n", atom);
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

      fprintf(logfile, "writing message: wb->buffsz=%d wb->index=%d\n", wb->buffsz, wb->index);
      dump_data(logfile, wb->buff, wb->index);
      write_cmd(wb->buff, wb->index);
      fprintf(logfile, "wrote message\n");
    } else {
      fprintf(logfile, "no message to write back\n");
    }
    CHECK_EI(ei_x_free(wb));
  }

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

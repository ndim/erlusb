/* erlusb-log.c - simple logging mechanism for debugging
 * Copyright (C) 2009 Hans Ulrich Niedermann <hun@n-dimensional.de>
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

#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>

#include "erlusb-log.h"
#include "ei.h"

static FILE *logfile = NULL;

void
log_init()
{
  logfile = fopen("erlusb.log", "a");
  setvbuf(logfile, NULL, _IONBF, 0);
}

void
log_close()
{
  fprintf(logfile, "erlusb.c finished.\n");
  fclose(logfile);
}

void
log_printf(const char *format, ...)
{
  va_list ap, aq;
  va_start(ap, format);
  va_copy(aq, ap);
  vfprintf(logfile, format, aq);
  va_end(aq);
  va_end(ap);
}

static
void
dump_data(FILE *fp, const char *data, const size_t len)
{
  size_t row;
  for (row=0; 16*row<len; row++) {
    size_t col;
    fprintf(fp, "%08x ", 16*row);
    for (col=0; col<16; col++) {
      size_t ofs = 16*row+col;
      if (col == 8) {
	fprintf(fp, " ");
      }
      if (ofs < len) {
	fprintf(fp, " %02x", 0xff&data[ofs]);
      } else {
	fprintf(fp, "   ");
      }
    }
    fprintf(fp, "  |");
    for (col=0; col<16; col++) {
      size_t ofs = 16*row+col;
      if (ofs < len) {
	char c = 0xff&data[ofs];
	fprintf(fp, "%c", ((32<=c)&&(c<127))?c:'.');
      } else {
	break;
      }
    }
    fprintf(fp, "|\n");
  }
}


void
log_data(const char *data, const size_t len)
{
  dump_data(logfile, data, len);
}

void
log_buff_term(const char *buff)
{
  int index=0;
  int version=-1;
  fprintf(logfile, "BUFF: ");
  if (0 != ei_decode_version(buff, &index, &version)) {
    fprintf(logfile, "error: could not decode version\n");
    return;
  }
  if (0 != ei_print_term(logfile, buff, &index)) {
    fprintf(logfile, "error: could not decode term\n");
    return;
  }
  fprintf(logfile, "\n");
}

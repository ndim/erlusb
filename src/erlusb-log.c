#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>

#include "erlusb-log.h"

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

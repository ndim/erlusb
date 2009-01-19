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


void
log_data(const char *data, const size_t len)
{
  dump_data(logfile, data, len);
}

#include "erlusb-ei.h"

FILE *logfile = NULL;

int
ei_x_encode_uint(ei_x_buff *wb,
		 unsigned int uint)
{
  return ei_x_encode_long(wb, (long)(uint)); /* FIXME: signs etc? */
}


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

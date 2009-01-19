#include "erlusb-ei.h"

int
ei_x_encode_uint(ei_x_buff *wb,
		 unsigned int uint)
{
  return ei_x_encode_long(wb, (long)(uint)); /* FIXME: signs etc? */
}

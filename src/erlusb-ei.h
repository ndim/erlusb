/* erlusb-ei.h - ei utility macros and functions
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

#ifndef ERLUSB_EI_H
#define ERLUSB_EI_H

#include <stdlib.h>

#include <erl_interface.h>
#include <ei.h>

#include "erlusb-log.h"

extern
int
ei_x_encode_uint(ei_x_buff *wb,
		 unsigned int uint);

#define CHECK_EI(call)						      \
  do {								      \
    int ret = (call);						      \
    if (ret != 0) {						      \
      log_printf("error running %s: returned %d\n",		      \
		 #call, ret);					      \
      exit(17);							      \
    }								      \
  } while (0)


#endif /* ! ERLUSB_EI_H */

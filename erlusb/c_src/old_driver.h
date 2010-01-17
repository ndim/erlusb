/* driver.h - backend specific part of Erlang USB interface
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

#ifndef DRIVER_H
#define DRIVER_H

#include <erl_interface.h>
#include <ei.h>

typedef struct {
  char *name;
  void (*init)(void);
  void (*exit)(void);
  void (*get_device_list)(ei_x_buff *wb);
  void (*send_packet)(ei_x_buff *wb,
		      void *packet,
		      long len);
} erlusb_driver_t;

extern erlusb_driver_t driver;

#endif /* !DRIVER_H */

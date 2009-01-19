/* erlusb-log.h - Erlang interface to libusb
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

#ifndef ERLUSB_LOG_H
#define ERLUSB_LOG_H

#include <stdlib.h>

extern
void
log_printf(const char *format, ...);

extern
void
log_data(const char *data, const size_t len);

extern
void
log_init();

extern
void
log_close();

extern
void
log_buff_term(const char *buff);

#endif /* !ERLUSB_LOG_H */

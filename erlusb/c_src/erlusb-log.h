/* erlusb-log.h - simple logging mechanism for debugging
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

#ifndef ERLUSB_LOG_H
#define ERLUSB_LOG_H

#include <stdlib.h>
#include <string.h>

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
# define LOGF(...)					       \
  do {							       \
    const char *const slash = strrchr(__FILE__, '/');	       \
    const char *const filename = (slash)?(slash+1):(__FILE__); \
    log_printf("%s:%d: %s(): ",				       \
	       filename, __LINE__, __FUNCTION__);	       \
    log_printf(__VA_ARGS__);				       \
  } while (0)
#elif defined(__GNUC__) && (__GNUC__ >= 2)
# define LOGF(format, args...)				       \
  do {							       \
    const char *const slash = strrchr(__FILE__, '/');	       \
    const char *const filename = (slash)?(slash+1):(__FILE__); \
    log_printf("%s:%d: %s(): ",				       \
	       filename, __LINE__, __FUNCTION__);	       \
    log_printf(format, ##args);				       \
  } while (0)
#else
# error Need stdc-like or GNU-like variadic macros
#endif

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

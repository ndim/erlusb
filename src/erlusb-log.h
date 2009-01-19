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

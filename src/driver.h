#ifndef DRIVER_H
#define DRIVER_H

#include <erl_interface.h>
#include <ei.h>

extern
void
driver_init();

extern
void
ei_x_encode_usb_bus_list(ei_x_buff *wb);

#endif /* !DRIVER_H */


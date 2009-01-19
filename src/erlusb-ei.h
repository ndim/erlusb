#ifndef ERLUSB_EI_H
#define ERLUSB_EI_H

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include <erl_interface.h>
#include <ei.h>

extern
int
ei_x_encode_uint(ei_x_buff *wb,
		 unsigned int uint);

extern
void
log_init();

extern
void
log_close();

extern FILE *logfile;


#define CHECK_EI(call) \
	do { \
		int ret = (call); \
		if (ret != 0) { \
			fprintf(logfile, "error running %s: returned %d\n", \
                                #call, ret); \
			exit(17); \
		} \
	} while (0)


#endif /* ! ERLUSB_EI_H */

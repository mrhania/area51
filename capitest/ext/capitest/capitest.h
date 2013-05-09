#ifndef __CAPITEST_H
#define __CAPITEST_H

/* Standard headers. */
#include <algorithm>
#include <cstring>

/* Ruby headers. */
#include <ruby.h>

typedef VALUE (*METHOD)(...);

extern "C" {
	void Init_capitest(void);
}

#endif

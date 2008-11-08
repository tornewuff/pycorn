#ifndef __PYCORN_MACH_H
#define __PYCORN_MACH_H

#include "pycorn_arch.h"

#define REGISTER(addr) (*(volatile u32 *)(addr))

#define FFRBR REGISTER(0x40100000)
#define FFTHR FFRBR
#define FFLSR REGISTER(0x40100014)
#define LSR_TEMT (1<<6)
#define LSR_TDRQ (1<<5)
#define LSR_DR   (1<<0)

#endif

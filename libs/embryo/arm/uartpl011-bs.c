/*
 * Bootstrap access to PL011 UART.
 *
 * No UART setup code is provided as it is assumed the Linux-style bootloader
 * has done this for us.
 *
 *
 * Copyright 2012 Torne Wuff
 *
 * This file is part of Pycorn.
 *
 * Pycorn is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include <stdint.h>
#include "bootstrap.h"

#define REGISTER(addr) (*(volatile uint8_t *)(addr))
#define REG_DR REGISTER(&__dbg_serial_phys__)
#define REG_FR REGISTER(&__dbg_serial_phys__ + 0x18)
#define FR_TXFF (1<<5)

// spins waiting for transmit FIFO to not be full, then writes byte
void boot_putchar(char c)
{
  while(REG_FR & FR_TXFF);
  REG_DR = c;
}

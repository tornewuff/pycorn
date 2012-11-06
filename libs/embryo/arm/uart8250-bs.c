/*
 * Bootstrap access to generic 8250-style UART
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

// This assumes that the UART has 8-bit registers which are mapped 4 bytes
// apart, which is common on ARM. The actual register definitions are
// 8250-compatible. Read a book.
#define REGISTER(addr) (*(volatile uint8_t *)(addr))
#define REG_THR REGISTER(&__dbg_serial_phys__)
#define REG_LSR REGISTER(&__dbg_serial_phys__ + 0x14)
#define LSR_TDRQ (1<<5)

// spins waiting for transmit data request, then writes byte
void boot_putchar(char c)
{
  while((REG_LSR & LSR_TDRQ) == 0);
  REG_THR = c;
}

/*
 * Bootstrap access to generic 16550-style UART
 *
 * Should work for many ARM serial UARTs. This is to be used before the MMU is
 * enabled and thus uses physical addressing, as well as providing some
 * conveniences for debug in an environment where printf() is not yet ready. No
 * support for reading is provided as during bootstrapping we are at most
 * printing debug messages.
 *
 * No UART setup code is provided as it is assumed the Linux-style bootloader
 * has done this for us.
 *
 *
 * Copyright 2008 Torne Wuff
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
// 16550-compatible. Read a book.
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

// Assumes the caller has passed the virtual address of a string from the .text
// segment, which we relocate to physical using the offset calculated at the
// start of the bootstrap. This cannot print non-constant strings!
void boot_putstr(const char *s)
{
  s -= bootdata->phys_to_virt;
  while (*s)
  {
    while((REG_LSR & LSR_TDRQ) == 0);
    REG_THR = *s++;
  }
}

// Prints a label string (if not NULL), then a 32-bit hex value, then a newline
// (if there was a label).
void boot_putint(const char *label, uint32_t i)
{
  if (label)
    boot_putstr(label);
  for (int bits = 28; bits >= 0; bits -= 4)
  {
    char nibble = (i >> bits) & 0xf;
    nibble += (nibble < 10) ? '0' : ('a' - 10);
    while((REG_LSR & LSR_TDRQ) == 0);
    REG_THR = nibble;
  }
  if (label)
    boot_putchar('\n');
}

/*
 * Generic memory mapped serial port, 8250/16550-like
 * Should work for many ARM serial UARTs.
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

#define REGISTER(addr) (*(volatile uint32_t *)(addr))

#define REG_THR REGISTER(&__dbg_serial_phys__)
#define REG_LSR REGISTER(&__dbg_serial_phys__ + 0x14)
#define LSR_TDRQ (1<<5)

void boot_putchar(char c)
{
  while((REG_LSR & LSR_TDRQ) == 0);
  REG_THR = c;
}

void boot_putstr(const char *s)
{
  s -= bootdata->phys_to_virt;
  while (*s)
  {
    while((REG_LSR & LSR_TDRQ) == 0);
    REG_THR = *s++;
  }
}

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

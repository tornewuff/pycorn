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

extern char __dbg_serial_virt__;

#define REGISTER(addr) (*(volatile uint8_t *)(addr))

#define REG_RBR REGISTER(&__dbg_serial_virt__)
#define REG_THR REG_RBR
#define REG_LSR REGISTER(&__dbg_serial_virt__ + 0x14)
#define LSR_TDRQ (1<<5)
#define LSR_DR   (1<<0)

int serial_write(const char* ptr, int len)
{
  int i;
  for (i = 0; i < len; ++i)
  {
    while((REG_LSR & LSR_TDRQ) == 0);
    REG_THR = ptr[i];
  }
  return len;
}

int serial_read(char* ptr, int len)
{
  if (len == 0)
    return 0;

  while((REG_LSR & LSR_DR) == 0);
  ptr[0] = REG_RBR;
  if (ptr[0] == '\r')
    ptr[0] = '\n';
  serial_write(ptr, 1);
  return 1;
}

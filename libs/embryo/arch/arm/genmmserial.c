/*
 * 'Real' system access to generic 16550-style UART
 *
 * Should work for many ARM serial UARTs. This is to be used after the MMU is
 * enabled, and thus uses virtual addressing. It doesn't provide any fancy
 * formatting because at this point you can use printf(). Input is also
 * provided, though it busy-waits on the serial port and thus is highly
 * inefficient.
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

// provided by linker script
extern char __dbg_serial_virt__;

// This assumes that the UART has 8-bit registers which are mapped 4 bytes
// apart, which is common on ARM. The actual register definitions are
// 16550-compatible. Read a book.
#define REGISTER(addr) (*(volatile uint8_t *)(addr))
#define REG_RBR REGISTER(&__dbg_serial_virt__)
#define REG_THR REG_RBR
#define REG_LSR REGISTER(&__dbg_serial_virt__ + 0x14)
#define LSR_TDRQ (1<<5)
#define LSR_DR   (1<<0)

// spins waiting for transmit data request between each byte
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

// spins waiting for a byte, then returns it. Never actually returns more than
// one byte. It assumes incoming serial uses CR for line terminators and
// converts this to NL for C. It also echos the read character.
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

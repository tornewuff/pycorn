/*
 * 'Real' system access to generic 8250-style UART
 *
 * Should work for many ARM serial UARTs. This is to be used after the MMU is
 * enabled, and thus uses virtual addressing. It doesn't provide any fancy
 * formatting because at this point you can use printf(). Input is also
 * provided, though it busy-waits on the serial port and thus is highly
 * inefficient.
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

// provided by linker script
extern char __dbg_serial_virt__;

// This assumes that the UART has 8-bit registers which are mapped 4 bytes
// apart, which is common on ARM. The actual register definitions are
// 8250-compatible. Read a book.
#define REGISTER(addr) (*(volatile uint8_t *)(addr))
#define REG_RBR REGISTER(&__dbg_serial_virt__)
#define REG_THR REG_RBR
#define REG_LSR REGISTER(&__dbg_serial_virt__ + 0x14)
#define LSR_TDRQ (1<<5)
#define LSR_DR   (1<<0)

// spins waiting for transmit data request before writing the byte
void serial_write(char c)
{
  while((REG_LSR & LSR_TDRQ) == 0);
  REG_THR = c;
}

// spins waiting for a byte, then returns it.
char serial_read()
{
  while((REG_LSR & LSR_DR) == 0);
  return REG_RBR;
}

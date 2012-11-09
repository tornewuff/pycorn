/*
 * 'Real' system access to PL011 UART.
 *
 * This is to be used after the MMU is enabled, and thus uses virtual
 * addressing. It doesn't provide any fancy formatting because at this point
 * you can use printf(). Input is also provided, though it busy-waits on the
 * serial port and thus is highly inefficient.
 *
 *
 * Copyright 2012 Torne Wuff
 *
 * This file is part of Pycorn.
 *
 * Pycorn is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 */

#include <stdint.h>

// provided by linker script
extern char __dbg_serial_virt__;

#define REGISTER(addr) (*(volatile uint8_t *)(addr))
#define REG_DR REGISTER(&__dbg_serial_virt__)
#define REG_FR REGISTER(&__dbg_serial_virt__ + 0x18)
#define FR_TXFF (1<<5)
#define FR_RXFE (1<<4)

// spins waiting for transmit FIFO to not be full before writing the byte
void serial_write(char c)
{
  while(REG_FR & FR_TXFF);
  REG_DR = c;
}

// spins waiting for a byte, then returns it.
char serial_read()
{
  while(REG_FR & FR_RXFE);
  return REG_DR;
}

/*
 * Bootstrap debug output.
 *
 * This assumes that platform-specific code has provided a function to output a
 * byte to the serial port. This is to be used before the MMU is enabled and
 * thus uses physical addressing, as well as providing some conveniences for
 * debug in an environment where printf() is not yet ready. No support for
 * reading is provided as during bootstrapping we are at most printing debug
 * messages.
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
#include "bootstrap.h"

// Assumes the caller has passed the virtual address of a string from the .text
// segment, which we relocate to physical using the offset calculated at the
// start of the bootstrap. This cannot print non-constant strings!
void boot_putstr(const char *s)
{
  s -= bootdata->phys_to_virt;
  while (*s)
  {
    boot_putchar(*s++);
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
    boot_putchar(nibble);
  }
  if (label)
    boot_putchar('\n');
}

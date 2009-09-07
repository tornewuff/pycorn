/*
 * Implementation of read and write which map stdin/out/err to serial.
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

extern int serial_read(void *ptr, int len);
extern int serial_write(const void *ptr, int len);

int _read_r(void *r, int file, void *ptr, int len)
{
  if (file == 0)
    return serial_read(ptr, len);
	else
  	return -1;
}

int _write_r(void *r, int file, const void *ptr, int len)
{
  if (file >= 1 && file <= 2)
    return serial_write(ptr, len);
	else
  	return -1;
}

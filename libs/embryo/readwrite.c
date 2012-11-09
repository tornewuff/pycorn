/*
 * Implementations of read() and write() for stdin/out/err
 *
 * stdin/out/err are mapped to the debug serial port, provided by the platform
 * specific code. No other file descriptors exist.
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

#include <errno.h>
#include <reent.h>

// provided by platform specific code somewhere.
extern char serial_read(void);
extern void serial_write(char c);

// read from the serial debug port, only stdin exists.
// Never actually returns more than one byte. It assumes incoming serial uses
// CR for line terminators but will also work with CR/NL (it throws away NL).
// It also echos the read character.
_ssize_t _read_r(struct _reent *r, int file, void *ptr, size_t len)
{
  char *buf = ptr;
  if (file == 0)
  {
    if (len == 0)
      return 0;

    do
    {
      buf[0] = serial_read();
    } while (buf[0] == '\n');

    if (buf[0] == '\r')
      buf[0] = '\n';

    _write_r(r, 1, buf, 1);
    return 1;
  }
  else
  {
    r->_errno = EBADF;
    return -1;
  }
}

// write to the serial debug port, stdout/err handled identically.
_ssize_t _write_r(struct _reent *r, int file, const void *ptr, size_t len)
{
  int i;
  const char *buf = ptr;
  if (file >= 1 && file <= 2)
  {
    for (i = 0; i < len; ++i)
    {
      // Convert C-style \n into \r\n to work with the widest range of terminals
      if (buf[i] == '\n')
        serial_write('\r');
      serial_write(buf[i]);
    }
    return len;
  }
  else
  {
    r->_errno = EBADF;
    return -1;
  }
}

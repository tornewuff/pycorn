/*
 * Lots of newlib system calls which are just stubs.
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
#include <sys/stat.h>

int _close_r(struct _reent *r, int file)
{
  if (file >= 0 && file <= 2)
    return 0;
  r->_errno = EBADF;
  return -1;
}

void _exit(int rc)
{
  for (;;) {}
}

int _fstat_r(struct _reent *r, int file, struct stat *st)
{
  if (file >= 0 && file <= 2)
  {
    st->st_mode = S_IFCHR;
    return 0;
  }
  r->_errno = EBADF;
  return -1;
}

int _getpid_r(struct _reent *r)
{
  return 1;
}

int _isatty_r(struct _reent *r, int file)
{
  if (file >= 0 && file <= 2)
    return 1;
  else
  {
    r->_errno = EBADF;
    return -1;
  }
}

int _kill_r(struct _reent *r, int pid, int sig)
{
  r->_errno = ENOSYS;
  return -1;
}

_off_t _lseek_r(struct _reent *r, int file, _off_t ptr, int dir)
{
  if (file >= 0 && file <= 2)
    r->_errno = ESPIPE;
  else
    r->_errno = EBADF;
  return -1;
}

int _open_r(struct _reent *r, const char *name, int flags, int mode)
{
  r->_errno = ENOENT;
  return -1;
}

int _stat_r(struct _reent *r, const char *file, struct stat *st)
{
  r->_errno = ENOENT;
  return -1;
}

int _unlink_r(struct _reent *r, const char *name)
{
  r->_errno = ENOENT;
  return -1;
}

/*
 * Stub system calls.
 *
 * Newlib needs a reasonably complete set of POSIX-like syscalls. Most of them
 * are not needed in embryo as we don't support opening files. Stubs are
 * provided here to make it link.
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
#include <sys/stat.h>

// Doesn't really close stdin/out/err, but pretends it has.
int _close_r(struct _reent *r, int file)
{
  if (file >= 0 && file <= 2)
    return 0;
  r->_errno = EBADF;
  return -1;
}

// Spin forever. Can't do much better ;)
void _exit(int rc)
{
  for (;;) {}
}

// stdin/out/err are char devices, nothing else exists.
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

// POSIX says if you don't have processes you are pid 1
int _getpid_r(struct _reent *r)
{
  return 1;
}

// stdin/out/err are tty's (debug console), nothing else exists.
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

// No processes, no signals
int _kill_r(struct _reent *r, int pid, int sig)
{
  r->_errno = ENOSYS;
  return -1;
}

// stdin/out/err are not seekable, nothing else exists.
_off_t _lseek_r(struct _reent *r, int file, _off_t ptr, int dir)
{
  if (file >= 0 && file <= 2)
    r->_errno = ESPIPE;
  else
    r->_errno = EBADF;
  return -1;
}

// No files exist.
int _open_r(struct _reent *r, const char *name, int flags, int mode)
{
  r->_errno = ENOENT;
  return -1;
}

// No files exist.
int _stat_r(struct _reent *r, const char *file, struct stat *st)
{
  r->_errno = ENOENT;
  return -1;
}

// No files exist.
int _unlink_r(struct _reent *r, const char *name)
{
  r->_errno = ENOENT;
  return -1;
}

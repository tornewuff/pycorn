/*
 * Fairly dumb implementation of sbrk() using symbols defined by the linker
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

#include <stdio.h>
#include <errno.h>
#include <reent.h>

extern char __heap_start__;
extern char __heap_end__;

void *_sbrk_r(struct _reent *r, ptrdiff_t incr)
{
  static char *current;
  char *prev;

  if (current == NULL)
    current = &__heap_start__;

  if (current + incr >= &__heap_end__ || current + incr < &__heap_start__)
  {
    r->_errno = ENOMEM;
    return (void *) -1;
  }

  prev = current;
  current += incr;
  return (void *) prev;
}

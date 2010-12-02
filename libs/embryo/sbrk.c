/*
 * Really dumb sbrk() implementation using a fixed size pool of memory.
 *
 * The linker provides __heap_start__ and __heap_end__ which are the bounds of
 * our fixed size pool of memory. How we'll deal with this later is not decided
 * yet, for now this works (unless you try and sbrk() too much).
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

#include <stdlib.h>
#include <errno.h>
#include <reent.h>

// provided by link script
extern char __heap_start__;
extern char __heap_end__;

// Just use the fixed size heap.
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

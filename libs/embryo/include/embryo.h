/*
 * Boot data definitions, to be shared by bootstrap and app
 *
 * The bootdata structure is "passed" to the app (it's mapped at a fixed
 * virtual address). Some of the values here might conceivably be useless to
 * it, but that's ok. The app can free the page if it chooses; it is not used
 * again once main() is called.
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

#ifndef __BOOTDATA_H__
#define __BOOTDATA_H__

#include <stdint.h>

// Conveniences to make code clearer
typedef uint32_t physaddr;
typedef void *virtaddr;

#define _BOOTDATA_STRUCT \
  physaddr rom_base;       /* Physical address where the rom was loaded     */\
  uint32_t machtype;       /* Machine type, as per linux boot protocol      */\
  physaddr taglist_ptr;    /* Taglist pointer, as per linux boot protocol   */\
  uint32_t phys_to_virt;   /* Offset for phys->virt address of .text section*/\
  physaddr next_free_page; /* Next unallocated physical page address        */\
  physaddr page_directory; /* Physical address of page directory            */\
  uint32_t initrd_size;    /* Size of initrd in bytes, 0 for no initrd      */\
  physaddr initrd_phys;    /* Physical address of initrd (invalid if size=0)*/\
  virtaddr initrd_virt;    /* Virtual address of initrd (invalid if size=0) */\

// The bootdata structure
typedef struct
{
  // include from macro; this is the public version
  _BOOTDATA_STRUCT
} embryo_bootdata_t;

extern embryo_bootdata_t *embryo_bootdata;

#endif

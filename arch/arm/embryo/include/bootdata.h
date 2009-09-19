/*
 * Boot data definitions, to be shared by bootstrap and app
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
typedef uint32_t virtaddr;

// The bootdata structure
typedef struct
{
  physaddr rom_base;
  uint32_t machtype;
  physaddr taglist_ptr;
  uint32_t phys_to_virt;
  physaddr next_free_page;
  physaddr page_directory;
  physaddr initrd_phys;
  virtaddr initrd_virt;
  uint32_t initrd_size;
} bootdata_t;

// Linker-defined section symbols
extern char __text_start__, __text_end__, __data_start__, __data_end__;
extern char __bss_start__, __bss_end__, __heap_start__, __heap_end__;
extern char __stack_start__, __stack_end__, __page_dir_virt__;
extern char __dbg_serial_virt__, __dbg_serial_phys__;
extern char __page_tbl_start__, __page_tbl_end__;
extern char __bootdata_virt__, __initrd_map_start__;

// MMU constants
#define SECTION_SHIFT 20
#define SECTION_SIZE (1 << SECTION_SHIFT)
#define SECTION_MASK (SECTION_SIZE - 1)
#define PAGE_SHIFT 12
#define PAGE_SIZE (1 << PAGE_SHIFT)
#define PAGE_MASK (PAGE_SIZE - 1)
#define PTBLS_PER_PAGE 4
#define PAGEDIR_SIZE (PAGE_SIZE * PTBLS_PER_PAGE)
#define PAGETABLE_SIZE (PAGE_SIZE / PTBLS_PER_PAGE)

#define PAGEALIGN_DOWN(x) ((x>>PAGE_SHIFT)<<PAGE_SHIFT)
#define PAGEALIGN_UP(x) (((x+PAGE_MASK)>>PAGE_SHIFT)<<PAGE_SHIFT)

#endif

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
typedef uint32_t virtaddr;

// The bootdata structure
typedef struct
{
  physaddr rom_base;        // Physical address where the rom was loaded
  uint32_t machtype;        // Machine type, as per linux boot protocol
  physaddr taglist_ptr;     // Taglist pointer, as per linux boot protocol
  uint32_t phys_to_virt;    // Offset for phys->virt address of .text section
  physaddr next_free_page;  // Next unallocated physical page address
  physaddr page_directory;  // Physical address of page directory
  physaddr initrd_phys;     // Physical address of initrd (invalid if size=0)
  virtaddr initrd_virt;     // Virtual address of initrd (invalid if size=0)
  uint32_t initrd_size;     // Size of initrd in bytes, 0 for no initrd
} bootdata_t;

// Linker-defined section symbols
extern char __text_start__, __text_end__, __data_start__, __data_end__;
extern char __bss_start__, __bss_end__, __heap_start__, __heap_end__;
extern char __stack_start__, __stack_end__, __page_dir_virt__;
extern char __dbg_serial_virt__, __dbg_serial_phys__;
extern char __page_tbl_start__, __page_tbl_end__;
extern char __bootdata_virt__, __initrd_map_start__;

// MMU constants for ARM. No tiny pages or similar legacy weirdness please.
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

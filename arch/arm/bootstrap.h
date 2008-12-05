/*
 * Bootstrap declarations
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

#ifndef __BOOTSTRAP_H__
#define __BOOTSTRAP_H__

#include <stdint.h>

typedef uint32_t physaddr;
typedef uint32_t virtaddr;

typedef struct
{
  physaddr rom_base;
  uint32_t machtype;
  physaddr taglist_ptr;
  uint32_t phys_to_virt;
  physaddr next_free_page;
  physaddr next_free_pagetable;
  physaddr page_directory;
} bootdata_t;

register bootdata_t *bootdata asm ("r9");

extern void boot_putchar(char c);
extern void boot_putstr(const char *s);
extern void boot_putint(const char *label, uint32_t i);

#define DBGCHAR(c) boot_putchar(c)
#define DBGSTR(s) boot_putstr(s)
#define DBGINT(l, i) boot_putint(l, i)

extern physaddr alloc_pages_zero(uint32_t bytes, uint32_t align);
extern void map_pages(virtaddr virt_start, virtaddr virt_end,
        physaddr phys_start);

extern void mmu_set_base(physaddr page_directory);
typedef void (*mmu_done_func)(int selfmap_index, uint32_t old_pde);
typedef void (*mmu_enable_func)(int selfmap_index, uint32_t old_pde,
    mmu_done_func next_func);
extern void mmu_enable(int selfmap_index, uint32_t old_pde,
    mmu_done_func next_func);
extern void mmu_invalidate_tlb();

extern void boot_after_mmu(int selfmap_index, uint32_t old_pde);
extern void _mainCRTStartup(void);

#endif

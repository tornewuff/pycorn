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
  physaddr next_free_pagetable;
  physaddr page_directory;
  virtaddr next_free_ptbl_virt;
} bootdata_t;

// Bootdata structure pointer lives in a register
register bootdata_t *bootdata asm ("r9");

// Linker-defined section symbols
extern char __text_start__, __text_end__, __data_start__, __data_end__;
extern char __bss_start__, __bss_end__, __heap_start__, __heap_end__;
extern char __stack_start__, __stack_end__, __page_dir_virt__;
extern char __dbg_serial_virt__, __dbg_serial_phys__;
extern char __page_table_virt__;

// MMU constants
#define SECTION_SHIFT 20
#define SECTION_SIZE (1 << SECTION_SHIFT)
#define SECTION_MASK (SECTION_SIZE - 1)
#define PAGE_SHIFT 12
#define PAGE_SIZE (1 << PAGE_SHIFT)
#define PAGE_MASK (PAGE_SIZE - 1)
#define PAGEDIR_SIZE (PAGE_SIZE * 4)
#define PAGETABLE_SIZE (PAGE_SIZE / 4)

// Pre-MMU debug printing
extern void boot_putchar(char c);
extern void boot_putstr(const char *s);
extern void boot_putint(const char *label, uint32_t i);
#define DBGCHAR(c) boot_putchar(c)
#define DBGSTR(s) boot_putstr(s)
#define DBGINT(l, i) boot_putint(l, i)

// Typedefs for functions involved in MMU enabling
typedef void (*mmu_done_func)(int selfmap_index, uint32_t old_pde);
typedef void (*mmu_enable_func)(int selfmap_index, uint32_t old_pde,
    mmu_done_func next_func);

// Functions defined in assembly
extern void _start(void);
extern void mmu_set_base(physaddr page_directory);
extern void mmu_enable(int selfmap_index, uint32_t old_pde,
    mmu_done_func next_func);
extern void mmu_invalidate_tlb(void);

// crt0 function
extern void _mainCRTStartup(void) __attribute__((noreturn));

#endif

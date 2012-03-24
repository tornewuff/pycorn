/*
 * Bootstrap declarations.
 *
 * This should not be used by the app, but *everything* in the bootstrap
 * *MUST* include this file, as it contains the bootdata register declaration
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

#ifndef __BOOTSTRAP_H__
#define __BOOTSTRAP_H__

#include <stdint.h>
#include <embryo.h>

// The private bootdata structure
typedef struct {
    // include from macro with public version
  _BOOTDATA_STRUCT
  
} bootdata_t;

// Bootdata structure pointer lives in a register
register bootdata_t *bootdata asm ("r9");

// Pre-MMU debug printing - wrapped in macros so they can be disabled
extern void boot_putchar(char c);
extern void boot_putstr(const char *s);
extern void boot_putint(const char *label, uint32_t i);

#ifdef DEBUG_EMBRYO
#define DBGCHAR(c) boot_putchar(c)
#define DBGSTR(s) boot_putstr(s)
#define DBGINT(l, i) boot_putint(l, i)
#else
#define DBGCHAR(c)
#define DBGSTR(s)
#define DBGINT(l, i)
#endif

// Typedefs for functions involved in MMU enabling
typedef void (*mmu_done_func)(int selfmap_index, uint32_t old_pde);
typedef void (*mmu_enable_func)(int selfmap_index, uint32_t old_pde,
    mmu_done_func next_func);

// Functions defined in start.S
extern void _start(void);
extern void mmu_set_base(physaddr page_directory);
extern void mmu_enable(int selfmap_index, uint32_t old_pde,
    mmu_done_func next_func);
extern void mmu_invalidate_tlb(void);

// crt0 function
extern void _mainCRTStartup(void) __attribute__((noreturn));

// memory functions
physaddr alloc_pages_zero(uint32_t bytes, uint32_t align);
physaddr get_page_table(int section_index, int skip_map);
void map_pages(virtaddr virt_start, virtaddr virt_end, physaddr phys_start);

// atag parsing
extern int parse_atags(void);

// Linker-defined section symbols
extern char __text_start__, __text_end__, __data_start__, __data_end__;
extern char __bss_start__, __bss_end__, __heap_start__, __heap_end__;
extern char __stack_start__, __stack_end__, __page_dir_virt__;
extern char __dbg_serial_virt__, __dbg_serial_phys__;
extern char __page_tbl_start__, __page_tbl_end__;
extern char __bootdata_virt__, __initrd_map_start__;
extern char __exc_stack_start__, __exc_stack_end__, __vectors__;

#endif

/*
 * Bootstrapping code, forming the .start section.
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

#include "macros.inc"

.macro CPWAIT r:req
    mrc     p15, 0, \r, c2, c0, 0
    mov     \r, \r
    sub     pc, pc, #4
.endm

    .section .start,"ax",%progbits
    .code 32
    .align 2

    /* Register assignments throughout .start:
     *
     * r8   physical addr of next free page table (0 if none allocated)
     * r9   physical base of page directory
     * r10  next free physical page
     * r11  phys->virt offset
     * sp   one stack entry
     * lr   return address (duh)
     */

    FUNC_START  _start
    // Set up r11 as phys->virt offset
    sub     r3, pc, #8
    ldr     r11, =_start
    sub     r11, r11, r3

    // Save the parameters passed by the bootloader
    cmp     r0, #0
    bne     1f
    .weak   machtype
    ldr     r0, =machtype
    cmp     r0, #0
    sub     r0, r0, r11
    strne   r1, [r0]
    .weak   taglist_ptr
    ldr     r0, =taglist_ptr
    cmp     r0, #0
    sub     r0, r0, r11
    strne   r2, [r0]
1:  // r0-r2 are now free to overwrite

    DEBUGMSG    Bootstrap entered.
    DEBUGREG    r11 phys->virt offset

    // Set up r10 as the next free physical page after the loaded image
    ldr     r0, =__text_end__
    sub     r10, r0, r11
    ldr     r0, =__data_start__
    ldr     r1, =__data_end__
    sub     r0, r1, r0
    add     r10, r10, r0
    DEBUGREG    r10 first free physaddr

    // Set up r8 as 0, as there are no page table pages allocated yet
    mov     r8, #0

    // Allocate a page directory and set it to r9 and TTBR
    // Map it into the virtual address space
    mov     r0, #0x4000
    mov     r1, #0x4000
    bl      BootPageAllocZero
    mov     r9, r0
    mcr     p15, 0, r9, c2, c0, 0
    DEBUGREG    r9 physaddr of pagedir
    ldr     r4, =0xf4000000
    add     r5, r4, #0x4000
    orr     r6, r9, #0x3f // AP 3, (R/W), cache/buffer
    bl      BootMapPages

    // Map the text section of the rom
    DEBUGMSG    Mapping text section...
    ldr     r4, =__text_start__
    ldr     r5, =__text_end__
    sub     r6, r4, r11
    orr     r6, #0xf // AP 0 (ROM), cache/buffer
    bl      BootMapPages

    // Map the data section of the rom
    DEBUGMSG    Mapping data section...
    ldr     r4, =__data_start__
    ldr     r5, =__data_end__
    ldr     r6, =__text_end__
    sub     r6, r6, r11
    orr     r6, #0x3f // AP 3 (R/W), cache/buffer
    bl      BootMapPages

    // Allocate and map the bss section
    DEBUGMSG    Allocating bss section...
    ldr     r4, =__bss_start__
    ldr     r5, =__bss_end__
    mov     r0, #0x1000
    sub     r1, r5, r4
    bl      BootPageAllocZero
    orr     r6, r0, #0x3f // AP 3 (R/W), cache/buffer
    DEBUGMSG    Mapping bss section...
    bl      BootMapPages

    // Allocate and map the heap section
    DEBUGMSG    Allocating heap section...
    ldr     r4, =__heap_start__
    ldr     r5, =__heap_end__
    mov     r0, #0x1000
    sub     r1, r5, r4
    bl      BootPageAllocZero
    orr     r6, r0, #0x3f // AP 3 (R/W), cache/buffer
    DEBUGMSG    Mapping heap section...
    bl      BootMapPages

    // Allocate and map the stack section
    DEBUGMSG    Allocating stack section...
    ldr     r4, =__stack_start__
    ldr     r5, =__stack_end__
    mov     r0, #0x1000
    sub     r1, r5, r4
    bl      BootPageAllocZero
    orr     r6, r0, #0x3f // AP 3 (R/W), cache/buffer
    DEBUGMSG    Mapping stack section...
    bl      BootMapPages

    // Map the debug UART
    DEBUGMSG    Mapping debug UART...
    ldr     r4, =0x40100000
    ldr     r5, =0x40101000
    orr     r6, r4, #0x33 // AP 3 (R/W), uncached/unbuffered
    bl      BootMapPages

    // Load a stack pointer
    ldr     sp, =__stack_end__
    DEBUGREG    sp initial stack pointer

    // Self-map the section containing this code
    // r4 gets page directory index, r5 gets old value
    DEBUGMSG    Self-mapping MMU enable code...
    mov     r4, pc, lsr #20
    ldr     r5, [r9, r4, lsl #2]
    mov     r0, r4, lsl #20
    orr     r0, #0xe  // section mapping, AP 0 (ROM), cache/buffer
    str     r0, [r9, r4, lsl #2]

    // Enable MMU
    DEBUGMSG    Enabling MMU...
    mov     r0, #0x1
    mcr     p15, 0, r0, c3, c0, 0       // set DACR, client of domain 0
    mrc     p15, 0, r0, c1, c0, 0       // get control register
    orr     r0, #0x1                    // mmu enable bit
    orr     r0, #0x200                  // ROM permission bit
    mcr     p15, 0, r0, c1, c0, 0       // set control register
    CPWAIT  r0
    
    // Branch to virtual mapped version of this code
    ldr     pc, =govirtual
govirtual:
    
    // Switch page directory to virtual address and revert self-mapping
    DEBUGMSG    Removing self-mapping...
    mov     r9, #0xf4000000
    str     r4, [r9, r4, lsl #2]
    mov     r0, #0
    mcr     p15, 0, r0, c8, c7, 0       // invalidate entire TLB
    CPWAIT  r0

    DEBUGMSG    Leaving bootstrap for CRT entry point.

    // Branch to the C library entry point
    b       _mainCRTStartup



    /* Allocate pages following the ROM image and zeros them
     *
     * r0: align bits clear (bytes, page aligned)
     * r1: alloc size (bytes, page aligned)
     * returns r0: phys addr of allocation
     * destroys r1-r3,ip,sp
     */
    FUNC_START BootPageAllocZero
    mov     sp, lr
    sub     r0, r0, #1
    add     r10, r10, r0
    bic     r0, r10, r0
    add     r10, r0, r1
    mov     r1, #0
    mov     r2, #0
    mov     r3, #0
    mov     ip, #0
    mov     lr, r0
2:  stmia   lr!, {r1-r3,ip}
    stmia   lr!, {r1-r3,ip}
    stmia   lr!, {r1-r3,ip}
    stmia   lr!, {r1-r3,ip}
    cmp     lr, r10
    blo     2b
    mov     pc, sp



    /* Map contiguous pages as extended small pages
     *
     * r4: start virtual address (bytes, page aligned)
     * r5: end virtual address (bytes, page aligned)
     * r6: first physical PTE (address, map attribs, type)
     * destroys r0-r7,ip,sp
     */
    FUNC_START BootMapPages
    mov     sp, lr
    DEBUGMSG    >BootMapPages
    DEBUGREG    r4 start virtual address
    DEBUGREG    r5 end virtual address
    DEBUGREG    r6 first PTE
    subs    r5, r5, r4              // r5 = number of bytes to map
    beq     6f                      // nothing to do?
    mov     r7, r4, lsr #20         // r7 = section index of first vaddr
    mov     r4, r4, lsl #12         // r4 = page index of first vaddr << 24
7:  ldr     r0, [r9, r7, lsl #2]    // r0 = existing page directory entry
    cmp     r0, #0
    bne     4f                      // page table already exists?
    movs    r0, r8
    bne     3f
    mov     r8, sp
    mov     r0, #0x1000
    mov     r1, #0x1000
    bl      BootPageAllocZero
    mov     sp, r8
    mov     r8, r0
3:  add     r8, r8, #0x400
    tst     r8, #0xc00
    moveq   r8, #0
    orr     r0, r0, #0x1
    str     r0, [r9, r7, lsl #2]    // store page directory entry
4:  bic     r0, r0, #0x1            // r0 = page table paddr
5:  str     r6, [r0, r4, lsr #22]   // store page table entry
    subs    r5, r5, #0x1000         // decrement bytes to map
    beq     6f                      // are we done?
    add     r6, r6, #0x1000         // increment paddr
    adds    r4, r4, #0x1000000      // increment second level index
    bne     5b                      // not done with this page table?
    add     r7, r7, #0x1            // increment section index
    b       7b
6:  DEBUGMSG    <BootMapPages
    mov     pc, sp



    .align 2
    .pool

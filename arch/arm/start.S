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

.macro FUNC_START name
    .global \name
\name:
.endm

.macro CPWAIT r:req
    mrc     p15, 0, \r, c2, c0, 0
    mov     \r, \r
    sub     pc, pc, #4
.endm

    .section .start,"ax",%progbits
    .code 32
    .align 2

    FUNC_START  _start
    // On entry, r0 is 0, r1 is the machine type, r2 is the taglist ptr
    // We use the 16kb preceding _start for temp space as the linux
    // boot protocol promises it will be there
    sub     r3, pc, #8

    // Set up r9 as the boot data struct pointer (start of the block)
    sub     r9, r3, #0x4000
    str     r3, [r9]

    // Save the parameters passed by the bootloader
    cmp     r0, #0
    movne   r1, #-1
    movne   r2, #-1
    str     r1, [r9, #4]
    str     r2, [r9, #8]

    // r0-r2 are now free to overwrite
    // Work out phys->virt offset
    ldr     r0, =_start
    sub     r0, r3
    str     r0, [r9, #12]

    // Set temporary boot stack starting at end of the block
    mov     sp, r3

    // Branch to C bootstrap function
    b       boot_start


    FUNC_START  mmu_set_base
    mcr     p15, 0, r0, c2, c0, 0
    mov     pc, lr


    FUNC_START  mmu_enable
    mov     lr, r2
    ldr     r2, [r9, #4]                // retrieve machtype
    ldr     r3, [r9, #8]                // retrieve taglist_ptr
    mov     ip, #0x1
    mcr     p15, 0, ip, c3, c0, 0       // set DACR, client of domain 0
    mrc     p15, 0, ip, c1, c0, 0       // get control register
    orr     ip, #0x1                    // mmu enable bit
    orr     ip, #0x200                  // ROM permission bit
    mcr     p15, 0, ip, c1, c0, 0       // set control register
    CPWAIT  ip                          // after this the MMU is on
    ldr     ip, =machtype
    cmp     ip, #0
    strne   r2, [ip]                    // copy machtype to .data
    ldr     ip, =taglist_ptr
    cmp     ip, #0
    strne   r3, [ip]                    // copy taglist_ptr to .data
    ldr     sp, =__stack_end__
    mov     pc, lr                      // branch to next func, virtual
    .weak   machtype
    .weak   taglist_ptr
    

    FUNC_START  mmu_invalidate_tlb
    mov     r0, #0
    mcr     p15, 0, r0, c8, c7, 0
    CPWAIT  r0
    mov     pc, lr


    .align 2
    .pool

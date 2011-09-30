/*
 * The actual bootstrap entry point and related.
 *
 * This bootstrapping code forms the .start section, so it can be placed at the
 * start of the image. The _start function is here, as are some MMU funcs which
 * must be implemented in assembly.
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

    // Convenience to declare global functions
.macro FUNC_START name
    .global \name
\name:
.endm

    // CPWAIT is an operation defined in the XScale manual
    // It serves as a barrier for CP15 operations.
    // It needs a spare register.
.macro CPWAIT r:req
    mrc     p15, 0, \r, c2, c0, 0
    mov     \r, \r
    sub     pc, pc, #4
.endm

    .section .start,"ax",%progbits
    .code 32
    .align 2

    // On entry, r0 is 0, r1 is the machine type, r2 is the taglist ptr
    // We use the 16kb preceding _start for temp space as the linux
    // boot protocol promises it will be there, at least for non-xip

    FUNC_START  _start

    // Get the physical address of this instruction (the base of the rom)
    sub     r3, pc, #8

    // Set up r9 as the struct bootdata_t pointer (start of the 16kb block)
    sub     r9, r3, #0x4000

    // Set bootdata->rom_base
    str     r3, [r9]

    // If the bootloader really did give us r0=0 as we expect, it's *hopefully*
    // following the linux boot protocol and we can set bootdata->machtype and
    // bootdata->taglist_ptr
    cmp     r0, #0
    movne   r1, #-1
    movne   r2, #-1
    str     r1, [r9, #4]
    str     r2, [r9, #8]

    // Set temporary boot stack starting at end of the block
    mov     sp, r3

    // Branch to C bootstrap function
    b       boot_start


    // Set the page directory physaddr to r0
    FUNC_START  mmu_set_base
    mcr     p15, 0, r0, c2, c0, 0
    mov     pc, lr


    // Program cp15 to enable the MMU. Passes r0/r1 through.
    // r2 must be the virtual address of the function to call next
    // (with r0/r1 as arguments)
    // This code must be mapped at the same phys/virt addresses, or
    // demons will fly out of your nose.
    FUNC_START  mmu_enable
    mov     ip, #0x1
    mcr     p15, 0, ip, c3, c0, 0       // set DACR, client of domain 0
    mrc     p15, 0, ip, c1, c0, 0       // get control register
    orr     ip, #0x1                    // mmu enable bit
    orr     ip, #0x200                  // ROM permission bit
    mcr     p15, 0, ip, c1, c0, 0       // set control register
    CPWAIT  ip                          // after this the MMU is on
    ldr     r9, =__bootdata_virt__      // switch bootdata pointer
    ldr     r3, =__exc_stack_end__      // load exception stack address
    mov     ip, #0xd1
    msr     cpsr_c, ip
    mov     sp, r3                      // set fiq stack
    mov     ip, #0xd2
    msr     cpsr_c, ip
    mov     sp, r3                      // set irq stack
    mov     ip, #0xd3
    msr     cpsr_c, ip
    mov     sp, r3                      // set svc stack
    mov     ip, #0xd7
    msr     cpsr_c, ip
    mov     sp, r3                      // set abt stack
    mov     ip, #0xdb
    msr     cpsr_c, ip
    mov     sp, r3                      // set und stack
    mov     ip, #0xdf
    msr     cpsr_c, ip                  // enter system mode for execution
    ldr     sp, =__stack_end__          // load virtual address for stack
    mov     pc, r2                      // branch to next func, virtual
    

    // Invalidate the whole TLB. Use after changing mappings.
    FUNC_START  mmu_invalidate_tlb
    mov     r0, #0
    mcr     p15, 0, r0, c8, c7, 0
    CPWAIT  r0
    mov     pc, lr


    .align 2
    .pool

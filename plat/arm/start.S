/*
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

    .section .start,"ax",%progbits
    .code 32
    .align 2

    FUNC_START  _start
    /* Save the parameters passed by the bootloader */
    cmp     r0, #0
    bne     1f
    ldr     r0, .Lmachtype
    cmp     r0, #0
    strne   r1, [r0]
    ldr     r0, .Ltaglist
    cmp     r0, #0
    strne   r2, [r0]

1:
    /* Load a stack pointer */
    ldr     sp, .Lstack

    /* Branch to the C library entry point */
    b       _mainCRTStartup

    .align 2
.Lstack:    
    .word   __stack_end__
.Lmachtype:
    .word   machtype
    .weak   machtype
.Ltaglist:
    .word   taglist_ptr
    .weak   taglist_ptr

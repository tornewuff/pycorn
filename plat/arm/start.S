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

.macro DEBUGMSG str:vararg
    adr     lr, 9f
    mov     r0, pc
    b       BootPutStr
    .asciz  "\str\n"
    .align  2
9:
.endm

.macro DEBUGREG r:req, str:vararg
    adr     lr, 9f
    mov     r0, pc
    b       BootPutStr
    .asciz  "\str: "
    .align  2
9:  mov     r0, \r
    bl      BootPutInt
    mov     r0, #'\n
    bl      BootPutChar
.endm

    .section .start,"ax",%progbits
    .code 32
    .align 2

    FUNC_START  _start
    // Save the parameters passed by the bootloader
    cmp     r0, #0
    bne     1f
    .weak   machtype
    ldr     r0, =machtype
    cmp     r0, #0
    strne   r1, [r0]
    .weak   taglist_ptr
    ldr     r0, =taglist_ptr
    cmp     r0, #0
    strne   r2, [r0]
1:  // r0-r2 are now free to overwrite

    DEBUGMSG    Bootstrap entered.

    // Load a stack pointer
    ldr     sp, =__stack

    DEBUGMSG    Leaving bootstrap for CRT entry point.

    // Branch to the C library entry point
    b       _mainCRTStartup

    .align 2
    .pool

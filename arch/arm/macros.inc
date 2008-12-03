/*
 * Various handy macros for ARM assembly code
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

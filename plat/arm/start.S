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
    ldr sp, .Lstack
    b   _mainCRTStartup

    .align 2
.Lstack:    
    .word   __stack

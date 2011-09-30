/*
 * The implementations of the exception vectors.
 *
 * The actual vector table is in _start.
 *
 *
 * Copyright 2011 Torne Wuff
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

    .text
    .code 32
    .align 2

    FUNC_START  _handle_undef
    FUNC_START  _handle_swi
    FUNC_START  _handle_prefetch_abort
    FUNC_START  _handle_irq
    FUNC_START  _handle_fiq

    sub     lr, lr, #4
    b       _exception_crash

    FUNC_START _handle_data_abort

    sub     lr, lr, #8

_exception_crash:
    sub     sp, sp, #72
    stmia   sp, {r0-r14}^
    mov     r0, sp
    add     ip, sp, #60
    mov     r1, lr
    mrs     r2, spsr
    mrs     r3, cpsr
    stmia   ip, {r1-r3}
    b       unexpected_exception

    .align 2
    .pool

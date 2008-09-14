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

    .text
    .code 32
    .align 2

    FUNC_START  _mainCRTStartup
    /* Annotation for EABI unwinding tables.  */
    .fnstart

    /* Zero the memory in the .bss section.  */
    mov a2, #0          /* Second arg: fill value */
    mov fp, a2          /* Null frame pointer */
    mov r7, a2          /* Null frame pointer for Thumb */
    ldr a1, .LC1        /* First arg: start of memory block */
    ldr a3, .LC2    
    sub a3, a3, a1      /* Third arg: length of block */
    bl  memset

    ldr r0, .Lfini
    bl  atexit

    bl  __libc_init_array

    mov r0, #0      /*  no arguments  */
    mov r1, #0      /*  no argv either */
    bl  main

    bl  exit        /* Should not return.  */

    /* Protect against unhandled exceptions.  */
    .cantunwind
    .fnend

    .align 2
.LC1:
    .word   __bss_start__
.LC2:
    .word   __bss_end__
.Lfini:
    .word   __libc_fini_array

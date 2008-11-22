/*
 * Generic memory mapped serial port, 8250/16550-like
 * Should work for many ARM serial UARTs.
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

    .text
    .code 32
    .align 2


    /* Print a null terminated string on the debug serial port
     *
     * r0: pointer to string
     * destroys r0-r3
     */
    FUNC_START  BootPutStr
    ldr     r1, =__dbg_serial_phys__
1:  ldrb    r3, [r0], #1
    cmp     r3, #0
    beq     3f
2:  ldr     r2, [r1, #0x14]
    tst     r2, #0x20
    beq     2b
    str     r3, [r1]
    b       1b
3:  mov     pc, lr


    /* Print a character on the debug serial port
     *
     * r0: character to print
     * destroys r1-r2
     */
    FUNC_START  BootPutChar
    ldr     r1, =__dbg_serial_phys__
2:  ldr     r2, [r1, #0x14]
    tst     r2, #0x20
    beq     2b
    str     r0, [r1]
    mov     pc, lr


    /* Print a 32-bit integer as hex on the debug serial port
     *
     * r0: integer to print
     * destroys r1-r3,ip
     */
    FUNC_START  BootPutInt
    ldr     r1, =__dbg_serial_phys__
    mov     ip, #28
1:  mov     r3, r0, ror ip
    and     r3, r3, #0xf
    cmp     r3, #10
    addlo   r3, r3, #'0
    addhs   r3, r3, #('a - 10)
2:  ldr     r2, [r1, #0x14]
    tst     r2, #0x20
    beq     2b
    str     r3, [r1]
    subs    ip, ip, #4
    bhs     1b
    mov     pc, lr


    .align 2
    .pool

/*
 * C library entry point. Expects a normal C environment (valid stack)
 * then goes on and initialises newlib stuff, then calls main.
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

#include <stdlib.h>
#include <string.h>

extern char __bss_start__, __bss_end__;
extern int main(int argc, char *argv[]);
extern void __libc_init_array(void);
extern void __libc_fini_array(void);

void _mainCRTStartup(void)
{
    size_t bss_size = &__bss_end__ - &__bss_start__;
    memset(&__bss_start__, 0, bss_size);

    atexit(__libc_fini_array);
    __libc_init_array();

    exit(main(0, 0));
}

// Define an empty _init and _fini since arm-eabi-gcc never generates
// code into them, but newlib expects them to exist.
void _init(void)
{
}

void _fini(void)
{
}

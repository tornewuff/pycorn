/*
 * C library entry point. Expects a normal C environment (valid stack)
 * then goes on and initialises newlib stuff, then calls main.
 *
 * This is a drastically cut down equivalent of newlib's normal entry point.
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

#include <stdlib.h>
#include <string.h>

// Linker provided symbols
extern char __bss_start__, __bss_end__;
extern void __libc_init_array(void);
extern void __libc_fini_array(void);

// Our main function! Hooray!
extern int main(int argc, char *argv[]);

// Declare that we don't return.
void _mainCRTStartup(void) __attribute__((noreturn));

void _mainCRTStartup(void)
{
    // Clear .bss
    size_t bss_size = &__bss_end__ - &__bss_start__;
    memset(&__bss_start__, 0, bss_size);

    // register destructors to be called at exit time
    atexit(__libc_fini_array);
    // call constructors
    __libc_init_array();

    // exit, returning whatever main returns. We have no args.
    exit(main(0, 0));
}

// Define an empty _init and _fini since arm-eabi-gcc never generates
// code into them, but newlib expects them to exist. Silly newlib.
void _init(void)
{
}

void _fini(void)
{
}

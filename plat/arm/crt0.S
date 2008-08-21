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
	.align	0

	FUNC_START	_start
	b	_mainCRTStartup


/* .text is used instead of .section .text so it works with arm-aout too.  */
	.text
	.code 32
	.align 	0

	FUNC_START	_mainCRTStartup
	/* Annotation for EABI unwinding tables.  */
	.fnstart

/* Start by setting up a stack */
	/*  Set up the stack pointer to a fixed value */
	/*  Changes by toralf:
	    - Go through all execution modes and set up stack for each of them.
	      Loosely based on init.s from ARM/Motorola example code.
              Note: Mode switch via CPSR is not allowed once in non-privileged
		    mode, so we take care not to enter "User" to set up its sp,
		    and also skip most operations if already in that mode. */

	ldr	r3, .Lstack
	/* Note: This 'mov' is essential when starting in User, and ensures we
		 always get *some* sp value for the initial mode, even if we 
		 have somehow missed it below (in which case it gets the same
		 value as FIQ - not ideal, but better than nothing.) */
	mov	sp, r3
	mrs	r2, CPSR
	tst	r2, #0x0F	/* Test mode bits - in User of all are 0 */
	beq	.LC23		/* "eq" means r2 AND #0x0F is 0 */
	msr     CPSR_c, #0xD1	/* FIRQ mode, interrupts disabled */
	mov 	sp, r3
	sub	sl, sp, #0x1000	/* This mode also has its own sl (see below) */
	
	mov	r3, sl	
	msr     CPSR_c, #0xD7	/* Abort mode, interrupts disabled */
	mov	sp, r3
	sub	r3, r3, #0x1000

	msr     CPSR_c, #0xDB	/* Undefined mode, interrupts disabled */
	mov	sp, r3
	sub	r3, r3, #0x1000

	msr     CPSR_c, #0xD2	/* IRQ mode, interrupts disabled */
	mov	sp, r3
	sub	r3, r3, #0x2000
		
	msr     CPSR_c, #0xD3	/* Supervisory mode, interrupts disabled */
	mov	sp, r3
	sub	r3, r3, #0x8000	/* Min size 32k */
	bic	r3, r3, #0x00FF	/* Align with current 64k block */
	bic	r3, r3, #0xFF00

	str	r3, [r3, #-4]	/* Move value into user mode sp without */ 
	ldmdb	r3, {sp}^       /* changing modes, via '^' form of ldm */ 
	
	orr	r2, r2, #0xC0	/* Back to original mode, presumably SVC, */
	msr	CPSR_c, r2	/* with FIQ/IRQ disable bits forced to 1 */
.LC23:
	/* Setup a default stack-limit in-case the code has been
	   compiled with "-mapcs-stack-check".  Hard-wiring this value
	   is not ideal, since there is currently no support for
	   checking that the heap and stack have not collided, or that
	   this default 64k is enough for the program being executed.
	   However, it ensures that this simple crt0 world will not
	   immediately cause an overflow event:  */
	sub	sl, r3, #64 << 10	/* Still assumes 256bytes below sl */
	/* Zero the memory in the .bss section.  */
	mov 	a2, #0			/* Second arg: fill value */
	mov	fp, a2			/* Null frame pointer */
	mov	r7, a2			/* Null frame pointer for Thumb */
	ldr	a1, .LC1		/* First arg: start of memory block */
	ldr	a3, .LC2	
	sub	a3, a3, a1		/* Third arg: length of block */
	bl	memset

	mov	r0, #0		/*  no arguments  */
	mov	r1, #0		/*  no argv either */

	/* Some arm/elf targets use the .init and .fini sections
	   to create constructors and destructors, and for these
	   targets we need to call the _init function and arrange
	   for _fini to be called at program exit.  */
	mov	r4, r0
	mov	r5, r1
	ldr	r0, .Lfini
	bl	atexit
	bl	__libc_init_array
	mov	r0, r4
	mov	r1, r5
	bl	main

	bl	exit		/* Should not return.  */

	
	/* For Thumb, constants must be after the code since only 
	   positive offsets are supported for PC relative addresses.  */
	
	.align 0
.Lstack:	
	.word	__stack

	/* Protect against unhandled exceptions.  */
	.cantunwind
	.fnend
.LC1:
	.word	__bss_start__
.LC2:
	.word	__bss_end__
.Lfini:
	.word	__libc_fini_array
	

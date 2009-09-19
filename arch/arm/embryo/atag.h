/*
 * ATAG format
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

#ifndef __ATAG_H__
#define __ATAG_H__

#include <stdint.h>

/* The list ends with an ATAG_NONE node. */
#define ATAG_NONE   0x00000000

struct tag_header {
    uint32_t size;
    uint32_t tag;
};

/* The list must start with an ATAG_CORE node */
#define ATAG_CORE   0x54410001

struct tag_core {
    uint32_t flags;     /* bit 0 = read-only */
    uint32_t pagesize;
    uint32_t rootdev;
};

/* it is allowed to have multiple ATAG_MEM nodes */
#define ATAG_MEM    0x54410002

struct tag_mem32 {
    uint32_t    size;
    uint32_t    start;  /* physical start address */
};

/* describes where the compressed ramdisk image lives (physical address) */
#define ATAG_INITRD2    0x54420005

struct tag_initrd {
    uint32_t start; /* physical start address */
    uint32_t size;  /* size of compressed ramdisk image in bytes */
};

/* command line: \0 terminated string */
#define ATAG_CMDLINE    0x54410009

struct tag_cmdline {
    char    cmdline[1]; /* this is the minimum size */
};

struct tag {
    struct tag_header hdr;
    union {
        struct tag_core     core;
        struct tag_mem32    mem;
        struct tag_initrd   initrd;
        struct tag_cmdline  cmdline;
    } u;
};

#define tag_next(t) ((struct tag *)((uint32_t *)(t) + (t)->hdr.size))
#define tag_size(type)  ((sizeof(struct tag_header) + sizeof(struct type)) >> 2)

#define for_each_tag(t,base)        \
    for (t = base; t->hdr.size; t = tag_next(t))

#endif

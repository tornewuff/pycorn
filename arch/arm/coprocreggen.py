#!/usr/bin/python
#
# Tool to generate assembly code used to access ARM coprocessor registers.
# 
# ARM coprocessor register accesses are all separate instruction encodings, so
# this is the easiest way to make them accessible. It reads the list of
# registers from the first argument, and writes out an assembly file to the
# filename given in the second argument. The assembly code is just a huge
# switch statement, which given an index 0..n reads/writes that coprocessor
# register; the index numbers are not expected to be stable, they are just the
# order of the list. The number of registers in each list is written out as a C
# header to the file named by the third argument (which is used for bounds
# checking). It writes out a mapping to the Python module named by the
# fourth argument.
# 
#
# Copyright 2008 Torne Wuff
#
# This file is part of Pycorn.
#
# Pycorn is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#

import sys

read = []
write = []

lineno = 1

# build up list of which registers need to exist, for read and write
infile = open(sys.argv[1])
for line in infile:
    hash = line.find('#')
    if hash >= 0:
        line = line[:hash]
    fields = line.split()
    if not fields:
        continue
    if len(fields) < 7:
        sys.exit('Too few fields on line %s' % lineno)
    rw = fields[5]
    fields[5:] = [' '.join(fields[6:])]
    if 'r' in rw:
        read.append(fields)
    if 'w' in rw:
        write.append(fields)
    lineno += 1
infile.close()

# output two giant asm functions containing a computed jump to the right
# generated instruction
asmfile = open(sys.argv[2], 'w')
print >>asmfile, """
    .text
    .code 32
    .align 2

    .global metalcpu_coprocread_asm
metalcpu_coprocread_asm:
    add pc, r0, lsl #3
    nop
"""
for fields in read:
    print >>asmfile, '    mrc p%s, %s, r0, c%s, c%s, %s  // %s' % tuple(fields)
    print >>asmfile, '    mov pc, lr'

print >>asmfile, """
    .global metalcpu_coprocwrite_asm
metalcpu_coprocwrite_asm:
    add pc, r0, lsl #3
    nop
"""
for fields in write:
    print >>asmfile, '    mcr p%s, %s, r1, c%s, c%s, %s  // %s' % tuple(fields)
    print >>asmfile, '    mov pc, lr'

asmfile.close()

# output header with the number of registers available for read and write
hdrfile = open(sys.argv[3], 'w')
print >>hdrfile, '#define COPROCREAD_REGS %s' % len(read)
print >>hdrfile, '#define COPROCWRITE_REGS %s' % len(write)
hdrfile.close()

# output python module containing two dictionaries mapping coprocessor params
# to the descriptions and index numbers
mapfile = open(sys.argv[4], 'w')
print >>mapfile, 'coprocread_map = {'
regindex = 0
for fields in read:
    print >>mapfile, ('(%s, %s, %s, %s, %s): ("%s", %s),'
            % tuple(fields+[regindex]))
    regindex += 1
print >>mapfile, '}'
print >>mapfile, 'coprocwrite_map = {'
regindex = 0
for fields in write:
    print >>mapfile, ('(%s, %s, %s, %s, %s): ("%s", %s),'
            % tuple(fields+[regindex]))
    regindex += 1
print >>mapfile, '}'
mapfile.close()

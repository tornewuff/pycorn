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

lineno = 0

infile = open(sys.argv[1])
for line in infile:
    lineno += 1
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
infile.close()

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

hdrfile = open(sys.argv[3], 'w')
print >>hdrfile, '#define COPROCREAD_REGS %s' % len(read)
print >>hdrfile, '#define COPROCWRITE_REGS %s' % len(write)
hdrfile.close()

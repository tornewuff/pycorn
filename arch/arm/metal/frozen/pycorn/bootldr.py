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

import struct

import _bootldr
from metal import mem

def _tagdata():
    header = struct.Struct('2I')
    addr = _bootldr.taglist_addr
    while True:
        headerbuf = mem.membuf(addr, header.size)
        (size, tag) = header.unpack(headerbuf)
        if tag == 0:
            break
        data = mem.membuf(addr + header.size, size * 4 - header.size)
        yield tag, data
        addr += size * 4

_tagtypes = {
        0x54410001: ('core', struct.Struct('3I'),
            ('flags', 'pagesize', 'rootdev')),
        0x54410002: ('mem32', struct.Struct('2I'),
            ('size', 'start')),
        0x54410006: ('serialnr', struct.Struct('Q'),
            ('serialnr',)),
        }

def taglist():
    for tag, data in _tagdata():
        (name, parser, fields) = _tagtypes[tag]
        values = parser.unpack(data)
        tagdict = dict(zip(fields, values))
        yield name, tagdict

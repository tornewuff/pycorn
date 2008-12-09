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

import _metalcpu
import metal._coprocmap as cpmap

def _coprocread_index(coproc, opcode_1, CRn, CRm, opcode_2):
    return cpmap.coprocread_map.get((coproc, opcode_1, CRn, CRm, opcode_2))

def _coprocwrite_index(coproc, opcode_1, CRn, CRm, opcode_2):
    return cpmap.coprocwrite_map.get((coproc, opcode_1, CRn, CRm, opcode_2))

def coproc_peek(coproc, opcode_1, CRn, CRm, opcode_2):
    reg_entry = _coprocread_index(coproc, opcode_1, CRn, CRm, opcode_2)
    if reg_entry is None:
        raise ValueError('coprocessor register not present/supported')
    return _metalcpu.coproc_read(reg_entry[1])

def coproc_poke(coproc, opcode_1, CRn, CRm, opcode_2, value):
    reg_entry = _coprocwrite_index(coproc, opcode_1, CRn, CRm, opcode_2)
    if reg_entry is None:
        raise ValueError('coprocessor register not present/supported')
    if value < -0x80000000 or value > 0xffffffff:
        raise ValueError('value must be between -0x80000000 and 0xffffffff')
    return _metalcpu.coproc_write(reg_entry[1], value)

def cp15_peek(CRn, CRm=0, opcode_2=0):
    return coproc_peek(15, 0, CRn, CRm, opcode_2)

def cp15_poke(CRn, CRm, opcode_2, value):
    return coproc_poke(15, 0, CRn, CRm, opcode_2, value)

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
import _coprocmap as cpmap
from metal import mem, register, bits
from metal.bits import Field, Bits

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
    mem.checkvalue(4, value)
    _metalcpu.coproc_write(reg_entry[1], value)

def cp15_peek(CRn, CRm=0, opcode_2=0):
    return coproc_peek(15, 0, CRn, CRm, opcode_2)

def cp15_poke(CRn, CRm, opcode_2, value):
    coproc_poke(15, 0, CRn, CRm, opcode_2, value)


class CoprocRegister(register.Register):

    def __init__(self, coproc, opcode_1, CRn, CRm=0, opcode_2=0):
        read = _coprocread_index(coproc, opcode_1, CRn, CRm, opcode_2)
        write = _coprocwrite_index(coproc, opcode_1, CRn, CRm, opcode_2)
        register.Register.__init__(self, read is not None,
                write is not None, 4)
        self.coproc = coproc
        self.opcode_1 = opcode_1
        self.CRn = CRn
        self.CRm = CRm
        self.opcode_2 = opcode_2
        if read is not None:
            self.name = read[0]
            self._readindex = read[1]
        if write is not None:
            self.name = write[0]
            self._writeindex = write[1]

    def __repr__(self):
        return 'CoprocRegister(%s, %s, %s, %s, %s)' % (self.coproc,
                self.opcode_1, self.CRn, self.CRm, self.opcode_2)

    def __str__(self):
        return '<%s coprocessor register "%s">' % (self._access(), self.name)

    def _read(self):
        return _metalcpu.coproc_read(self._readindex)

    def _write(self, value):
        _metalcpu.coproc_write(self._writeindex, value)


class SystemControlCoprocessor(object):

    class MainIDRegister(CoprocRegister, bits.Bitfield):

        def __init__(self):
            CoprocRegister.__init__(self, 15, 0, 0, 0, 0)
            bits.Bitfield.__init__(self)

        interpretation = Field(Bits[15:12], "ID code interpretation")

        # if interpretation is 0, pre-ARM7 processor. Not handled yet.
        # if interpretation is 7, ARM7 family processor. Not handled yet.
        # otherwise, fields are defined as follows:

        implementor = Field(Bits[31:24], "Implementor code")
        variant = Field(Bits[23:20], "Variant number")
        architecture = Field(Bits[19:16], "Architecture code")
        partno = Field(Bits[15:4], "Primary part number")
        revision = Field(Bits[3:0], "Revision number")

    main_id = MainIDRegister()

    class CacheTypeRegister(CoprocRegister, bits.Bitfield):

        def __init__(self):
            CoprocRegister.__init__(self, 15, 0, 0, 0, 1)
            bits.Bitfield.__init__(self)

        ctype = Field(Bits[28:25], "Cache type")
        separate = Field(Bits[24], "Separate caches")
        data_size = Field(Bits[23:12], "Data cache size")
        ins_size = Field(Bits[11:0], "Instruction cache size")

    cache_type = CacheTypeRegister()

    class ControlRegister(CoprocRegister, bits.Bitfield):

        def __init__(self):
            CoprocRegister.__init__(self, 15, 0, 1, 0, 0)
            bits.Bitfield.__init__(self)

        hi_vectors = Field(Bits[13], "Exception vector relocation")
        icache = Field(Bits[12], "Instruction cache enable/disable")
        btb = Field(Bits[11], "Branch target buffer enable")
        rom_prot = Field(Bits[9], "ROM protection")
        sys_prot = Field(Bits[8], "System protection")
        endian = Field(Bits[7], "Big/little endian")
        dcache = Field(Bits[2], "Data cache enable/disable")
        align = Field(Bits[1], "Alignment fault check enable/disable")
        mmu = Field(Bits[0], "MMU enable/disable")

    control = ControlRegister()

system_control = SystemControlCoprocessor()

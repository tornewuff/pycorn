# ARM-specific bootstrap code.
#
# Decodes the bootdata struct passed by the C bootstrap, to locate the initrd.
#
#
# Copyright 2010 Torne Wuff
#
# This file is part of Pycorn.
#
# Pycorn is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#

import struct
import cStringIO

import _bootstrap
from _metalmem import membuf

# struct bootdata_t from bootstrap.h
_bootdata_t = struct.Struct('PIPIPPIPP')

# create a buffer for the bootdata and unpack it
_bootdata_buf = membuf(_bootstrap.bootdata_virt, _bootdata_t.size)
_bootdata = _bootdata_t.unpack(_bootdata_buf)

# if the initrd size is nonzero, create a buffer and stringio for it
if _bootdata[6] > 0:
    initrd = membuf(_bootdata[8], _bootdata[6])
    initrd_file = cStringIO.StringIO(initrd)
else:
    initrd = None
    initrd_file = None

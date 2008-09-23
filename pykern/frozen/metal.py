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

import _metal

def peek(address, bytes=4):
    if bytes == 4:
        f = _metal.peek32
    elif bytes == 2:
        f = _metal.peek16
    elif bytes == 1:
        f = _metal.peek8
    else:
        raise ValueError('bytes must be 1, 2 or 4')
    if address % bytes != 0:
        raise ValueError('address must be aligned to a multiple of bytes')
    return f(address)

membuf = _metal.membuf

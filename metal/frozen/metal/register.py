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

from metal import mem

class Register(object):

    def __init__(self, read, write, bytes):
        if not read and not write:
            raise ValueError('register must be readable or writable')
        self.readable = read
        self.writable = write
        self.bytes = bytes

    def _access(self):
        if self.readable:
            if self.writable:
                return 'read/write'
            else:
                return 'read-only'
        else:
            return 'write-only'

    def _read(self):
        raise NotImplementedError

    def _write(self, value):
        raise NotImplementedError

    def peek(self):
        if not self.readable:
            raise TypeError('not a readable register')
        return self._read()

    def poke(self, value):
        if not self.writable:
            raise TypeError('not a writable register')
        mem.checkvalue(self.bytes, value)
        self._write(value)


class MemMapRegister(Register):

    def __init__(self, address, read=True, write=True, bytes=4):
        Register.__init__(self, read, write, bytes)
        mem.checkaddr(address, bytes)
        self.address = address
        if read:
            self._reader = mem.reader(bytes)
        if write:
            self._writer = mem.writer(bytes)

    def __repr__(self):
        return 'MemMapRegister(%s, read=%s, write=%s, bytes=%s)' % (
                hex(self.address), self.readable, self.writable, self.bytes)

    def __str__(self):
        return '<%s %s-byte memory-mapped register at %s>' % (self._access(),
                self.bytes, hex(self.address))

    def _read(self):
        return self._reader(self.address)

    def _write(self, value):
        self._writer(self.address, value)

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


class _BitsType(type):

    def __getitem__(cls, key):
        return cls(key)


class Bits(object):

    __metaclass__ = _BitsType

    def __init__(self, range):
        self.ranges = []
        outshift = 0
        if not isinstance(range, tuple):
            range = (range,)
        for b in reversed(range):
            if isinstance(b, slice):
                if b.start < b.stop or b.step is not None:
                    raise ValueError("ranges must be MSB:LSB, with no step")
                bits = b.start - b.stop + 1
                mask = ((1 << bits) - 1) << b.stop
                self.ranges.append((mask, b.stop, outshift))
                outshift += bits
            elif isinstance(b, int):
                self.ranges.append((1<<b, b, outshift))
                outshift += 1
            else:
                raise ValueError("ranges must be slices or integers")
        self.valuewidth = outshift

    def extract(self, data):
        value = 0
        for mask, rshift, lshift in self.ranges:
            value |= ((data & mask) >> rshift) << lshift
        return value

    def insert(self, data, value):
        for mask, rshift, lshift in self.ranges:
            data &= ~mask
            data |= ((value >> lshift) << rshift) & mask
        return data


class Field(object):

    def __init__(self, bits, name):
        self.bits = bits
        self.name = name

    def __get__(self, obj, objtype=None):
        if obj is None:
            return self
        return self.bits.extract(obj._cachedvalue)

    def __set__(self, obj, value):
        obj._cachedvalue = self.bits.insert(obj._cachedvalue, value)


class _BitfieldType(type):

    def __init__(cls, name, bases, dict):
        type.__init__(cls, name, bases, dict)
        cls.fields = []
        for name, field in dict.iteritems():
            if isinstance(field, Field):
                cls.fields.append(field)
                field.shortname = name


class Bitfield(object):

    __metaclass__ = _BitfieldType

    def __init__(self):
        self._cachedvalue = None

    def load(self):
        self._cachedvalue = self._read()

    def save(self):
        self._write(self._cachedvalue)

    def __enter__(self):
        self.load()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        if exc_type is None:
            self.save()

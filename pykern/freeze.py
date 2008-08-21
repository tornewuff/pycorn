# Copyright 2008 Torne Wuff, but loosely based on freeze.py from the main
# Python distribution's Tools directory.
#
# This version is much dumber and intentionally makes no effort whatsoever
# to resolve dependencies. It just freezes what it's given.
#
# This file is part of Pycorn.
#
# Pycorn is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

import os
import sys
import marshal

header = """\
#include "Python.h"

"""
middle = """
static struct _frozen _PyImport_FrozenModules[] = {
"""
trailer = """\
    {0, 0, 0} /* sentinel */
};

struct _frozen *PyImport_FrozenModules = _PyImport_FrozenModules;
"""

def makefreeze(dict):
    outfp = open('frozen.c', 'w')
    outfp.write(header)
    done = []
    mods = dict.keys()
    mods.sort()
    for mod in mods:
        m = dict[mod]
        mangled = "__".join(mod.split("."))
        print "freezing", mod, "..."
        str = marshal.dumps(m['code'])
        size = len(str)
        if m['package']:
            # Indicate package by negative size
            size = -size
        done.append((mod, mangled, size))
        writecode(outfp, mangled, str)
    print "generating table of frozen modules"
    outfp.write(middle)
    for mod, mangled, size in done:
        outfp.write('\t{"%s", M_%s, %d},\n' % (mod, mangled, size))
    outfp.write('\n')
    outfp.write(trailer)
    outfp.close()

def writecode(outfp, mod, str):
    outfp.write('static unsigned char M_%s[] = {' % mod)
    for i in range(0, len(str), 16):
        outfp.write('\n\t')
        for c in str[i:i+16]:
            outfp.write('%d,' % ord(c))
    outfp.write('\n};\n')



mods = {}

for modfile in sys.argv[1:]:
    package = False
    namein = modfile[:-3]
    nameout = []
    while namein:
        temp = os.path.split(namein)
        nameout.insert(0, temp[1])
        namein = temp[0]
    if nameout[-1] == "__init__":
        package = True
        del nameout[-1]
    modname = '.'.join(nameout)
    f = open(modfile)
    text = f.read() + '\n'
    f.close()
    code = compile(text, modfile, "exec")
    mods[modname] = {'code': code, 'package': package}

makefreeze(mods)

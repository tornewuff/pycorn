# Pycorn bootstrap.
#
# This module is the first Python code run on boot, and handles the required
# setup to be able to import non-frozen modules (from an initrd zipfile).
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

import sys
import cStringIO

from _embryo import initrd_size, initrd_virt
from _metalmem import membuf

from bootstrap.mini_zipimport import MiniZipImport
from bootstrap.version import GIT_VERSION

# Print version info/banner
print 'Pycorn %s' % GIT_VERSION, '(Python %s.%s.%s)' % sys.version_info[0:3]

# Clear existing path, which is useless
del sys.path[:]

# if the initrd size is nonzero, create a buffer and stringio for it
if initrd_size > 0:
    initrd = membuf(initrd_virt, initrd_size)
    initrd_file = cStringIO.StringIO(initrd)
else:
    initrd = None
    initrd_file = None

# If an initrd was provided, add it to the path via mini_zipimport
if initrd_file:
    sys.path_hooks.append(MiniZipImport(initrd_file))
    sys.path.append('/initrd')

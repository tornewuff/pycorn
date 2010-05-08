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

from bootstrap.arch import initrd_file
from bootstrap.mini_zipimport import MiniZipImport
from bootstrap.version import version_info

# Print version info/banner
print 'Pycorn r%s' % version_info['revno'],
if not version_info['clean']:
    print '(modified)',
print '(Python %s.%s.%s)' % sys.version_info[0:3]
print 'Revid:', version_info['revision_id']
print 'Built:', version_info['build_date']

# Clear existing path, which is useless
del sys.path[:]

# If an initrd was provided, add it to the path via mini_zipimport
if initrd_file:
    sys.path_hooks.append(MiniZipImport(initrd_file))
    sys.path.append('/initrd')

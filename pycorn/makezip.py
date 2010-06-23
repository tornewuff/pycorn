#
# Simple tool to zip up a bunch of python modules into a single zipfile.
#
# First argument is the output zipfile, the rest are root directories to search
# for files. All files are included except for dotfiles, but only the parts of
# the path *under* the given directories are stored as the pathnames of the
# file in the zip. This allows multiple directories in the source to insert
# modules into the same package.
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

import os
import sys
import fnmatch
import zipfile

def scandir(dir, shortdir):
    """Recursively scan a directory looking for files to zip."""
    for entry in os.listdir(dir):
        if fnmatch.fnmatch(entry, '.*'):
            continue
        path = os.path.join(dir, entry)
        shortpath = os.path.join(shortdir, entry)
        if os.path.isdir(path):
            scandir(path, shortpath)
        elif os.path.isfile(path):
            print "zipping", shortpath, "..."
            output.write(path, shortpath)

output = zipfile.ZipFile(sys.argv[1], 'w', zipfile.ZIP_STORED)
for dir in sys.argv[2:]:
    scandir(dir, "")
output.close()

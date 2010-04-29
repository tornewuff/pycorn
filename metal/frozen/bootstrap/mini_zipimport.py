# Minimal zipimporter.
#
# This module implements an import hook which can import modules from a
# zipfile, provided as a file-like object.
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

import imp
import sys

from bootstrap.mini_zipfile import ZipFile


class MiniZipImport:

    def __init__(self, initrd_file):
        self.zipfile = ZipFile(initrd_file)

    def __call__(self, path):
        if path != '/initrd':
            raise ImportError('mini_zipimport only handles the initrd')
        return self

    def _check_zipfile(self, modulename):
        basepath = '/'.join(modulename.split('.'))
        for suffix, package in ('.py', False), ('/__init__.py', True):
            path = basepath + suffix
            if self.zipfile.exists(path):
                return path, package
        raise ImportError("Can't find module %s in initrd" % modulename)

    def _read_source(self, modulename):
        path, package = self._check_zipfile(modulename)
        fullpath = '/initrd/' + path
        source = self.zipfile.read(path)
        source = source.replace('\r\n', '\n')
        source = source.replace('\r', '\n')
        return source, fullpath, package

    def find_module(self, fullname, path=None):
        try:
            path, package = self._check_zipfile(fullname)
            return self
        except ImportError:
            return None

    def load_module(self, fullname):
        source, path, package = self._read_source(fullname)
        code = compile(source, path, 'exec')
        mod = sys.modules.get(fullname)
        need_cleanup = False
        if mod is None:
            mod = sys.modules[fullname] = imp.new_module(fullname)
            need_cleanup = True
        mod.__file__ = path
        if package:
            mod.__path__ = ['/initrd']
        mod.__loader__ = self
        try:
            exec code in mod.__dict__
            return mod
        except:
            if need_cleanup:
                del sys.modules[fullname]
            raise

    def get_data(self, path):
        if not path.startswith('/initrd/'):
            raise IOError("Path %r doesn't start with /initrd/" % path)
        path = path[8:]
        try:
            return self.zipfile.read(path)
        except KeyError:
            raise IOError('Path %r not found in initrd' % path)

    def is_package(self, fullname):
        path, package = self._check_zipfile(fullname)
        return package

    def get_code(self, fullname):
        source, path, package = self._read_source(fullname)
        return compile(source, path, 'exec')

    def get_source(self, fullname):
        source, path, package = self._read_source(fullname)
        return source

    def get_filename(self, fullname):
        path, package = self._check_zipfile(fullname)
        return path


from pycorn.bootstrap import initrd_file
sys.path_hooks[:] = [MiniZipImport(initrd_file)]
sys.path[:] = ['/initrd']

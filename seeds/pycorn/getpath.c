/*
 * Various functions Python calls to find its files: we have no filesystem
 * at the C library level, so just return fairly arbitrary strings.
 *
 * Copyright 2008 Torne Wuff
 *
 * This file is part of Pycorn.
 *
 * Pycorn is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

char *Py_GetPath(){ return "/"; }
char *Py_GetPrefix() { return "/"; }
char *Py_GetExecPrefix() { return "/"; }
char *Py_GetProgramFullPath() { return "/python"; }

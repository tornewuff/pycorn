/*
 * Copyright 2008 Torne Wuff
 *
 * This file is part of Pycorn.
 *
 * Pycorn is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

#include <Python.h>

unsigned int machtype = -1;
void *taglist_ptr = (void*)-1;

PyMODINIT_FUNC
initbootldr(void)
{
    PyObject *m, *machine, *taglist_addr;

    m = Py_InitModule("_bootldr", NULL);
    if (m == NULL)
        return;

    machine = PyLong_FromUnsignedLong(machtype);
    if (machine != NULL)
        PyModule_AddObject(m, "machtype", machine);
    taglist_addr = PyLong_FromVoidPtr(taglist_ptr);
    if (taglist_addr != NULL)
        PyModule_AddObject(m, "taglist_addr", taglist_addr);
}

__attribute__((constructor)) void appendbootldr()
{
    PyImport_AppendInittab("_bootldr", initbootldr);
}

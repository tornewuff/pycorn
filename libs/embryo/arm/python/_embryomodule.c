/*
 * _embryo module. Provides the values of various constant addresses which
 * were used by the embryo code.
 *
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

#include <Python.h>
#include <embryo.h>

#define PyModule_AddUnsignedLongConstant(module, name, value) { \
        PyObject *o = PyLong_FromUnsignedLong(value); \
        if (o) PyModule_AddObject(m, name, o); }

#define PyModule_AddVoidPtrConstant(module, name, value) { \
        PyObject *o = PyLong_FromVoidPtr(value); \
        if (o) PyModule_AddObject(m, name, o); }

PyMODINIT_FUNC
initembryo(void)
{
    PyObject *m;

    m = Py_InitModule3("_embryo", NULL, "Definitions of embryo fixed addresses");
    if (!m)
        return;

    // embryo_bootdata structure
    PyModule_AddUnsignedLongConstant(m, "rom_base", embryo_bootdata->rom_base);
    PyModule_AddUnsignedLongConstant(m, "machtype", embryo_bootdata->machtype);
    PyModule_AddUnsignedLongConstant(m, "taglist_ptr", embryo_bootdata->taglist_ptr);
    PyModule_AddUnsignedLongConstant(m, "phys_to_virt", embryo_bootdata->phys_to_virt);
    PyModule_AddUnsignedLongConstant(m, "next_free_page", embryo_bootdata->next_free_page);
    PyModule_AddUnsignedLongConstant(m, "page_directory", embryo_bootdata->page_directory);
    PyModule_AddUnsignedLongConstant(m, "initrd_size", embryo_bootdata->initrd_size);
    PyModule_AddUnsignedLongConstant(m, "initrd_phys", embryo_bootdata->initrd_phys);
    PyModule_AddUnsignedLongConstant(m, "initrd_virt", embryo_bootdata->initrd_virt);
}

__attribute__((constructor)) void appendembryo()
{
    PyImport_AppendInittab("_embryo", initembryo);
}

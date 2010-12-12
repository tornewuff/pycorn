/*
 * _bootstrap module. Provides the values of various constant addresses which
 * were used by the bootstrap code.
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

extern char __text_start__, __text_end__, __data_start__, __data_end__;
extern char __bss_start__, __bss_end__, __heap_start__, __heap_end__;
extern char __stack_start__, __stack_end__, __page_dir_virt__;
extern char __dbg_serial_virt__, __page_tbl_start__, __page_tbl_end__;
extern char __bootdata_virt__;

#define PyModule_AddUnsignedLongConstant(module, name, value) { \
        PyObject *o = PyLong_FromUnsignedLong(value); \
        if (o) PyModule_AddObject(m, name, o); }

#define PyModule_AddVoidPtrConstant(module, name, value) { \
        PyObject *o = PyLong_FromVoidPtr(value); \
        if (o) PyModule_AddObject(m, name, o); }

PyMODINIT_FUNC
initbootstrap(void)
{
    PyObject *m;

    m = Py_InitModule3("_bootstrap", NULL, "Definitions of bootstrap fixed addresses");
    if (!m)
        return;

    PyModule_AddVoidPtrConstant(m, "text_start", &__text_start__);
    PyModule_AddVoidPtrConstant(m, "text_end", &__text_end__);
    PyModule_AddVoidPtrConstant(m, "data_start", &__data_start__);
    PyModule_AddVoidPtrConstant(m, "data_end", &__data_end__);
    PyModule_AddVoidPtrConstant(m, "bss_start", &__bss_start__);
    PyModule_AddVoidPtrConstant(m, "bss_end", &__bss_end__);
    PyModule_AddVoidPtrConstant(m, "heap_start", &__heap_start__);
    PyModule_AddVoidPtrConstant(m, "heap_end", &__heap_end__);
    PyModule_AddVoidPtrConstant(m, "stack_start", &__stack_start__);
    PyModule_AddVoidPtrConstant(m, "stack_end", &__stack_end__);
    PyModule_AddVoidPtrConstant(m, "page_dir_virt", &__page_dir_virt__);
    PyModule_AddVoidPtrConstant(m, "page_tbl_start", &__page_tbl_start__);
    PyModule_AddVoidPtrConstant(m, "page_tbl_end", &__page_tbl_end__);
    PyModule_AddVoidPtrConstant(m, "dbg_serial_virt", &__dbg_serial_virt__);
    PyModule_AddVoidPtrConstant(m, "bootdata_virt", &__bootdata_virt__);
}

__attribute__((constructor)) void appendbootstrap()
{
    PyImport_AppendInittab("_bootstrap", initbootstrap);
}

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
#include <bootdata.h>

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

    // Linker-defined addresses
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

    // Bootdata structure
    bootdata_t *bootdata = (bootdata_t *) &__bootdata_virt__;
    PyModule_AddUnsignedLongConstant(m, "rom_base", bootdata->rom_base);
    PyModule_AddUnsignedLongConstant(m, "machtype", bootdata->machtype);
    PyModule_AddUnsignedLongConstant(m, "taglist_ptr", bootdata->taglist_ptr);
    PyModule_AddUnsignedLongConstant(m, "phys_to_virt", bootdata->phys_to_virt);
    PyModule_AddUnsignedLongConstant(m, "next_free_page", bootdata->next_free_page);
    PyModule_AddUnsignedLongConstant(m, "page_directory", bootdata->page_directory);
    PyModule_AddUnsignedLongConstant(m, "initrd_size", bootdata->initrd_size);
    PyModule_AddUnsignedLongConstant(m, "initrd_phys", bootdata->initrd_phys);
    PyModule_AddUnsignedLongConstant(m, "initrd_virt", bootdata->initrd_virt);
}

__attribute__((constructor)) void appendembryo()
{
    PyImport_AppendInittab("_embryo", initembryo);
}

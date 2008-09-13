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

static PyObject *
metal_peek(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned val;

    if (!PyArg_ParseTuple(args, "I", &addr))
        return NULL;
    val = *(unsigned*)addr;
    return Py_BuildValue("I", val);
}

static PyObject *
metal_poke(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned val;

    if (!PyArg_ParseTuple(args, "II", &addr, &val))
        return NULL;
    *(unsigned*)addr = val;
    Py_RETURN_NONE;
}

static const PyMethodDef MetalMethods[] = {
    {"peek", metal_peek, METH_VARARGS, "Read memory at the given address."},
    {"poke", metal_poke, METH_VARARGS, "Write memory at the given address."},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
initmetal(void)
{
    PyObject *m;

    m = Py_InitModule("_metal", MetalMethods);
    if (m == NULL)
        return;
}

__attribute__((constructor)) void appendmetal()
{
    PyImport_AppendInittab("_metal", initmetal);
}

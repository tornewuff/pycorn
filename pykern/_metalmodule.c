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
metal_peek32(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned val;

    if (!PyArg_ParseTuple(args, "I", &addr))
        return NULL;
    if ((addr & 0x3) != 0)
    {
        PyErr_SetString(PyExc_ValueError, "32-bit access must be 4-byte aligned");
        return NULL;
    }
    val = *(unsigned*)addr;
    return Py_BuildValue("I", val);
}

static PyObject *
metal_poke32(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned val;

    if (!PyArg_ParseTuple(args, "II", &addr, &val))
        return NULL;
    if ((addr & 0x3) != 0)
    {
        PyErr_SetString(PyExc_ValueError, "32-bit access must be 4-byte aligned");
        return NULL;
    }
    *(unsigned*)addr = val;
    Py_RETURN_NONE;
}

static PyObject *
metal_peek16(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned short val;

    if (!PyArg_ParseTuple(args, "I", &addr))
        return NULL;
    if ((addr & 0x1) != 0)
    {
        PyErr_SetString(PyExc_ValueError, "16-bit access must be 2-byte aligned");
        return NULL;
    }
    val = *(unsigned short*)addr;
    return Py_BuildValue("H", val);
}

static PyObject *
metal_poke16(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned short val;

    if (!PyArg_ParseTuple(args, "IH", &addr, &val))
        return NULL;
    if ((addr & 0x1) != 0)
    {
        PyErr_SetString(PyExc_ValueError, "16-bit access must be 2-byte aligned");
        return NULL;
    }
    *(unsigned short*)addr = val;
    Py_RETURN_NONE;
}

static PyObject *
metal_peek8(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned char val;

    if (!PyArg_ParseTuple(args, "I", &addr))
        return NULL;
    val = *(unsigned char*)addr;
    return Py_BuildValue("B", val);
}

static PyObject *
metal_poke8(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned char val;

    if (!PyArg_ParseTuple(args, "IB", &addr, &val))
        return NULL;
    *(unsigned char*)addr = val;
    Py_RETURN_NONE;
}

static const PyMethodDef MetalMethods[] = {
    {"peek32", metal_peek32, METH_VARARGS, "Read a 32-bit word at the given address."},
    {"poke32", metal_poke32, METH_VARARGS, "Write a 32-bit word at the given address."},
    {"peek16", metal_peek16, METH_VARARGS, "Read a 16-bit halfword at the given address."},
    {"poke16", metal_poke16, METH_VARARGS, "Write a 16-bit halfword at the given address."},
    {"peek8", metal_peek8, METH_VARARGS, "Read an 8-bit byte at the given address."},
    {"poke8", metal_poke8, METH_VARARGS, "Write an 8-bit byte at the given address."},
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

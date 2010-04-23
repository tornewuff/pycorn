/*
 * _metalmem module. Provides primitives to issue single reads and writes to
 * arbitrary memory addresses, and to create a buffer object pointing at an
 * arbitrary range of memory. No checking of addresses is done whatsoever; this
 * is so that higher level constructs can check addresses just once and then
 * make accesses more quickly later.
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

static PyObject *
metalmem_peek32(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned val;

    if (!PyArg_ParseTuple(args, "I", &addr))
        return NULL;
    val = *(unsigned*)addr;
    return Py_BuildValue("I", val);
}

static PyObject *
metalmem_poke32(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned val;

    if (!PyArg_ParseTuple(args, "II", &addr, &val))
        return NULL;
    *(unsigned*)addr = val;
    Py_RETURN_NONE;
}

static PyObject *
metalmem_peek16(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned short val;

    if (!PyArg_ParseTuple(args, "I", &addr))
        return NULL;
    val = *(unsigned short*)addr;
    return Py_BuildValue("H", val);
}

static PyObject *
metalmem_poke16(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned short val;

    if (!PyArg_ParseTuple(args, "IH", &addr, &val))
        return NULL;
    *(unsigned short*)addr = val;
    Py_RETURN_NONE;
}

static PyObject *
metalmem_peek8(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned char val;

    if (!PyArg_ParseTuple(args, "I", &addr))
        return NULL;
    val = *(unsigned char*)addr;
    return Py_BuildValue("B", val);
}

static PyObject *
metalmem_poke8(PyObject *self, PyObject *args)
{
    unsigned addr;
    unsigned char val;

    if (!PyArg_ParseTuple(args, "IB", &addr, &val))
        return NULL;
    *(unsigned char*)addr = val;
    Py_RETURN_NONE;
}

static PyObject *
metalmem_membuf(PyObject *self, PyObject *args)
{
    unsigned addr;
    Py_ssize_t len;
    int writable = 0;

    if (!PyArg_ParseTuple(args, "In|i", &addr, &len, &writable))
        return NULL;
    if (writable)
        return PyBuffer_FromReadWriteMemory((void*)addr, len);
    else
        return PyBuffer_FromMemory((void*)addr, len);
}

static const PyMethodDef MetalMemMethods[] = {
    {"peek32", metalmem_peek32, METH_VARARGS, "Read a 32-bit word at the given address."},
    {"poke32", metalmem_poke32, METH_VARARGS, "Write a 32-bit word at the given address."},
    {"peek16", metalmem_peek16, METH_VARARGS, "Read a 16-bit halfword at the given address."},
    {"poke16", metalmem_poke16, METH_VARARGS, "Write a 16-bit halfword at the given address."},
    {"peek8", metalmem_peek8, METH_VARARGS, "Read an 8-bit byte at the given address."},
    {"poke8", metalmem_poke8, METH_VARARGS, "Write an 8-bit byte at the given address."},
    {"membuf", metalmem_membuf, METH_VARARGS, "Create a buffer object pointing at the given address"},
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC
init_metalmem(void)
{
    PyObject *m;

    m = Py_InitModule("_metalmem", MetalMemMethods);
    if (m == NULL)
        return;
}

__attribute__((constructor)) void append_metalmem()
{
    PyImport_AppendInittab("_metalmem", init_metalmem);
}

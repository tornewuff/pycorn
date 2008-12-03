/*
 * Main pykern function. Just calls Python for now.
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

#include "Python.h"
#include <stdio.h>

char *pyargs[] = { "python", "-v", "-S", "-i", NULL };

int main()
{
  int r = Py_Main((sizeof(pyargs)/sizeof(char*))-1, pyargs);
  printf("Python has quit, bye!\n");
  return r;
}

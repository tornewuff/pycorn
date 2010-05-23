/*
 * Main pycorn function. Just calls Python for now.
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

/* Arguments:
 * -v for verbose if DEBUG_PYSTARTUP is defined
 * -S to not load site.py since it doesn't exist.
 * -i to always force the interactive prompt to show up.
 * -c "__import__('bootstrap')" to make the frozen-in bootstrap code run
 */
char *pyargs[] = { "python",
#ifdef DEBUG_PYSTARTUP
    "-v",
#endif
    "-S", "-i", "-c", "__import__('bootstrap')", NULL };

int main()
{
  int r = Py_Main((sizeof(pyargs)/sizeof(char*))-1, pyargs);
  printf("Python has quit, bye!\n");
  return r;
}

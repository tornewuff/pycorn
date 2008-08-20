#include <stdio.h>

extern int Py_Main(int, char*[]);

char *Py_GetPath(){ return "/"; }
char *Py_GetPrefix() { return "/"; }
char *Py_GetExecPrefix() { return "/"; }
char *Py_GetProgramFullPath() { return "/python"; }

//char *pyargs[] = { "python", "-c", "import sys\nwhile True: print 'You pressed %s' % (ord(sys.stdin.read(1)))", NULL };
char *pyargs[] = { "python", "-v", "-S", "-i", NULL };

int main()
{
  int r = Py_Main((sizeof(pyargs)/sizeof(char*))-1, pyargs);
  printf("Python has quit, bye!\n");
  return r;
}

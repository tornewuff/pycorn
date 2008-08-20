#include <stdio.h>

extern int Py_Main(int, char*[]);

//char *pyargs[] = { "python", "-c", "import sys\nwhile True: print 'You pressed %s' % (ord(sys.stdin.read(1)))", NULL };
char *pyargs[] = { "python", "-i", NULL };

int main()
{
  int r = Py_Main((sizeof(pyargs)/sizeof(char*))-1, pyargs);
  printf("Python has quit, bye!\n");
  return r;
}

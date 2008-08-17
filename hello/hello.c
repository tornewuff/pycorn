#include <stdio.h>

extern void serial_init();

int main()
{
  serial_init();
  printf("Hello World!\n");
  return 0;
}

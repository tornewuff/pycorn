#include <stdio.h>
#include <sys/iosupport.h>

volatile unsigned int* FFRBR = (unsigned int*)0x40100000;
volatile unsigned int* FFTHR = (unsigned int*)0x40100000;
volatile unsigned int* FFLSR = (unsigned int*)0x40100014;
#define LSR_TEMT (1<<6)
#define LSR_DR   (1<<0)

int serial_write(struct _reent *r, int fd, const char* ptr, int len)
{
  int i;
  for (i = 0; i < len; ++i)
  {
    while((*FFLSR & LSR_TEMT) == 0);
    *FFTHR = ptr[i];
  }
  return len;
}

int serial_read(struct _reent *r, int fd, char* ptr, int len)
{
  if (len == 0)
    return 0;

  while((*FFLSR & LSR_DR) == 0);
  ptr[0] = *FFRBR;
  if (ptr[0] == '\r')
    ptr[0] = '\n';
  serial_write(NULL, 0, ptr, 1);
  return 1;
}

const devoptab_t dotab_serial =
{
  "serial",
  0,
  NULL,
  NULL,
  serial_write,
  serial_read
};

void serial_init() __attribute__((constructor));

void serial_init()
{
  devoptab_list[STD_IN] = &dotab_serial;
  devoptab_list[STD_OUT] = &dotab_serial;
  devoptab_list[STD_ERR] = &dotab_serial;
}

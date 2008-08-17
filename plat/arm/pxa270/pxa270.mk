CFLAGS += -mcpu=xscale -mtune=xscale
KERNELMKIMAGE += -a 0xa0008000 -e 0xa0008000
BOARDOBJECTS := serial.o

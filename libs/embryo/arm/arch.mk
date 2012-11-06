O := start.o vectors.o bootstrap.o atag.o crt0.o boot-debug.o

ifdef EMBRYO_UART8250
O += uart8250-bs.o uart8250.o
endif

OBJECTS += $(ARCH)/$( $(O))

O := start.o bootstrap.o atag.o crt0.o

ifdef EMBRYO_GENMMSERIAL
O += genmmserial-bs.o genmmserial.o
endif

OBJECTS += $(ARCH)/$( $(O))

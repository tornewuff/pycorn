O := start.o bootstrap.o crt0.o

ifdef EMBRYO_GENMMSERIAL
O += genmmserial-bs.o genmmserial.o
endif

EMBRYOOBJECTS += $(ARCHPATH)/embryo/$( $(O))

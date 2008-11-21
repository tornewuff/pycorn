CFLAGS += -mcpu=xscale -mtune=xscale
PYCFLAGS += -mcpu=xscale -mtune=xscale
KERNELMKIMAGE += -a 0xa0008000 -e 0xa0008000
MACHOBJECTS := bootdebug.o serial.o

ifdef OBJECTS
$(phony run): $(notdir $(CURDIR)).flash
	@&echo You can exit qemu with C-a x (C-a a x under screen)
	qemu-system-arm -M verdex -pflash $(input) -nographic -m 289

%.flash: %.uimage $(ROOT)/downloads/u-boot-verdex-600-r1604.bin $(MACHPATH)/u-boot.env
	&rm -f $(output)
	dd of=$(output) bs=4k count=4k if=/dev/zero
	dd of=$(output) bs=4k conv=notrunc if=$(ROOT)/downloads/u-boot-verdex-600-r1604.bin
	dd of=$(output) bs=4k conv=notrunc seek=63 if=$(MACHPATH)/u-boot.env
	dd of=$(output) bs=4k conv=notrunc seek=64 if=$(input)
endif

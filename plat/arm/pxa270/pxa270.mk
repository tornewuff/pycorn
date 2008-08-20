CFLAGS += -mcpu=xscale -mtune=xscale
PYCFLAGS += -mcpu=xscale -mtune=xscale
KERNELMKIMAGE += -a 0xa0008000 -e 0xa0008000
BOARDOBJECTS := serial.o

ifdef OBJECTS
$(phony run): $(notdir $(CURDIR)).flash
	@&echo You can exit qemu with C-a x (C-a a x under screen)
	qemu-system-arm -M verdex -pflash $(input) -nographic -m 289

%.flash: %.uimage $(BOARDPATH)/u-boot.bin $(BOARDPATH)/u-boot.env
	&rm -f $(output)
	dd of=$(output) bs=4k count=4k if=/dev/zero
	dd of=$(output) bs=4k conv=notrunc if=$(BOARDPATH)/u-boot.bin
	dd of=$(output) bs=4k conv=notrunc seek=63 if=$(BOARDPATH)/u-boot.env
	dd of=$(output) bs=4k conv=notrunc seek=64 if=$(input)
endif

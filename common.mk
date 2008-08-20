# Disable makepp's builtin link rules as they're not particularly cross-compile-y
makepp_no_builtin_linker = 1

# Target selection, stuck here for now
ARCH := arm
BOARD := pxa270
PREFIX := arm-eabi-

# Commands
CC := $(PREFIX)gcc
OBJCOPY := $(PREFIX)objcopy
MKIMAGE := mkimage

# Flags
CFLAGS := -g -Wall -O2 -fomit-frame-pointer
BOARDPATH := $(ROOT)/plat/$(ARCH)/$(BOARD)
LDFLAGS := -B$(BOARDPATH) -specs=$(BOARD).specs
LDDEPS := $(BOARDPATH)/$(BOARD)$( .specs .ld _crt0.o)
MKIMAGEFLAGS := -A $(ARCH) -O linux
KERNELMKIMAGE := $(MKIMAGEFLAGS) -T kernel

# Get board-specific stuff
include $(ROOT)/plat/$(ARCH)/$(BOARD)/$(BOARD).mk

# Rules

ifdef OBJECTS
%.uimage: %.bin
	$(MKIMAGE) -C none $(KERNELMKIMAGE) -n $(basename $(input)) -d $(input) $(output)

%.bin: %.elf
	$(OBJCOPY) -O binary $(input) $(output)

$(notdir $(CURDIR)).elf: $(OBJECTS) $(BOARDPATH)/$(BOARDOBJECTS) $(LDDEPS)
	$(CC) $(OBJECTS) $(BOARDPATH)/$(BOARDOBJECTS) $(LDFLAGS) $(LIBS) -o $(output)
endif

%.o: %.S
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.s
	$(CC) $(ASFLAGS) -c $(input) -o $(output)

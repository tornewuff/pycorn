# Disable makepp's builtin rules as they're not particularly cross-compile-y
makepp_no_builtin = 1

# Target selection, stuck here for now
ARCH := arm
BOARD := pxa270
PREFIX := arm-eabi-

# Commands
CC := $(PREFIX)gcc
OBJCOPY := $(PREFIX)objcopy
MKIMAGE := mkimage

# Flags and paths
ARCHPATH := $(ROOT)/plat/$(ARCH)
BOARDPATH := $(ROOT)/plat/$(ARCH)/$(BOARD)
CFLAGS := -g -Wall -O2 -fomit-frame-pointer
CPPFLAGS := -I$(ARCHPATH) -I$(BOARDPATH) -I$(ROOT)/libs/$( $(dir $(LIBS)))include
PYCFLAGS := -fomit-frame-pointer -Werror -Wno-error=strict-aliasing -Wno-error=char-subscripts
LDFLAGS := -B$(BOARDPATH) -B$(ARCHPATH) -specs=$(BOARD).specs
LDDEPS := $(BOARDPATH)/$(BOARD)$( .specs .ld) $(ARCHPATH)/crt0.o
MKIMAGEFLAGS := -A $(ARCH) -O linux
KERNELMKIMAGE := $(MKIMAGEFLAGS) -T kernel
LIBOBJECTS := $(ROOT)/libs/$( $(LIBS))

# Get board-specific stuff
include $(ROOT)/plat/$(ARCH)/$(ARCH).mk
include $(ROOT)/plat/$(ARCH)/$(BOARD)/$(BOARD).mk

# Rules

ifdef OBJECTS
%.uimage: %.bin
	$(MKIMAGE) -C none $(KERNELMKIMAGE) -n $(basename $(input)) -d $(input) $(output)

%.bin: %.elf
	$(OBJCOPY) -O binary $(input) $(output)

$(notdir $(CURDIR)).elf: $(OBJECTS) $(BOARDPATH)/$( $(BOARDOBJECTS)) $(ARCHPATH)/$( $(ARCHOBJECTS)) $(LIBOBJECTS) $(LDDEPS)
	$(CC) $(OBJECTS) $(BOARDPATH)/$( $(BOARDOBJECTS)) $(ARCHPATH)/$( $(ARCHOBJECTS)) $(LDFLAGS) $(LIBOBJECTS) $(SYSLIBS) -o $(output)
endif

# we disable the scanner because it causes problems with the dynamically created includes from our libs
%.o: %.c $(ROOT)/libs/$( $(dir $(LIBS)))stamp-include : scanner none
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.S
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.s
	$(CC) $(ASFLAGS) -c $(input) -o $(output)

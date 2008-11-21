# Disable makepp's builtin rules as they're not particularly cross-compile-y
makepp_no_builtin = 1

# Target selection, stuck here for now
ARCH := arm
MACH := pxa270
PREFIX := arm-eabi-

# Commands
CC := $(PREFIX)gcc
OBJCOPY := $(PREFIX)objcopy
MKIMAGE := mkimage

# Flags and paths
SHAREDPATH := $(ROOT)/shared
ARCHPATH := $(ROOT)/arch/$(ARCH)
MACHPATH := $(ROOT)/arch/$(ARCH)/machine/$(MACH)
CFLAGS := -g -Wall -O2 -fomit-frame-pointer -Werror
CPPFLAGS := -I$(ARCHPATH) -I$(MACHPATH) -I$(ROOT)/libs/$( $(dir $(LIBS)))include
PYCFLAGS := -fomit-frame-pointer -Werror -Wno-error=strict-aliasing -Wno-error=char-subscripts
LDFLAGS := -B$(MACHPATH) -B$(ARCHPATH) -specs=$(ARCH).specs -nodefaultlibs -lc -lgcc -T $(MACH).ld
LDDEPS := $(MACHPATH)/$(MACH).ld $(ARCHPATH)/$( crt0.o $(ARCH).ld $(ARCH).specs)
MKIMAGEFLAGS := -A $(ARCH) -O linux
KERNELMKIMAGE := $(MKIMAGEFLAGS) -T kernel
LIBOBJECTS := $(ROOT)/libs/$( $(LIBS))

# Get machine-specific stuff
include $(SHAREDPATH)/shared.mk
include $(ARCHPATH)/$(ARCH).mk
include $(MACHPATH)/$(MACH).mk

# Rules

ifdef OBJECTS
%.uimage: %.bin
	$(MKIMAGE) -C none $(KERNELMKIMAGE) -n $(basename $(input)) -d $(input) $(output)

%.bin: %.elf
	$(OBJCOPY) -O binary $(input) $(output)

$(notdir $(CURDIR)).elf: $(OBJECTS) $(MACHPATH)/$( $(MACHOBJECTS)) $(ARCHPATH)/$( $(ARCHOBJECTS)) $(SHAREDPATH)/$( $(SHAREDOBJECTS)) $(LIBOBJECTS) $(LDDEPS)
	$(CC) $(OBJECTS) $(MACHPATH)/$( $(MACHOBJECTS)) $(ARCHPATH)/$( $(ARCHOBJECTS)) $(SHAREDPATH)/$( $(SHAREDOBJECTS)) $(LIBOBJECTS) $(LDFLAGS) $(SYSLIBS) -o $(output)
endif

%.o: %.c $(prebuild $(ROOT)/libs/$( $(dir $(LIBS)))stamp-include)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.S
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.s
	$(CC) $(ASFLAGS) -c $(input) -o $(output)

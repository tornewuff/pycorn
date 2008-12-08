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
CFLAGS := -g -Wall -O2 -fomit-frame-pointer -std=gnu99 -Werror
CPPFLAGS := -I$( $(PYINCLUDE))/include
PYCFLAGS := -fomit-frame-pointer -Werror -Wno-error=strict-aliasing -Wno-error=char-subscripts
LDFLAGS := -B$(MACHPATH) -B$(ARCHPATH) -nostdlib -lc -lgcc -T $(MACH).ld
LDDEPS := $(MACHPATH)/$(MACH).ld $(ARCHPATH)/$(ARCH).ld
MKIMAGEFLAGS := -A $(ARCH) -O linux
KERNELMKIMAGE := $(MKIMAGEFLAGS) -T kernel
FREEZEDIRS := $( $(SHAREDPATH) $(ARCHPATH))/frozen

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

$(notdir $(CURDIR)).elf: $(ARCHPATH)/$( $(ARCHOBJECTS)) $(MACHPATH)/$( $(MACHOBJECTS)) $(SHAREDPATH)/$( $(SHAREDOBJECTS)) $(OBJECTS) $(LDDEPS)
	$(CC) $(ARCHPATH)/$( $(ARCHOBJECTS)) $(MACHPATH)/$( $(MACHOBJECTS)) $(SHAREDPATH)/$( $(SHAREDOBJECTS)) $(OBJECTS) $(LDFLAGS) $(SYSLIBS) -o $(output)
endif

%.o: %.c $(prebuild $( $(PYINCLUDE))/stamp-include)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.S
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.s
	$(CC) $(ASFLAGS) -c $(input) -o $(output)

# Disable makepp's builtin rules as they're not particularly cross-compile-y
makepp_no_builtin = 1

# Target selection, stuck here for now
ARCH := arm
MACH := pxa270
PREFIX := arm-eabi-

# Commands
CC := $(PREFIX)gcc
AR := $(PREFIX)ar
OBJCOPY := $(PREFIX)objcopy

# Flags and paths
SHAREDPATH := $(ROOT)/shared
ARCHPATH := $(ROOT)/arch/$(ARCH)
MACHPATH := $(ROOT)/arch/$(ARCH)/machine/$(MACH)
CFLAGS := -g -Wall -O2 -pipe -fomit-frame-pointer -std=gnu99 -Werror
CPPFLAGS := -I$( $(PYINCLUDE))/include
PYCFLAGS := -pipe -fomit-frame-pointer -fno-strict-aliasing -Werror -Wno-error=char-subscripts
LDFLAGS := -L$(absolute_filename $(MACHPATH)) -L$(absolute_filename $(ARCHPATH)) -L$(absolute_filename $(ROOT)/embryo) -specs=$(absolute_filename $(ARCHPATH)/embryo/embryo.specs) -Tembryo.ld
LDDEPS := $(MACHPATH)/embryo.ld $(ARCHPATH)/$(ARCH).ld $(ROOT)/embryo/libembryo.a
FREEZEDIRS := $( $(SHAREDPATH) $(ARCHPATH))/frozen

# Get machine-specific stuff
include $(ARCHPATH)/$(ARCH).mk
include $(MACHPATH)/$(MACH).mk

# Include shared python modules
include $(ROOT)/pymodules/pymodules.mk

# Rules

ifdef OBJECTS
%.bin: %.elf
	$(OBJCOPY) -O binary $(input) $(output)

$(notdir $(CURDIR)).elf: $(OBJECTS) $(LDDEPS)
	$(CC) $(OBJECTS) $(LDFLAGS) $(SYSLIBS) -o $(output)
endif

%.o: %.c $(prebuild $( $(PYINCLUDE))/stamp-include)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.S
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.s
	$(CC) $(ASFLAGS) -c $(input) -o $(output)

# Disable makepp's builtin rules as they're not particularly cross-compile-y
makepp_no_builtin := 1

include $(ROOT)/config.mk

# Commands
CC := $(PREFIX)gcc
AR := $(PREFIX)ar
OBJCOPY := $(PREFIX)objcopy
PYTHON := hostpython

# Paths
ARCHPATH := $(ROOT)/arch/$(ARCH)
MACHPATH := $(ROOT)/arch/$(ARCH)/machine/$(MACH)

# Flags
CFLAGS := -g -Wall -O2 -pipe -fomit-frame-pointer -std=gnu99 -Werror
CPPFLAGS := -I$( $(PYINCLUDE))/include
PYCFLAGS := -pipe -fomit-frame-pointer -fno-strict-aliasing -Werror -Wno-error=char-subscripts -Wno-error=unused-function -Wno-error=unused-variable
LDFLAGS := -L$(absolute_filename $(MACHPATH)) -L$(absolute_filename $(ARCHPATH)) -Tembryo.ld
LDDEPS := $(MACHPATH)/embryo.ld $(ARCHPATH)/$(ARCH).ld

# Get machine-specific stuff
include $(ARCHPATH)/$(ARCH).mk
include $(MACHPATH)/$(MACH).mk

# Include arch-independant metal code
include $(ROOT)/metal/metal.mk

PYTHONVER := 2.5.5
PYTHONVERMINOR := 2.5

# Rules

ifndef USE_BINARIES
%.o: %.c $(prebuild $( $(PYINCLUDE))/stamp-include)
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.S
	$(CC) $(ASFLAGS) $(CPPFLAGS) -c $(input) -o $(output)

%.o: %.s
	$(CC) $(ASFLAGS) -c $(input) -o $(output)
endif

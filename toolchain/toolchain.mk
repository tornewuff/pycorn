CFLAGS := -O2 -pipe
LDFLAGS :=
TOOLSTARGET := arm-eabi

BINUTILSVER := 2.22
BINUTILSTAR := binutils-$(BINUTILSVER).tar.bz2
BINUTILSDIR := binutils-$(BINUTILSVER)

GCCVER := 4.6.3
GCCTAR := gcc-core-$(GCCVER).tar.bz2
GCCDIR := gcc-$(GCCVER)

GDBVER := 7.4
GDBTAR := gdb-$(GDBVER).tar.bz2
GDBDIR := gdb-$(GDBVER)

NEWLIBVER := 1.18.0
NEWLIBTAR := newlib-$(NEWLIBVER).tar.gz
NEWLIBDIR := newlib-$(NEWLIBVER)

PYTHONVER := 2.5.5
PYTHONTAR := Python-$(PYTHONVER).tar.bz2
PYTHONDIR := Python-$(PYTHONVER)

export PATH := $(TOOLSPREFIX)/bin:$(PATH)

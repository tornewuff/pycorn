CFLAGS := -O2 -pipe
LDFLAGS :=
TOOLSTARGET := arm-eabi

BINUTILSVER := 2.19.1
BINUTILSTAR := binutils-$(BINUTILSVER).tar.bz2
BINUTILSDIR := binutils-$(BINUTILSVER)

GCCVER := 4.4.1
GCCTAR := gcc-core-$(GCCVER).tar.bz2
GCCDIR := gcc-$(GCCVER)

GDBVER := 6.8
GDBTAR := gdb-$(GDBVER).tar.bz2
GDBDIR := gdb-$(GDBVER)

NEWLIBVER := 1.17.0
NEWLIBTAR := newlib-$(NEWLIBVER).tar.gz
NEWLIBDIR := newlib-$(NEWLIBVER)

export PATH := $(TOOLSPREFIX)/bin:$(PATH)

CFLAGS += -fno-dwarf2-cfi-asm
PYCFLAGS += -fno-dwarf2-cfi-asm
PYMODULES += $(ARCHPATH)/pymodules/$( _bootldrmodule.o _metalcpu_coproc.o _metalcpumodule.o)
MKIMAGE := mkimage
MKIMAGEFLAGS := -A arm -O linux
KERNELMKIMAGE := $(MKIMAGEFLAGS) -T kernel

ifdef OBJECTS
%.uimage: %.bin
       $(MKIMAGE) -C none $(KERNELMKIMAGE) -n $(basename $(input)) -d $(input) $(output)
endif

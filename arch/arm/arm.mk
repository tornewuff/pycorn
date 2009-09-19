CFLAGS += -fno-dwarf2-cfi-asm
PYCFLAGS += -fno-dwarf2-cfi-asm
CPPFLAGS += -I$(ARCHPATH)/embryo/include
PYMODULES += $(ARCHPATH)/pymodules/$( _bootldrmodule.o _metalcpu_coproc.o _metalcpumodule.o)
MKIMAGE := mkimage
MKIMAGEFLAGS := -A arm -O linux
KERNELMKIMAGE := $(MKIMAGEFLAGS) -T kernel
INITRDMKIMAGE := $(MKIMAGEFLAGS) -T ramdisk

ifdef OBJECTS
%.uimage: %.bin
	$(MKIMAGE) -C none $(KERNELMKIMAGE) -n $(basename $(input)) -d $(input) $(output)

ifdef RAMDISK
$(RAMDISK).uimage: $(RAMDISK)
	$(MKIMAGE) -C none $(INITRDMKIMAGE) -n $(input) -d $(input) $(output)
endif
endif

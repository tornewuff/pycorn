CFLAGS += -fno-dwarf2-cfi-asm
PYCFLAGS += -fno-dwarf2-cfi-asm
EXTMODULES += $(ARCHPATH)/metal/ext/$( _bootstrapmodule.o _metalcpu_coproc.o _metalcpumodule.o)
FREEZEDIRS += $(ARCHPATH)/metal/frozen
INITRDDIRS += $(ARCHPATH)/metal/initrd

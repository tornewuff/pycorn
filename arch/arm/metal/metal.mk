EXTMODULES += $(ARCHPATH)/metal/ext/$( _bootstrapmodule.o _metalcpu_coproc.o _metalcpumodule.o)
FREEZEDIRS += $(ARCHPATH)/metal/frozen
FREEZEDEPS += $(ARCHPATH)/metal/frozen/**/*.py
INITRDDIRS += $(ARCHPATH)/metal/initrd
INITRDDEPS += $(ARCHPATH)/metal/initrd/**

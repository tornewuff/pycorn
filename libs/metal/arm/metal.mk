EXTMODULES += $(METALDIR)/$(ARCH)/$( _metalcpu_coproc.o _metalcpumodule.o)
FREEZEDIRS += $(METALDIR)/$(ARCH)/frozen
FREEZEDEPS += $(METALDIR)/$(ARCH)/frozen/**/*.py
INITRDDIRS += $(METALDIR)/$(ARCH)/initrd
INITRDDEPS += $(METALDIR)/$(ARCH)/initrd/**/*

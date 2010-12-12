METALDIR := $(ROOT)/libs/metal

EXTMODULES += $(METALDIR)/_metalmemmodule.o
INITRDDIRS += $(METALDIR)/initrd
INITRDDEPS += $(METALDIR)/initrd/**

include $(METALDIR)/$(ARCH)/metal.mk

EMBRYODIR := $(ROOT)/libs/embryo
EMBRYOPATHS := $(EMBRYODIR)/$(ARCH)/$(MACH) $(EMBRYODIR)/$(ARCH) $(EMBRYODIR)
EMBRYOPATHSABS := $(absolute_filename $(EMBRYOPATHS))

include $(ROOT)/libs/embryo/$(ARCH)/$(MACH)/embryo.mk
include $(ROOT)/libs/embryo/$(ARCH)/embryo.mk

CPPFLAGS += -I$(wildcard $(EMBRYOPATHSABS)/include)
LDFLAGS += -L$(EMBRYOPATHSABS) -specs=$(absolute_filename $(EMBRYODIR)/embryo.specs) -Tembryo.ld
LDDEPS += $(EMBRYODIR)/embryo.specs $(EMBRYODIR)/libembryo.a $(EMBRYODIR)/$(ARCH)/$(MACH)/embryo.ld $(EMBRYODIR)/$(ARCH)/$(ARCH).ld

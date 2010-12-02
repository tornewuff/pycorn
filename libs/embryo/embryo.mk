EMBRYODIR := $(ROOT)/libs/embryo
EMBRYOPATHS := $(EMBRYODIR)/arch/$(ARCH) $(EMBRYODIR)
EMBRYOPATHSABS := $(absolute_filename $(EMBRYOPATHS))

CPPFLAGS += -I$(EMBRYOPATHSABS)/include
LDFLAGS += -L$(EMBRYOPATHSABS) -specs=$(absolute_filename $(EMBRYODIR)/embryo.specs)
LDDEPS += $(EMBRYODIR)/embryo.specs $(EMBRYODIR)/libembryo.a

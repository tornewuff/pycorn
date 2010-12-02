EMBRYOPATHS := $(ROOT)/libs/embryo/arch/$(ARCH) $(ROOT)/libs/embryo
EMBRYOPATHSABS := $(absolute_filename $(EMBRYOPATHS))

CPPFLAGS += -I$(EMBRYOPATHSABS)/include
LDFLAGS += -L$(EMBRYOPATHSABS) -specs=$(absolute_filename $(ROOT)/libs/embryo/embryo.specs)
LDDEPS += $(ROOT)/libs/embryo/embryo.specs $(ROOT)/libs/embryo/libembryo.a

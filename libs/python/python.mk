PYTHONDIR := $(ROOT)/libs/python

CPPFLAGS += -I$(PYTHONDIR)/include
CPPDEPS += $(PYTHONDIR)/stamp-include
LDFLAGS += -L$(PYTHONDIR) -lpython$(PYTHONVERMINOR) -lm
LDDEPS += $(PYTHONDIR)/libpython$(PYTHONVERMINOR).a

FREEZEDIRS += $(PYTHONDIR)/frozen
FREEZEDEPS += $(PYTHONDIR)/stamp-frozen
INITRDDIRS += $(PYTHONDIR)/initrd
INITRDDEPS += $(PYTHONDIR)/stamp-initrd

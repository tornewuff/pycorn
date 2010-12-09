CPPFLAGS += -I$(ROOT)/libs/python/include
CPPDEPS += $(ROOT)/libs/python/stamp-include
LDFLAGS += -L$(ROOT)/libs/python -lpython$(PYTHONVERMINOR) -lm
LDDEPS += $(ROOT)/libs/python/libpython$(PYTHONVERMINOR).a

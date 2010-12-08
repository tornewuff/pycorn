EMBRYODIR := $(ROOT)/libs/embryo
EMBRYOPATHS := $(EMBRYODIR)/$(ARCH)/$(MACH) $(EMBRYODIR)/$(ARCH) $(EMBRYODIR)
EMBRYOPATHSABS := $(absolute_filename $(EMBRYOPATHS))

MKIMAGE := $(PYTHON) $(EMBRYODIR)/mkimage.py
MKIMAGEFLAGS := -A $(ARCH) -O linux
KERNELMKIMAGE := $(MKIMAGEFLAGS) -T kernel
INITRDMKIMAGE := $(MKIMAGEFLAGS) -T ramdisk

CPPFLAGS += -I$( $(wildcard $(EMBRYOPATHSABS)/include))
LDFLAGS += -L$(EMBRYOPATHSABS) -specs=$(absolute_filename $(EMBRYODIR)/embryo.specs) -Tembryo.ld
LDDEPS += $(EMBRYODIR)/embryo.specs $(EMBRYODIR)/libembryo.a $(EMBRYODIR)/$(ARCH)/$(MACH)/embryo.ld $(EMBRYODIR)/$(ARCH)/$(ARCH).ld

include $(ROOT)/libs/embryo/$(ARCH)/$(MACH)/embryo.mk
include $(ROOT)/libs/embryo/$(ARCH)/embryo.mk

ifdef MAKING_SEED

ifndef USE_BINARIES
$(TARGET).uimage: $(TARGET).bin $(EMBRYODIR)/mkimage.py
	$(MKIMAGE) -C none $(KERNELMKIMAGE) -n $(basename $(input)) -d $(input) $(output)

$(TARGET).bin: $(TARGET).elf
	$(OBJCOPY) -O binary $(input) $(output)

$(TARGET).elf: $(OBJECTS) $(LDDEPS)
	$(CC) $(OBJECTS) $(LDFLAGS) $(SYSLIBS) -o $(output)
endif

ifdef RAMDISK
$(RAMDISK).uimage: $(RAMDISK) $(EMBRYODIR)/mkimage.py
	$(MKIMAGE) -C none $(INITRDMKIMAGE) -n $(input) -d $(input) $(output)
endif

endif

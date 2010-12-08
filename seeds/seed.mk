$(phony all): $(TARGET).flash

MAKING_SEED := 1

include common.mk
include $(ROOT)/libs/embryo/embryo.mk

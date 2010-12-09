# binutils 2.19 generates a loadable .eh_frame section from .cfi directives
# even though it should be generating a non-loadable .debug_frame section.
# so, until we are using a newer binutils, add this option to force gcc to
# generate the unwinding info itself, which it does right
CFLAGS += -fno-dwarf2-cfi-asm
PYCFLAGS += -fno-dwarf2-cfi-asm

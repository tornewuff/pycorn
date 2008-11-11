/* Default linker script, for normal executables */
OUTPUT_FORMAT("elf32-littlearm", "elf32-bigarm",
	      "elf32-littlearm")
OUTPUT_ARCH(arm)
ENTRY(_start)
__image_loads_to__  = 0xa0008000;
__init_heap_size__  = 0x400000;
__stack_end__       = 0xa4000000;
SECTIONS
{
  . = __image_loads_to__;
  .text :
  {
    __text_start__ = .;
    *(.start)
    KEEP (*(.init))
    *(.text .stub .text.* .gnu.linkonce.t.*)
    /* .gnu.warning sections are handled specially by elf32.em.  */
    *(.gnu.warning)
    *(.glue_7t) *(.glue_7) *(.vfp11_veneer) *(.v4_bx)
    KEEP (*(.fini))
    *(.rodata .rodata.* .gnu.linkonce.r.*)
    . = ALIGN(0x1000); /* make .text a whole number of pages */
    __text_end__ = .;
  }
  .data :
  {
    __data_start__ = .;
    PROVIDE_HIDDEN (__preinit_array_start = .);
    KEEP (*(.preinit_array))
    PROVIDE_HIDDEN (__preinit_array_end = .);
    PROVIDE_HIDDEN (__init_array_start = .);
    KEEP (*(SORT(.init_array.*)))
    KEEP (*(.init_array))
    PROVIDE_HIDDEN (__init_array_end = .);
    PROVIDE_HIDDEN (__fini_array_start = .);
    KEEP (*(.fini_array))
    KEEP (*(SORT(.fini_array.*)))
    PROVIDE_HIDDEN (__fini_array_end = .);
    *(.data .data.* .gnu.linkonce.d.*)
    . = ALIGN(0x1000); /* make .data a whole number of pages */
    __data_end__ = .;
  }
  .bss :
  {
    __bss_start__ = .;
    *(.bss .bss.* .gnu.linkonce.b.*)
    *(COMMON)
    . = ALIGN(0x1000); /* make .bss a whole number of pages */
    __bss_end__ = .;
    __heap_start__ = .;
    __heap_end__ = __heap_start__ + __init_heap_size__;
  }
}

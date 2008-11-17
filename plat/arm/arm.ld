/* Default linker script, for normal executables */
OUTPUT_FORMAT("elf32-littlearm", "elf32-bigarm",
	      "elf32-littlearm")
OUTPUT_ARCH(arm)
ENTRY(_start)
SECTIONS
{
  .text (DEFINED(__rom_map_start__) ? __rom_map_start__ : __image_loads_to__) :
    AT(DEFINED(__rom_map_start__) ? 0 : __image_loads_to__)
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
  .data (DEFINED(__ram_map_start__) ? __ram_map_start__ : . ) :
    AT((DEFINED(__ram_map_start__) ? 0 : __image_loads_to__) + SIZEOF(.text))
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
    __heap_end__ = __heap_start__ + __heap_size_min__;
  }
  __stack_end__ = DEFINED(__ram_map_end__) ? __ram_map_end__ : __ram_end__;
  __stack_start__ = __stack_end__ - __stack_size_min__;
}

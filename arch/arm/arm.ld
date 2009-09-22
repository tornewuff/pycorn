/* GO GO MAGICAL LINKER SCRIPT
 *
 * This is intended to be included by a platform-specific script which defines
 * the various symbols used - it will produce either a virtually or physically
 * linked image depending which symbols are present.
 *
 * Some of the sections included might not actually appear in any inputs but
 * best not to mess with it too much :)
 */
OUTPUT_FORMAT("elf32-littlearm", "elf32-bigarm",
              "elf32-littlearm")
OUTPUT_ARCH(arm)

/* The entry point is in libembryo.a rather than being a seperate crt0.o
 * so we force the object with _start in to be included in the link here.
 */
ENTRY(_start)
EXTERN(_start)

/* Default size of the fixed heap */
/* It doesn't grow yet, but it may later, so it's a minimum. */
__heap_size_min__  = 0x400000;
/* Default size of the original thread stack */
/* It doesn't grow yet, but it may later, so it's a minimum. */
__stack_size_min__ = 0x20000;

/* For virtual addressing, we need definitions for:
 *   __rom_map_start__      Start address to map read only data
 *   __ram_map_start__      Start address to map writable data
 *   __ram_map_end__        Last address to map writable data
 *   __dbg_serial_virt__    Virtual address to map serial registers at
 *
 * For physical addressing, we need definitions for:
 *   __image_loads_to__     Physical address where image will be loaded
 *   __ram_end__            Last physical address of writable memory
 *   __dbg_serial_phys__    Physical address of serial registers
 */

SECTIONS
{
  .text (DEFINED(__rom_map_start__) ? __rom_map_start__ : __image_loads_to__) :
    AT(DEFINED(__rom_map_start__) ? 0 : __image_loads_to__)
  {
    __text_start__ = .;
    *(.start)
    *(.text .stub .text.* .gnu.linkonce.t.*)
    /* .gnu.warning sections are handled specially by elf32.em.  */
    *(.gnu.warning)
    *(.glue_7t) *(.glue_7) *(.vfp11_veneer) *(.v4_bx)
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
  __dbg_serial_virt__ = DEFINED(__dbg_serial_virt__) ? __dbg_serial_virt__ : __dbg_serial_phys__;
}

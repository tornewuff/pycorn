/* 0 - 0000ffff is left unmapped to guard against null pointer derefs */
__ram_map_start__   = 0x00010000;
/* FCSE cuts virtual address space into 32MB chunks, we might use this later
 * so for now all the RAM mappings are expected to fit in the first 32MB */
__ram_map_end__     = 0x02000000;
/* ROM gets stuffed up near the top out of the way */
__rom_map_start__   = 0xf0000000;
/* fixed address mappings go right at the top of the address space just below
 * the vectors */
__page_dir_virt__   = 0xfff00000; /* 16kb */
__dbg_serial_virt__ = 0xfff10000; /* 4kb */

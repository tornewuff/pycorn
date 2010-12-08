/* pxa270 has a 16550-compatible UART here with 32-bit register spacing */
__dbg_serial_phys__ = 0x40100000;

INCLUDE arm-mmu-single.ld
INCLUDE arm.ld

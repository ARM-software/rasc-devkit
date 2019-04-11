SCRIPT_NAME=elf
OUTPUT_FORMAT="elf32-rasc-little"
MAXPAGESIZE=4
TEXT_START_ADDR=0
ARCH=rasc
EMBEDDED=yes

# Set a default stack of 0x4000 bytes unless the user has defined his own
# stack (by creating an array called _rasc_stack in a .rascstack section).
OTHER_BSS_SECTIONS=".rascstack        :
  {
    *(.rascstack)
    . = . + (DEFINED (_rasc_stack) ? 0 : (DEFINED (__rascstack_size) ? __rascstack_size : 0x4000));
    _stack = .;
    _stack_size = SIZEOF(.rascstack);
  }"

TEMPLATE_NAME=elf32


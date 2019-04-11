ARM RASC Devkit
===============

The ARM RASC devkit is a modified GCC toolchain containing a C compiler and associated binutils for use with ARM's internal RASC microprocessor.

## Building

Fetch all files and change to the base directory containing _makefile_.

```
> make rasc-all
```
or
```
> make rasc-gcc rasc-gcc-install
> make rasc-binutils rasc-binutils-install
```

The standard installation _PREFIX_ variable defaults to '$(pwd)/out' and can be changed on the command line
if required. Resulting binaries end up in '$(PREFIX)/bin' and are location independent.

## Support

This modified source has not been compiled for targets other than the ARM RASC; nor expected to work for
purposes other than generating and disassembling RASC binaries. No support is provided nor will this release
be maintained.

This source was originally based on _gcc 4.2.3_ revision _132046_, from GCC's 4.2 branch:
```
> svn://gcc.gnu.org/svn/gcc/branches/gcc-4_2-branch/ -r 132046
```


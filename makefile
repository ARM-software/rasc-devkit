all : rasc-all

# Override the default prefix with: make PREFIX=/my/installdir
# Note: the GCC build infrastructure do not handle relative paths.
PREFIX=$(shell pwd)/out

# Add to path
PATH:=$(PREFIX)/bin:$(PATH)
export PATH

# Build-directory suffix
BSUFFIX=build

# Absolute path to the RASC include files.
RASC_INCLUDE_DIR=$(shell cd newlib/newlib/libc/include && pwd)

# The GCC VERSUFFIX
revision=$(shell svn info | awk '/Revision:/ { print $$2 }')
builddate=$(shell date "+%F %R")
VERSUFFIX=$(shell printf " (rasc-devkit-1.1.4 r$(revision) $(builddate))")

##################################################################
#
# Make the RASC toolchain
#
rasc-binutils:
	rm -rf rasc-binutils-$(BSUFFIX)
	mkdir rasc-binutils-$(BSUFFIX)
	cd rasc-binutils-$(BSUFFIX) && \
		env CC="gcc -m32" ../binutils/configure \
			--target=rasc-elf \
			--prefix=$(PREFIX) \
			--program-prefix=rasc-elf- \
			--disable-nls
	env CC="gcc -m32" make -C rasc-binutils-$(BSUFFIX) all
.PHONY : rasc-binutils

rasc-binutils-install:
	make -C rasc-binutils-$(BSUFFIX) install

rasc-gcc:
	rm -rf rasc-gcc-$(BSUFFIX)
	mkdir rasc-gcc-$(BSUFFIX)
	cd gcc && \
		contrib/gcc_update --touch
	cd rasc-gcc-$(BSUFFIX) && \
		env CC="gcc -m32" ../gcc/configure \
			--target=rasc-elf \
			--prefix=$(PREFIX) \
			--program-prefix=rasc-elf- \
			--with-newlib \
			--with-headers=$(RASC_INCLUDE_DIR) \
			--disable-nls \
			--enable-languages="c" && \
		sed 's/#define VERSUFFIX .*/#define VERSUFFIX "$(VERSUFFIX)"/'\
			../gcc/gcc/version.c > ../gcc/gcc/version.c.tmp && \
		mv ../gcc/gcc/version.c.tmp ../gcc/gcc/version.c
	env CC="gcc -m32" make -C rasc-gcc-$(BSUFFIX) all
.PHONY : rasc-gcc

rasc-gcc-install:
	make -C rasc-gcc-$(BSUFFIX) install

rasc-all: rasc-binutils rasc-binutils-install
rasc-all: rasc-gcc rasc-gcc-install

clean:
	rm -rf rasc-binutils-$(BSUFFIX)
	rm -rf rasc-gcc-$(BSUFFIX)

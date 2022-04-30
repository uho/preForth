# Makefile for preForth and seedForth
#
# make bootstrap should produce two identical files: preForth1.$(ASM) and preForth.$(ASM) 

# Set HOSTFORTH to the Forth system that generates the initial preForth
# ------------------------------------------------------------------------
HOSTFORTH=gforth
# HOSTFORTH=sf   # SwiftForth >3.7
# ------------------------------------------------------------------------

ASZ80=../asxv5pxx/asxmak/linux/exe/asz80
ASLINK=../asxv5pxx/asxmak/linux/exe/aslink

# need to install intelhex package in Python first:
#   pip3 install --user intelhex
HEX2BIN=python3 $(HOME)/.local/bin/hex2bin.py

EMU_Z80=../emu_z80/emu_z80

.PHONY: all
all: \
preForthDemo.bin \
preForth.bin \
seedForth.bin \
seedForthDemo.seed \
seedForthBoot.seed \
seedForthInteractive.seed

.PHONY: test
test: runseedforthdemo runseedforthinteractive

.PHONY: runseedforthdemo
runseedforthdemo: seedForth seedForthDemo.seed
	./seedForth seedForthDemo.seed

.PHONY: runseedfortinteractive
runseedforthinteractive: seedForth seedForthInteractive.seed
	./seed

UNIXFLAVOUR=$(shell uname -s)
ASM=asm

# build preForthDemo via the host Forth first, so that it
# can be used for basic debugging of the preForth runtime
preForthDemo.$(ASM): \
../common/preForth-bootstrap.fs \
../common/preForth-cold.fs \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-z80-backend.pre \
../common/preForth.pre \
../common/preForthDemo.pre
	cat \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
../common/preForthDemo.pre \
|$(HOSTFORTH) \
../common/preForth-bootstrap.fs \
../common/preForth-rts-nonstandard.pre \
preForth-z80-backend.pre \
../common/preForth.pre \
../common/preForth-cold.fs \
>$@

preForth.$(ASM): \
../common/preForth-bootstrap.fs \
../common/preForth-cold.fs \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-z80-backend.pre \
../common/preForth.pre
	cat \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-z80-backend.pre \
../common/preForth.pre \
|$(HOSTFORTH) \
../common/preForth-bootstrap.fs \
../common/preForth-rts-nonstandard.pre \
preForth-z80-backend.pre \
../common/preForth.pre \
../common/preForth-cold.fs \
>$@

%.$(ASM): \
%.pre \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth.bin
	$(EMU_Z80) preForth.bin \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
$< \
>$@

%.bin: %.$(ASM)
	$(ASZ80) -l -o $<
	$(ASLINK) -n -m -u -i $(<:.$(ASM)=.ihx) $(<:.$(ASM)=.rel)
	$(HEX2BIN) $(<:.$(ASM)=.ihx) $@

# run preForth on its own source code to perform a bootstrap 
# should produce identical results
bootstrap: \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-z80-backend.pre \
../common/preForth.pre \
preForth.bin \
preForth.$(ASM)
	$(EMU_Z80) preForth.bin \
preForth-z80-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-z80-backend.pre \
../common/preForth.pre \
>preForth1.$(ASM)
	cmp preForth.$(ASM) preForth1.$(ASM)

# preForth connected to stdin - output to stdout
.PHONY: visible-bootstrap
visible-bootstrap: preForth.bin preForth-z80-backend.pre ../common/preForth.pre 
	$(EMU_Z80) preForth.bin preForth-z80-backend.pre ../common/preForth.pre

# ------------------------------------------------------------------------
# Docker support (for Linux version)
# ------------------------------------------------------------------------
# create a linux image based on Dockerfile
.PHONY: docker-image
docker-image: Dockerfile
	docker build -t preforth .

# run the docker image
.PHONY: run
rundocker: docker-image
	docker run -i -t --rm preforth /preForth/seed
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# seedForth
# ------------------------------------------------------------------------
seedForth.$(ASM): \
seedForth-z80-header.pre \
preForth-z80-rts.pre \
seedForth-z80-rts.pre \
seedForth-z80.pre \
../common/seedForth16bit.pre \
../common/seedForth.pre \
preForth.bin
	$(EMU_Z80) preForth.bin \
seedForth-z80-header.pre \
preForth-z80-rts.pre \
seedForth-z80-rts.pre \
seedForth-z80.pre \
../common/seedForth16bit.pre \
../common/seedForth.pre \
>seedForth.$(ASM)

# a little ugly, because gforth insists upon echoing anything read from
# stdin, and also does not allow parsing words to cross a file boundary,
# so we will concatenate the tokenizer and source into a *.fs file first
seedForthDemo.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthDemo.seedsource \
seedForthDemoz80.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthBoot.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthRuntime16bit.seedsource \
../common/seedForthRuntime.seedsource \
seedForthRuntimez80.seedsource \
../common/seedForthBoot.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthInteractive.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthRuntime16bit.seedsource \
../common/seedForthRuntime.seedsource \
seedForthRuntimez80.seedsource \
../common/seedForthInteractive.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

.PHONY: clean
clean:
	rm -f *.$(ASM) *.o *.seed preForthDemo preForth seedForth __temp__.fs

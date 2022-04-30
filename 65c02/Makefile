# Makefile for preForth and seedForth
#
# make bootstrap should produce two identical files: preForth1.$(ASM) and preForth.$(ASM) 

# Set HOSTFORTH to the Forth system that generates the initial preForth
# ------------------------------------------------------------------------
HOSTFORTH=gforth
# HOSTFORTH=sf   # SwiftForth >3.7
# ------------------------------------------------------------------------

AS6500=../asxv5pxx/asxmak/linux/exe/as6500
ASLINK=../asxv5pxx/asxmak/linux/exe/aslink

# need to install intelhex package in Python first:
#   pip3 install --user intelhex
HEX2BIN=python3 $(HOME)/.local/bin/hex2bin.py

LOAD_ADDR=0x300

EMU_65C02=../emu_65c02/emu_65c02

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
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-65c02-backend.pre \
../common/preForth.pre \
../common/preForthDemo.pre
	cat \
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
../common/preForthDemo.pre \
|$(HOSTFORTH) \
../common/preForth-bootstrap.fs \
../common/preForth-rts-nonstandard.pre \
preForth-65c02-backend.pre \
../common/preForth.pre \
../common/preForth-cold.fs \
>$@

preForth.$(ASM): \
../common/preForth-bootstrap.fs \
../common/preForth-cold.fs \
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-65c02-backend.pre \
../common/preForth.pre
	cat \
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-65c02-backend.pre \
../common/preForth.pre \
|$(HOSTFORTH) \
../common/preForth-bootstrap.fs \
../common/preForth-rts-nonstandard.pre \
preForth-65c02-backend.pre \
../common/preForth.pre \
../common/preForth-cold.fs \
>$@

%.$(ASM): \
%.pre \
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth.bin
	$(EMU_65C02) preForth.bin \
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
$< \
>$@

%.bin: %.$(ASM)
	$(AS6500) -l -o $<
	$(ASLINK) -n -m -u -i -b zpage=0 -b text=$(LOAD_ADDR) $(<:.$(ASM)=.ihx) $(<:.$(ASM)=.rel)
	$(HEX2BIN) $(<:.$(ASM)=.ihx) $@

# run preForth on its own source code to perform a bootstrap 
# should produce identical results
bootstrap: \
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-65c02-backend.pre \
../common/preForth.pre \
preForth.bin \
preForth.$(ASM)
	$(EMU_65C02) preForth.bin \
preForth-65c02-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-65c02-backend.pre \
../common/preForth.pre \
>preForth1.$(ASM)
	cmp preForth.$(ASM) preForth1.$(ASM)

# preForth connected to stdin - output to stdout
.PHONY: visible-bootstrap
visible-bootstrap: preForth.bin preForth-65c02-backend.pre ../common/preForth.pre 
	$(EMU_65C02) preForth.bin preForth-65c02-backend.pre ../common/preForth.pre

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
seedForth-65c02-header.pre \
preForth-65c02-rts.pre \
seedForth-65c02-rts.pre \
seedForth-65c02.pre \
../common/seedForth16bit.pre \
../common/seedForth.pre \
preForth.bin
	$(EMU_65C02) preForth.bin \
seedForth-65c02-header.pre \
preForth-65c02-rts.pre \
seedForth-65c02-rts.pre \
seedForth-65c02.pre \
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
seedForthDemo65c02.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthBoot.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthRuntime16bit.seedsource \
../common/seedForthRuntime.seedsource \
seedForthRuntime65c02.seedsource \
../common/seedForthBoot.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthInteractive.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthRuntime16bit.seedsource \
../common/seedForthRuntime.seedsource \
seedForthRuntime65c02.seedsource \
../common/seedForthInteractive.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

.PHONY: clean
clean:
	rm -f *.$(ASM) *.o *.seed preForthDemo preForth seedForth __temp__.fs

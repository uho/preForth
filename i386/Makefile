# Makefile for preForth and seedForth
#
# make bootstrap should produce two identical files: preForth1.$(ASM) and preForth.$(ASM) 

# Set HOSTFORTH to the Forth system that generates the initial preForth
# ------------------------------------------------------------------------
HOSTFORTH=gforth
# HOSTFORTH=sf   # SwiftForth >3.7
# ------------------------------------------------------------------------

.PHONY: all
all: \
preForthDemo \
preForth \
seedForth \
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
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-i386-backend.pre \
../common/preForth.pre \
../common/preForthDemo.pre
	cat \
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
../common/preForthDemo.pre \
|$(HOSTFORTH) \
../common/preForth-bootstrap.fs \
../common/preForth-rts-nonstandard.pre \
preForth-i386-backend.pre \
../common/preForth.pre \
../common/preForth-cold.fs \
>$@

preForth.$(ASM): \
../common/preForth-bootstrap.fs \
../common/preForth-cold.fs \
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-i386-backend.pre \
../common/preForth.pre
	cat \
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-i386-backend.pre \
../common/preForth.pre \
|$(HOSTFORTH) \
../common/preForth-bootstrap.fs \
../common/preForth-rts-nonstandard.pre \
preForth-i386-backend.pre \
../common/preForth.pre \
../common/preForth-cold.fs \
>$@

%.$(ASM): \
%.pre \
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth
	./preForth \
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
$< \
>$@

ifeq ($(UNIXFLAVOUR),Linux)
# assemble and link executable on linux
%: %.$(ASM)
	fasm $< $@.o
	LDEMULATION=elf_i386 ld -arch i386 -o $@ \
-dynamic-linker /lib32/ld-linux.so.2 \
/usr/lib/i386-linux-gnu/crt1.o /usr/lib/i386-linux-gnu/crti.o \
$@.o \
-lc /usr/lib/i386-linux-gnu/crtn.o
	# rm $@.o
else
ifeq ($(UNIXFLAVOUR),Darwin)
# assemble and link executable on MacOS
%: %.$(ASM)
	fasm $< $@.o
	objconv -fmacho32 -nu $@.o $@_m.o
	ld -arch i386 -macosx_version_min 10.6 -o $@ \
$@_m.o /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib/crt1.o \
/usr/lib/libc.dylib
	# rm $@.o $@_m.o
endif
endif

# run preForth on its own source code to perform a bootstrap 
# should produce identical results
bootstrap: \
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-i386-backend.pre \
../common/preForth.pre \
preForth \
preForth.$(ASM)
	./preForth \
preForth-i386-rts.pre \
../common/preForth-rts-nonstandard.pre \
../common/preForth-rts.pre \
preForth-i386-backend.pre \
../common/preForth.pre \
>preForth1.$(ASM)
	cmp preForth.$(ASM) preForth1.$(ASM)

# preForth connected to stdin - output to stdout
.PHONY: visible-bootstrap
visible-bootstrap: preForth preForth-i386-backend.pre ../common/preForth.pre 
	./preForth preForth-i386-backend.pre ../common/preForth.pre

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
seedForth-i386-header.pre \
preForth-i386-rts.pre \
seedForth-i386-rts.pre \
seedForth-i386.pre \
../common/seedForth32bit.pre \
../common/seedForth.pre \
preForth
	./preForth \
seedForth-i386-header.pre \
preForth-i386-rts.pre \
seedForth-i386-rts.pre \
seedForth-i386.pre \
../common/seedForth32bit.pre \
../common/seedForth.pre \
>seedForth.$(ASM)

# a little ugly, because gforth insists upon echoing anything read from
# stdin, and also does not allow parsing words to cross a file boundary,
# so we will concatenate the tokenizer and source into a *.fs file first
seedForthDemo.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthDemo.seedsource \
seedForthDemoi386.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthBoot.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthRuntime32bit.seedsource \
../common/seedForthRuntime.seedsource \
seedForthRuntimei386.seedsource \
../common/seedForthBoot.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthInteractive.seed: \
../common/crc10.forth \
../common/../common/seedForth-tokenizer.fs \
../common/seedForthRuntime32bit.seedsource \
../common/seedForthRuntime.seedsource \
seedForthRuntimei386.seedsource \
../common/seedForthInteractive.seedsource
	cat $^ >__temp__.fs
	$(HOSTFORTH) __temp__.fs -e bye >$@
	rm __temp__.fs

.PHONY: clean
clean:
	rm -f *.$(ASM) *.o *.seed preForthDemo preForth seedForth __temp__.fs

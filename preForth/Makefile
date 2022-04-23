# Makefile for preForth and seedForth
#
# make bootstrap should produce two identical files: preForth1.asm and preForth.asm 

# Set HOSTFORTH to the Forth system that generates the initial preForth
# ------------------------------------------------------------------------
HOSTFORTH=gforth
# HOSTFORTH=sf   # SwiftForth >3.7
# ------------------------------------------------------------------------

.PHONY=all
all: \
preForth \
seedForth \
seedForthDemo.seed \
seedForthBoot.seed \
seedForthInteractive.seed

.PHONY=test
test: runseedforthdemo runseedforthinteractive

.PHONY=runseedforthdemo
runseedforthdemo: seedForth seedForthDemo.seed
	./seedForth seedForthDemo.seed

.PHONY=runseedfortinteractive
runseedforthinteractive: seedForth seedForthInteractive.seed
	./seed

UNIXFLAVOUR=$(shell uname -s)
EXT=asm

preForth.asm: \
preForth-i386-rts.pre \
preForth-rts.pre \
preForth-i386-backend.pre \
preForth.pre \
load-i386-preForth.fs
	cat \
preForth-i386-rts.pre \
preForth-rts.pre \
preForth-i386-backend.pre \
preForth.pre \
|$(HOSTFORTH) load-i386-preForth.fs >preForth.asm

%.asm: %.pre preForth-i386-rts.pre preForth-rts.pre preForth
	./preForth preForth-i386-rts.pre preForth-rts.pre $< >$@

ifeq ($(UNIXFLAVOUR),Linux)
# assemble and link executable on linux
%: %.asm
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
%: %.asm
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
preForth-rts.pre \
preForth-i386-backend.pre \
preForth.pre \
preForth \
preForth.$(EXT)
	./preForth \
preForth-i386-rts.pre \
preForth-rts.pre \
preForth-i386-backend.pre \
preForth.pre \
>preForth1.$(EXT)
	cmp preForth.$(EXT) preForth1.$(EXT)

# preForth connected to stdin - output to stdout
.PHONY=visible-bootstrap
visible-bootstrap: preForth preForth-i386-backend.pre preForth.pre 
	./preForth preForth-i386-backend.pre preForth.pre

# ------------------------------------------------------------------------
# Docker support (for Linux version)
# ------------------------------------------------------------------------
# create a linux image based on Dockerfile
.PHONY=docker-image
docker-image: Dockerfile
	docker build -t preforth .

# run the docker image
.PHONY=run
rundocker: docker-image
	docker run -i -t --rm preforth /preForth/seed
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# seedForth
# ------------------------------------------------------------------------
seedForth.$(EXT): \
seedForth-i386-header.pre \
preForth-i386-rts.pre \
seedForth-i386.pre \
seedForth.pre \
preForth
	./preForth \
seedForth-i386-header.pre \
preForth-i386-rts.pre \
seedForth-i386.pre \
seedForth.pre \
>seedForth.$(EXT)

# a little ugly, because gforth insists upon echoing anything read from
# stdin, and also does not allow parsing words to cross a file boundary,
# so we will concatenate the tokenizer and source into a *.fs file first
seedForthDemo.seed: seedForth-tokenizer.fs seedForthDemo.seedsource
	cat $^ >__temp__.fs
	gforth __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthBoot.seed: \
seedForth-tokenizer.fs \
seedForthRuntime.seedsource \
seedForthBoot.seedsource
	cat $^ >__temp__.fs
	gforth __temp__.fs -e bye >$@
	rm __temp__.fs

seedForthInteractive.seed: \
seedForth-tokenizer.fs \
seedForthRuntime.seedsource \
seedForthInteractive.seedsource
	cat $^ >__temp__.fs
	gforth __temp__.fs -e bye >$@
	rm __temp__.fs

.PHONY=clean
clean:
	rm -f *.asm *.o *.seed preForthdemo preForth seedForth __temp__.fs

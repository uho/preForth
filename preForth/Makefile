# Makefile for preForth and seedForth
#
# make bootstrap should produce two identical files: preForth1.asm and preForth.asm 

# Set HOSTFORTH to the Forth system that generates the initial preForth
# ------------------------------------------------------------------------
HOSTFORTH=gforth
# HOSTFORTH=sf   # SwiftForth >3.7
# ------------------------------------------------------------------------

.PHONY=all
all: preForth seedForth seedForthDemo.seed seedForthInteractive.seed

.PHONY=test
test: runseedforthdemo runseedforthinteractive

.PHONY=runseedforthdemo
runseedforthdemo: seedForth seedForthDemo.seed
	cat seedForthDemo.seed |./seedForth

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
	cat preForth-i386-rts.pre preForth-rts.pre $< |./preForth >$@

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
	cat \
preForth-i386-rts.pre \
preForth-rts.pre \
preForth-i386-backend.pre \
preForth.pre \
|./preForth >preForth1.$(EXT)
	cmp preForth.$(EXT) preForth1.$(EXT)

# preForth connected to stdin - output to stdout
.PHONY=visible-bootstrap
visible-bootstrap: preForth preForth-i386-backend.pre preForth.pre 
	cat preForth-i386-backend.pre preForth.pre |./preForth

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
seedForth.$(EXT): seedForth-i386-header.pre seedForth-i386.pre seedForth.pre preForth
	cat seedForth-i386-header.pre seedForth-i386.pre seedForth.pre |./preForth >seedForth.$(EXT)

%.seed: %.seedsource seedForth-tokenizer.fs
	gforth seedForth-tokenizer.fs $<


.PHONY=clean
clean:
	rm -f *.asm *.o *.seed preForthdemo preForth seedForth

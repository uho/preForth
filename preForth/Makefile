# Makefile for preForth
#
# make bootstrap should produce two identical files: preForth1.asm and preForth.asm 
# or preForth1.c and preForth.c


# Set PLATFORM to the intended preForth platform
# ------------------------------------------------------------------------
# PLATFORM=symbolic
PLATFORM=i386
# PLATFORM=C
# ------------------------------------------------------------------------


# Set HOSTFORTH to the Forth system that generates the initial preForth
# ------------------------------------------------------------------------
HOSTFORTH=gforth
# HOSTFORTH=sf   # SwiftForth >3.7
# ------------------------------------------------------------------------

.PHONY=all
all: preForth simpleForth forth runforth

.PHONY=runforth
runforth: ./forth hi.forth
	cat hi.forth - | ./forth

.PHONY=runseedforth
runseedforth: seedForth seedForthDemo.seed
	cat seedForthDemo.seed | ./seedForth


.PHONY=test
test: preForth.pre preForth-$(PLATFORM)-backend.pre load-$(PLATFORM)-preForth.fs 
	cat preForth-$(PLATFORM)-backend.pre simpleForth.pre | $(HOSTFORTH) load-$(PLATFORM)-preForth.fs 

# preForth connected to stdin - output to stdout
.PHONY=visible-bootstrap
visible-bootstrap: preForth preForth-$(PLATFORM)-backend.pre preForth.pre 
	cat preForth-$(PLATFORM)-backend.pre preForth.pre | ./preForth

# ------------------------------------------------------------------------
# i386 version MacOS and Linux
# ------------------------------------------------------------------------
ifeq ($(PLATFORM),C)
  UNIXFLAVOUR=stdC
  EXT=c
else
  UNIXFLAVOUR=$(shell uname -s)
  EXT=asm
endif

seedForth-i386.asm: seedForth-i386.pre preForth
	cat seedForth-i386.pre | ./preForth >seedForth-i386.asm

# preForth connected to stdin - output to preForth.asm
preForth.asm: preForth.pre preForth-$(PLATFORM)-backend.pre load-$(PLATFORM)-preForth.fs
	cat preForth-$(PLATFORM)-rts.pre preForth-rts.pre preForth-$(PLATFORM)-backend.pre preForth.pre \
	    | $(HOSTFORTH) load-$(PLATFORM)-preForth.fs >preForth.asm

preForth: preForth.$(UNIXFLAVOUR)
	cp preForth.$(UNIXFLAVOUR) preForth

%.asm: %.pre preForth preForth-$(PLATFORM)-rts.pre preForth-rts.pre
	cat preForth-$(PLATFORM)-rts.pre preForth-rts.pre $< | ./preForth >$@

%: %.$(UNIXFLAVOUR)
	cp $< $@

# assemble and link executable on linux
%.Linux: %.asm
	fasm $< $@.o
	ld -arch i386 -o $@ \
	   -dynamic-linker /lib32/ld-linux.so.2 \
	   /usr/lib/i386-linux-gnu/crt1.o /usr/lib/i386-linux-gnu/crti.o \
	   $@.o \
	   -lc /usr/lib/i386-linux-gnu/crtn.o
	# rm $@.o

# assemble and link executable on MacOS
%.Darwin: %.asm
	fasm $< $@.o
	objconv -fmacho32 -nu $@.o $@_m.o
	ld -arch i386 -macosx_version_min 10.6 -o $@ \
	  $@_m.o /usr/lib/crt1.o /usr/lib/libc.dylib
	# rm $@.o $@_m.o

# run preForth on its own source code to perform a bootstrap 
# should produce identical results
bootstrap: preForth preForth-$(PLATFORM)-backend.pre preForth.pre preForth.$(EXT)
	cat preForth-$(PLATFORM)-rts.pre preForth-rts.pre preForth-$(PLATFORM)-backend.pre preForth.pre\
           | ./preForth >preForth1.$(EXT)
	cmp preForth.$(EXT) preForth1.$(EXT)

# ------------------------------------------------------------------------
# C version
# ------------------------------------------------------------------------
# preForth connected to stdin - output to preForth.c
preForth.c: preForth.pre preForth-C-backend.pre load-C-preForth.fs
	cat preForth-C-rts.pre preForth-rts.pre preForth-C-backend.pre preForth.pre \
	   | $(HOSTFORTH) load-C-preForth.fs >preForth.c

%.c: %.pre preForth preForth-C-rts.pre preForth-rts.pre
	cat preForth-C-rts.pre preForth-rts.pre $< | ./preForth >$@

%.stdC: %.c
	gcc -O3 -Wno-implicit-function-declaration -o $@ $<

# get C version assembly listing
preForth.s: preForth.c
	gcc -S -O3 -Wno-implicit-function-declaration -o preForth.s preForth.c


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
	docker run -i -t --rm preforth	
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# simpleForth
# ------------------------------------------------------------------------
simpleForth.$(EXT): simpleForth.pre simpleForth-$(PLATFORM)-backend.pre preForth-$(PLATFORM)-rts.pre preForth-rts.pre preForth
	cat preForth-$(PLATFORM)-rts.pre preForth-rts.pre simpleForth-$(PLATFORM)-backend.pre simpleForth.pre \
            | ./preForth >simpleForth.$(EXT)

simpleForth: simpleForth.$(UNIXFLAVOUR)
	cp simpleForth.$(UNIXFLAVOUR) simpleForth

%.asm: %.simple simpleForth simpleForth-$(PLATFORM)-rts.simple simpleForth-rts.simple
	cat simpleForth-$(PLATFORM)-rts.simple simpleForth-rts.simple $< | ./simpleForth >$@

# ------------------------------------------------------------------------
# seedForth
# ------------------------------------------------------------------------
seedForth.$(EXT): seedForth-$(PLATFORM).pre preForth
	cat seedForth-$(PLATFORM).pre | ./preForth >seedForth.$(EXT)

seedForth: seedForth.$(UNIXFLAVOUR)
	cp seedForth.$(UNIXFLAVOUR) seedForth

%.seed: %.seedsource seedForth-tokenizer.fs
	gforth seedForth-tokenizer.fs $<


.PHONY=clean
clean:
	rm -f *.asm *.o *.fas *.s *.c *.Darwin *.Linux preForthdemo simpleForthDemo simpleForth preForth forth seedForth seedForthDemo.seed

CFLAGS=-g -Wall
LDFLAGS=-g

ASZ80=../asxv5pxx/asxmak/linux/exe/asz80
ASLINK=../asxv5pxx/asxmak/linux/exe/aslink

# need to install intelhex package in Python first:
#   pip3 install --user intelhex
HEX2BIN=python3 $(HOME)/.local/bin/hex2bin.py

.PHONY: all
all: emu_z80 test.bin

emu_z80: emu_z80.o z80/z80.o z80/z80.o
	$(CC) $(LDFLAGS) -o $@ $^

test.bin: test.ihx
	$(HEX2BIN) $< $@

test.ihx: test.rel
	$(ASLINK) -n -m -u -i $@ $^

test.rel: test.asm
	$(ASZ80) -l -o $<

.PHONY: clean
clean:
	rm -f *.bin *.hlr *.ihx *.lst *.map *.o *.rel *.rst emu_z80

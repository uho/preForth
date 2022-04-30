CFLAGS=-g -Wall -O3 -DVR_6502_EMU_STATIC=1
LDFLAGS=-g

AS6500=../asxv5pxx/asxmak/linux/exe/as6500
ASLINK=../asxv5pxx/asxmak/linux/exe/aslink

# need to install intelhex package in Python first:
#   pip3 install --user intelhex
HEX2BIN=python3 $(HOME)/.local/bin/hex2bin.py

LOAD_ADDR=0x300

.PHONY: all
all: emu_65c02 test.bin

emu_65c02: emu_65c02.o vrEmu6502/src/vrEmu6502.o
	$(CC) $(LDFLAGS) -o $@ $^

test.bin: test.ihx
	$(HEX2BIN) $< $@

test.ihx: test.rel
	$(ASLINK) -n -m -u -i -b text=$(LOAD_ADDR) $@ $^

test.rel: test.asm
	$(AS6500) -l -o $<

.PHONY: clean
clean:
	rm -f *.bin *.hlr *.ihx *.lst *.map *.o *.rel *.rst emu_65c02 z80/*.o

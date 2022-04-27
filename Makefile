.PHONY: all
all: asxv5pxx common emu_65c02 emu_z80 i386 z80

.PHONY: asxv5pxx
asxv5pxx:
	$(MAKE) $(MAKEFLAGS) -C asxv5pxx/asxmak/linux/build asz80 as6500 aslink

.PHONY: common
common:
	$(MAKE) $(MAKEFLAGS) -C common

.PHONY: emu_65c02
emu_65c02: asxv5pxx
	$(MAKE) $(MAKEFLAGS) -C emu_65c02

.PHONY: emu_z80
emu_z80: asxv5pxx
	$(MAKE) $(MAKEFLAGS) -C emu_z80

.PHONY: i386
i386: common
	$(MAKE) $(MAKEFLAGS) -C i386

.PHONY: z80
z80: emu_z80 common
	$(MAKE) $(MAKEFLAGS) -C z80

clean:
	$(MAKE) $(MAKEFLAGS) -C asxv5pxx/asxmak/linux/build clean
	# avoid git complaining of changes in subrepo:
	touch asxv5pxx/asxmak/linux/exe/_exe
	$(MAKE) $(MAKEFLAGS) -C common clean
	$(MAKE) $(MAKEFLAGS) -C emu_z80 clean
	$(MAKE) $(MAKEFLAGS) -C i386 clean

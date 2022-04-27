.PHONY: all
all: common i386

.PHONY: common
common:
	$(MAKE) $(MAKEFLAGS) -C common

.PHONY: i386
i386: common
	$(MAKE) $(MAKEFLAGS) -C i386

clean:
	$(MAKE) $(MAKEFLAGS) -C common clean
	$(MAKE) $(MAKEFLAGS) -C i386 clean

# Set HOSTFORTH to the Forth system that generates the initial preForth
# ------------------------------------------------------------------------
HOSTFORTH=gforth
# HOSTFORTH=sf   # SwiftForth >3.7
# ------------------------------------------------------------------------

.PHONY: all
all: crc10.forth

crc10.forth: crc10_gen.forth
	$(HOSTFORTH) $^ -e bye >$@

.PHONY: clean
clean:
	rm -f crc10.forth

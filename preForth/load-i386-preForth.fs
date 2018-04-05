\ load i386 preForth on top of a host Forth system

include load-preForth.fs
include preForth-i386-rts.pre
include preForth-rts.pre
include preForth-i386-backend.pre
include preForth.pre

cold

bye

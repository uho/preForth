\ load C preForth on top of a host Forth system

include load-preForth.fs
include preForth-C-rts.pre
include preForth-rts.pre
include preForth-C-backend.pre
include preForth.pre

cold

bye

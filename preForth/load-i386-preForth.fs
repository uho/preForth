\ load i386 preForth on top of a host Forth system
\ omit the standard words (either machine code or high level code)

include preForth-bootstrap.fs
\ include preForth-i386-rts.pre
include preForth-rts-nonstandard.pre
\ include preForth-rts.pre
include preForth-i386-backend.pre
include preForth.pre

cold
bye

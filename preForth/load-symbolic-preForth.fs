\ load symbolic preForth on top of a host Forth system

include load-preForth.fs
include preForth-symbolic-rts.pre
include preForth-rts.pre
include preForth-symbolic-backend.pre
include preForth.pre

cold

bye

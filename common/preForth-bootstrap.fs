\ preForth runtime system - compatibility package for bootstrap

\ since preForth is mostly a subset of standard Forth, preForth programs can
\ basically run under gForth or similar for bootstrapping, after including:
\   preForth-bootstrap.fs: adjust environment (not needed under pure preForth)
\   preForth-rts-nonstandard.fs: useful words (needed under/part of preForth)

\ This : allows for recursion by using a word's name.
defined -smudge [IF] \ SwiftForth
: : : -smudge ;
[THEN]

defined reveal [IF] \ gforth
: : : reveal ;
[THEN]

\ ignore tail recursion optimization
\ the host system is assumed to have a large enough stack to handle the
\ steady growth of the stack as the compiler loops through the input file
: tail ( -- ) ;

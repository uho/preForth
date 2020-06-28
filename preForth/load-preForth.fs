\ Load preForth on GForth or SwiftForth connected to stdin and stdout.

\ : [DEFINED] BL WORD FIND NIP 0<> ; IMMEDIATE

[DEFINED] warnings [IF] \ e.g. gforth
 warnings off
[THEN]

[DEFINED] warning [IF] \ e.g. SwiftForth
 warning off
[THEN]

Variable ch

\ key reads from stdin so it can be used with pipes and input redirection.
: key ( -- c )
    ch 1 stdin read-file throw
    1 < IF  4 ( eof )  ELSE ch c@ THEN
    ; \ dup emit ;

\ This : allows for recursion by using a word's name.
[defined] -smudge [IF] \ SwiftForth
: : : -smudge ;
[THEN]

[defined] reveal [IF] \ gforth
: : : reveal ;
[THEN]


\ Define pre and code so they skip their body

: pre ( -- )
   BEGIN refill WHILE
     source  s" ;" compare 0= IF POSTPONE \ EXIT THEN
   REPEAT ;

: prefix pre ;
: prelude pre ;
: preamble pre ;
: code pre ;

: tail ;

include borrow.fs

wordlist Constant preForth

preForth set-current

: borrow borrow ;
: primitive borrow ;
: tail tail ;

preForth 1 set-order

borrow include
borrow :
borrow ;
borrow \
borrow ( 
borrow .s

borrow pre
borrow prefix
borrow prelude
borrow preamble
borrow code

borrow later
later ?dup
later 0=
later negate 
later +
later 1+
later 1-
later =
later <
later >
later case?

later over
later rot
later nip
later 2drop
later pick
later roll

later bl
later space
later tab
later cr
later u.
later .

later show
later _dup
later _drop
later _swap

primitive emit
primitive key
primitive dup
primitive swap
primitive 0<
primitive ?exit
primitive drop
primitive recurse
primitive >r
primitive r>
primitive -
\ nest
\ unnest
\ lit

borrow bye 

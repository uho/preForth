# Bootstrapping Forth

## preForth

preForth is a minimal non-interactive Forth kernel that can bootstrap itself and can be used as an easy-to-port basis for a full Forth implementation.

preForth feels like Forth - it's mainly a sublanguage of ANS-Forth - but is significantly reduced in its capabilities.

### Features: minimal control structures, no immediate words, strings on stack, few primitives

just

- Stack
- Returnstack
- Only ?exit and recursion as control structures
- :-definitions
- optional tail call optimization
- IO via KEY/EMIT
- signed single cell decimal numbers (0-9)+
- character constants via 'c'-notation
- output single cell decimal numbers

and

- no immediate words, i.e.
- no control structures IF ELSE THEN BEGIN WHILE REPEAT UNTIL
- no defining words
- no DOES>
- no memory @ ! CMOVE ALLOT ,
- no pictured numeric output
- no input stream
- no state
- no base
- no dictionary, no EXECUTE, not EVALUATE
- no CATCH and THROW
- no error handling

### Prerequisites:

  Just 13 primitives: emit key dup swap drop 0< ?exit >r r> - nest unnest lit

## simpleForth

simpleForth is an extension to preForth built using preForth. It is still non-interactive but adds 

- control structures IF ELSE THEN BEGIN WHILE REPEAT UNTIL
- definitions with and without headers in generated code
- memory: @ ! c@ c! allot c, , 
- variable, constants
- ['] execute
- immediate definitions

## Forth

Forth is a simple interactive Forth system built using simpleForth.
Forth is open ended and has a yet incomplete set of features. Work in progress.


# How to use:

An i386-Backend (32Bit) indirect threaded code implementation based on [FASM](https://flatassembler.net/) is pre-configured.
PreForth initially bootstraps on with [gforth](https://www.gnu.org/software/gforth/) or [swiftForth](https://www.forth.com/swiftforth/). 
You'll need one of these for the first bootstrap.

    cd preForth
    make

This will successively compile preForth, simpleForth, then Forth.

If successful issue

    $ ./Forth

    Forth 1.2.0

    last * warm cold empty patch minor major banner quit restart REPEAT WHILE AGAIN 
    UNTIL BEGIN THEN ELSE IF ; : constant variable header cmove compile, , allot here 
    dp +! clearstack interpret parse-name \ .( ( parse (interpreters ?word (compilers ,word 
    immediate !flags @flags or and #immediate ] [ interpreters compilers handlers ,'x' ?'x' 
    ,# ?# scan skip source /string >in query #tib tib accept min words .name l>interp l>name 
    l>flags type count cell+ cells find-name .s prefix? compare 2dup 2drop rot off on ?dup + 
    space bl cr . u. negate > 1- nip = 0= pick 1+ < over depth execute c! ! c@ @ ?branch 
    branch lit exit unnest - r> >r ?exit 0< drop swap dup key emit bye 

Inspect sources and generated files.

*Have fun. May the Forth be with you.*


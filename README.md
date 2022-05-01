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

## seedForth

seedForth is an extended system with random access memory, control structures, compiling and defining words. Its kernel the so called *seedForth bed* is built using preForth. Seed Forth bed has about 30 primitives as well as a compiler and interpreter. 

seedForth accepts source code in tokenized source code form (.seed files). the seedForth tokenizer reads text source code (.seedsource files) and converts them to token form. 

You can create applications by writing the appropriate seedForth text source code, tokenize it using the tokenizer (`make xxx.seed`) and then feed the token source code into the seedForth bet (`cat xxx.seed | ./seedForth`) in order to let the application grow.

On of these applications is seedForth/interactive.

## seedForth/interactive

seedForth/interactive extends seedForth to become a full featured interactive Forth system. For this it defines a searchable dictionary of (text based) headers and a text based interpreter and compiler.
On startup seedForth/interactive accepts Forth source code in text form.

# How to use:

An i386-Backend (32Bit) indirect threaded code implementation based on [FASM](https://flatassembler.net/) is pre-configured.
PreForth initially bootstraps on with [gforth](https://www.gnu.org/software/gforth/) or [swiftForth](https://www.forth.com/swiftforth/). 
You'll need one of these for the first bootstrap.

    cd preForth
    make

This will successively compile preForth, seedForth then seedForth/interactive.

If successful issue

    $ ./seed
    
and seedForth/interactive will start up showing
    
    ..................................................
    .
    seedForth/interactive 2.2.0
    ---------------------------
    380005 bytes free
 
and more test output. Tweek `hi.forth` as desired.

Inspect sources and generated files.

*Have fun. May the Forth be with you.*

# Copyright and license

Please see the files `COPYRIGHT` and `LICENSE` in the root of this repository.

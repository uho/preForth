# Things to do

+ wordlists, search order, vocabulary   split into traditional (Vocabulary only/also) and modern word set (get-order)

+ number output with given base <# # #s hold #> u. u.r . .r    um/mod /mod / mod   currently using globals BASE, HLD and PAD

- Assembler (proof of concept) i386 stm8 ...

+ | based on allocated headers

+ packages à la swiftForth (currently based on headerless definitions) with vocabularies

- dual xt headers

+ high level multi tasker definitions

   : (activate) ( xt -- )
        pause execute stop ; \ is pause neccesary?  use catch?  store error in status variable


- more Standard words (at least CORE words w/ exceptions such as BASE STATE)

- Divison SM/REM FM/MOD ...

- umbilical Block-Interface

- file interface open-file read read-line write close-file, mmap

- file i/o and include, savesystem

- interleaved tokenizer and token-interpreting seedForth (another flavor of interactivity)

+ extension tokens 01 - 0F,  currently using only 01 and 02 for a total of 768-2 tokens.

- relative branches

- DO LOOP 

+ FOR NEXT

- more arithmetic  log2 ...

- experiments with redefinition and recursion 
       
       | name found during definition | redefinition replaces definition with same name   | comment
       |------------------------------|---------------------------------------------------|--------------
       |     yes                      |     yes                                           | no defer, lisp style
       |     no                       |     yes                                           | 
       |     yes                      |     no                                            | natural recursion
       |     no                       ]     no                                            | classic

- interpretive conditionals

+ conditional compilation ( how would this work with tokens? )

- source code library WANT REQUIRE (maybe also on tokens)

- intermediate definitions (forgettable)

- dictionary experiments
   hash names to cell  look for value  find xt in other table (à la colorForth)

- OOF

- Standard non-extensible modern text interpreter/compiler (w/ or without STATE)

- FIG Forth style non-exensible text interpreter/compiler

- Recognizers based text interpreter/compiler (based on new terms)

- marker, save, empty

+ Prefix Bit and Prefix Dictionary Search  -> needs to adjust intput stream... not just find prefix words in dict...

+ Unicode names, emojies

+ ANSI color

+ ANS error codes

- and XCHARS wordset and support for unicode characters such as '∆' -> U+0394

+ interactive decompiler

+ dump

* State machine / decision table using graphics characters

+ Cursor positioning AT-XY

+ Status line

- AT?

- String Stack

- locals

- generalize linked list handling (map/filter)

+ key? and raw-terminal

- Mark Humphrey style interpreter

- can Macro be use in seedForth/Interactive

- native code (with peephole optimization) compile,

- implementing the tokenizer in seedForth

+ remove CATCH and THROW and fp from simplify seedForth kernel

+ simplify seedForth kernel: interpret loop does not special case 0 but executes EXIT to break the loop

- move seedForth to HolonTalk

- random number generator

+ hash function

- interned strings

- screens - disk i/o and block interface or in memory à la colorForth, mmap

+ Manfred Mahlow's VOCS with ITEM and STICKY
   - non-nested Vocs could be implemented by parsing the input-stream for the next token and EVALUATEing it. 
     Nesting however would require that the rest of the input-stream would be available so that a Voc following a voc can still parse...
   - ITEM and STICKY requires changes in the outer interpreter and the header structure.
     ITEM words set the context to the voc that was active when ITEM was executed.
     STICKY words extend the context one more parsed word.

+ Charles Moore's screen setup (uhdForth) with command line at the top and screens below.

+ ColorForth/uhdForth like experiments

- two thread structure: like colorForth, would let the user type (steer) while processing in the background (chipchuck)

- Structure preForth, seedForth and seedForth/interactive in Emacs pages.

- output on standard error

- input/output on predefined files


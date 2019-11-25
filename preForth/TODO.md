# Things to do

- wordlists, search order

- number output with given base <# # #s hold #> u. u.r . .r    um/mod /mod / mod

- Assembler (proof of concept) i386 stm8 ...

+ | based on allocated headers

+ packages à la swiftForth (currently based on headerless definitions)

- dual xt headers

- high level multi tasker definitions

- more Standard words (at least CORE words w/ exceptions such as BASE STATE)

- umbilical Block-Interface

- file interface open-file read read-line write close-file

- interleaved tokenizer and token-interpreting seedForth (another flavor of interactivity)

- extension tokens 01 - 0F

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

- intermediate definitions (forgettable)

- dictionary experiments
   hash names to cell  look for value  find xt in other table (à la colorForth)

- OOF

- Standard non-extensible modern text interpreter/compiler (w/ or without STATE)

- FIG Forth style non-exensible text interpreter/compiler

- Recognizers based text interpreter/compiler (based on new terms)

- marker

+ Prefix Bit and Prefix Dictionary Search  -> needs to adjust intput stream... not just find prefix words in dict...

+ Unicode names, emojies

+ ANSI color

+ ANS error codes

- and XCHARS wordset

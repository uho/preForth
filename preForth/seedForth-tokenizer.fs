\ seedForth tokenizer (byte-tokenized source code)

\ load on on top of gforth   uho  2018-04-13

\ -----------------------------

WARNINGS OFF

VARIABLE OUT

: PROGRAM ( <name> -- )
   BL WORD COUNT R/W CREATE-FILE THROW OUT ! ;

: SUBMIT ( c -- )
   PAD C!  PAD 1 OUT @ WRITE-FILE THROW ;

: END ( -- )
   .S CR 0 SUBMIT OUT @ CLOSE-FILE THROW BYE ;


Variable #FUNS  0 #FUNS !
: FUN: ( <name> -- )
    CREATE #FUNS @ ,  1 #FUNS +!
  DOES> @ SUBMIT ;

FUN: bye       FUN: emit        FUN: key       FUN: dup			\ 00 01 02 03
FUN: swap      FUN: drop        FUN: 0<        FUN: ?exit		\ 04 05 06 07
FUN: >r        FUN: r>          FUN: -         FUN: unnest		\ 08 09 0A 0B
FUN: lit       FUN: @           FUN: c@        FUN: !			\ 0C 0D 0E 0F
FUN: c!        FUN: execute     FUN: branch    FUN: ?branch		\ 10 11 12 13
FUN: negate    FUN: +           FUN: 0=        FUN: ?dup		\ 14 15 16 17
FUN: cells     FUN: +!          FUN: h@        FUN: h,			\ 18 19 1A 1B
FUN: here      FUN: allot       FUN: ,         FUN: c,			\ 1C 1D 1E 1F
FUN: fun       FUN: interpreter FUN: compiler  FUN: create		\ 20 21 22 23
FUN: does>     FUN: cold        FUN: depth     FUN: compile,	\ 24 25 26 27
FUN: new       FUN: and         FUN: or

: [ ( -- )  0 SUBMIT ;
: ] ( -- )  compiler ;

: ': ( <name> -- ) FUN: fun ;
: ;' ( -- ) unnest [ ;

: # ( x -- )  key  SUBMIT ;    \ x is placed in the token file as a single byte, as defined by key/SUBMIT
: #, ( x -- ) lit [ # , ] ;    \ x is placed in memory as a cell-sized quantity (32/64 bit), as defined by comma

\ Control structure macros

: AHEAD ( -- addr ) branch [ here  0 # , ] ;
: IF ( -- addr )   ?branch [ here  0 # , ] ;
: THEN ( addr -- ) [ here swap ! ] ;
: ELSE ( addr1 -- addr2 )  branch  [ here 0 # ,  swap ] THEN ;

: BEGIN ( -- addr )  [ here ] ;
: AGAIN ( addr -- )   branch [ , ] ;
: UNTIL ( addr -- )  ?branch [ , ] ;
: WHILE ( addr1 -- addr2 addr1 )  IF [ swap ] ;
: REPEAT ( addr -- ) AGAIN THEN ;

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

Variable #FUNS

: FUNCTIONS ( u -- )   #FUNS ! ;


: #FUN: ( <name> n -- )
    CREATE dup , 1+ FUNCTIONS DOES> @ SUBMIT ;
    
: FUN: ( <name> -- )
    #FUNS @ #FUN: ;

$02 #FUN: key
$0A #FUN: -
$29 #FUN: couple

: byte# ( c -- )
    ( seedForth ) key    
    SUBMIT ;

: # ( x -- )      \ x is placed in the token file. Handle also negative and large numbers
     dup 0<    IF  0 byte#   negate recurse  ( seedForth ) -  EXIT THEN
     dup $FF > IF  dup 8 rshift  recurse  $FF and  byte#  ( seedForth ) couple EXIT THEN
     byte# ;    

$22 #FUN: compiler

: [ ( -- )  0 SUBMIT ;
: ] ( -- )  compiler ;

\ Literal numbers

$0C #FUN: lit
$1E #FUN: ,
$1F #FUN: c,

: #, ( x -- ) lit [ # , ] ;    \ x is placed in memory as a cell-sized quantity (32/64 bit), as defined by comma


\ locals support 

: mark ( -- )
    s" marker ***forget-locals***" evaluate ;

: release ( -- ) 
    s" ***forget-locals***" evaluate ;

$33 #FUN: lallot    
$34 #FUN: >l            
$35 #FUN: +l

$0D #FUN: preforth-@
$0F #FUN: preforth-!

variable 'to
: to? ( -- f ) 'to @  'to off ;
: to ( -- ) 'to on ;

: local: ( n <local-name> -- ) 
    \ cr dup . ." local: " >in @ bl word count type >in !
    Create here  1 cells allot  ! ( can't use , ) 
    Does> @ 
    ( preforth ) #, +l 
    to? IF preforth-! ELSE preforth-@ THEN ;

0 [if]
: create-locals ( n1 <ln-1> ... <l0> -- )  
    >in @ ( n1 >in )
    bl word count
    2dup s" :}" compare 0= IF 2drop ( n1 >in ) drop EXIT THEN
         s" --" compare 0= IF '}' parse 2drop  ( n1 >in ) drop EXIT THEN
    ( n1 >in )
    over 1+ recurse ( n1 >in n2 )
    >r  >in @ >r  >in !  local: ( n1 )  r> >in ! 
    >l  ( generate code to push value on locals stack ) 
    r> ( n2 )
;
[then]

: create-locals ( <local-name0> <local-name1> ... -- nlocals )
    0 ( nlocals )  false >r
    BEGIN ( nlocals )
      BEGIN
          >in @  bl word count   ( nlocals >in c-addr u )
          2dup s" :}" compare 0= IF r> drop 2drop ( nlocals >in ) drop EXIT THEN
          2dup s" --" compare 0= IF r> drop 2drop '}' parse 2drop  ( nlocals >in ) drop  EXIT THEN
               s" |"  compare 0= 
      WHILE ( nlocals >in ) drop 
          r> drop  true >r
      REPEAT
      ( nlocals >in )
      >in !  dup local: 
      r@ IF  0 #, THEN ( addtional 0s for initialized locals )
      1+
    AGAIN ;

Variable nlocals
: {:  ( -- )  create-locals
  dup nlocals ! 0 ?DO >l LOOP ; ( generate code to push value on locals stack )

$18 #FUN: cells

: ?free-locals ( -- )
   nlocals @ IF  nlocals @ negate  #, ( seedforth ) cells lallot THEN ;


\ Strings

$32 #FUN: $lit

: ", ( c-addr u -- )
    dup # ( seedForth) c,  BEGIN dup WHILE >r dup char+ swap c@ # ( seedForth) c,  r> 1- REPEAT 2drop ;

: ," ( ccc" -- )   [char] " parse ", ;

: $, ( c-addr u -- )  $lit [ ", ] ;

: s" ( ccc" -- )   [char] " parse $, ;  \ only in compile mode

: ,name ( ccc<space> -- ) parse-name ", ;


  $00 #FUN: bye       $01 #FUN: emit        ( $02 #FUN: key )      $03 #FUN: dup
  $04 #FUN: swap      $05 #FUN: drop          $06 #FUN: 0<         $07 #FUN: ?exit
  $08 #FUN: >r        $09 #FUN: r>         (  $0A #FUN: - )        $0B #FUN: unnest
( $0C #FUN: lit )     $0D #FUN: @            $0E #FUN: c@          $0F #FUN: !
  $10 #FUN: c!        $11 #FUN: execute       $12 #FUN: branch     $13 #FUN: ?branch
  $14 #FUN: negate    $15 #FUN: +             $16 #FUN: 0=         $17 #FUN: ?dup
( $18 #FUN: cells )   $19 #FUN: +!            $1A #FUN: h@         $1B #FUN: h,
  $1C #FUN: here      $1D #FUN: allot       ( $1E #FUN: , )      ( $1F #FUN: c, )
  $20 #FUN: fun       $21 #FUN: interpreter ( $22 #FUN: compiler ) $23 #FUN: create
  $24 #FUN: does>     $25 #FUN: cold          $26 #FUN: depth      $27 #FUN: compile,
  $28 #FUN: new     ( $29 #FUN: couple  )     $2A #FUN: and        $2B #FUN: or
  $2C #FUN: catch     $2D #FUN: throw         $2E #FUN: sp@        $2F #FUN: sp!
  $30 #FUN: rp@       $31 #FUN: rp!         ( $32 #FUN: $lit )
( $33 #FUN: lallot    $34 #FUN: >l            $35 #FUN: +l )

$36 FUNCTIONS

\ Definitions

: EXIT ( -- )  ?free-locals unnest ;
: ': ( <name> -- ) FUN: fun  mark ;
: ;' ( -- ) ( seedforth ) EXIT [    release  ;

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


\ Another seedForth tokenizer    2019-10-18

\ seedForth does not support hex so put some useful constants in decimal
255 Constant xFF
1023 Constant x3FF
1024 Constant x400
65261 Constant xFEED

\ exceptions
100 Constant except_hash_table_full

\ hash table entry structure
0 Constant _hash_table_xt
1 cells Constant _hash_table_name_addr
2 cells Constant _hash_table_name_len
3 cells Constant #hash_table

\ the sizing below accommodates up to 1K word definitions
\ (the same as the number of tokens available to seedForth)
x3FF Constant hash_table_mask
x400 Constant hash_table_size
Create hash_table
hash_table_size #hash_table * dup allot hash_table swap 0 fill

: hash_table_index ( entry -- addr )
    #hash_table * hash_table + ;

: hash_table_find ( name_addr name_len -- entry_addr found )
    \ calculate CRC10 of the symbol name
    \ initial value is same as hash table mask (all 1s)
    2dup hash_table_mask crc10
    \ hash_table_mask and

    \ using the CRC10 as the starting entry, look circularly
    \ for either a null entry (not found) or a matching entry
    hash_table_size 0 ?DO ( name_addr name_len entry )
        dup >r hash_table_index >r ( name_addr name_len R: entry entry_addr )

        \ check for null entry
        r@ _hash_table_xt + @ 0= IF
            2drop r> r> drop false UNLOOP exit
        THEN

        \ check for matching entry
        2dup
        r@ _hash_table_name_addr + @
        r@ _hash_table_name_len + @
        compare 0= IF
            2drop r> r> drop true UNLOOP exit
        THEN

        \ go to next entry, circularly
        r> drop
        r> 1+ hash_table_mask and
    LOOP

    \ not found, and no room for new entry
    except_hash_table_full throw
;

: token@ ( c-addr u -- x )
    \ get entry address and flag for found/empty
    hash_table_find

    \ if found, return value of _xt, otherwise 0
    IF _hash_table_xt + @ ELSE drop 0 THEN
;

: ?token ( c-addr u -- x )
    \ get entry address and flag for found/empty
    2dup hash_table_find

    \ if empty, copy symbol name and fill in entry
    0= IF
        >r
        here r@  _hash_table_name_addr + !
        dup r@ _hash_table_name_len + !
        here swap dup allot cmove
        r>
    ELSE
        nip nip
    THEN

    \ return address of _xt for caller to fill in
    _hash_table_xt +
;

\ VARIABLE OUTFILE

\ : submit ( c -- )
\     PAD C!  PAD 1 OUTFILE @ WRITE-FILE THROW ;
\
\ : submit-token ( x -- )
\     dup 255 > IF  dup 8 rshift  SUBMIT  THEN  SUBMIT ;
: emit-token ( x -- )
    dup xFF > IF  dup 8 rshift  emit  THEN  emit ;

Variable #tokens  0 #tokens !
: Token ( <name> -- )
   :noname  
   #tokens @  postpone Literal  postpone emit-token  postpone ; \ SUBMIT-TOKEN  postpone ;  
   parse-name 
   \ cr  #tokens @ 3 .r space 2dup type \ tell user about used tokens
   ?token ! 1 #tokens +! ;

: Macro ( <name> -- )
   parse-name ?token :noname xFEED ;

: end-macro ( 'hash colon-sys -- )
   xFEED - abort" end-macro without corresponding Macro"
   postpone ;  ( 'hash xt )  swap ! ; immediate

: seed ( i*x <name> -- j*x ) 
    parse-name token@ dup 0= abort" is undefined"    postpone Literal   postpone execute ; immediate


(  0 $00 ) Token bye
4 #tokens !
(  4 $04 ) Token eot       Token dup           Token swap       Token drop          
(  8 $08 ) Token 0<        Token ?exit         Token >r         Token r>
( 12 $0C ) Token -         Token exit          Token lit        Token @             
( 16 $10 ) Token c@        Token !             Token c!         Token execute       
( 20 $14 ) Token branch    Token ?branch       Token negate     Token +             
( 24 $18 ) Token 0=        Token ?dup          Token cells      Token +!            
( 28 $1C ) Token h@        Token h,            Token here       Token allot         
( 32 $20 ) Token ,         Token c,            Token fun        Token interpreter   
( 36 $24 ) Token compiler  Token create        Token does>      Token cold          
( 40 $28 ) Token depth     Token dodoes        Token new        Token couple        
( 44 $2C ) Token and       Token or            Token sp@        Token sp!           
( 48 $30 ) Token rp@       Token rp!           Token $lit       Token num
( 52 $34 ) Token um*       Token um/mod        Token unused     Token key?          
( 56 $38 ) Token token     Token usleep        Token hp         Token key
( 60 $3C ) Token emit      Token eemit

\ generate token sequences for numbers

: seed-byte ( c -- )
   seed key   emit ; \ SUBMIT ;

: seed-number ( x -- )      \ x is placed in the token file. Handle also negative and large numbers
   dup 0<    IF  0 seed-byte   negate recurse  seed -   EXIT THEN
   dup xFF > IF  dup 8 rshift  recurse  xFF and  seed-byte  seed couple EXIT THEN
   seed-byte ;   

: char-lit? ( c-addr u -- x flag )
   3 - IF drop 0 false EXIT THEN
   dup c@ ''' -  IF drop 0 false EXIT THEN
   dup 2 chars + c@ ''' -  IF  drop 0 false EXIT THEN
   char+ c@ true ;

: process-digit? ( x c -- x' flag )
   '0' - dup 10 u< IF  swap 10 * + true EXIT THEN  drop false ;

: process-number? ( c-addr u -- x flag )
         dup 0= IF 2drop 0 false EXIT THEN
         over c@ '-' = dup >r IF 1 /string THEN
     >r >r 0 r> r> bounds 
     ?DO ( x )  
        I c@ process-digit? 0= IF UNLOOP r> drop false EXIT THEN ( x d )
     LOOP 
     r> IF negate THEN true ;

: seed-name ( c-addr u -- )
         2dup  token@ dup IF nip nip execute EXIT THEN drop
         2dup  char-lit? IF nip nip seed num  seed-number seed exit  EXIT THEN drop
         2dup  process-number? IF nip nip seed num  seed-number seed exit  EXIT THEN drop
         cr type ."  not found" abort ;

: seed-line ( -- )
   BEGIN parse-name dup WHILE  seed-name  REPEAT 2drop ; 

: seed-file ( -- )
   BEGIN refill WHILE  seed-line REPEAT ;

\ : PROGRAM ( <name> -- )
\    parse-name R/W CREATE-FILE THROW OUTFILE !
\    seed-file ;

\ Macro END ( -- )
\    .S CR  0 SUBMIT  OUTFILE @ CLOSE-FILE THROW BYE end-macro

\ eot is overloaded to either:
\ - return from compilation state to interpretive state
\   (used for compiling ; and various control flow constructs)
\ - quit the interpreter if invoked in interpretive state
\   (can overloading because control flow is not allowed here)
\ this means that if the token stream runs out and starts to return
\ EOT characters, we will first terminate any word definition that
\ was in progress, then we'll do an automatic bye (in the old way,
\ there was an automatic bye token appended to seed file, but this
\ was annoying because seedForthInteractive had to read and drop it)
Macro [ ( -- )  seed eot      end-macro  \ eot
Macro ] ( -- )  seed compiler end-macro  \ compiler

Macro : ( <name> -- )  seed fun  Token  end-macro
Macro ; ( -- )         seed exit   seed [ end-macro

\ generate token sequences for strings

: seed-stack-string ( c-addr u -- )
   dup >r
   BEGIN dup WHILE ( c-addr u )
      over c@ seed-number 1 /string      
   REPEAT ( c-addr u ) 
   2drop 
   r> seed-number
;

: seed-string ( c-addr u -- )
   dup seed-number  seed c,  
   BEGIN dup WHILE 
      >r dup char+ swap c@   seed-number seed c,   
      r> 1- 
   REPEAT 2drop 
;

Macro ," ( ccc" -- )   '"' parse seed-string end-macro

: $, ( c-addr u -- )  
   seed $lit 
   seed [ 
   seed-string
   seed ] 
;

Macro $name ( <name> -- )
   parse-name seed-stack-string
end-macro

Macro $( \ ( ccc) -- )
  ')' parse seed-stack-string
end-macro

Macro s" ( ccc" -- )  \ only in compile mode
  '"' parse $, 
end-macro 


\ Control structure macros
: forward ( -- )
   seed [   
   seed here   
         0 seed-number  seed , 
         seed ] 
;

: back ( -- )
   seed [ 
   seed , 
   seed ]
;


Macro AHEAD ( -- addr ) 
        seed branch  forward
end-macro

Macro IF ( -- addr )   
        seed ?branch forward
end-macro


Macro THEN ( addr -- ) 
  seed [ 
  seed here 
  seed swap 
  seed ! 
  seed ] 
end-macro

Macro ELSE ( addr1 -- addr2 )  
  seed branch forward 
  seed [ 
  seed swap 
  seed ] 
  seed THEN 
end-macro

Macro BEGIN ( -- addr )  
  seed [ 
  seed here 
  seed ] 
end-macro

Macro AGAIN ( addr -- )  
  seed branch  back 
end-macro

Macro UNTIL ( addr -- )  
  seed ?branch back 
end-macro

Macro WHILE ( addr1 -- addr2 addr1 )  
  seed IF 
  seed [ 
  seed swap 
  seed ] 
end-macro

Macro REPEAT ( addr -- ) 
  seed AGAIN 
  seed THEN 
end-macro

Macro ( ( -- )
  postpone (
end-macro

Macro \ ( -- )
  postpone \
end-macro

Macro Definer ( <name> -- )
   Macro
      postpone Token
      #tokens @  1 #tokens +! 
      postpone Literal
      postpone emit-token \ SUBMIT-TOKEN
      seed fun
   postpone end-macro
end-macro

\ for defining Macros later in seedForth
Macro Macro ( <name> -- )
   Macro
end-macro

Macro end-macro
   postpone end-macro
end-macro

Macro seed ( <name> -- )
   postpone seed
end-macro

Macro save-#tokens
   postpone #tokens
   postpone @
end-macro

Macro restore-#tokens
   postpone #tokens
   postpone !
end-macro

seed-file
\ user code has to be concatenated here
\ it cannot be in a separate file when running via gforth
\ it cannot have a partial last line when running via seedForth

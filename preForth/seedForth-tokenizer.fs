\ Another seedForth tokenizer    2019-10-18

: fnv1a ( c-addr u -- x )
    2166136261 >r
    BEGIN dup WHILE  over c@ r> xor 16777619 um* drop    $FFFFFFFF and >r 1 /string REPEAT 2drop r> ;

15 Constant #hashbits
1 #hashbits lshift Constant #hashsize

#hashbits 16 < [IF]

  #hashsize 1 - Constant tinymask
  : fold ( x1 -- x2 )  dup   #hashbits rshift  xor  tinymask and ;

[ELSE] \ #hashbits has 16 bits or more

  #hashsize 1 - Constant mask 
  : fold ( x1 -- x2 )  dup   #hashbits rshift  swap mask and  xor ;

[THEN]

Create tokens  #hashsize cells allot  tokens #hashsize cells 0 fill

: 'token ( c-addr u -- addr )
    fnv1a fold  cells tokens + ;

: token@ ( c-addr u -- x )  'token @ ;

: ?token ( c-addr u -- x )  
    2dup 'token dup @ 
    IF  
       >r cr type ."  collides with another token " 
       cr source type cr r> @ name-see abort 
    THEN nip nip ;

VARIABLE OUTFILE

: submit ( c -- )
    PAD C!  PAD 1 OUTFILE @ WRITE-FILE THROW ;

: submit-token ( x -- )
    dup 255 > IF  dup 8 rshift  SUBMIT  THEN  SUBMIT ;

: <name> ( -- c-addr u )  bl word count ;

Variable #tokens  0 #tokens !
: Token ( <name> -- )
   :noname  
   #tokens @  postpone LITERAL  postpone SUBMIT-TOKEN  postpone ;  
   <name> 
   cr  #tokens @ 3 .r space 2dup type \ tell user about used tokens
   ?token ! 1 #tokens +! ;

: Macro ( <name> -- )
   <name> ?token :noname $FEED ;

: end-macro ( 'hash colon-sys -- )
   $FEED - Abort" end-macro without corresponding Macro"
   postpone ;  ( 'hash xt )  swap ! ; immediate

: seed ( i*x <name> -- j*x ) 
    <name> token@ dup 0= Abort" is undefined"    postpone LITERAL   postpone EXECUTE ; immediate


(  0 $00 ) Token bye       Token prefix1       Token prefix2    Token emit          
(  4 $04 ) Token key       Token dup           Token swap       Token drop          
(  8 $08 ) Token 0<        Token ?exit         Token >r         Token r>
( 12 $0C ) Token -         Token exit          Token lit        Token @             
( 16 $10 ) Token c@        Token !             Token c!         Token execute       
( 20 $14 ) Token branch    Token ?branch       Token negate     Token +             
( 24 $18 ) Token 0=        Token ?dup          Token cells      Token +!            
( 28 $1C ) Token h@        Token h,            Token here       Token allot         
( 32 $20 ) Token ,         Token c,            Token fun        Token interpreter   
( 36 $24 ) Token compiler  Token create        Token does>      Token cold          
( 40 $28 ) Token depth     Token compile,      Token new        Token couple        
( 44 $2C ) Token and       Token or            Token sp@        Token sp!           
( 48 $30 ) Token rp@       Token rp!           Token $lit       Token num
( 52 $34 ) Token um*       Token um/mod        Token unused     Token key?          
( 56 $38 ) Token token

\ generate token sequences for numbers

: seed-byte ( c -- )
   seed key   SUBMIT ;

: seed-number ( x -- )      \ x is placed in the token file. Handle also negative and large numbers
   dup 0<    IF  0 seed-byte   negate recurse  seed -   EXIT THEN
   dup $FF > IF  dup 8 rshift  recurse  $FF and  seed-byte  seed couple EXIT THEN
   seed-byte ;   

: char-lit? ( c-addr u -- x flag )
   3 - IF drop 0 false EXIT THEN
   dup c@ [char] ' -  IF drop 0 false EXIT THEN
   dup 2 chars + c@ [char] ' -  IF  drop 0 false EXIT THEN
   char+ c@ true ;

: process-digit? ( x c -- x' flag )
   '0' - dup 10 u< IF  swap 10 * + true EXIT THEN  drop false ;

: number? ( c-addr u -- x flag )
	 dup 0= IF 2drop 0 false EXIT THEN
 	 over c@ '-' = dup >r IF 1 /string THEN
     >r >r 0 r> r> bounds 
     ?DO ( x )  
      	I c@ process-digit? 0= IF unloop r> drop false EXIT THEN ( x d )
     LOOP 
     r> IF negate THEN true ;

: seed-name ( c-addr u -- )
	 2dup  token@ dup IF nip nip execute EXIT THEN drop
	 2dup  char-lit? IF nip nip seed num  seed-number seed exit  EXIT THEN drop
	 2dup  number? IF nip nip seed num  seed-number seed exit  EXIT THEN drop
	 cr type ."  not found" abort ;

: seed-line ( -- )
   BEGIN <name> dup WHILE  seed-name  REPEAT 2drop ; 

: seed-file ( -- )
   BEGIN refill WHILE  seed-line REPEAT ;

: PROGRAM ( <name> -- )
   <name> R/W CREATE-FILE THROW OUTFILE !
   seed-file ;

Macro END ( -- )
   .S CR  0 SUBMIT  OUTFILE @ CLOSE-FILE THROW BYE end-macro

Macro [ ( -- )  seed bye      end-macro  \ bye
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

Macro ," ( ccc" -- )   [char] " parse seed-string end-macro

: $, ( c-addr u -- )  
   seed $lit 
   seed [ 
   seed-string
   seed ] 
;

Macro $name ( <name> -- )
   <name> seed-stack-string
end-macro

Macro $( \ ( ccc) -- )
  [char] ) parse seed-stack-string
end-macro

Macro s" ( ccc" -- )  \ only in compile mode
  [char] " parse $, 
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
      postpone SUBMIT-TOKEN
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

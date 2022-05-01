\ Another seedForth tokenizer    2019-10-18

\ seedForth does not support hex so put some useful constants in decimal
255 Constant xFF
1023 Constant x3FF
1024 Constant x400
65261 Constant xFEED

\ exceptions
100 Constant except-hash-table-full

\ hash table entry structure
0 Constant _hash-table-xt
1 cells Constant _hash-table-name-addr
2 cells Constant _hash-table-name-len
3 cells Constant #hash-table

\ the sizing below accommodates up to 1K word definitions
\ (the same as the number of tokens available to seedForth)
x3FF Constant hash-table-mask
x400 Constant hash-table-size
Create hash-table
hash-table-size #hash-table * dup allot hash-table swap 0 fill

: hash-table-index ( entry -- addr )
    #hash-table * hash-table + ;

: hash-table-find ( name-addr name-len -- entry-addr found )
    \ calculate CRC10 of the symbol name
    \ initial value is same as hash table mask (all 1s)
    2dup hash-table-mask crc10
    \ hash-table-mask and

    \ using the CRC10 as the starting entry, look circularly
    \ for either a null entry (not found) or a matching entry
    hash-table-size 0 ?DO ( name-addr name-len entry )
        dup >r hash-table-index >r ( name-addr name-len R: entry entry-addr )

        \ check for null entry
        r@ _hash-table-name-len + @ 0= IF
            2drop r> r> drop false UNLOOP exit
        THEN

        \ check for matching entry
        2dup
        r@ _hash-table-name-addr + @
        r@ _hash-table-name-len + @
        compare 0= IF
            2drop r> r> drop true UNLOOP exit
        THEN

        \ go to next entry, circularly
        r> drop
        r> 1+ hash-table-mask and
    LOOP

    \ not found, and no room for new entry
    except-hash-table-full throw
;

: token@ ( c-addr u -- x )
    \ get entry address and flag for found/empty
    hash-table-find

    \ if found, return value of -xt, otherwise 0
    \ note: value of -xt can be 0 if caller has not filled it in yet
    \ (e.g. occurs between a Macro call and corresponding end-macro)
    IF _hash-table-xt + @ ELSE drop 0 THEN
;

: ?token ( c-addr u -- x )
    \ get entry address and flag for found/empty
    2dup hash-table-find

    \ if empty, copy symbol name and fill in entry
    0= IF
        >r
        here r@  _hash-table-name-addr + !
        dup r@ _hash-table-name-len + !
        here swap dup allot cmove
        r>
    ELSE
        nip nip
    THEN

    \ return address of -xt for caller to fill in
    _hash-table-xt +
;

\ VARIABLE OUTFILE

: emit-token ( x -- )
    dup xFF > IF  dup 8 rshift  emit  THEN  emit ;

\ The following words "Token", "Macro", "end-macro" and "seed" are the heart of
\ the tokenizer -- either "Token" or "Macro" makes an entry in the hash table,
\ and each hash table entry points to an anonymous function which is called by
\ "seed" when that token is encountered in the input stream. If you define it
\ with "Token", you get a canned routine that simply emits the corresponding
\ token into the *.seed file, but if you define it with "Macro" you specify the
\ routine to be executed when that token is compiled. So macros allows you to
\ compile control structures and so forth. "Macro" operates similarly to ":" in
\ that it switches to compilation mode. "end-macro" operates similarly to ";"
\ in that it finishes compilation, then it writes your routine into hash table.
\ Note that a difference between "Token" and "Macro" is that "Token" generates
\ a new token number (hence assuming we will emit a "fun" token to make the
\ seedForth kernel do the same at the other side), whereas "Macro" does not.
Variable #tokens  0 #tokens !
Variable last-xt-ptr \ will be copied to colon-xt-ptr during colon definition
: Token ( <name> -- )
   :noname
     #tokens @  postpone Literal
     postpone emit-token
     postpone ;
   parse-name ?token dup last-xt-ptr ! !
   1 #tokens +! ;

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
   seed key   emit ;

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

\ eot is overloaded to either:
\ - return from compilation state to interpretive state
\   (used for compiling ; and various control flow constructs)
\ - quit the interpreter if invoked in interpretive state
\   (can overload it because control flow is not allowed here)
\ this means that if the token stream runs out and starts to return
\ EOT characters, we will first terminate any word definition that
\ was in progress, then we'll do an automatic bye (in the old way,
\ there was an automatic bye token appended to seed file, but this
\ was annoying because seedForthInteractive had to read and drop it)
Macro [ ( -- )
  seed eot
end-macro
Macro ] ( -- )
  seed compiler
end-macro

\ the colon-xt-ptr points into the hash table entry of the symbol
\ being defined during a colon-definition, indirecting through this
\ pointer gives the routine the tokenizer executes when compiling
\ that symbol later -- initially it just outputs the symbol's token,
\ but by editing the value at the colon-xt-ptr we can implement a
\ chain of actions to execute before outputting the symbol's token
Variable colon-xt-ptr  0 colon-xt-ptr !
Macro : ( <name> -- )
  Token  last-xt-ptr @ colon-xt-ptr !
  seed fun
end-macro
Macro ; ( -- )
  0 colon-xt-ptr !
  seed exit
  seed [
end-macro

\ New style defining-words (regular Forth syntax)
\ Call the Create macro inside a definer-definition, e.g.
\   : Variable Create drop 0 , ;
\ This scheme can also handle SOME more complex cases, e.g.
\   : 2Variable Create drop 0 , Create drop 0 , ;
\ But, it does not have the full generality of Forth Create, since we cannot
\ properly handle <name> arguments to Forth words when working via tokenizer
Macro Create
  colon-xt-ptr @ 0= abort" Create outside of colon-definition"

  \ tokenizer side of creating a new defining-word
  \ hook the routine at colon-xt-ptr to call Token then call the old routine,
  \ so each time seed source calls e.g. Variable, we'll consume the name and
  \ create the corresponding token (then the new variable can be referenced)
  :noname
    postpone Token
    colon-xt-ptr @ @  postpone Literal  postpone execute
    postpone ;
  colon-xt-ptr @ !

  \ seedForth side of creating a new defining-word
  \ compile "create dup h," instead of "Create" in body of new defining-word,
  \ makes the corresponding token in seedForth heads table to keep us in sync
  seed create
  seed dup
  seed h,
end-macro

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

\ Old style defining-words (special seedForth syntax)
\ Use "Definer" instead of ":" for definition that begins with "create", e.g.
\   Definer Variable create drop 0 , ;
\ Note: above style is deprecated, please use the Create macro instead, e.g.
\   : Variable Create drop 0 , ;

\ A Definer-definition is similar to a :-definition, see for reference:
\   Macro : ( <name> -- )  Token  seed fun  end-macro
\ However, "Token" is replaced by "Macro", so that we will get control again
\ when the user invokes the Definer-definition, e.g. if user calls "Variable".
\ The "Token" routine compiles an anonymous function referring to the current
\ #token and then increments #token, and we do the same thing here -- we have
\ "# tokens @" in the Macro-body and then "1 #tokens +!" before the "seed fun".
Macro Definer ( <name> -- )
   Macro
      \ take name of e.g. Variable being defined and create a token for it
      postpone Token
      \ compile a call to the user's Definer-definition (original token no)
      #tokens @  postpone Literal
      postpone emit-token
   postpone end-macro
   1 #tokens +!
   seed fun
   \ user's code for Definer-body will begin with a call to "create", so
   \ prefix their code with the sequence "here h," to give the token that
   \ they create a header, and therefore keep the token numbering in sync
   seed here
   seed h,
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

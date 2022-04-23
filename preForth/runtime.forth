: ( 
   ')' parse 2drop ; immediate

: \ 
   source nip >in ! ; immediate

: AHEAD  ( -- c:orig )
    postpone branch  here 0 , ; immediate

: IF ( -- c:orig )
    postpone ?branch here 0 , ; immediate

: THEN ( c:orig -- )
    here swap ! ; immediate

: ELSE ( c:orig1 -- c:orig2 )
    postpone AHEAD  swap  postpone THEN ; immediate

: BEGIN ( -- c:dest )
    here ; immediate

: WHILE ( c: orig -- c:dest c:orig )
    postpone IF swap ; immediate

: AGAIN ( c:orig -- )
    postpone branch , ; immediate

: UNTIL ( c:orig -- )
    postpone ?branch , ; immediate

: REPEAT ( c:orig c:dest -- )
    postpone AGAIN   postpone THEN ; immediate

\ are these necessary? 
\ you can use the phrase  dup x = IF drop  instead of   x case? IF  or  x OF 
: case? ( n1 n2 -- true | n1 false )
    over = dup IF nip THEN ;

: OF ( n1 n2 -- n1 | )
    postpone case?  postpone IF ; immediate

: :noname ( -- xt ) 
    new ] ;

: Variable ( <name> )
    Create 0 , ;

: Constant ( x <name> -- )
    Create , Does> @ ;

0 Constant false
false invert Constant true

: fill ( c-addr u x -- )
     >r BEGIN ( c-addr u )  
          dup 
        WHILE ( c-addr u )
           r@ third c!
           1 /string
        REPEAT ( c-addr u )
    2drop r> drop
;

: lshift ( x u -- ) BEGIN ?dup WHILE swap 2* swap 1-  REPEAT ;

\ if we don't have u2/ but only 2* and 0< we need to implement u2/ with a loop. Candidate for primitive
: u2/ ( x1 -- x2 )
   0  8 cells 1-  \ for every bit
   BEGIN ( x q n )
      ?dup 
   WHILE  ( x q n )
      >r 2*  over 0< IF 1+ THEN  >r 2* r> r> 1- 
   REPEAT ( x q n )
   nip ;

: rshift ( x u -- ) BEGIN ?dup WHILE swap u2/ swap 1-  REPEAT ;

: * ( n1 n2 -- )
   2dup xor 0< >r abs swap abs um* drop r> IF negate THEN ;

: u/ ( u1 u2 -- u3 )  >r 0 r> um/mod nip ;

: FOR ( n -- )
    postpone BEGIN 
    postpone >r ; immediate

: NEXT ( -- )
    postpone r> 
    postpone 1-
    postpone dup
    postpone 0<
    postpone UNTIL
    postpone drop ; immediate

: recurse ( -- )  last @ _xt @ compile, ; immediate

\ conditional compilation

| : next-token ( -- c-addr u )
    BEGIN 
      parse-name dup 0= 
    WHILE ( c-addr u )
      2drop refill 0= -39 and throw
    REPEAT ( c-addr u ) ;

| : ([ELSE]) ( level c-addr u -- level' )
        2dup s" [IF]" compare 0= IF 2drop 1+ exit THEN
        2dup s" [ELSE]" compare 0= IF 2drop 1- dup IF 1+ THEN exit THEN
             s" [THEN]" compare 0= IF 1- THEN ;

: [ELSE] ( -- )
    1 BEGIN ( level ) next-token ([ELSE]) ?dup 0= UNTIL ; immediate

: [IF] ( f -- ) ?exit postpone [ELSE] ; immediate

: [THEN] ; immediate

: abort ( -- )  -1 throw ;

: chars ; immediate

: char+ 1+ ;

' exit Alias EXIT

: bounds ( addr count -- limit addr)  over + swap ;

| : (abort") ( f c-addr u -- )  rot IF  errormsg 2! -2 throw THEN 2drop ;

: abort" ( f -- ) 
    postpone s" 
    postpone (abort") ; immediate

: DO ( to from -- )
     postpone swap
     postpone BEGIN
     postpone >r postpone >r ; immediate

: LOOP ( -- )
     postpone r> 
     postpone 1+ 
     postpone r> 
     postpone 2dup postpone = postpone UNTIL 
     postpone 2drop ; immediate

: I ( -- )
     postpone r@ ; immediate

\ : ?DO ( to from -- )
\     postpone 2dup
\     postpone -
\     postpone IF  postpone DO ; immediate

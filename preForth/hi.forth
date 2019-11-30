0 echo !

cr .( ⓪ )

: 2drop  drop drop ;
: ( 
   ')' parse 2drop ; immediate

: \ 
   source nip >in ! ; immediate

\ cr .( hi - doing some test )
\ t{ 3 4 + -> 7 }t   \ pass
\ t{ 3 -> }t         \ wrong number of results
\ t{ 3 4 + -> 8 }t   \ incorrect result

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

: s" ( ccc" -- c-addr u ) \ compile only
    postpone $lit
    '"' parse
    dup 0= -39 and throw   
    here over 1+ allot place ; immediate  

cr .( ① )
cr

: :noname ( -- xt ) 
    new ] ;

: Variable ( <name> )
    Create 0 , ;

: Constant ( x <name> -- )
    Create , Does> @ ;

0 Constant false
false invert Constant true


: on  ( addr -- ) true  swap ! ;
: off ( addr -- ) false swap ! ;


: fill ( c-addr u x -- )
     >r BEGIN ( c-addr u )  
          dup 
        WHILE ( c-addr u )
           r@ third c!
           1 /string
        REPEAT ( c-addr u )
    2drop r> drop
;

: erase ( c-addr u -- )  0 fill ;
: blank ( c-addr u -- ) bl fill ;

\ : xor ( x1 x2 -- x3 ) 
\    2dup or >r  invert swap invert or r> and ;
\
\ t{ 15 10 xor -> 5 }t
\ t{ 21845 dup xor -> 0 }t  \ $5555
\ t{ 21845 dup 2* xor -> 65535 }t

: 0> ( n -- f )  0 > ;

t{  10 0> -> -1 }t
t{   0 0> ->  0 }t
t{ -10 0> ->  0 }t

: 2>r ( x1 x2 -- r:x1 r:x2 ) 
   swap r> swap >r swap >r >r ;

: 2r> ( r:x1 r:x2 -- x1 x2 )
   r>   r> swap r> swap >r  swap ;

: 2r@ ( r:x1 r:x2 -- r:x1 r:x2 x1 x2 )
   r>   r> r> 2dup >r >r swap  rot >r ;

: 2>r-test ( x1 x2 -- x1 x2 )  2>r r> r> swap ;
t{ 3 4 2>r-test -> 3 4 }t

: 2r>-test ( x1 x2 -- x1 x2 )  swap >r >r  2r> ;
t{ 3 4 2r>-test -> 3 4 }t

: 2r@-test ( x1 x2 -- x1 x2 )  2>r  2r@  2r> 2drop ;
t{ 3 4 2r@-test -> 3 4 }t


: n>r ( x1 ... xn -- r: xn ... x1 n )
   dup                        \  --
   BEGIN ( xn ... x1 n n' )
      ?dup
   WHILE ( xn ... x1 n n' )
      rot r> swap >r >r    ( xn ... n n' ) ( R: ... x1 )
      1-                   ( xn ... n n' ) ( R: ... x1 )
   REPEAT ( n )
   r> swap >r >r ;

: nr> ( R: x1 .. xn n -- xn .. x1 n )
\ Pull N items and count off the return stack.
   r>  r> swap >r dup
   BEGIN
      ?dup
   WHILE
      r> r> swap >r -rot
      1-
   REPEAT ;

: n>r-test ( x1 x2 -- n x1 x2 )  2 n>r r> r> r> ;
t{ 3 4 n>r-test -> 2 3 4 }t

: nr>-test ( x1 x2 -- x1 x2 n )  >r >r 2 >r  nr> ;
t{ 3 4 nr>-test -> 3 4 2 }t

: 2rot ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
    2>r 2swap 2r> 2swap ;

t{ 1 2 3 4 5 6 2rot -> 3 4 5 6 1 2 }t


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

t{ -1 u2/  dup 1+ u< -> -1 }t
t{ -1 u2/  10 +  dup 10 + u< -> -1 }t


: rshift ( x u -- ) BEGIN ?dup WHILE swap u2/ swap 1-  REPEAT ;

: s>d ( n -- d )  dup 0< ;

t{ 1 3 lshift -> 8 }t
\ t{ 48 3 rshift -> 6 }t

: <> ( x1 x2 -- f ) = 0= ;
t{ 3 3 <> -> 0 }t
t{ 'x' 'u' <> -> -1 }t


: pick ( xn ... xi ... x0 i -- xn ... xi ... x0 xi )
    1+ cells sp@ + @ ;
t{ 10 20 30 1 pick ->  10 20 30 20 }t

: recursive ( -- )  reveal ; immediate

: roll ( xn-1 ... x0 i -- xn-1 ... xi-1 xi+1 ... x0 xi )
    recursive ?dup IF swap >r 1- roll r> swap THEN ;

t{ 10 20 30 1 roll ->  10 30 20 }t

| Variable (to) (to) off

: Value ( x -- ) 
    Create , 
    Does> 
       (to) @ IF ! (to) off ELSE @ THEN ;

: to ( x <name> -- )  (to) on ;

5 Value val
t{ val  42 to val  val -> 5 42 }t


\ : u< ( u1 u2 -- f )
\   over 0< IF  dup 0< IF < exit THEN \ both large
\               2drop false exit THEN  \ u1 is larger
\   dup 0<  IF  2drop true exit THEN \ u2 is larger
\   <  \ both small
\ ;


:  within ( test low high -- flag ) 
     over - >r - r>  u<  ;

t{ 2 3 5 within -> false }t
t{ 3 3 5 within -> true }t
t{ 4 3 5 within -> true }t
t{ 5 3 5 within -> false }t
t{ 6 3 5 within -> false }t

Variable up

: User ( x -- )
    Create cells , Does> @ up @ + ;

0 User u1
1 User u2
2 User u3

: n' parse-name find-name ;


\ cr cr words cr
cr .( ready )
cr .( ② )

\ : test s" xlerb" evaluate ;

: * ( n1 n2 -- )
   2dup xor 0< >r abs swap abs um* drop r> IF negate THEN ;

: fac ( n -- ) recursive
    dup 0= IF drop 1 exit THEN
    dup 1- fac * ;

t{ 6 fac -> 720 }t

: fib ( n1 -- n2 ) recursive
    dup 0=  IF exit THEN
    dup 1 = IF exit THEN
    dup 1- fib  swap 2 - fib + ;

t{ 10 fib -> 55 }t

: sqr ( u -- u^2 )  dup * ;

: u/ ( u1 u2 -- u3 )  >r 0 r> um/mod nip ;

: sqrt ( u^2 -- u )
    dup 0= ?exit
    dup >r dup
    BEGIN ( xi-1 xi )
      nip dup
      \ x = (x + n//x) // 2
      r@ over u/ + u2/ ( xi xi+1 )
      2dup over 1+ over = >r = r> or
    UNTIL ( xi xi+1 )
    drop r> drop ;

t{ 15 sqrt -> 3 }t
t{ 16 sqrt -> 4 }t

: pyth ( a b -- c )
    swap sqr  swap sqr  + sqrt ;

t{ 3 4 pyth -> 5 }t
t{ 65535 dup * sqrt -> 65535 }t



\ remove headers from dictionary
| : unlink-header ( addr name -- ) \ 2dup ." unlink " . .
     dup >r ( _link ) @ swap !  r> dispose ;

: remove-headers ( -- )
   context @ dup @ 
   BEGIN ( addr name )
      dup 
   WHILE ( addr name )
      dup headerless? IF over >r unlink-header r> ELSE nip THEN ( addr )
      dup @ 
   REPEAT
   2drop ;

| : hidden-word ." still there - " ;

: visible-word ( -- ) hidden-word hidden-word ;


: save-mem ( c-addr1 u1 -- c-addr2 u2 )
    dup >r allocate throw swap over r@ cmove r> ;

: s( ( -- c-addr u )
    ')' parse  save-mem ; immediate

cr .( ③ )

\ : Marker ( <name> -- )
\    Create here , hp @ , Does> 2@  here - allot   hp ! ;
\ Cannot access hp  what about dictionary headers?

\ remove-headers

: package ( <name> -- )  parse-name 2drop ;
: private ( -- ) heads off ;
: public ( -- ) heads on ;
: end-package ( -- ) remove-headers ;


package test
  : a ." a" ;
private
  : b ." b" ;
public
  : c a b ." c" ;
end-package

t{ s( abc) s( abc) compare -> 0 }t
t{ s( abc) s( ab)  compare -> 1 }t
t{ s( ab)  s( abc) compare -> -1 }t
t{ s( abc) s( def)  compare -> -1 }t
t{ s( def) s( abc)  compare -> 1 }t

: Defer ( <name> -- )
    Create 0 , Does> @ execute ;

Defer %defer  ' %defer >body 2 cells -  @  Constant dodefer
              ' %defer >body 1 cells -  @  Constant dodoes


\ highly implementation specific
: backpatch1 ( xt1 xt2 -- ) >body >r
    >body 1 cells -  r@ !
    [ ' exit ] Literal >body 1 cells - r> cell+ ! ;

: dp! ( addr -- )  here - allot ;

: backpatch ( xt1 xt2 -- ) 
    here >r  >body dp!  compile,  postpone exit  r> dp! ;

: hallo ." original" ;
: moin hallo hallo ;

: abc ." backpatched" ;

' abc ' hallo backpatch




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

: cntdwn 65535 FOR r@ . NEXT ;

: ²  sqr ;
: √  sqrt ;

: ⟼  -> ;

: testall ( -- ) \ see if sqrt works for all 32 bit numbers
    65535 FOR
       t{ r@ ² √  ⟼  r@ }t
    NEXT ." ⚑" ;

cr .( ➍ )

Variable Δ

: ❤️ ." love" ;
: ♩ ." pling" ;
: :smile: ." 😀" ;

Variable ∆t

Variable voc-link  0 voc-link !

: Vocabulary ( <name> -- )  
   wordlist Create here voc-link @ , voc-link ! last @ , , 
   Does> 2 cells +  @  >r get-order nip r> swap set-order ;

: .voc ( wid -- ) 
   dup forth-wordlist = IF drop ." Forth " exit THEN
   voc-link @
   BEGIN ( wid link )
     dup
   WHILE ( wid link )
     2dup  2 cells + @ = IF  nip cell+ @ _name count type space exit THEN
     @ 
   REPEAT ( wid 0 )
   drop u. ;

' .voc ' .wordlist backpatch


: recurse ( -- )  last @ _xt @ compile, ; immediate

: cntd ( n -- ) ?dup 0= ?exit dup . 1- recurse '.' emit ;


\ number output:  <# # #s #> sign hold holds base . u. .r u.r

Variable base
Variable hld

: hold ( c -- )   -1 hld +!  hld @ c! ;

\ : holds ( c-addr u -- )  recursive
\    dup 0= IF 2drop exit THEN 
\    over c@ >r  1 /string holds  r> hold ;

: holds ( c-addr u -- )
   BEGIN dup WHILE 1- 2dup + c@ hold REPEAT 2drop ;

: mu/mod ( d n1 -- rem d.quot ) 
   >r   0 r@  um/mod   r> swap >r um/mod  r> ; 

: <# ( -- )  pad hld ! ;

: # ( ud1 -- ud2 )  
     base @ mu/mod  rot 9 over < IF [ 'A' '9' 1+ - ] Literal + THEN '0' + hold ;

: #s ( ud1 -- d.0 )  BEGIN #  2dup or 0= UNTIL ;

: #> ( ud -- c-addr u )  2drop hld @ pad over - ; 

: sign ( n -- )  0< IF '-' hold THEN ;

: decimal ( -- ) 10 base ! ; decimal
: hex     ( -- ) 16 base ! ;

| : (.) ( n -- ) dup abs 0 <# #s rot sign #> ;
: dot ( n -- )  (.) type space ; ' dot ' . backpatch
: .r ( n l -- )  >r (.) r> over - 0 max spaces type ;

| : (u.) ( u -- ) 0 <# #s #> ;
: u. ( u -- ) (u.) type space ;
: u.r ( u l -- )  >r (u.) r> over - 0 max spaces type ;

: at-xy ( u1 u2 -- ) \ col row
    esc ." [" 1+  0 u.r ." ;" 1+ 0 u.r ." H" ;

\ : at? CSI 6n 

: clreol ( -- )
    esc ." [K" ;

: save-cursor-position ( -- ) 27 emit '7' emit ;
: restore-cursor-position  ( -- ) 27 emit '8' emit ;

: show-status ( -- )
   save-cursor-position reverse
   base @ >r decimal 
   0 1 at-xy  clreol  80 spaces  0 1 at-xy  
     ."  seedForth 😉 "
     ." | order: " order  
     ." | base: "  r@ . 
     ." | stack: " .s  
   r> base !
   normal restore-cursor-position ;

' show-status ' .status >body !

only Forth also definitions
Vocabulary root

: only ( -- ) only root ;

root definitions

: order order ;
: definitions definitions ;
: words words ;
: Forth Forth ;
: only only ;
: also also ;
: bye bye ;

only Forth also definitions order

: mod ( u1 u2 -- u3 ) 0 swap um/mod drop ;

: prime? ( u -- f )
    dup 2 = IF drop true exit THEN
    dup 2 mod 0= IF drop false exit THEN
    3 BEGIN ( u i )
        2dup dup * < 0= 
      WHILE ( u i )
        2dup mod  0= IF 2drop false exit THEN
        2+
      REPEAT ( u i )
      2drop true 
;

: th.prime ( u -- )
    1 BEGIN over WHILE 1+ dup prime? IF swap 1- swap THEN REPEAT nip ; 

cr cr cr .( The ) 10001 dup . .( st prime is ) th.prime . 

echo on


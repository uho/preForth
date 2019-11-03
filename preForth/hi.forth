0 echo !
: 2drop  drop drop ;
: ( 
   ')' parse 2drop ; immediate

: \ 
   source nip >in ! ;


cr .( hi - doing some test )
t{ 3 4 + -> 7 }t
t{ 3 -> }t
t{ 3 4 + -> 8 }t



: on ( addr -- ) -1 swap ! ;
: off ( addr -- ) 0 swap ! ;


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

: s"  
    postpone $lit  '"' parse here over 1+ allot place ; immediate

: :noname ( -- xt ) 
    new ] ;

: Variable ( <name> )
    Create 0 , ;

: Constant ( x <name> -- )
    Create , Does> @ ;


Variable up

: User ( x -- )
    Create cells , Does> @ up @ + ;


0 User u1
1 User u2
2 User u3

: n' parse-name last @ find-name ;


cr cr words cr
cr .( ready )

echo on

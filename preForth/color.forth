\ load prepared colorForth

Variable color

: Color ( u -- )
    Create immediate , Does>  @ color ! ; 

0 Color WHITE{
0 Color BLACK{
1 Color RED{
2 Color GREEN{
3 Color YELLOW{

0 Color }COLOR

: ign ( c-addr u -- )
    2drop ;

: def ( c-addr u -- )  
    s" : " pad place  pad +place  pad count evaluate reveal 
    postpone [ ;

: find-def ( c-addr u -- xt imm? true | x false )
    2dup find-name ?dup IF >r 2drop r@ _xt @ r> immediate? true exit THEN
    ?# 0= 0= -13 and throw ;

: comp ( c-addr u -- )
    find-def 
    IF    IF execute ELSE compile, THEN exit THEN
    ['] lit compile, , ;

: exec ( c-addr u -- )
    find-def IF drop execute THEN ;

: CYAN{ ( -- )
    '}' parse parse-name 2drop ; immediate

Create actions
    ' ign ,   ' def ,  ' comp ,  ' exec ,

: handle-token ( c-addr u -- )
    2dup s" }COLOR" compare 0= IF 2drop 3 color ! exit THEN
    actions  color @ cells + @ execute ;

: ColorForth ( -- ) ." : "
    input-echo off
    echo off
    BEGIN next-token dup WHILE
       2dup s" END" compare 0= IF 2drop echo on input-echo on exit THEN
       ( cr color @ . ." | " 2dup type ." --> " ) handle-token REPEAT drop ;

echo on cr cr .( Welcome! ) input-echo on
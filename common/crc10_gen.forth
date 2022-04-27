\ run this as follows:
\ ./seedForth seedForthBoot.seed runtime.forth crc10_gen.forth >crc10.forth

817 Constant poly \ CRC-10/ATM, truncated polynomial 0x233, bit reversed 0x331

\ we don't want MS-DOS style line endings in the generated CRC table file
: lf 10 emit ;

: gen
  ." Create crc10_tab" lf
  256 0 ?DO
    I 8 0 ?DO
      \ split into LSB and the remaining bits
      \ in seedForth: 2 u/mod swap
      \ in gForth: 2 /mod swap
      dup 1 rshift swap 1 and

      IF poly xor THEN
    LOOP
    . ',' emit
    I 7 and 7 = IF lf ELSE space THEN
  LOOP
  lf
  ." : crc10 ( addr len crc -- crc )" lf
  ."   swap 0 ?DO ( addr crc )" lf
  ."     \ retrieve next character" lf
  ."     over I + c@" lf
  lf
  ."     \ xor into low bits of crc" lf
  ."     xor" lf
  lf
  ."     \ separate into table index and remaining bits" lf
  ."     \ 256 u/mod swap" lf
  ."     dup 8 rshift swap 255 and" lf
  lf
  ."     \ look up new bits from table" lf
  ."     cells crc10_tab + @" lf
  lf
  ."     \ combine with remaining (shifted right) bits" lf
  ."     xor" lf
  ."   LOOP" lf
  ."   nip" lf
  ." ;" lf
  lf
  ." \ testing:" lf
  ." \ Create hello 104 c, 101 c, 108 c, 108 c, 111 c," lf
  ." \ hello 5 1023 crc10 . ;" lf
;

gen

STDIN_DATA = 0
STDOUT_DATA = 1
STDERR_DATA = 2
STDIN_STATUS = 3
STDOUT_STATUS = 4
STDERR_STATUS = 5
USLEEP_LO = 6
USLEEP_HI = 7
SYS_EXIT = 8

	.area	text

	ld	hl,message
	ld	b,message_end - message
print_message:
	ld	a,(hl)
	inc	hl
	out	(STDERR_DATA),a
	djnz	print_message

in_wait:
	in	a,(STDIN_STATUS)
	or	a
	jr	nz,in_char

	ld	a,<1000
	out	(USLEEP_LO),a
	ld	a,>1000
	out	(USLEEP_HI),a
	jr	in_wait

in_char:
	in	a,(STDIN_DATA)
	cp	4 ; EOT
	jr	z,done

	ld	e,a

out_wait:
	in	a,(STDOUT_STATUS)
	or	a
	jr	nz,out_char

	ld	a,<1000
	out	(USLEEP_LO),a
	ld	a,>1000
	out	(USLEEP_HI),a
	jr	out_wait

out_char:
	ld	a,e
	out	(STDOUT_DATA),a
	jr	in_wait

done:	ld	a,0
	out	(SYS_EXIT),a

	.area	text

message:
	.ascii	/hello, world/
	.db	0xa
message_end:

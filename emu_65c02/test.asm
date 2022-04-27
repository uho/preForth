STDIN_DATA = 0x200
STDOUT_DATA = 0x201
STDERR_DATA = 0x202
STDIN_STATUS = 0x203
STDOUT_STATUS = 0x204
STDERR_STATUS = 0x205
USLEEP_LO = 0x206
USLEEP_HI = 0x207
SYS_EXIT = 0x208

	.r65c02

	.area	text

	cld

	ldx	#0
	ldy	#message_end - message
print_message:
	lda	message,x
	sta	STDERR_DATA
	inx
	dey
	bne	print_message

in_wait:
	lda	STDIN_STATUS
	bne	in_char

	lda	#<1000
	sta	USLEEP_LO
	lda	#>1000
	sta	USLEEP_HI
	bra	in_wait

in_char:
	lda	STDIN_DATA
	clc
	cmp	#4 ; EOT
	beq	done

	tax

out_wait:
	lda	STDOUT_STATUS
	bne	out_char

	lda	#<1000
	sta	USLEEP_LO
	lda	#>1000
	sta	USLEEP_HI
	bra	out_wait

out_char:
	stx	STDOUT_DATA
	bra	in_wait

done:	lda	#0
	sta	SYS_EXIT

	.area	text

message:
	.ascii	/hello, world!/
	.db	0xa
message_end:

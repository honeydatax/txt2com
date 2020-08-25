;;;-----------------------------------------
;;; Start MC68HC11 gcc assembly output
;;; gcc compiler 3.3.6-m68hc1x-20060122
;;; Command:	/usr/lib/gcc-lib/m68hc11/3.3.6-m68hc1x-20060122/cc1 -quiet -D__GNUC__=3 -D__GNUC_MINOR__=3 -D__GNUC_PATCHLEVEL__=6 -Dmc68hc1x -D__mc68hc1x__ -D__mc68hc1x -D__INT__=32 -Dmc6811 -DMC6811 -Dmc68hc11 strcat.c -quiet -dumpbase strcat.c -auxbase-strip strcat -o strcat.S
;;; Compiled:	Tue Aug 25 08:48:49 2020
;;; (META)compiled by GNU C version 4.6.3.
;;;-----------------------------------------
	.file	"strcat.c"
	.mode mlong
	.sect	.text
	.globl	strslen
	.type	strslen,@function
strslen:
	ldx	*_.frame
	pshx
	pshx
	pshx
	pshx
	pshx
	sts	*_.frame
	ldx	*_.d1
	pshx
	ldx	*_.d2
	pshx
	ldx	*_.frame
	std	1,x
	ldy	*_.frame
	clr	6,y
	clr	5,y
	clr	4,y
	clr	3,y
.L2:
	ldx	*_.frame
	ldx	5,x
	stx	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldy	*_.frame
	ldd	1,y
	addd	*_.d2
	ldx	*_.frame
	std	7,x
	ldy	*_.frame
	ldy	7,y
	ldab	0,y
	cmpb	#0
	bne	.L4
	bra	.L3
.L4:
	ldy	*_.frame
	ldd	5,y
	ldx	3,y
	addd	#1
	bcc	.L5
	inx	
.L5:
	ldy	*_.frame
	std	5,y
	stx	3,y
	bra	.L2
.L3:
	ldy	*_.frame
	ldd	5,y
	std	*_.d2
	ldd	3,y
	std	*_.d1
	ldd	*_.d2
	ldx	*_.d1
	puly
	sty	*_.d2
	puly
	sty	*_.d1
	puly
	puly
	puly
	puly
	puly
	sty	*_.frame
	rts
	.size	strslen, .-strslen
	.globl	strcats
	.type	strcats,@function
strcats:
	ldx	*_.frame
	pshx
	tsx
	xgdx
	addd	#-10
	xgdx
	txs
	sts	*_.frame
	ldx	*_.d1
	pshx
	ldx	*_.d2
	pshx
	ldx	*_.frame
	std	1,x
	ldy	*_.frame
	clr	6,y
	clr	5,y
	clr	4,y
	clr	3,y
	ldx	*_.frame
	ldd	1,x
	bsr	strslen
	std	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldy	*_.frame
	ldd	1,y
	addd	*_.d2
	ldx	*_.frame
	std	7,x
.L7:
	ldy	*_.frame
	ldy	5,y
	sty	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldx	*_.frame
	ldd	15,x
	addd	*_.d2
	ldy	*_.frame
	std	9,y
	ldy	*_.frame
	ldy	9,y
	ldab	0,y
	cmpb	#0
	bne	.L9
	bra	.L8
.L9:
	ldx	*_.frame
	ldx	5,x
	stx	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldy	*_.frame
	ldd	7,y
	ldx	*_.d2
	std	*_.tmp
	xgdx
	addd	*_.tmp
	xgdx
	ldy	*_.frame
	ldy	5,y
	sty	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldy	*_.frame
	ldd	15,y
	addd	*_.d2
	std	*_.tmp
	ldy	*_.tmp
	ldab	0,y
	stab	0,x
	ldy	*_.frame
	ldd	5,y
	ldx	3,y
	addd	#1
	bcc	.L10
	inx	
.L10:
	ldy	*_.frame
	std	5,y
	stx	3,y
	bra	.L7
.L8:
	ldx	*_.frame
	ldx	5,x
	stx	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldy	*_.frame
	ldd	7,y
	ldx	*_.d2
	std	*_.tmp
	xgdx
	addd	*_.tmp
	xgdx
	ldy	*_.frame
	ldy	5,y
	sty	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldy	*_.frame
	ldd	15,y
	addd	*_.d2
	std	*_.tmp
	ldy	*_.tmp
	ldab	0,y
	stab	0,x
	ldy	*_.frame
	ldd	5,y
	std	*_.d2
	ldd	3,y
	std	*_.d1
	ldd	*_.d2
	ldx	*_.d1
	puly
	sty	*_.d2
	puly
	sty	*_.d1
	tsy
	xgdy
	addd	#10
	xgdy
	tys
	puly
	sty	*_.frame
	rts
	.size	strcats, .-strcats
	; extern	memcpy
	; extern	memset
	.section	.rodata
.LC0:
	.string	"hello "
	.zero	43
.LC1:
	.string	"world\n"
	.sect	.text
	.globl	main
	.type	main,@function
main:
	ldx	*_.frame
	pshx
	tsx
	xgdx
	addd	#-52
	xgdx
	txs
	sts	*_.frame
	ldd	#1
	addd	*_.frame
	ldy	#.LC0
	ldx	#7
	pshx
	pshy
	bsr	memcpy
	puly
	puly
	ldd	*_.frame
	addd	#8
	ldx	#43
	pshx
	ldx	#0
	pshx
	ldx	#0
	pshx
	bsr	memset
	puly
	puly
	puly
	ldd	#.LC1
	ldx	*_.frame
	std	51,x
	ldy	*_.frame
	ldx	51,y
	pshx
	ldd	#1
	addd	*_.frame
	bsr	strcats
	ins
	ins
	clra
	clrb
	ldx	#0
	tsy
	xgdy
	addd	#52
	xgdy
	tys
	puly
	sty	*_.frame
	rts
	.size	main, .-main
	.ident	"GCC: (GNU) 3.3.6-m68hc1x-20060122"

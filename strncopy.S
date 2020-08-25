;;;-----------------------------------------
;;; Start MC68HC11 gcc assembly output
;;; gcc compiler 3.3.6-m68hc1x-20060122
;;; Command:	/usr/lib/gcc-lib/m68hc11/3.3.6-m68hc1x-20060122/cc1 -quiet -D__GNUC__=3 -D__GNUC_MINOR__=3 -D__GNUC_PATCHLEVEL__=6 -Dmc68hc1x -D__mc68hc1x__ -D__mc68hc1x -D__INT__=32 -Dmc6811 -DMC6811 -Dmc68hc11 strncopy.c -quiet -dumpbase strncopy.c -auxbase-strip strncopy -o strncopy.S
;;; Compiled:	Tue Aug 25 08:09:09 2020
;;; (META)compiled by GNU C version 4.6.3.
;;;-----------------------------------------
	.file	"strncopy.c"
	.mode mlong
	.sect	.text
	.globl	strncopy
	.type	strncopy,@function
strncopy:
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
.L2:
	ldy	*_.frame
	ldd	5,y
	std	9,y
	ldd	3,y
	std	7,y
	ldy	*_.frame
	ldd	7,y
	cpd	17,y
	blt	.L4
	ldy	*_.frame
	ldd	7,y
	cpd	17,y
	bgt	.L3
	ldy	*_.frame
	ldd	9,y
	cpd	19,y
	bls	.L4
	bra	.L3
.L4:
	ldx	*_.frame
	ldx	5,x
	stx	*_.d2
	clr	*_.d1+1
	clr	*_.d1
	ldy	*_.frame
	ldd	1,y
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
	bcc	.L6
	inx	
.L6:
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
	tsy
	xgdy
	addd	#10
	xgdy
	tys
	puly
	sty	*_.frame
	rts
	.size	strncopy, .-strncopy
	.section	.rodata
.LC0:
	.string	"hello world\n"
	.sect	.text
	.globl	main
	.type	main,@function
main:
	ldx	*_.frame
	pshx
	tsx
	xgdx
	addd	#-1026
	xgdx
	txs
	sts	*_.frame
	ldd	*_.frame
	addd	#1025
	ldy	#.LC0
	pshb
	psha
	pulx
	sty	0,x
	ldx	#5
	pshx
	ldx	#0
	pshx
	ldd	*_.frame
	addd	#1025
	std	*_.tmp
	ldy	*_.tmp
	ldx	0,y
	pshx
	ldd	#1
	addd	*_.frame
	bsr	strncopy
	puly
	puly
	puly
	clrb
	ldx	*_.frame
	stab	6,x
	clra
	clrb
	ldx	#0
	tsy
	xgdy
	addd	#1026
	xgdy
	tys
	puly
	sty	*_.frame
	rts
	.size	main, .-main
	.ident	"GCC: (GNU) 3.3.6-m68hc1x-20060122"

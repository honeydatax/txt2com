;;;-----------------------------------------
;;; Start MC68HC11 gcc assembly output
;;; gcc compiler 3.3.6-m68hc1x-20060122
;;; Command:	/usr/lib/gcc-lib/m68hc11/3.3.6-m68hc1x-20060122/cc1 -quiet -D__GNUC__=3 -D__GNUC_MINOR__=3 -D__GNUC_PATCHLEVEL__=6 -Dmc68hc1x -D__mc68hc1x__ -D__mc68hc1x -D__INT__=32 -Dmc6811 -DMC6811 -Dmc68hc11 sub.c -quiet -dumpbase sub.c -auxbase-strip sub -o sub.S
;;; Compiled:	Sat Aug 22 01:34:24 2020
;;; (META)compiled by GNU C version 4.6.3.
;;;-----------------------------------------
	.file	"sub.c"
	.mode mlong
	.sect	.text
	.globl	sub
	.type	sub,@function
sub:
	ldy	*_.frame
	pshy
	pshx
	pshx
	sts	*_.frame
	ldy	*_.d1
	pshy
	ldy	*_.d2
	pshy
	ldy	*_.frame
	std	3,y
	stx	1,y
	ldy	*_.frame
	ldd	3,y
	std	*_.d2
	ldd	1,y
	std	*_.d1
	ldd	*_.d2
	ldx	*_.d1
	ldy	*_.frame
	subd	11,y
	xgdx
	sbcb	10,y
	sbca	9,y
	xgdx
	puly
	sty	*_.d2
	puly
	sty	*_.d1
	puly
	puly
	puly
	sty	*_.frame
	rts
	.size	sub, .-sub
	.globl	main
	.type	main,@function
main:
	ldx	*_.frame
	pshx
	pshx
	pshx
	sts	*_.frame
	ldd	#10
	pshb
	psha
	clra
	clrb
	pshb
	psha
	ldd	#10
	ldx	#0
	bsr	sub
	puly
	puly
	ldy	*_.frame
	std	3,y
	stx	1,y
	clra
	clrb
	ldx	#0
	puly
	puly
	puly
	sty	*_.frame
	rts
	.size	main, .-main
	.ident	"GCC: (GNU) 3.3.6-m68hc1x-20060122"

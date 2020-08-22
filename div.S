;;;-----------------------------------------
;;; Start MC68HC11 gcc assembly output
;;; gcc compiler 3.3.6-m68hc1x-20060122
;;; Command:	/usr/lib/gcc-lib/m68hc11/3.3.6-m68hc1x-20060122/cc1 -quiet -D__GNUC__=3 -D__GNUC_MINOR__=3 -D__GNUC_PATCHLEVEL__=6 -Dmc68hc1x -D__mc68hc1x__ -D__mc68hc1x -D__INT__=32 -Dmc6811 -DMC6811 -Dmc68hc11 div.c -quiet -dumpbase div.c -auxbase-strip div -o div.S
;;; Compiled:	Sat Aug 22 01:37:23 2020
;;; (META)compiled by GNU C version 4.6.3.
;;;-----------------------------------------
	.file	"div.c"
	.mode mlong
	.globl	__divsi3
	.sect	.text
	.globl	divl
	.type	divl,@function
divl:
	ldy	*_.frame
	pshy
	pshx
	pshx
	sts	*_.frame
	ldy	*_.frame
	std	3,y
	stx	1,y
	ldy	*_.frame
	ldd	11,y
	pshb
	psha
	ldd	9,y
	pshb
	psha
	ldy	*_.frame
	ldd	3,y
	ldx	1,y
	bsr	__divsi3
	puly
	puly
	puly
	puly
	puly
	sty	*_.frame
	rts
	.size	divl, .-divl
	; extern	div
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
	bsr	div
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

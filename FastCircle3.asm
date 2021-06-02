; Fast Circle
; Srbislav D. Nesic
; Moj Mikro, Jugoslavija, Januar 1986.

 ORG 50000

TEMPS  EQU $0D4D
CHANOP EQU $1601
PLOTBC EQU $22E9
STKSTR EQU $2AB6
FPTOA  EQU $2DD5
DEFADD EQU 23563
COORDS EQU 23677

FPCALC EQU $28
FPEND  EQU $38

; BASIC entry:
; DEF FN c(x, y, r) = USR 50000
; LET o = FN c(200, 100, r)
; is equal to
; CIRCLE 200, 100, ABS r
; POKE 23677, 200
; POKE 23678, 100
; LET o = 2 * PI * ABS r
START
 LD IX, (DEFADD)

 LD A, (IX+$02)
 LD E, (IX+$03)
 LD D, (IX+$04)
 LD C, (IX+$05)
 LD B, (IX+$06)
 CALL STKSTR
 CALL FPTOA
 LD (XC), A

 LD A, (IX+$0A)
 LD E, (IX+$0B)
 LD D, (IX+$0C)
 LD C, (IX+$0D)
 LD B, (IX+$0E)
 CALL STKSTR
 CALL FPTOA
 LD (YC), A

 LD A, (IX+$12)
 LD E, (IX+$13)
 LD D, (IX+$14)
 LD C, (IX+$15)
 LD B, (IX+$16)
 CALL STKSTR
 RST FPCALC
 DEFB $2A ; abs
 DEFB $31 ; dup
 DEFB FPEND
 CALL FPTOA
 LD (RAD), A

 CALL NC, CIRCLE

 POP BC
 RST FPCALC
 DEFB $A3     ; PI/2
 DEFB FPEND
 LD (HL), $83 ; mul by 4 = 2 PI
 RST FPCALC
 DEFB $04     ; mul
 DEFB FPEND

 RET

; Assembler entry:
; CIRCLE (xc), (yc), (rad)
CIRCLE ; ORG + 93
 LD A, 2
 CALL CHANOP

; Init: x = 0, y = r, u = 1, v = r
 XOR A
 LD (X), A
 LD D, A
 LD H, A
 INC A
 LD E, A
 LD A, (RAD)
 LD (Y), A
 LD L, A
 LD (U), DE
 LD (V), HL

 AND A
 JR NZ, DOTR

; Single dot when r = 0
DOT
 LD BC, (CENTRE)
 CALL PIXELBC
 JP EXIT

; Pixel PLOTs
PIXELXC ; PLOT xc, a
 LD B, A
 LD A, (XC)
 LD C, A
 JR PIXELBC
PIXELYC ; PLOT a, yc
 LD C, A
 LD A, (YC)
PIXELAC ; PLOT c, a
 LD B, A
PIXELBC ; PLOT c, b
 LD A, 256-176
 ADD A, B
 RET C
 JP PLOTBC ; ROM PLOT

; PLOT xc + r, yc
DOTR
 LD A, (RAD)
 LD C, A
 LD A, (XC)
 ADD A, C
 CALL NC, PIXELYC

; PLOT xc - r, yc
DOTL
 LD A, (RAD)
 LD C, A
 LD A, (XC)
 SUB C
 CALL NC, PIXELYC

; PLOT xc, yc + r
DOTU
 LD A, (RAD)
 LD B, A
 LD A, (YC)
 ADD A, B
 CALL NC, PIXELXC

; PLOT xc, yc - r
DOTD
 LD A, (RAD)
 LD B, A
 LD A, (YC)
 SUB B
 CALL NC, PIXELXC

; Main loop:
; x++
; if (u > v) {
;  y--
;  u += 2x + 1 - v
;  v = 2y
; } else {
;  u += 2x + 1
; }
LOOP
 LD A, (X)
 INC A ; x++
 LD (X), A

 LD C, A    ;  c = x
 LD A, (Y)  ;  a = y
 LD B, 0    ; bc = x
 LD HL, (U) ; hl = u
 LD DE, (V) ; de = v

 SBC HL, DE
 JR C, CONT
 JR Z, CONT

 DEC A ; y--
 LD (Y), A

 ADD HL, BC
 ADD HL, BC
 INC HL
 LD (U), HL ; u += 2x + 1 - v

 LD L, A
 LD H, 0
 ADD HL, HL
 LD (V), HL ; v = 2y
 JR TEST

CONT
 ADD HL, DE
 ADD HL, BC
 ADD HL, BC
 INC HL
 LD (U), HL ; u += 2x + 1

; if (x < y) plot 8 dots else
; if (x = y) plot 4 dots else
; exit loop
TEST
 SUB C
 JP C, EXIT
 JR Z, DOT4
 
; PLOT xc + x, yc + y
DOT0
;LD A, (X)
;LD C, A ; x is already in register c
 LD A, (XC)
 ADD A, C
 JR C, DOT2
 LD C, A
 LD A, (Y)
 LD B, A
 LD A, (YC)
 ADD A, B
 CALL NC, PIXELAC

; PLOT xc + x, yc - y
DOT1
 LD A, (X)
 LD C, A
 LD A, (XC)
 ADD A, C
 JR C, DOT2
 LD C, A
 LD A, (Y)
 LD B, A
 LD A, (YC)
 SUB B
 CALL NC, PIXELAC

; PLOT xc - x, yc + y
DOT2
 LD A, (X)
 LD C, A
 LD A, (XC)
 SUB C
 JR C, DOT4
 LD C, A
 LD A, (Y)
 LD B, A
 LD A, (YC)
 ADD A, B
 CALL NC, PIXELAC

; PLOT xc - x, yc - y
DOT3
 LD A, (X)
 LD C, A
 LD A, (XC)
 SUB C
 JR C, DOT4
 LD C, A
 LD A, (Y)
 LD B, A
 LD A, (YC)
 SUB B
 CALL NC, PIXELAC

; PLOT xc + y, yc + x
DOT4
 LD A, (Y)
 LD C, A
 LD A, (XC)
 ADD A, C
 JR C, DOT6
 LD C, A
 LD A, (X)
 LD B, A
 LD A, (YC)
 ADD A, B
 CALL NC, PIXELAC

; PLOT xc + y, yc - x
DOT5
 LD A, (Y)
 LD C, A
 LD A, (XC)
 ADD A, C
 JR C, DOT6
 LD C, A
 LD A, (X)
 LD B, A
 LD A, (YC)
 SUB B
 CALL NC, PIXELAC

; PLOT xc - y, yc + x
DOT6
 LD A, (Y)
 LD C, A
 LD A, (XC)
 SUB C
 JP C, LOOP
 LD C, A
 LD A, (X)
 LD B, A
 LD A, (YC)
 ADD A, B
 CALL NC, PIXELAC

; PLOT xc - y, yc - x
DOT7
 LD A, (Y)
 LD C, A
 LD A, (XC)
 SUB C
 JP C, LOOP
 LD C, A
 LD A, (X)
 LD B, A
 LD A, (YC)
 SUB B
 CALL NC, PIXELAC

 JP LOOP

EXIT
 CALL TEMPS
 LD BC, (CENTRE)
 LD (COORDS), BC
 RET

CENTRE
XC  DEFB 0 ; ORG + 443
YC  DEFB 0 ; ORG + 444
RAD DEFB 0 ; ORG + 445
X   DEFB 0
Y   DEFB 0
U   DEFW 0
V   DEFW 0

; Regsiters entry:
CIRCLEABC ; CIRCLE c, b, a
 LD (CENTRE), BC
CIRCLEA ; CIRCLE (xc), (yc), a
 LD (RAD), A
 JP CIRCLE

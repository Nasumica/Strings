; Srbislav D. Nesic, Srbija

STKSTR EQU $2AB6
DEFADD EQU 23563

FPCALC EQU $28

FRC0 EQU $E0
FST0 EQU $C0
FRC1 EQU $E1
FST1 EQU $C1
FMUL EQU $04
FSUB EQU $03
FDEL EQU $02
FONE EQU $A1
FJPT EQU $00
FJMP EQU $33
FLT0 EQU $36
FGT0 EQU $37
FEND EQU $38
FEXC EQU $01
FNOT EQU $30
FNUL EQU $A0
FDIV EQU $05
FADD EQU $0F
FDUP EQU $31
FRSK EQU $3D
FABS EQU $2A
FSGN EQU $29
FPIH EQU $A3


 ORG 60000


; DEF FN G(x) = USR 60000 : REM Gamma function
; DEF FN f(x) = FN G(x + 1) : REM Factorial, x!
; DEF FN b(n, k) = FN f(n) / (FN f(k) * FN f(n - k)) : REM binomial coef
; DEF FN d(x)=FN f(x*0.5) * 2^(x*0.5) * (PI*0.5)^((COS (PI*x)-1)*0.25) : REM x!!

GAMMA ; faster than LN, slower than ATN
 LD IX, (DEFADD)

 LD A, (IX+$02)
 LD E, (IX+$03)
 LD D, (IX+$04)
 LD C, (IX+$05)
 LD B, (IX+$06)
 CALL STKSTR ; x, max x = 34.82635 (34.8442562727059)

 POP BC

FPGAMMA ; x pn calculator stack
 RST FPCALC ; x
 DEFB FST0, FDEL ; save x in MEM0
 DEFB FONE ; init multiplication / division

MULTIPLY
 DEFB FRC0, FONE, FSUB, FST1 ; MEM1 = x - 1
 DEFB FGT0, FNOT, FJPT ; if not > 0 jump to division
 DEFB DIVIDE-$
 DEFB FRC1, FST0 ; MEM0 = MEM1, x = x - 1
 DEFB FMUL, FJMP; multiply and repeat
 DEFB MULTIPLY-$

DIVIDE
 DEFB FRC0, FGT0, FJPT ; if x > 0 division done
 DEFB CHECKCASE-$
 DEFB FRC0, FDIV ; else divide
 DEFB FRC0, FONE, FADD, FST0, FDEL ; x = x + 1
 DEFB FDUP, FNOT, FNOT, FJPT ; if result <> 0 repeat
 DEFB DIVIDE-$
 DEFB FEND ; else terminate
 RET

CHECKCASE
 DEFB FONE, FRC0, FSUB, FGT0, FJPT ; if x < 1 jump to no integer case
 DEFB NOINT-$
 DEFB FEND ; else done
 RET

NOINT
 DEFB FNUL ; init Taylor sum = 0
 DEFB FEND

 LD HL, TSGR ; coef list
 LD B, 20

HORNER
 PUSH BC

 PUSH HL
 RST FPCALC
 DEFB FRC0, FMUL, FEND ; sum = sum * x
 POP HL

 LD A, (HL)
 INC HL
 LD E, (HL)
 INC HL
 LD D, (HL)
 INC HL
 LD C, (HL)
 INC HL
 LD B, (HL)
 INC HL

 PUSH HL
 CALL STKSTR
 RST FPCALC
 DEFB FADD, FEND ; sum = sum + coef
 POP HL

 POP BC
 DJNZ HORNER

 RST FPCALC
 DEFB FDIV, FEND ; final result

 RET

; Taylor Serie - Gamma Reciprocal
; N[CoefficientList[Series[1/Gamma[z], {z, 0, 19}], z], 24]
TSGR
 DEFB $5F, $65, $73, $B3, $AE ; (19)  1.04342671169110E-10
 DEFB $63, $A2, $5A, $67, $6E ; (18) -1.18127457048702E-09
 DEFB $65, $2B, $DE, $1F, $E2 ; (17)  5.00200764446922E-09
 DEFB $65, $52, $25, $BD, $D1 ; (16)  6.11609510448142E-09
 DEFB $6A, $DC, $CC, $33, $33 ; (15) -2.05633841697761E-07
 DEFB $6D, $18, $12, $84, $EE ; (14)  1.13302723198170E-06
 DEFB $6D, $A7, $D6, $A0, $FE ; (13) -1.25049348214267E-06
 DEFB $71, $A8, $E7, $45, $7A ; (12) -2.01348547807882E-05
 DEFB $74, $06, $45, $3C, $67 ; (11)  1.28050282388116E-04
 DEFB $74, $E1, $B2, $7F, $38 ; (10) -2.15241674114951E-04
 DEFB $77, $98, $B8, $89, $67 ; ( 9) -1.16516759185907E-03
 DEFB $79, $6C, $8C, $E2, $94 ; ( 8)  7.21894324666310E-03
 DEFB $7A, $9D, $A5, $79, $42 ; ( 7) -9.62197152787697E-03
 DEFB $7C, $AC, $D7, $88, $1E ; ( 6) -4.21977345555443E-02
 DEFB $7E, $2A, $89, $19, $06 ; ( 5)  1.66538611382291E-01
 DEFB $7C, $AC, $0A, $F4, $7D ; ( 4) -4.20026350340952E-02
 DEFB $80, $A7, $E7, $A0, $13 ; ( 3) -6.55878071520254E-01 (gamma^2 - pi^2/6)/2
 DEFB $80, $13, $C4, $67, $E3 ; ( 2)  0.577215664901532861 (Euler gamma constant, see ZX SUDOKU 2007)
 DEFB $00, $00, $01, $00, $00 ; ( 1)  1
 DEFB $00, $00, $00, $00, $00 ; ( 0)  0



; DEF FN Q(x) = USR 60205 : REM square root function

SQRT ; 3 x faster than ROM SQR and more accurate
 LD IX, (DEFADD)

 LD A, (IX+$02)
 LD E, (IX+$03)
 LD D, (IX+$04)
 LD C, (IX+$05)
 LD B, (IX+$06)
 CALL STKSTR

 POP BC

FPSQRT ; x pn calculator stack
 RST FPCALC ; x
 DEFB FRSK ; re-stack (if x integer convert to fp)
 DEFB FDUP, FGT0, FJPT ; if x > 0
 DEFB POSARG-$
 DEFB FDUP, FLT0, FJPT ; if x < 0
 DEFB NEGARG-$
 DEFB FEND ; x = 0
 RET

NEGARG ; x < 0
 DEFB FDEL, FEND ; delete and end
 RST 8 ; error restart
 DEFB 9 ; A Invalid argument

POSARG ; x > 0
 DEFB FST0, FEND ; MEM0 = x
 
 LD BC, $FF81 ; not mask and bias
 LD A, (HL) ; biased exponent
 SUB C ; true exponent
 JR C, LESSONE ; if exponent < 0 then x < 1
 INC A ; round
 RRA   ; divide exponent by 2
 JR ILOG2
LESSONE ; x < 1
 XOR B ; negate (with wound)
 RRA   ; divide exponent by 2
 NEG   ; negate

ILOG2 ; q = 2 ^ INT(LOG2(x) / 2), very near SQRT x
 ADD A, C ; re-bias exponent
 LD (HL), A
 XOR A ; clear mantissa
 INC HL
 LD (HL), A
 INC HL
 LD (HL), A
 INC HL
 LD (HL), A
 INC HL
 LD (HL), A

 LD B, 5 ; iterations
SQRTNEWTON
 LD A, B
 OR A
 RET Z
 PUSH BC

 RST FPCALC
 DEFB FDUP ; q, q
 DEFB FRC0 ; q, q, x
 DEFB FEXC ; q, x, q
 DEFB FDIV ; q, x/q
 DEFB FADD ; q = q + x/q
 DEFB FEND
 DEC (HL)  ; q = q/2

 POP BC
 DJNZ SQRTNEWTON

 RET

 

; DEF FN h(x, y) = USR 60290 : REM hypotenuse(x, y)

HYPOT
 LD IX, (DEFADD)

 LD A, (IX+$02)
 LD E, (IX+$03)
 LD D, (IX+$04)
 LD C, (IX+$05)
 LD B, (IX+$06)
 CALL STKSTR ; x

 LD A, (IX+$0A)
 LD E, (IX+$0B)
 LD D, (IX+$0C)
 LD C, (IX+$0D)
 LD B, (IX+$0E)
 CALL STKSTR ; y
 
 POP BC

FHYPOT ; complex ABS
 RST FPCALC      ; x, y
 DEFB FDUP, FNOT, FJPT ; if y = 0 then |x|
 DEFB HYPABS-$   ; x, 0
 DEFB FEXC       ; y, x
 DEFB FDUP, FNOT, FJPT ; if x = 0 then |y|
 DEFB HYPABS-$   ; y, 0
 DEFB FDUP, FMUL ; y, x^2
 DEFB FEXC       ; x^2, y
 DEFB FDUP, FMUL ; x^2, y^2
 DEFB FADD, FEND ; x^2 + y^2
 JP FPSQRT       ; sqrt(x^2 + y^2)
HYPABS
 DEFB FDEL, FABS, FEND ; delete 0, calculate ABS, end
 RET

Sqrt2       DEFB 129, 53, 4, 243, 52
GoldenRatio DEFB 129, 79, 27, 188, 221
Dodeca      DEFB 129, 7, 156, 124, 154 ; 2 ^ 1/12

; DEF FN Q(x) = USR 60205 : REM square root function
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

 org 60205

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
 RST FPCALC
 DEFB FRSK, FST0, FEND

 CALL $34E9 ; test zero
 RET C ; x = 0
 
 INC HL
 LD A, (HL)
 LD C, $80 ; sign bit mask
 AND C
 JR Z, POSARG
 RST 8 ; x < 0
 DEFB 9 ; A Invalid argument

POSARG
 DEC HL
 LD A, (HL) ; biased exponent
 SUB C      ; true exponent
 RRA        ; divide exponent by 2
 INC A      ; round-up
 ADD A, C   ; re-bias exponent
 LD (HL), A

; XOR A ; clear mantissa
; INC HL
; LD (HL), A
; INC HL
; LD (HL), A
; INC HL
; LD (HL), A
; INC HL
; LD (HL), A

 LD B, 5 ; iterations
SQRTNEWTON

 RST FPCALC ; preserves B register
 DEFB FDUP ; q, q
 DEFB FRC0 ; q, q, x
 DEFB FEXC ; q, x, q
 DEFB FDIV ; q, x/q
 DEFB FADD ; q = q + x/q
 DEFB FEND
 DEC (HL)  ; q = q/2

 DJNZ SQRTNEWTON

 RET


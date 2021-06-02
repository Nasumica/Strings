  org 50000


TEMPS  EQU $0D4D
CHANOP EQU $1601
PLOTBC EQU $22E9
STKSTR EQU $2AB6
STACKA EQU $2D28
FPTOA  EQU $2DD5
HLxDE  EQU $30A9
DEFADD EQU 23563
COORDS EQU 23677

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

initialization

  call im2init

  ld a, 0
  call setup_lang

  ld a, 1
  call copy_instr

  call vect_solid

  call init_row_adr

  ld a, (total_frets)
  call calc_fretx

  ld d, 2
  ld e, 0
  ld a, (flags)
  and a
  jr z, srb_coa
  ld hl, eng_ico
  jr print_coa
srb_coa
  ld hl, srb_ico
print_coa
  call print_ico

  call plot_fifths

  ld de, 0
  ld (atpos), de
  ld b, 16
  ld hl, instr
  ld ix, flags
  bit 0, (ix+0)
  jr z, print_instr_name
  ld a, 16
  call add_hla
print_instr_name
  call print_str

  call calc_hvat
  call draw_neck
  call repaint_fifths
  ld b, 1 ; once
  call play_chord

  jr done

  ld bc, 500
  call $1F3D ; pause 0
  call $028E ; key-scan
  ld a, d
  cp -1
  jr nz, done
  ld a, e
  cp -1
  jr nz, done



cont

done
;  LD      BC,$0000        
;  CALL    $1F05 ; used mem
;  LD      B,H             
;  LD      C,L

;  call im1init

  ld bc, midi

  ret

include "d:\zx\dev\graph.asm"

add_hla
  add a, l
  ld l, a
  ret nc
  inc h
  ret

draw_neck
;  ld de, $0700
;  ld (atpos), de
  ld a, (wires)
  ld d, a
  ld b, a
  ld a, 8
  sub b
  jr z, draw_neck_go
  ld b, a
  ld a, 8
  add a, d
  ld d, a
draw_neck_clear
  push bc
  push de
  call empty_row_attr
  pop de
  inc d
  pop bc
  djnz draw_neck_clear
draw_neck_go
  ld a, (wires)
  dec a
  and a
  rla
  rla
  rla
  ld d, 64+4
  ld e, 17
  add a, d
  ld d, a
  ld b, 0
  ld ix, hvat
  ld a, 48
  ld (attribute), a
draw_neck_loop
  push bc
  ld a, (wires)
  sub b
  add a, 7
  ld (aty), a
  xor a
  ld (atx), a
  ld a, (ix+16)
  call print_note_name
  ld b, 30
  ld a, " "
  call print_repl
  pop bc
  push bc
  call draw_frets
  pop bc
  ld hl, capo
  ld a, b
  call add_hla
  ld a, (hl)
  ld hl, fretx
  call add_hla
  ld a, (hl)
  sub e
  push bc
  ld b, a
  ld a, (ix+0) ; hvat
  cp -1
  jr z, draw_neck_dash
  call finger_down
  call hline
  jr draw_neck_full
draw_neck_dash
  call dline
draw_neck_full  
  pop bc
  ld a, d
  sub 8
  ld d, a
  inc b
  ld a, (wires)
  sub b
  inc ix
  jr nz, draw_neck_loop

  ld a, 56
  ld (attribute), a

  ld d, 7
  call empty_row

  ld a, 3
  call draw_layout
  ld a, 5
  call draw_layout
  ld a, 7
  call draw_layout
  ld a, 9
  call draw_layout
  ld a, 12
  call draw_dlayout
  ld a, 15
  call draw_layout
  ld a, 17
  call draw_layout
  ld a, 19
  call draw_layout
  ld a, 21
  call draw_layout
  ld a, 24
  call draw_dlayout
  ret

finger_down ; d = y, a = fret
  and a
  ret z
  dec a
  push hl
  push de
  push bc
  ld hl, fingerx
  call add_hla
  ld a, (hl)
  ld e, a
  dec e
  dec d
  dec d

  ld b, 3
  call hline
  inc d

  ld b, 3
  call hline
  inc d

  inc d
  ld b, 3
  call hline

  inc d
  ld b, 3
  call hline

  pop bc
  pop de
  pop hl
  ret

draw_layout ; a - fret
  ld hl, total_frets
  dec a
  cp (hl)
  ret nc
  ld hl, fingerx
  call add_hla
  ld c, (hl)
  ld b, 62
  ld a, 1
  jp circleabc


draw_dlayout ; a - fret
  dec a
  ld hl, total_frets
  cp (hl)
  ret nc
  ld hl, fingerx
  call add_hla
  ld c, (hl)
  ld b, 62
  push bc
  dec c
  ld a, 1
  call circleabc
  pop bc
  inc c
  ld a, 1
  jp circleabc

draw_frets ; b = wire, d = y
  ld hl, capo
  ld a, b
  call add_hla
  ld a, (hl)
  ld c, a
  ld a, (total_frets)
  push af
  sub c
  ld b, a
  pop af
  ld hl, fretx
  call add_hla
  inc hl
  inc b
  push de
  ld a, d
  and $f8
  ld d, a
draw_frets_loop
  push bc
  dec hl
  push hl
  ld a, (hl)
  ld e, a
  call vpole
  pop hl
  pop bc
  djnz draw_frets_loop
  ld a, (hl)
  inc a
  ld e, a
  call vpole
  pop de
  ret



calc_fretx
;  xor a
;  ld a, neck_lengt
  defb 62
neck_length
  defb 235
  call STACKA

  ld a, (total_frets) ; total frets
  call stk_ma_dodeca

  rst fpcalc
  defb fdiv, fst0, fdel, fend ; mem[0]  = neck / (1 - 2^(-frets/12))

  ld a, (total_frets)
  ld hl, fretx
  push af
  call add_hla
  pop af

calc_fretx_loop
  push af
  push hl

  call stk_ma_dodeca
  rst fpcalc
  defb frc0, fmul, fend ; mem[0] * (1 - 2^(-a/12))

  call $2314 ; stk to a
  inc a

  cpl ; reverse

  pop hl
  ld (hl), a
  dec hl

  pop af
  or a
  jr z, calc_fingerx
  dec a
  jr calc_fretx_loop

calc_fingerx
  ld a, (total_frets)
  ld b, a
  ld hl, fretx
  ld a, (hl)
  ld de, fingerx
calc_fingerx_loop
  inc hl
  ld c, (hl)
  add a, c
  rr a ; midpoint
  jr nc, $+3
  inc a ; round up
  ld (de), a
  ld a, c
  inc de
  djnz calc_fingerx_loop
  ret


find_accord
;  ld a, 40
;  ld (attribute), a
  ld hl, accords

find_loop
  ld c, (hl)
  inc hl
  ld b, (hl)
  inc hl
  ld a, b
  or c
  jr z, find_finish

  xor a
tone_loop
  push hl
  ld l, c
  ld h, b
  or a
  sbc hl, de
  pop hl
  push af
  call z, print_accord
  call dodeca_rot
  pop af
  inc a
  cp 12
  jr nz, tone_loop

  ld a, accordnl
  call add_hla
  jr find_loop

find_finish
  ld a, (aty)
  cp 1
  jr z, find_done
  ld a, 14
  ld (atx), a
  ld a, " "
  ld b, accordnl+2
  call print_repl
  ld a, (aty)
  dec a
  ld (aty), a
  jr find_finish
find_done
  ld a, 56
  ld (attribute), a
  ret


print_accord
  push hl
  push af
  ld a, 14
  ld (atx), a
  pop af
  call print_note_name
;  ld a, " "
;  call print_a
  pop hl

  push hl
  push bc
  ld b, accordnl
  call print_str
  ld a, (aty)
  dec a
  ld (aty), a
  pop bc
  pop hl
  ret

print_note_name ; notename(a), 0-11
  push hl
  cp 12
  jr nc, print_note_none
  ld hl, flags
  bit 0, (hl)
  jr z, srb_note
  ld hl, eng_note_names
  jr print_nn_cont
srb_note
  ld hl, srb_note_names
print_nn_cont
  add a, a
  call add_hla
  ld a, (hl)
  call print_a
  inc hl
  ld a, (hl)
  jr print_note_done
print_note_none
  ld a, " "
  call print_a
print_note_done
  call print_a
  pop hl
  ret

dodeca_rot ; 12 bit rotate DE
  ld a, d
  and $f
  rr a
  rr e
  jr nc, $+4
  or $8
  ld d, a
  ld a, e
  rr a
  ret


dodeca ; a = a mod 12
  ld c, a
  ld d, 12

c_div_d ; c = c div d; a = c mod d
  ld b, 8
  xor a
  sla c
  rla
  cp d
  jr c, $+4
  inc c
  sub d
  djnz $-8
  ret
  

print_fingerpos
  ld a, 5
  ld (aty), a
  xor a
  ld (atx), a
  ld hl, hvat
  ld a, (wires)
  ld b, a
  call add_hla
print_fp_loop
  dec hl
  ld a, (hl)
  cp -1
  jr z, print_fp_x
  ld c, " "
  cp 10
  jr c, print_fp_cont
  sub 10
  ld c, 224
  cp 10
  jr c, print_fp_cont
  sub 10
  inc c
  jr print_fp_cont
print_fp_x
  ld a, 40
  ld c, 32
print_fp_cont
  and a
  jr nz, print_fp_final
  add a, 31
print_fp_final
  add a, "0"
  call print_a
  ld a, c
  call print_under
  djnz print_fp_loop
  ld a, (wires)
  ld b, a
  ld a, 8
  sub b
  ret z
  ld b, a
  ld a, " "
print_fp_fill
  call print_a
  call print_under
  djnz print_fp_fill
print_fp_done
  ret


calc_hvat
  ld a, 5
  ld (aty), a
  ld hl, pattern
  ld b, 12
  xor a
clr_pat_loop
  ld (hl), a
  inc hl
  djnz clr_pat_loop

  ld hl, tuning
  ld de, hvat
  ld ix, midi

  ld b, 8
calc_pat_loop
  ld a, (hl)
  and a
  jr z, calc_pat_more ; no more wires

  ld c, a ; tuning
  ld a, (de) ; hvat
  ld (ix+0), a
  ld (ix+8), a
  cp max_frets
  jr nc, calc_pat_next ; 

  cp (ix-16) ; compare with capo
  jr z, capo_finger
  jr c, capo_finger
  jr calc_finger
capo_finger
  xor a
  ld (de), a
  ld a, (ix-16) ; else capo

calc_finger
  add a, c ; a = fret + tuning
  ld c, (ix-16)
  sub c
  ld (ix+0), a

  push de
  push hl

  push bc
  call dodeca ; mod 12
  pop bc

  ld (ix+8), a

  ld hl, pattern
  call add_hla

  ld a, 1
  ld (hl), a

  pop hl
  pop de
calc_pat_next
  inc hl
  inc de
  inc ix
  djnz calc_pat_loop

calc_pat_more
  ld a, 8
  sub b
  ld (wires), a

  ld de, 0
  ld b, 12
  ld hl, pattern+12
calc_pat_bin
  dec hl
  ld a, (hl)
  rr a
  rl e
  rl d
  djnz calc_pat_bin
  ld (pat_code), de

  call find_accord

  call print_fingerpos

  ld b, 1
  ;call play_chord ; play once

  ld bc, hvat

  ret


play_chord
  ld c, b
  ld a, (wires)
  ld b, a
  ld hl, midi
  call add_hla
  ld de, 22*1024+256
play_chord_loop
  push bc
  dec hl
  push hl

  ld a, (hl)
  cp $ff ; X - no play
  jr z, play_pause

  ex af, af'
;  halt

;  ld a, 48 + 2
  defb 62
attr_play
  defb 50
  ld c, 32
  call fill_mem

  push de

  ld a, $7D ; 1/25 sec 7C
  ld de, $D723
  ld bc, $3D0A
  call $2AB6 ; stk-str

  ex af, af'

  ld de, 60 ; midi - 5 octaves
  sub e
  jr nc, $+3
  dec d
  ld c, d
  ld e, d
  ld d, a
  xor a
  ld b, a
  call $2AB6 ; stack-str

  call $03F8 ; ROM beep

  pop de

;  ld a, 48
  defb 62
attr_mute
  defb 48
  ld c, 32
  call fill_mem

  jr play_once_next

play_pause
;  halt

play_once_next
  ld a, 32
  add a, e
  ld e, a
  jr nc, $+3
  inc d
  pop hl
  pop bc
  djnz play_chord_loop

play_once_done
  ld b, c
  djnz play_chord

  ret


fill_mem ; de, c, a
  push de
  ld b, c
fill_mem_loop
  ld (de), a
  inc de
  djnz fill_mem_loop
  pop de
  ret

swap_mem ; hl, de, b
  ld a, (de)
  ld c, a
  ld a, (hl)
  ld (de), a
  ld (hl), c
  inc hl
  inc de
  djnz swap_mem
  ret


dehl_div_10 ; dehl = dehl div 10; a = dehl mod 10
  ld c, 10
dehl_div_c ; dehl = dehl div c; a = dehl mod c
  ld b, 32  
  xor a
  add hl, hl
  rl e
  rl d
  rla
  cp c
  jr c, $+4
  inc l
  sub c
  djnz $-11
  ret


stk_a_dodeca ; put 2^(a/12) on calc stack
  call dodeca
  add a, a
  add a, a
  ld hl, mantisses
  add a, l
  ld l, a
  jr nc, $+3
  inc h
  ld a, $81
  add a, c
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  ld c, (hl)
  inc hl
  ld b, (hl)
  call $2AB6 ; stk-store
  ret

stk_ma_dodeca ; put 1 - 2^(-a/12) on stack
  call dodeca
  and a
  jr z, stk_ma_octave
  inc c
  ld b, a
  ld a, 12
  sub b
stk_ma_octave
  add a, a
  add a, a
  ld hl, mantisses
  add a, l
  ld l, a
  jr nc, $+3
  inc h
  ld a, $81
  sub c
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  ld c, (hl)
  inc hl
  ld b, (hl)
  call $2AB6 ; stk-store
  rst fpcalc
  defb fone, fexc, fsub, fend
  ret

copy_instr ; a = instr
  and a
  ret z
  ld hl, total_instr
  dec a
  cp (hl)
  ret nc
  ld l, a
  inc a
  ld (cur_instr), a
  ld c, instruments_mark-instruments
  ld e, c
  xor a
  ld b, a
  ld d, a
  ld h, a
  call hlxde
  ex de, hl
  ld hl, instruments
  add hl, de
  ld de, wires
  ldir
  ret

plot_fifths
  ld de, %010101001010
  ld hl, ffcircle
  ld b, 12
plot_ff_loop
  push bc
  ld c, (hl)
  inc hl
  ld b, (hl)
  inc hl
  push hl
  push de
  ld a, e
  and 1
  call mode_flag
;  call z, mode_erase
;  call nz, mode_plot
  xor a
  call circleabc
  inc a
  call circlea
  call mode_plot
  inc a
  call circlea
  pop de
  pop hl
  call dodeca_rot
  pop bc
  djnz plot_ff_loop
  ret

repaint_fifths
  call mode_erase
  ld de, (old_code)
  call draw_fifths
  ld de, (target_old)
  call mode_plot
  ld de, (target_new)
  ld (target_old), de
  call vect_doted
  call draw_fifths
  call vect_solid
  ld de, (pat_code)
  ld (old_code), de

draw_fifths ; de = code
  ld a, d
  or e
  ret z
  call poly_begin
  ld hl, fifthsxy
  ld b, 12
draw_ff_loop
  push de
  ld c, e
  ld a, (hl)
  ld e, a
  inc hl
  ld a, (hl)
  ld d, a
  inc hl
  ld a, c
  and 1
  call nz, poly_point
  pop de
  call dodeca_rot
  djnz draw_ff_loop
  call poly_close
  ret

is_de_1
  ld a, d
  cp 0
  ret nz
  ld a, e
  cp 1
  ret

setup_lang ; a
  ld (flags), a
  cp 0
  jr z, setup_lang_cyr
  cp 1
  jr z, setup_lang_eng
  ret

setup_lang_eng
  ld de, dur_name
  ld hl, eng_dur
  ld bc, accordnl
  ldir
  ld de, mol_name
  ld hl, eng_mol
  ld bc, accordnl
  ldir
  ld hl, accords
setup_lang_eng_iter
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  ld a, e
  or d
  jr z, setup_lang_done
  call is_de_1
  jr z, setup_lang_done
  ld b, accordnl
setup_lang_eng_loop
  ld a, (hl)
  cp 128+32
  jr c, setup_lang_eng_cont  
  sub 96
setup_lang_eng_cont
  ld (hl), a
  inc hl
  djnz setup_lang_eng_loop
  jr setup_lang_eng_iter

setup_lang_cyr
  ld de, dur_name
  ld hl, srb_dur
  ld bc, accordnl
  ldir
  ld de, mol_name
  ld hl, srb_mol
  ld bc, accordnl
  ldir
  ld hl, accords
setup_lang_cyr_iter
  ld e, (hl)
  inc hl
  ld d, (hl)
  inc hl
  ld a, e
  or d
  jr z, setup_lang_done
  call is_de_1
  jr z, setup_lang_done
  ld b, accordnl
setup_lang_cyr_loop
  ld a, (hl)
  cp 128+32
  jr nc, setup_lang_cyr_cont
  cp 64
  jr c, setup_lang_cyr_cont
  add a, 96
setup_lang_cyr_cont
  ld (hl), a
  inc hl
  djnz setup_lang_cyr_loop
  jr setup_lang_cyr_iter

setup_lang_done
  ret


ffz  equ 0
ffh  equ 11
ffq  equ 19 ; 11 * SQR 3
fff  equ 22
ffxc equ 228
ffyc equ 28

fifthsxy
  defb ffxc+ffz, ffyc-fff
  defb ffxc+ffh, ffyc-ffq
  defb ffxc+ffq, ffyc-ffh
  defb ffxc+fff, ffyc-ffz
  defb ffxc+ffq, ffyc+ffh
  defb ffxc+ffh, ffyc+ffq
  defb ffxc-ffz, ffyc+fff
  defb ffxc-ffh, ffyc+ffq
  defb ffxc-ffq, ffyc+ffh
  defb ffxc-fff, ffyc+ffz
  defb ffxc-ffq, ffyc-ffh
  defb ffxc-ffh, ffyc-ffq

  
ggz  equ 0
ggh  equ 13
ggq  equ 22 ; 11 * SQR 3
ggf  equ 25
ggxc equ 228
ggyc equ 28

ffcircle
  defb ggxc+ggz, ggyc-ggf
  defb ggxc+ggh, ggyc-ggq
  defb ggxc+ggq, ggyc-ggh
  defb ggxc+ggf, ggyc-ggz
  defb ggxc+ggq, ggyc+ggh
  defb ggxc+ggh, ggyc+ggq
  defb ggxc-ggz, ggyc+ggf
  defb ggxc-ggh, ggyc+ggq
  defb ggxc-ggq, ggyc+ggh
  defb ggxc-ggf, ggyc+ggz
  defb ggxc-ggq, ggyc-ggh
  defb ggxc-ggh, ggyc-ggq

mantisses ; can be calculated from semi-tone table ($046E)
  defb $00, $00, $00, $00 ; 2^( 0/12) = 1.0000000000
  defb $07, $9C, $7C, $97 ; 2^( 1/12) = 1.0594630944
  defb $0F, $AC, $D6, $1E ; 2^( 2/12) = 1.1224620483
  defb $18, $37, $F0, $52 ; 2^( 3/12) = 1.1892071150
  defb $21, $45, $17, $CC ; 2^( 4/12) = 1.2599210499
  defb $2A, $DC, $08, $48 ; 2^( 5/12) = 1.3348398542
sqrt2
  defb $35, $04, $F3, $34 ; 2^( 6/12) = 1.4142135624
  defb $3F, $C8, $86, $BB ; 2^( 7/12) = 1.4983070769
  defb $4B, $2F, $F5, $2A ; 2^( 8/12) = 1.5874010520
  defb $57, $44, $FC, $CB ; 2^( 9/12) = 1.6817928305
  defb $64, $11, $F0, $3A ; 2^(10/12) = 1.7817974363
  defb $71, $A1, $BF, $39 ; 2^(11/12) = 1.8877486254
sqrt3
  defb $5D, $B3, $D7, $43

max_frets equ 25
fretx
  defs max_frets+1
fingerx
  defs max_frets

srb_note_names
  defm "C "
  defm "C","#"+96
  defm "D "
  defm "D","#"+96
  defm "E "
  defm "F "
  defm "F","#"+96
  defm "G "
  defm "G","#"+96
  defm "A "
  defm "B "
  defm "H "

eng_note_names
  defm "C "
  defm "C","#"+96
  defm "D "
  defm "D","#"+96
  defm "E "
  defm "F "
  defm "F","#"+96
  defm "G "
  defm "G","#"+96
  defm "A "
  defm "A","#"+96
  defm "B "


srb_dur defm "dur     " 
srb_mol defm "mol     " 
eng_dur defm "major   " 
eng_mol defm "minor   " 

accordnl equ 8
accords

  defw %000010010001 ; 1 3 5 
dur_name
  defm "dur     " ; ( 1)

  defw %000010001001 ; 1 3- 5 
mol_name
  defm "mol     " ; ( 7)

  defw %010010010001 ; 1 3 5 7- 
  defm "7       " ; (15)

  defw %001001001001 ; 1 3- 5- 6 
  defm "dim     " ; (14)

  defw %010010001001 ; 1 3- 5 7- 
  defm "m7      " ; (30)

  defw %000010000101 ; 1 2 5 
  defm "sus2    " ; ( 2)

  defw %000010100001 ; 1 4 5 
  defm "sus4    " ; ( 3)

  defw %000001010001 ; 1 3 5- 
  defm "5-      " ; ( 4)

  defw %000100010001 ; 1 3 5+ 
  defm "5+      " ; ( 5)

  defw %000010010101 ; 1 3 5 9 
  defm "+9      " ; ( 6)

  defw %000010001101 ; 1 3- 5 9 
  defm "m+9     " ; ( 8)

  defw %000001001001 ; 1 3- 5- 
  defm "m5-     " ; ( 9)

  defw %001010010001 ; 1 3 5 6 
  defm "6       " ; (10)

  defw %001010010101 ; 1 3 5 6 9 
  defm "6/9     " ; (11)

  defw %001010001001 ; 1 3- 5 6 
  defm "m6      " ; (12)

  defw %001010001101 ; 1 3- 5 6 9 
  defm "m6/9    " ; (13)

  defw %010010100001 ; 1 4 5 7- 
  defm "7sus4   " ; (16)

  defw %010100010001 ; 1 3 5+ 7- 
  defm "7/5+    " ; (17)

  defw %010001010001 ; 1 3 5- 7- 
  defm "7/5-    " ; (18)

  defw %010010011001 ; 1 3 5 7- 9+ 
  defm "7/9+    " ; (19)

  defw %010010010011 ; 1 3 5 7- 9- 
  defm "7/9-    " ; (20)

  defw %010100011001 ; 1 3 5+ 7- 9+ 
  defm "7/9+/5+ " ; (21)

  defw %010100010011 ; 1 3 5+ 7- 9- 
  defm "7/9-/5+ " ; (22)

  defw %010001010011 ; 1 3 5- 7- 9- 
  defm "7/9-/5- " ; (23)

  defw %010010110001 ; 1 3 5 7- 11 
  defm "7/11    " ; (24)

  defw %010011010001 ; 1 3 5 7- 11+ 
  defm "7/11+   " ; (25)

  defw %100010010001 ; 1 3 5 7 
  defm "maj7    " ; (26)

  defw %100001010001 ; 1 3 5- 7 
  defm "maj7/5- " ; (27)

  defw %100100010001 ; 1 3 5+ 7 
  defm "maj7/5+ " ; (28)

  defw %100011010001 ; 1 3 5 7 11+ 
  defm "maj7/11+" ; (29)

  defw %010001001001 ; 1 3- 5- 7- 
  defm "m7/5-   " ; (31)

  defw %010010001011 ; 1 3- 5 7- 9- 
  defm "m7/9-   " ; (32)

  defw %010010101001 ; 1 3- 5 7- 11 
  defm "m7/11   " ; (33)

  defw %100010001001 ; 1 3- 5 7 
  defm "m/maj7  " ; (34)

  defw %010010010101 ; 1 3 5 7- 9 
  defm "9       " ; (35)

  defw %010100010101 ; 1 3 5+ 7- 9 
  defm "9/5+    " ; (36)

  defw %010001010101 ; 1 3 5- 7- 9 
  defm "9/5-    " ; (37)

  defw %010011010101 ; 1 3 5 7- 9 11+ 
  defm "9/11+   " ; (38)

  defw %100010010101 ; 1 3 5 7 9 
  defm "maj9    " ; (39)

  defw %100100010101 ; 1 3 5+ 7 9 
  defm "maj9/5+ " ; (40)

  defw %100011010101 ; 1 3 5 7 9 11+ 
  defm "maj9/11+" ; (41)

  defw %010010001101 ; 1 3- 5 7- 9 
  defm "m9      " ; (42)

  defw %010001001101 ; 1 3- 5- 7- 9 
  defm "m9/5-   " ; (43)

  defw %100010001101 ; 1 3- 5 7 9 
  defm "m9/maj7 " ; (44)

  defw %010010110101 ; 1 3 5 7- 9 11 
  defm "11      " ; (45)

  defw %010010110011 ; 1 3 5 7- 9- 11 
  defm "11/9-   " ; (46)

  defw %100010110101 ; 1 3 5 7 9 11 
  defm "maj11   " ; (47)

  defw %010010101101 ; 1 3- 5 7- 9 11 
  defm "m11     " ; (48)

  defw %011010010101 ; 1 3 5 7- 9 13 
  defm "13      " ; (49)

  defw %011010011001 ; 1 3 5 7- 9+ 13 
  defm "13/9+   " ; (50)

  defw %011010010011 ; 1 3 5 7- 9- 13 
  defm "13/9-   " ; (51)

  defw %011001010011 ; 1 3 5- 7- 9- 13 
  defm "13/9-/5-" ; (52)

  defw %101010010101 ; 1 3 5 7 9 13 
  defm "maj13   " ; (53)

  defw %011010001101 ; 1 3- 5 7- 9 13 
  defm "m13     " ; (54)

  defw %010000010001 ; 1 3 7- 
  defm "7-5     " ; (55)

  defw %010000001001 ; 1 3- 7- 
  defm "m7-5    " ; (56)


  defw %000000000001 ; 1
  defm "prima   " 
  defw %000000000101 ; 1 2
  defm "secunda " 
  defw %000000010001 ; 1 3
  defm "terca   " 
  defw %000000100001 ; 1 4
  defm "quarta  " 
  defw %000001000001 ; 1 5
  defm "quarta+ "
  defw %000010000001 ; 1 5
  defm "quinta  " 
  defw %001000000001 ; 1 6
  defm "sexta   " 
  defw %100000000001 ; 1 7
  defm "septima " 

  defw 0 ; eof

flags
  defb %00000000

target_new defw 0
target_old defw 0


wires
  defb 6
total_frets
  defb 18
instr
  defm "G"+96,"i"+96,"t"+96,"a"+96,"r"+96,"a"+96, "          "
  defm "Guitar          "
tuning
  defb 64, 59, 55, 50, 45, 40,  0,  0
capo
  defb  0,  0,  0,  0,  0,  0,  0,  0
hvat
  defb  1,  3,  3,  3,  1,  1,  0,  0
midi
  defb 0, 0, 0, 0, 0, 0, 0, 0
mods
  defb 0, 0, 0, 0, 0, 0, 0, 0
pattern
  defb 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
pat_code
  defw 0
old_code
  defw 0

cur_instr
  defb 1
total_instr 
  defb 13 ; calc by init
instruments ; wires[1] + srb[16] + eng[16] + tuning[8] + capo[8] + hvat[8] = 57 bytes
  defb 6, 18
  defm "G"+96,"i"+96,"t"+96,"a"+96,"r"+96,"a"+96, "          "
  defm "Guitar          "
  defb 64, 59, 55, 50, 45, 40,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
instruments_mark
  defb 4, 24
  defb "B"+96,"a"+96,"s"+96," ","g"+96,"i"+96,"t"+96,"a"+96,"r"+96,"a"+96, "      "
  defm "Bass            "
  defb 43, 38, 33, 28,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  1,  2,  3,  4,  0,  0,  0,  0

  defb 4, 18
  defb "P"+96,"r"+96,"i"+96,"m"+96,"            "
  defm "Prim            "
  defb 74, 69, 64, 59,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  2,  1,  3,  2,  0,  0,  0,  0

  defb 4, 18
  defb "B"+96,"i"+96,"s"+96,"e"+96,"r"+96,"n"+96,"i"+96,"c"+96,"a"+96, "       "
  defm "Bisernica       "
  defb 76, 71, 66, 61,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  0,  2,  3,  0,  0,  0,  0,  0

  defb 3, 20
  defm "B"+96,"a"+96,"l"+96,"a"+96,"l"+96,"a"+96,"j"+96,"k"+96,"a"+96,"       "
  defm "Balalaika       "
  defb 69, 64, 64,  0,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  3,  3,  0,  0,  0,  0,  0,  0

  defb 5, 20
  defm "B"+96,"e"+96,"n"+96,"x"+96,"o"+96,"           "
  defm "Banjo           "
  defb 62, 59, 55, 50, 67,  0,  0,  0
  defb  0,  0,  0,  0,  5,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0

  defb 4, 25
  defb "B"+96,"u"+96,"z"+96,"u"+96,"k"+96,"i"+96, "          "
  defm "Bouzouki        "
  defb 62, 57, 53, 48,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  1,  0,  0,  0,  0,  0,  0,  0

  defb 3, 25
  defb "B"+96,"u"+96,"z"+96,"u"+96,"k"+96,"i"+96, " ", "t"+96,"r"+96,"i"+96,"k"+96,"o"+96,"r"+96,"d"+96, "  "
  defm "Bouzouki 3-cord "
  defb 62, 57, 50,  0,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  3,  0,  0,  0,  0,  0,  0,  0

  defb 4, 15
  defb "U"+96,"k"+96,"u"+96,"l"+96,"e"+96,"l"+96,"e"+96, "         "
  defm "Ukulele         "
  defb 69, 64, 60, 55,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  3,  0,  0,  0,  0,  0,  0,  0

  defb 4, 20
  defb "M"+96,"a"+96,"n"+96,"d"+96,"o"+96,"l"+96,"i"+96,"n"+96,"a"+96, "       "
  defm "Mandolin        "
  defb 76, 69, 62, 55,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  2,  3,  0,  1,  0,  0,  0,  0

  defb 4, 20
  defb "M"+96,"a"+96,"n"+96,"d"+96,"o"+96,"~"+96,"e"+96,"l"+96,"o"+96, "       "
  defm "Mandochello     "
  defb 57, 50, 43, 36,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0

  defb 8, 18
  defm "G"+96,"i"+96,"t"+96,"a"+96,"r"+96,"a"+96," 8        "
  defm "Guitar 8        "
  defb 64, 59, 55, 50, 45, 40, 35, 30
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  0,  0,  1,  0,  2,  0,  0,  2

  defb 2, 18
  defm "]"+96,"i"+96,"f"+96,"t"+96,"e"+96,"l"+96,"i"+96,"j"+96,"a"+96,"       "
  defm "Qifteli         "
  defb 52, 59,  0,  0,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0
  defb  0,  0,  0,  0,  0,  0,  0,  0

  defs 0 ; eof




  ld hl, 23658
  res 3, (hl) ; caps-lock

  ld bc, 0
  call $1F3D ; pause 0
  call $028E ; key-scan


MYINT
;  ld hl, (23294)
;  inc hl
;  ld (23294), hl
  ret


  org $feff ; IM 2 address
IM2

  ex af, af'
  push af
  ex af, af'
  push af

  exx
  push bc
  push de
  push hl

  exx
  push bc
  push de
  push hl

  push ix
  push iy

  call myint

  pop iy
  pop ix

  pop hl
  pop de
  pop bc
  exx

  pop hl
  pop de
  pop bc
  exx

  pop af
  ex af, af'
  pop af
  ex af, af'

  jp $0038

IM2init
  di
  ld a, $fe
  ld i, a
  im 2
  ei
  halt
  ret

IM1init
  di
  im 1
  ei
  halt
  ret
  
  


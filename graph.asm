
clear_screen
  ld (attribute), a
  ld hl, $5800
  ld d, h
  ld e, l
  inc de
  ld a, 63
  ld bc, 768
  dec bc
  halt
  ld (hl), a
  ldir
  
  ld hl, $4000
  ld d, h
  ld e, l
  inc de
  ld bc, $1800
  dec bc
  xor a
  ld (hl), a
  ldir
  
  ld hl, $5800
  ld d, h
  ld e, l
  inc de
  ld a, (attribute)
  ld bc, 768
  dec bc
  halt
  ld (hl), a
  ldir

  ld de, 0
  ld (atpos), de
  ld (polyxy), de
  ld (lastxy), de
  
  ret

empty_row_attr
  ld a, (attribute)
empty_row ; d = y, a = attr
  ex af, af'
  ld a, d
empty_row_reduce
  cp 24
  jr c, empty_row_cont
  sub 24
  jr empty_row_reduce
empty_row_cont
  push de
  add a, a
  ld hl, row_adr
  add a, l
  ld l, a
  jr nc, $+3
  inc h
  ld a, (hl)
  inc hl
  ld h, (hl)
  ld l, a
  ld b, 8
empty_row_loop
  push bc
  push hl 
  ld d, h
  ld e, l
  inc de
  xor a
  ld (hl), a
  ld bc, 31
  ldir
  pop hl
  inc h
  pop bc
  djnz empty_row_loop
  
  pop de
  ld e, 0
  and a
  rr d
  rr e
  rr d
  rr e
  rr d
  rr e
  ld hl, 22528
  add hl, de
  ex af, af'
  
  ld (hl), a
  ld d, h
  ld e, l
  inc de
  ld bc, 31
  ldir
  
  ret
  
get_prn_at ; bc not used
  ld de, (atpos)
get_prn_adr ; d=y, e=x
  ld a, d
  add a, a
  ld hl, row_adr
  add a, l
  ld l, a
  jr nc, $+3
  inc h
  ld a, (hl)
  inc hl
  ld h, (hl)
  ld l, a
  ld a, e
  add a, l
  ld l, a
  jr nc, $+3
  inc h
  ret
  
get_attr_at ; bc not used
  ld de, (atpos)
get_attr_adr ; d = y, e = x; d*32 + e + 22528
  ld a, e
  ld e, 0
  and %00011111
  rr d
  rr e
  rr d
  rr e
  rr d
  rr e ; de = d * 32
  or e
  ld e, a
  ld hl, 22528
  add hl, de
  ret
    

print_at ; at de
  ld (atpos), de

print_a
  ld (lastchar), a
  cp 13
  jp z, print_cr
  cp 10
  jp z, print_lf
  
  push hl
  push bc
  push de

  ld c, a ; save a
  call get_attr_at
  ld a, (attribute)
  ld (hl), a
  
  call get_prn_at
  ld a, c
  and %01111111
  ld d, 0
  ld e, a
  rl e
  rl d
  rl e
  rl d
  rl e
  rl d

  push hl
  ld a, c
  and %10000000
  jr z, print_a_rom
  ld hl, font ; RAM font
  jr print_a_cont
print_a_rom
  ld hl, $3C00 ; ROM font
  ;ld hl, ($5C36)

print_a_cont
  add hl, de
  ex de, hl
  pop hl
  ld b, 8
print_a_loop
  ld a, (de)
  ld (hl), a
  inc de
  inc h
  djnz print_a_loop
  call adv_at
  ld a, c
  pop de
  pop bc
  pop hl
  ret
  
  
print_under
  ld c, a
  ld de, (atpos)
  ld a, e
  dec a
  ld (atx), a
  ld a, d
  inc a
  ld (aty), a
  ld a, c
  call print_a
  ld (atpos), de
  ret
  

print_ico_at
  ld de, (atpos)
print_ico ; de = at, hl = shape
  push de ; save for attr
  ld b, 2
print_ico_loop
  ld c, b
  push de ; save pos
  push hl ; save shape
  call get_prn_adr ; hl = screen adr
  ex de, hl ; de = adr
  pop hl ; restore shape

  ld b, 8 ; 8 bytes
print_ico_scanline
  ld a, (hl) ; right byte
  inc e
  ld (de), a
  inc hl
  ld a, (hl) ; left byte
  dec e
  ld (de), a
  inc hl
  inc d ; next scanline
  djnz print_ico_scanline

  pop de ; restore pos
  inc d ; next row
  ld b, c
  djnz print_ico_loop
  
  pop de ; restore pos

;print_attr  
  push hl ; save shape
  call get_attr_adr
  ex de, hl
  pop hl

  ld a, (hl)
  inc hl
  ld (de), a

  inc de
  ld a, (hl)
  inc hl
  ld (de), a

  ld a, 31
  add a, e
  ld e, a
  jr nc, $+3
  inc d
  ld a, (hl)
  inc hl
  ld (de), a

  inc de
  ld a, (hl)
  inc hl
  ld (de), a
  
  ret


print_sprite
  ld de, (atpos)
  ld c, a
  ld a, (attribute)
  ld b, a
  ld a, (hl)
  ld (attribute), a
  ld a, c
  call print_at
  inc hl
  ld a, (hl)
  ld (attribute), a
  inc c
  ld a, c
  inc d
  call print_at
  inc hl
  ld a, (hl)
  ld (attribute), a
  inc c
  ld a, c
  dec d
  inc e
  call print_at
  inc hl
  ld a, (hl)
  ld (attribute), a
  inc c
  ld a, c
  inc d
  call print_at
  ld a, b
  ld (attribute), a
  ret

  
print_str ; hl = address, b = length  
  xor a
  or b
  ret z
print_str_loop
  ld a, (hl)
  call print_a
  inc hl
  djnz print_str_loop
  ret  

print_cr
  ld a, 32
  jr adv_at_row
adv_at
  ld a, (atx)
  inc a
adv_at_row
  and %00011111
  ld (atx), a
  jr nz, adv_at_done
print_lf
  ld a, (aty)
  inc a
  cp 24
  jr c, adv_at_next
  xor a
adv_at_next
  ld (aty), a
adv_at_done
  ret
  

clear_row
  xor a
  ld (atx), a
  ld a, " "
fill_row
  ld b, 32
print_repl ; a, b
  call print_a
  djnz print_repl
  and a
  ret
  


; Get Screen Address
; 
; Returns the screen address and pixel mask corresponding
; to a given pixel coordinate.
;
; enter: d = y coord
;        e = x coord
; exit : hl = screen address, b = pixel mask
; uses : af, bc, hl, de unchanged

get_scr_adr
  ld a, d    ; A = Y
  and $07    ; A = 00000SSS
  or $40     ; A = 01000SSS
  ld h, a    ; H = 01000SSS
  ld a, d    ; A = Y coord = BBLLLSSS
  rra
  rra
  rra        ; A = ???BBLLL
  and $18    ; A = 000BB000
  or h       ; A = 010BBSSS
  ld h, a    ; H = 010BBSSS top 8 bits of address done

  ld a, e    ; A = X coord = CCCCCTTT
  and $07    ; A = 00000TTT
  ld b, a    ; B = 00000TTT = which pixel?
  ld a, $80  ; A = 10000000
  jr z, gsa_norotate   ; if B=0, A is the right pixel so skip

gsa_rotloop
  rra        ; rotate the pixel right one place B times
  djnz gsa_rotloop

gsa_norotate
  ld b, a    ; B = pixel mask
  ld c, e
  srl c 
  srl c
  srl c      ; C = 000CCCCC
  ld a, d    ; A = Y coord = BBLLLSSS
  rla
  rla        ; A = LLLSSS??
  and $e0    ; A = LLL00000
  or c       ; A = LLLCCCCC
  ld l, a    ; L = LLLCCCCC
  ld a, b    ; A = mask
  ret        ; HL = 010BBSS LLLCCCCC, the screen address!
  

mode_flag ; if 0 then erase else plot
  jr z, mode_erase
  
mode_plot
  push hl
  ld hl, $b600 ; nop, or (hl)
  ld (plot_mode), hl
  pop hl
  ret

mode_erase
  push hl
  ld hl, $a62f ; cpl, and (hl)
  ld (plot_mode), hl
  pop hl
  ret

mode_switch
  push hl
  ld hl, $ae00 ; nop, xor (hl)
  ld (plot_mode), hl
  pop hl
  ret

plotxy ; e = x, d = y (no check)
  ld (lastxy), de
plot
  call get_scr_adr
plot_mode
  nop
  or (hl)
  ld (hl), a
  ret

vpole ; e = x; d = y
  call get_scr_adr
  ld c, a
  ld a, d ; a = 8 - y mod 8
  cpl
  and 7
  inc a
  ld b, a
vpole_loop
  ld a, c
  call plot_mode
  inc h
  djnz vpole_loop
  ret

hline ; e = x, d = y, b = len
  push bc
  call get_scr_adr
  pop bc
hline_loop
  ld c, a
  call plot_mode
  ld a, c
  rrca
  cp $80
  jr nz, hline_cont
  inc l
hline_cont
  djnz hline_loop
  ret

dline  
  ld hl, dline_pattern
dline_hl ; e = x, d = y, b = len
  push de
  ld a, d
  and 7
  add a, l
  ld l, a
  jr nc, $+3
  inc h
  ld a, (hl)
  push af
  push bc
  call get_scr_adr
  pop bc
  pop de
dline_loop
  ld c, a
  and d
  jr z, dline_erase
  ld a, c
  or (hl)
  jr dline_cont
dline_erase
  ld a, c
  cpl
  and (hl)
dline_cont
  ld (hl), a
  ld a, c
  rrca
  cp $80
  jr nz, dline_next
  inc l
dline_next
  djnz dline_loop
  pop de
  ret


CIRCLEABC
  ld (centre), bc
CIRCLEA
  ld (rad), a

CIRCLE
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
 JP CIRCLE_EXIT

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
 LD A, 256-192
 ADD A, B
 RET C
 ld e, c
 ld d, b
 JP plot ; ROM PLOT

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
CIRCLE_LOOP
 LD A, (X)
 INC A ; x++
 LD (X), A

 LD C, A    ;  c = x
 LD A, (Y)  ;  a = y
 LD B, 0    ; bc = x
 LD HL, (U) ; hl = u
 LD DE, (V) ; de = v

 SBC HL, DE
 JR C, CIRCLE_CONT
 JR Z, CIRCLE_CONT

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
 JR CIRCLE_TEST

CIRCLE_CONT
 ADD HL, DE
 ADD HL, BC
 ADD HL, BC
 INC HL
 LD (U), HL ; u += 2x + 1

; if (x < y) plot 8 dots else
; if (x = y) plot 4 dots else
; exit loop
CIRCLE_TEST
 SUB C
 JP C, CIRCLE_EXIT
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
 JP C, CIRCLE_LOOP
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
 JP C, CIRCLE_LOOP
 LD C, A
 LD A, (X)
 LD B, A
 LD A, (YC)
 SUB B
 CALL NC, PIXELAC

 JP CIRCLE_LOOP

CIRCLE_EXIT
 LD A, (RAD)
 RET


poly_begin
  xor a
  ld (polycnt), a
  ret
  
poly_point ; c = x, b = y (max 255 points)
  push bc
  push hl
  ld a, (polycnt)
  inc a
  ld (polycnt), a
  cp 1
  jr z, poly_point_moveto
  call lineto
  jr poly_point_done
poly_point_moveto
  ld (polyxy), de
  ld (lastxy), de
poly_point_done
  pop hl
  pop bc
  ret
  
poly_close ; close path
  ld a, (polycnt)
  and a
  ret z
  ld de, (polyxy)
  cp 3
  ld a, 0
  ld (polycnt), a
  jp c, plotxy

lineto ; (last) -- de
  ld b, d
  ld c, e
  ld de, (lastxy)
  jr line_calc

line ; de -- bc
  push bc
  call plotxy
  pop bc
line_calc
  ld a, c
  sub e
  jr c, line_xneg
  jr z, line_vert ;
  ld e, 1
  jr line_cont
line_vert ;
  ld e, a ;
  jr line_cont ;
line_xneg
  ld e, -1
  neg
line_cont
  ld c, a
  ld a, b
  sub d
  jr c, line_yneg
  jr z, line_horz
  ld d, 1
  jr line_done
line_horz
  ld d, a
  jr line_done
line_yneg
  ld d, -1
  neg
line_done
  ld b, a


; Modified ROM algoritam
vect ; (lastxy) -- c = abs dx, b = abs dy, e = sgn dx, d = sgn dy
	ld hl, lastxy ; doted line prep
	ld a, (hl)
	inc hl
	xor (hl)
	and 1
	ld (vect_flag), a

	exx	
	push hl ; hl' register is used by system
	exx

	ld a, c			 
	cp b			 
	jr nc, vect_x_ge_y ; dx >= dy
	ld l, c			
	push de			
	xor a			 
	ld e, a ; set stepx = 0
	jr vect_hl_maxmin		
vect_x_ge_y
	or c			
	jr z, vect_done ; both zero
	ld l, b ; l = min(dy, dx)
	ld b, c ; b = max(dx, dy)
	push de
	ld d, 0 ; set stepy = 0
vect_hl_maxmin
	ld h, b ; h = max(dx, dy)
	ld a, b
	rra ; div 2
vect_loop
	add a, l		 
	jr c, vect_diag
	cp h
	jr c, vect_hr_vt
vect_diag ; go diagonal
	sub h			 
	ld c, a			
	exx			 
	pop bc
	push bc
	jr vect_step		
vect_hr_vt ; go horiz or vert
	ld c, a			 
	push de			
	exx			 
	pop bc			
vect_step
	ld hl, (lastxy)
	ld a, b
	add a, h
	ld b, a
	ld a, c			 
	inc a			 
	add a, l		
	jr c, vect_range
	jr z, vect_done ; X out of screen (Y check in plot)
	jr vect_plot
vect_range
	jr nz, vect_done ; Z after C denotes X = 255, so plot
vect_plot
	dec a
	ld c, a
	ld (lastxy), bc
	
	ld a, (vect_flag)
vect_mode
	or 1
	; xor 1 for doted: 238 1
	; or  1 for solid: 246 1
	ld (vect_flag), a

	call nz, pixelbc
	
	exx
	ld a, c
	djnz vect_loop
	pop de

vect_done
	exx			
	pop hl		
	exx			
	ret
	
vect_solid
  ld a, 246 ; or
  ld (vect_mode), a
  ret
vect_doted
  ld a, 238 ; xor
  ld (vect_mode), a
  ret

  
init_row_adr
  ld hl, row_adr
  ld de, 0
  ld b, 24
init_row_adr_loop
  push bc
  push hl
  call get_scr_adr
  ld b, h
  ld c, l
  pop hl
  ld (hl), c
  inc hl
  ld (hl), b
  inc hl
  ld a, 8
  add a, d
  ld d, a
  pop bc
  djnz init_row_adr_loop
  ret
  
vect_flag defb 0
  
row_adr
  defw $4000, $4020, $4040, $4060, $4080, $40A0, $40C0, $40E0
  defw $4800, $4820, $4840, $4860, $4880, $48A0, $48C0, $48E0
  defw $5000, $5020, $5040, $5060, $5080, $50A0, $50C0, $50E0

lastxy
lastx defb 0
lasty defb 0

polyxy defw 0
polycnt defb 0

atpos
atx defb 0
aty defb 0
lastchar defb 0

attribute defb 56


CENTRE
XC  DEFB 0
YC  DEFB 0
RAD DEFB 0
X   DEFB 0
Y   DEFB 0
U   DEFW 0
V   DEFW 0


dline_pattern
  defb %00000000
  defb %11111111
  defb %00000000
  defb %00000000
  defb %01010101 ; --------
  defb %00000000
  defb %00000000
  defb %11111111


srb_ico 
  defw %0000000000000000 ;
  defw %0000000110000000 ;        #
  defw %0011100110011100 ;   ###  #
  defw %0000100110010000 ;     #  #
  defw %0000100110010000 ;     #  #
  defw %0011100110011100 ;   ###  #
  defw %0000000110000000 ;        #
  defw %0011111111111100 ;   ######
  defw %0011111111111100 ;   ######
  defw %0000000110000000 ;        #
  defw %0011100110011100 ;   ###  #
  defw %0000100110010000 ;     #  #
  defw %0000100110010000 ;     #  #
  defw %0011100110011100 ;   ###  #
  defw %0000000110000000 ;        #
  defw %0000000000000000 ;

  defb 87, 87, 87, 87

  
mak_ico
  defw %1000000110000001
  defw %1100000110000011
  defw %1110000110000111
  defw %0111000110001110
  defw %0001100110011000
  defw %0000010000100000
  defw %0000000000000000
  defw %1111100110011111
  defw %1111100110011111
  defw %0000000000000000
  defw %0000010000100000
  defw %0001100110011000
  defw %0111000110001110
  defw %1110000110000111
  defw %1100000110000011
  defw %1000000110000001

  defb 86, 86, 86, 86

 
slo_ico
  defw %0000000000000000
  defw %0001100000011000
  defw %0001100000011000
  defw %0000000000000000
  defw %0000000110000000
  defw %0000000110000000
  defw %0000000000000000
  defw %0000000000000000
  defw %0000000110000000
  defw %0010001111000100
  defw %0111011111101110
  defw %1111111111111111
  defw %1110001111000111
  defw %0001110000111000
  defw %1110001111000111
  defw %0001110000111000
  
  defb 78, 78, 79, 79
  

hrv_ico
  defw %0001110000111000
  defw %0001110000111000
  defw %0001110000111000
  defw %1110001111000111
  defw %1110001111000111
  defw %1110001111000111
  defw %0001110000111000
  defw %0001110000111000
  defw %0001110000111000
  defw %0001110000111000
  defw %1110001111000111
  defw %1110001111000111
  defw %1110001111000111
  defw %0001110000111000
  defw %0001110000111000
  defw %0001110000111000

  defb 87, 87, 87, 87


alb_ico
  defw %0000000000000000
  defw %0001111001111000
  defw %0000111111110000
  defw %0000001111000000
  defw %0111111111111110
  defw %0000111111110000
  defw %0111111111111110
  defw %0000111111110000
  defw %0111111111111110
  defw %0000001111100000
  defw %0111111111111100
  defw %0000101111010000
  defw %0111001111001110
  defw %0001000110001000
  defw %0001000110001000
  defw %0000000000000000

  defb 80, 80, 80, 80


eng_ico
  defw %0000000110000000 ;
  defw %0000000110000000 ;        #
  defw %0000000110000000 ;   ###  #
  defw %0000000110000000 ;     #  #
  defw %0000000110000000 ;     #  #
  defw %0000000110000000 ;   ###  #
  defw %0000000110000000 ;        #
  defw %1111111111111111 ;   ######
  defw %1111111111111111 ;   ######
  defw %0000000110000000 ;        #
  defw %0000000110000000 ;   ###  #
  defw %0000000110000000 ;     #  #
  defw %0000000110000000 ;     #  #
  defw %0000000110000000 ;   ###  #
  defw %0000000110000000 ;        #
  defw %0000000110000000 ;

  defb 122, 122, 122, 122


  
font
; " " - 32
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;

; "!" - 33
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00000000 ;

; """ - 34
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00100100 ;   #  #
  defb %00100100 ;   #  #

; "#" - 35
  defb %00000000 ;
  defb %01010000 ;
  defb %11111000 ; 
  defb %01010000 ;
  defb %11111000 ;
  defb %01010000 ;
  defb %00000000 ;
  defb %00000000 ;
  
; "$" - 36
  defb %00000000 ;
  defb %00001110 ;
  defb %00010000 ;
  defb %01111100 ;
  defb %00100000 ;
  defb %01111100 ;
  defb %00010000 ;
  defb %00001110 ;

; "%" - 37
  defb %00000000 ;
  defb %01100010 ;  ##   #
  defb %01100100 ;  ##  #
  defb %00001000 ;     #
  defb %00010000 ;    #
  defb %00100110 ;   #  ##
  defb %01000110 ;  #   ##
  defb %00000000 ;

; "&" - 38
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00101000 ;   # #
  defb %00010000 ;    #
  defb %00101010 ;   # # #
  defb %01000100 ;  #   #
  defb %00111010 ;   ### #
  defb %00000000 ;

; "'" - 39
  defb %00000000 ;
  defb %00001000 ;     #
  defb %00010000 ;    #
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;

; "(" - 40
  defb %00000000 ;
  defb %00000100 ;      #
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00000100 ;      #
  defb %00000000 ;

; ")" - 41
  defb %00000000 ;
  defb %00100000 ;   #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00100000 ;   #
  defb %00000000 ;

; "*" - 42
  defb %00000000 ;
  defb %00000000 ;
  defb %00010000 ;    # #
  defb %01010100 ;     #
  defb %00111000 ;   #####
  defb %01010100 ;     #
  defb %00010000 ;    # #
  defb %00000000 ;

; "+" - 43
  defb %00000000 ;
  defb %00000000 ;
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00111110 ;   #####
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00000000 ;

; "," - 44
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00010000 ;    #

; "-" - 45
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00111110 ;   #####
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;

; "." - 46
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00011000 ;    ##
  defb %00011000 ;    ##
  defb %00000000 ;

; "/" - 47
  defb %00000000 ;
  defb %00000010 ;
  defb %00000100 ;       #
  defb %00001000 ;      #
  defb %00010000 ;     #
  defb %00100000 ;    #
  defb %01000000 ;   #
  defb %00000000 ;

; "0" - 48
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000110 ;  #   ##
  defb %01001010 ;  #  # #
  defb %01010010 ;  # #  #
  defb %01100010 ;  ##   #
  defb %00111100 ;   ####
  defb %00000000 ;

; "1" - 49
  defb %00000000 ;
  defb %00011000 ;    ##
  defb %00101000 ;   # #
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00001000 ;     #
  defb %00111110 ;   #####
  defb %00000000 ;

; "2" - 50
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %00000010 ;       #
  defb %00111100 ;   ####
  defb %01000000 ;  #
  defb %01111110 ;  ######
  defb %00000000 ;

; "3" - 51
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %00001100 ;     ##
  defb %00000010 ;       #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "4" - 52
  defb %00000000 ;
  defb %00001000 ;     #
  defb %00011000 ;    ##
  defb %00101000 ;   # #
  defb %01001000 ;  #  #
  defb %01111110 ;  ######
  defb %00001000 ;     #
  defb %00000000 ;

; "5" - 53
  defb %00000000 ;
  defb %01111110 ;  ######
  defb %01000000 ;  #
  defb %01111100 ;  #####
  defb %00000010 ;       #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "6" - 54
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000000 ;  #
  defb %01111100 ;  #####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "7" - 55
  defb %00000000 ;
  defb %01111110 ;  ######
  defb %00000010 ;       #
  defb %00000100 ;      #
  defb %00001000 ;     #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00000000 ;

; "8" - 56
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "9" - 57
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00111110 ;   #####
  defb %00000010 ;       #
  defb %00111100 ;   ####
  defb %00000000 ;

; ":" - 58
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00000000 ;
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00000000 ;

; ";" - 59
  defb %00000000 ;
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00000000 ;
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00100000 ;   #

; "<" - 60
  defb %00000000 ;
  defb %00000000 ;
  defb %00000100 ;      #
  defb %00001000 ;     #
  defb %00010000 ;    #
  defb %00001000 ;     #
  defb %00000100 ;      #
  defb %00000000 ;

; "=" - 61
  defb %00000000 ;
  defb %00000000 ;
  defb %00000000 ;
  defb %00111110 ;   #####
  defb %00000000 ;
  defb %00111110 ;   #####
  defb %00000000 ;
  defb %00000000 ;

; ">" - 62
  defb %00000000 ;
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00001000 ;     #
  defb %00000100 ;      #
  defb %00001000 ;     #
  defb %00010000 ;    #
  defb %00000000 ;

; "?" - 63
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %00000100 ;      #
  defb %00001000 ;     #
  defb %00000000 ;
  defb %00001000 ;     #
  defb %00000000 ;

; "@" - 64
  defb %00000000 ;
  defb %10010010 ; #  #  #
  defb %01010100 ;  # # #
  defb %00111000 ;   ###
  defb %01010100 ;  # # #
  defb %10010010 ; #  #  #
  defb %10010010 ; #  #  #
  defb %00000000 ;

; "A" - 65
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01111110 ;  ######
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "B" - 66
  defb %00000000 ;
  defb %01111110 ;  ######
  defb %01000000 ;  #
  defb %01111100 ;  #####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01111100 ;  #####
  defb %00000000 ;

; "C" - 67
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01111110 ;  ######
  defb %00000010 ;       #

; "D" - 68
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %11111110 ; #######
  defb %10000010 ; #     #

; "E" - 69
  defb %00000000 ;
  defb %01111110 ;  ######
  defb %01000000 ;  #
  defb %01111100 ;  #####
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01111110 ;  ######
  defb %00000000 ;

; "F" - 70
  defb %00000000 ;
  defb %01111100 ;  #####
  defb %10010010 ; #  #  #
  defb %10010010 ; #  #  #
  defb %10010010 ; #  #  #
  defb %10010010 ; #  #  #
  defb %01111100 ;  #####
  defb %00000000 ;

; "G" - 71
  defb %00000000 ;
  defb %01111110 ;  ######
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %00000000 ;

; "H" - 72
  defb %00000000 ;
  defb %01000010 ;  #    #
  defb %00100100 ;   #  #
  defb %00011000 ;    ##
  defb %00011000 ;    ##
  defb %00100100 ;   #  #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "I" - 73
  defb %00000000 ;
  defb %01000010 ;  #    #
  defb %01000110 ;  #   ##
  defb %01001010 ;  #  # #
  defb %01010010 ;  # #  #
  defb %01100010 ;  ##   #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "J" - 74
  defb %00000000 ;
  defb %00000010 ;       #
  defb %00000010 ;       #
  defb %00000010 ;       #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "K" - 75
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01001000 ;  #  #
  defb %01010000 ;  # #
  defb %01101000 ;  ## #
  defb %01000100 ;  #   #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "L" - 76
  defb %00000000 ;
  defb %00011110 ;    ####
  defb %00100010 ;   #   #
  defb %00100010 ;   #   #
  defb %00100010 ;   #   #
  defb %00100010 ;   #   #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "M" - 77
  defb %00000000 ;
  defb %01000010 ;  #    #
  defb %01100110 ;  ##  ##
  defb %01011010 ;  # ## #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "N" - 78
  defb %00000000 ;
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01111110 ;  ######
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "O" - 79
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "P" - 80
  defb %00000000 ;
  defb %01111110 ;  ######
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "Q" - 81
  defb %00000000 ;
  defb %00111000 ;   ###
  defb %01001000 ;  #  #
  defb %01001110 ;  #  ###
  defb %01001001 ;  #  #  #
  defb %01001001 ;  #  #  #
  defb %10001110 ; #   ###
  defb %00000000 ;

; "R" - 82
  defb %00000000 ;
  defb %01111100 ;  #####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01111100 ;  #####
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %00000000 ;

; "S" - 83
  defb %00000000 ;
  defb %00111100 ;   ####
  defb %01000010 ;  #    #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "T" - 84
  defb %00000000 ;
  defb %11111110 ; #######
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00000000 ;

; "U" - 85
  defb %00000000 ;
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00100100 ;   #  #
  defb %00011100 ;    ###
  defb %00001000 ;     #
  defb %00110000 ;   ##
  defb %00000000 ;

; "V" - 86
  defb %00000000 ;
  defb %01111100 ;  #####
  defb %01000010 ;  #    #
  defb %01111100 ;  #####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01111100 ;  #####
  defb %00000000 ;

; "W" - 87
  defb %00000000 ;
  defb %10001000 ; #   #
  defb %10001000 ; #   #
  defb %11111110 ; #######
  defb %10001001 ; #   #  #
  defb %10001001 ; #   #  #
  defb %10001110 ; #   ###
  defb %00000000 ;

; "X" - 88
  defb %00000000 ;
  defb %01000010 ;  #   #
  defb %01000010 ;  #   #
  defb %01000010 ;  #   #
  defb %01000010 ;  #   #
  defb %01000010 ;  #   #
  defb %01111110 ;  #####
  defb %00011000 ;    #

; "Y" - 89
  defb %00001000 ;     #
  defb %01010100 ;  # # #
  defb %01001000 ;  #  #
  defb %01010000 ;  # #
  defb %01101000 ;  ## #
  defb %01000100 ;  #   #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "Z" - 90
  defb %00000000 ;
  defb %01111110 ;  ######
  defb %00000010 ;       #
  defb %00001100 ;     ##
  defb %00000010 ;       #
  defb %01000010 ;  #    #
  defb %00111100 ;   ####
  defb %00000000 ;

; "[" - 91
  defb %00000000 ;
  defb %10010010 ; #     #
  defb %10010010 ; #  #  #
  defb %10010010 ; #  #  #
  defb %10010010 ; #  #  #
  defb %10010010 ; #  #  #
  defb %11111110 ; #######
  defb %00000000 ;

; "\" - 92
  defb %00000000 ;
  defb %11111000 ; #####
  defb %01000000 ;  #
  defb %01111100 ;  #####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000100 ;      #
  defb %00001000 ;     #

; "]" - 93
  defb %00000000 ;
  defb %11111000 ; #####
  defb %01000000 ;  #
  defb %01111100 ;  #####
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00000000 ;

; "^" - 94
  defb %00000000 ;
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %01000010 ;  #    #
  defb %00111110 ;   #####
  defb %00000010 ;       #
  defb %00000010 ;       #
  defb %00000000 ;

; "_" - 95
  defb %00001000 ;     #
  defb %01111110 ;  ######
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %00000000 ;

; "`" - 96
  defb %00000000 ;
  defb %00000000 ;
  defb %10010010 ; #  #  #
  defb %01010100 ;  # # #
  defb %00111000 ;   ###
  defb %01010100 ;  # # #
  defb %10010010 ; #  #  #
  defb %00000000 ;

; "a" - 97
  defb %00000000 ;
  defb %00000000 ;
  defb %00111000 ;   ###
  defb %00000100 ;      #
  defb %00111100 ;   ####
  defb %01000100 ;  #   #
  defb %00111100 ;   ####
  defb %00000000 ;

; "b" - 98
  defb %00000000 ;
  defb %00111000 ;   ###
  defb %01000000 ;  #
  defb %00111000 ;   ###
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00111000 ;   ###
  defb %00000000 ;

; "c" - 99
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01111110 ;  ######
  defb %00000010 ;       #

; "d" - 100
  defb %00000000 ;
  defb %00000000 ;
  defb %00011100 ;    ###
  defb %00100100 ;   #  #
  defb %00100100 ;   #  #
  defb %00100100 ;   #  #
  defb %01111110 ;  ######
  defb %01000010 ;  #    #

; "e" - 101
  defb %00000000 ;
  defb %00000000 ;
  defb %00111000 ;   ###
  defb %01000100 ;  #   #
  defb %01111000 ;  ####
  defb %01000000 ;  #
  defb %00111100 ;   ####
  defb %00000000 ;

; "f" - 102
  defb %00000000 ;
  defb %00010000 ;    #
  defb %00111000 ;   ###
  defb %01010100 ;  # # #
  defb %01010100 ;  # # #
  defb %01010100 ;  # # #
  defb %00111000 ;   ###
  defb %00010000 ;    #

; "g" - 103
  defb %00000000 ;
  defb %00000000 ;
  defb %01111100 ;  #####
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %00000000 ;

; "h" - 104
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %00101000 ;   # #
  defb %00010000 ;    #
  defb %00101000 ;   # #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "i" - 105
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01001100 ;  #  ##
  defb %01010100 ;  # # #
  defb %01100100 ;  ##  #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "j" - 106
  defb %00000000 ;
  defb %00000100 ;      #
  defb %00000000 ;
  defb %00000100 ;      #
  defb %00000100 ;      #
  defb %00000100 ;      #
  defb %00100100 ;   #  #
  defb %00011000 ;    ##

; "k" - 107
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01001000 ;  #  #
  defb %01110000 ;  ###
  defb %01001000 ;  #  #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "l" - 108
  defb %00000000 ;
  defb %00000000 ;
  defb %00011100 ;    ###
  defb %00100100 ;   #  #
  defb %00100100 ;   #  #
  defb %00100100 ;   #  #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "m" - 109
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01101100 ;  ## ##
  defb %01010100 ;  # # #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "n" - 110
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01111100 ;  #####
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "o" - 111
  defb %00000000 ;
  defb %00000000 ;
  defb %00111000 ;   ###
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00111000 ;   ###
  defb %00000000 ;

; "p" - 112
  defb %00000000 ;
  defb %00000000 ;
  defb %01111100 ;  #####
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "q" - 113
  defb %00000000 ;
  defb %00000000 ;
  defb %00111000 ;   ###
  defb %01001000 ;  #  #
  defb %01001110 ;  #  ###
  defb %01001001 ;  #  #  #
  defb %10001110 ; #   ###
  defb %00000000 ;

; "r" - 114
  defb %00000000 ;
  defb %00000000 ;
  defb %01111000 ;  ####
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01111000 ;  ####
  defb %01000000 ;  #
  defb %01000000 ;  #

; "s" - 115
  defb %00000000 ;
  defb %00000000 ;
  defb %00011100 ;    ###
  defb %00100000 ;   #
  defb %00100000 ;   #
  defb %00100000 ;   #
  defb %00011100 ;    ###
  defb %00000000 ;

; "t" - 116
  defb %00000000 ;
  defb %00000000 ;
  defb %01111100 ;  #####
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00010000 ;    #
  defb %00000000 ;

; "u" - 117
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00100100 ;   #  #
  defb %00011000 ;    ##
  defb %00001000 ;     #
  defb %00110000 ;   ##

; "v" - 118
  defb %00000000 ;
  defb %00000000 ;
  defb %01111000 ;  ####
  defb %01000100 ;  #   #
  defb %01111000 ;  ####
  defb %01000100 ;  #   #
  defb %01111000 ;  ####
  defb %00000000 ;

; "w" - 119
  defb %00000000 ;
  defb %00000000 ;
  defb %01001000 ;  #  #
  defb %01001000 ;  #  #
  defb %01111110 ;  ######
  defb %01001001 ;  #  #  #
  defb %01001110 ;  #  ###
  defb %00000000 ;

; "x" - 120
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01111100 ;  #####
  defb %00010000 ;    #

; "y" - 121
  defb %00001000 ;
  defb %00010000 ;
  defb %01000100 ;  #   #
  defb %01001000 ;  #  #
  defb %01110000 ;  ###
  defb %01001000 ;  #  #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "z" - 122
  defb %00000000 ;
  defb %00000000 ;
  defb %01111000 ;  ####
  defb %00000100 ;      #
  defb %00111000 ;   ###
  defb %00000100 ;      #
  defb %01111000 ;  ####
  defb %00000000 ;

; "{" - 123
  defb %00000000 ;
  defb %00000000 ;
  defb %01010100 ;  #   #
  defb %01010100 ;  # # #
  defb %01010100 ;  # # #
  defb %01010100 ;  # # #
  defb %01111100 ;  #####
  defb %00000000 ;

; "|" - 124
  defb %01000000 ;
  defb %11100000 ;  #
  defb %01000000 ; ###
  defb %01111000 ;  ####
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00011000 ;    ##

; "}" - 125
  defb %01000000 ;
  defb %11100000 ;  #
  defb %01000000 ; ###
  defb %01111000 ;  ####
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00000000 ;

; "~" - 126
  defb %00000000 ;
  defb %00000000 ;
  defb %01000100 ;  #   #
  defb %01000100 ;  #   #
  defb %00111100 ;   ####
  defb %00000100 ;      #
  defb %00000100 ;      #
  defb %00000000 ;

; "" - 127
  defb %00001000 ;     #
  defb %00010000 ;    #
  defb %01111100 ;  #####
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %01000000 ;  #
  defb %00000000 ;


; 224
  defb %01111110
  defb %00000000
  defb %00000000
  defb %00000000
  defb %00000000
  defb %00000000
  defb %00000000
  defb %00000000

; 225
  defb %01111110
  defb %00000000
  defb %01111110
  defb %00000000
  defb %00000000
  defb %00000000
  defb %00000000
  defb %00000000

  defb % 00010000
  defb % 00110000
  defb % 01011110
  defb % 10000010
  defb % 10000010
  defb % 01011110
  defb % 00110000
  defb % 00010000

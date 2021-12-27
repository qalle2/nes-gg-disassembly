; NES Game Genie - PRG ROM (ASM6)
; Indentation: 16 spaces. Maximum length of identifiers: 15 characters.
; TODO: some labels are still too long; try: grep "^[a-z0-9_]\{16,99\}" prg.asm
; TODO: some subs starting from check_button_b need tidying up.

; --- Constants -----------------------------------------------------------------------------------

; Notes:
; - Each variable takes one byte unless otherwise mentioned.
; - Signed integers are two's complement unless otherwise noted.

; CPU addresses - RAM
ram_clear_ptr   equ $00    ; RAM clear pointer (2 bytes, overlaps)
ppu_ctrl_mirror equ $00
ppu_mask_mirror equ $01
temp1           equ $04    ; a temporary variable, many uses
vram_addr_hi    equ $05    ; VRAM address high
joypad1_status  equ $07
joypad2_status  equ $08
skip_nmi        equ $0c
graphic_width   equ $0d    ; width of graphic in nybbles/tiles
graphic_height  equ $0e    ; height of graphic in nybbles/tiles
ram_prog_target equ $10    ; RAM program target
graphic_x_offs  equ $15    ; graphic X offset
graphic_y_offs  equ $16    ; graphic Y offset
graphic_x       equ $17
graphic_y       equ $18
graphic_nybble  equ $19
attr_fill_byte  equ $1a    ; attribute fill byte
vram_block_x    equ $1e
vram_block_y    equ $1f
nybble_vram_lo  equ $20    ; low
nybble_vram_hi  equ $21    ; high
multiply_temp   equ $27    ; temporary for multiplication
vram_block_cost equ $28
vram_buf_rd_pos equ $29    ; next read position in VRAM buffer
vram_buf_wr_pos equ $2a    ; next write position in VRAM buffer
vram_buf_free   equ $2b    ; number of free bytes in VRAM buffer
vram_budget     equ $2c
metasprite_x    equ $2d
always_zero1    equ $2e    ; always 0
metasprite_y    equ $2f
always_zero2    equ $30    ; always 0
graphic_id      equ $31
unused1         equ $33
metaspr_width   equ $34    ; metasprite width  in nybbles/tiles
metaspr_height  equ $35    ; metasprite height in nybbles/tiles
always_00011100 equ $36    ; always %00011100
grafic_dataleft equ $37    ; graphic data left
metaspr_index   equ $38    ; metasprite index
graphics_ptr    equ $39    ; graphics pointer (2 bytes)
nybbles_left_x  equ $3b
sprite_y        equ $3c    ; 2 bytes
sprite_x        equ $3e    ; 2 bytes
nybble_offs     equ $40    ; nybble offset
always_one      equ $41    ; 1
always_zero3    equ $42    ; 0
odd_frame_flag1 equ $43
scroll_x_mirror equ $45
scroll_y_mirror equ $46
nmi_done        equ $49
unused2         equ $4a
first_free_byte equ $4b
keybd_graphic   equ $4c    ; keyboard graphic
keybd_graphic_x equ $4d    ; keyboard graphic X position
keybd_graphic_y equ $4e    ; keyboard graphic Y position
hand_metasprite equ $4f    ; hand cursor; metasprite number (0)
hand_x_pixel    equ $50    ; 14-238
hand_y_pixel    equ $51    ; 60-204
hand_x_letr_trg equ $52    ; hand cursor - X target in letters (0-7)
hand_y_letr_trg equ $53    ; hand cursor - Y target in letters (0-4)
revolv_metaspr  equ $54    ; revolving cursor - metasprite number (1)
revolv_x        equ $55    ; revolving cursor - X pos (10-234)
revolv_y        equ $56    ; revolving cursor - Y pos (152-216)
hand_x_spd_ptr  equ $57    ; hand cursor - X speed pointer (2 bytes)
hand_y_spd_ptr  equ $59    ; hand cursor - Y speed pointer (2 bytes)
hand_y_spd_offs equ $5b    ; hand cursor - Y speed offset
hand_x_spd_offs equ $5c    ; hand cursor - X speed offset
last_x_inp_acc  equ $5d    ; last X input accepted (pad_left/pad_right)
last_y_inp_acc  equ $5e    ; last Y input accepted (pad_up/pad_down)
odd_frame_flag2 equ $5f
hand_x_keyboard equ $60    ; hand cursor - last X position on virtual keyboard (0-7)
hand_y_keyboard equ $61    ; hand cursor - last Y position on virtual keyboard (0-1)
hand_x_letr     equ $62    ; hand cursor - X position in letters (0-7)
hand_y_letr     equ $63    ; hand cursor - Y position in letters (0-4)
revolv_x_letr1  equ $64    ; revolving cursor - X position in letters (0-7)
revolv_y_letr1  equ $65    ; revolving cursor - Y position in letters (0-2)
prev_btn_a_stat equ $66    ; previous status of button A
prev_btn_b_stat equ $67    ; previous status of button B
revolv_x_target equ $68    ; revolving cursor - X target (10-234)
revolv_y_target equ $69    ; revolving cursor - Y target (152-216 in increments of 32)
revolv_trg_spd  equ $6a    ; revolving cursor - target speed (signed, excess-128)
revolv_spd_x    equ $6b    ; revolving cursor - X speed (signed)
revolv_spd_y    equ $6c    ; revolving cursor - Y speed (signed)
revolv_phase    equ $6d    ; revolving cursor - phase of animation (0-15)
revolv_pos      equ $6e    ; revolving cursor - X/Y pos (arg for sub)
revolv_target   equ $6f    ; revolving cursor - X/Y target (arg for sub)
parti_start_x   equ $70    ; particles - start X position
parti_start_y   equ $71    ; particles - start Y position
parti_set_flag  equ $72    ; particles - which set to spawn
parti_set1timer equ $73    ; particles - timer of set 1 (counts up!)
parti_set2timer equ $74    ; particles - timer of set 2 (counts up!)
particle_timer  equ $75    ; particles - timer (counts down!)
flying_x        equ $76    ; flying letter - X position
flying_y        equ $77    ; flying letter - Y position
flying_x_spd    equ $78    ; flying letter - X speed (signed, -14...+14)
flying_y_spd    equ $79    ; flying letter - Y speed (3-9)
flying_timelft1 equ $7a    ; flying letter - time left 1
flying_metaspr  equ $7b    ; flying letter - metasprite number (2)
flying_timelft2 equ $7c    ; flying letter - time left 2
entered_letr    equ $7d    ; entered letter
rows_left       equ $7e    ; only in fill_atrblkrows
anim_colr_phase equ $7f    ; phase of animated color (0-7)
code_ptr        equ $80    ; code pointer (2 bytes, overlaps)
anim_colr_delay equ $80    ; delay of animated color (0-4)
revolv_x_letr2  equ $81    ; revolving cursor - X position in letters (0-7?)
decoded_cod_ptr equ $82    ; pointer to decoded codes (2 bytes, overlaps)
revolv_y_letr2  equ $82    ; revolving cursor - Y position in letters (2-4)
revolv_y_ltr2pr equ $83    ; revolving cursor - previous Y position in letters (2-4)
temp2           equ $84    ; temporary, many uses
code_length     equ $85
code_enab_mask  equ $86    ; code enable bitmask
comp_enab_mask  equ $87    ; compare value enable bitmask
codes_left      equ $88    ; number of codes left to decode
genie_ctrl_val  equ $89    ; Game Genie hardware control value
decoded_code    equ $8a    ; 4 bytes; current decoded Game Genie code (see below)
decoded_codes   equ $90    ; 16 bytes (12 actually used); all decoded Game Genie codes
;                            (see "arrays" below)
sprite_data     equ $0200  ; interleaved sprite data; 256 bytes; copied to OAM
vram_buffer     equ $0300  ; 256 bytes; several VRAM blocks to be copied to VRAM
attr_data_copy  equ $0400  ; 64 bytes; copy of attribute table data
vram_block      equ $0440  ; 35 bytes; a block of bytes to be copied to VRAM
;                            (see "arrays" below)
sprite_data_atr equ $0463  ; 64 bytes; attributes  of sprites (see "arrays" below)
sprite_data_x   equ $04a3  ; 64 bytes; X positions of sprites (see "arrays" below)
sprite_data_y   equ $04e3  ; 64 bytes; Y positions of sprites (see "arrays" below)
sprite_data_til equ $0523  ; 64 bytes; tiles       of sprites (see "arrays" below)
metaspr_indexes equ $0563  ; 50 bytes; indexes to metasprites
metasprites     equ $0595  ; 46 bytes; objects consisting of several hardware sprites
;                            (see "arrays" below)
parti_spds_x    equ $060b  ; 64 bytes; horizontal speeds of flying particles (signed, only
;                            indexes 32-63 used)
parti_spds_y    equ $062b  ; 64 bytes; vertical speeds of flying particles (signed, only
;                            indexes 32-63 used)
entered_letrs   equ $066b  ; 24 bytes; letters entered by user (0=none, 3-18=AEPOZXLUGKISTVYN)

; Arrays:
;
; decoded_code:
;     bytes: address high, address low, replace value, compare value
; decoded_codes:
;     bytes for each code: address high, address low, compare value, replace value
;     (note: order differs from decoded_code)
; vram_block:
;     3 bytes: payload size (1-32), address high, address low
;     1-32 bytes: payload
; sprite_data_atr, sprite_data_x, sprite_data_y, sprite_data_til:
;      0-19: hand cursor      (5*4 sprites)
;     20-23: revolving cursor (2*2 sprites)
;     24-39: flying letter    (4*4 sprites)
;     32-47: 1st particle set (16  sprites; overlaps with flying letter)
;     48-63: 2nd particle set (16  sprites)
; metasprites:
;     2 + 5*4 bytes for hand cursor
;     2 + 2*2 bytes for revolving cursor
;     2 + 4*4 bytes for flying letter
;     for each one:
;         1 byte:               width  in hardware sprites
;         1 byte:               height in hardware sprites
;         width * height bytes: indexes to planar sprite data

; CPU addresses - NES memory-mapped registers; see:
; - https://wiki.nesdev.org/w/index.php?title=PPU_registers
; - https://wiki.nesdev.org/w/index.php?title=APU_registers
; - https://wiki.nesdev.org/w/index.php?title=Controller_reading_code
ppu_ctrl        equ $2000
ppu_mask        equ $2001
ppu_status      equ $2002
oam_addr        equ $2003
ppu_scroll      equ $2005
ppu_addr        equ $2006
ppu_data        equ $2007
oam_dma         equ $4014
pulse1_regs     equ $4000  ; $4000-$4003
noise_regs      equ $400c  ; $400c, $400e-$400f
sound_ctrl      equ $4015
joypad1         equ $4016
joypad2         equ $4017

; CPU addresses - Game Genie memory-mapped registers;
; see: https://wiki.nesdev.org/w/index.php?title=Game_Genie
genie_ctrl      equ $8000  ; master control
genie_codes     equ $8001  ; -$800c (12 bytes; addr hi, addr lo, compare, replace for 3 codes)
genie_unknown1  equ $fff0
genie_unknown2  equ $fff1

; PPU addresses
vram_nametable0 equ $2000  ; Name Table 0
vram_attrtable0 equ $23c0  ; Attribute Table 0
vram_palette    equ $3f00  ; palette

; joypad bitmasks
pad_a           equ 1<<7
pad_b           equ 1<<6
pad_select      equ 1<<5
pad_start       equ 1<<4
pad_up          equ 1<<3
pad_down        equ 1<<2
pad_left        equ 1<<1
pad_right       equ 1<<0

; colors
color_anim0     equ $21  ; animated 0 (sky blue)
color_anim1     equ $2c  ; animated 1 (cyan)
color_anim2     equ $2b  ; animated 2 (green)
color_anim3     equ $28  ; animated 3 (yellow)
color_anim4     equ $27  ; animated 4 (orange)
color_anim5     equ $25  ; animated 5 (pink 1)
color_anim6     equ $24  ; animated 6 (pink 2)
color_anim7     equ $2c  ; animated 7 (cyan)
color_anim_init equ $26  ; initial animated color (red, never seen)
color_bg        equ $0d  ; background (black, causes problems with some TVs)
color_hand1     equ $26  ; hand cursor 1 (red)
color_hilite    equ $20  ; highlight (white)
color_inputarea equ $00  ; input area (gray)
color_keyboard  equ $2c  ; virtual keyboard (cyan)
color_multiuse1 equ $13  ; letters       / revolving cursor 1 / particles 1 (purple)
color_multiuse2 equ $20  ; hand cursor 2 / revolving cursor 2 / particles 2 (white)
color_unused1   equ $00  ; gray
color_unused2   equ $28  ; yellow

; --- Macros --------------------------------------------------------------------------------------

; prevent ASM6 from optimizing a 16-bit address between $00-$ff to an 8-bit address
macro dec_abs _word
                db $ce
                dw _word
endm
macro lda_abs _word
                db $ad
                dw _word
endm
macro ldx_abs _word
                db $ae
                dw _word
endm
macro sta_abs _word
                db $8d
                dw _word
endm

macro add _operand  ; add without carry
                clc
                adc _operand
endm

macro sub _operand  ; subtract with carry
                sec
                sbc _operand
endm

; copy value via A (note: for clarity, these macros aren't used if A is read later)
macro copy _src, _dst
                lda _src
                sta _dst
endm
macro copy_abs _src, _dst  ; force STA absolute for _dst (see macros above)
                lda _src
                db $8d
                dw _dst
endm

macro ldaxy _a, _x, _y  ; load A, X, Y (args for some functions)
                lda _a
                ldx _x
                ldy _y
endm

; -------------------------------------------------------------------------------------------------

                base $f000  ; last 4 KiB of CPU address space

init1           ; Part 1/3 of initialization. Called by reset vector.

                sei                        ; ignore IRQs
                cld                        ; disable decimal mode
                copy #%00000000, ppu_ctrl  ; disable NMI
                ldx #$ff
                txs                        ; initialize stack pointer

                lda #$00            ; do something to unknown Game Genie registers
                sta genie_unknown1
                jsr delay
                sta genie_unknown2
                jsr delay
                sta genie_unknown1

                jmp init2  ; continue initialization

delay           ldx #96  ; Wait. Called by init1.
                ldy #8
-               dex
                bne -
                dey
                bne -
                rts

init2           ; Part 2/3 of initialization. Called by init1.

                ldx #10         ; wait for VBlank 10 times
-               lda ppu_status
                bpl -
                dex
                bne -

                ldx #$ff  ; reinitialize stack pointer (why?)
                txs

                copy #$07, ram_clear_ptr+1  ; clear RAM (fill $0000-$07ff with $00)
                lda #$00
                sta ram_clear_ptr+0
                tay
-               sta (ram_clear_ptr),y
                iny
                bne -
                dec ram_clear_ptr+1
                bpl -

                lda #%00000110       ; hide sprites and background
                sta ppu_mask_mirror
                sta ppu_mask

                lda #%10000000       ; enable NMI; use 8*8-pixel sprites, Pattern Table 0,
                sta ppu_ctrl_mirror  ; Name Table 0 and 1-byte VRAM address autoincrement
                sta ppu_ctrl

                dec_abs vram_buf_free  ; set to 255

                jmp init3  ; continue initialization

; -------------------------------------------------------------------------------------------------

read_joypads    ; Read joypads. Out: joypad1_status, joypad2_status.
                ; Bits: A, B, select, start, up, down, left, right. Called by do_every_frame.

                copy #%00000001, joypad1  ; initialize joypads
                copy #%00000000, joypad1

                ldy #8              ; read joypad 1 (for each button, copy least significant bit
-               lda joypad1         ; of joypad1 to joypad1_status via carry)
                ror
                rol joypad1_status
                dey
                bne -

                ldy #8              ; read joypad 2 in similar fashion
-               lda joypad2
                ror
                rol joypad2_status
                dey
                bne -

                rts

; -------------------------------------------------------------------------------------------------

                ; The non-maskable interrupt routine.
                ; In: skip_nmi (if set, skip PPU stuff). Out: nmi_done (set when exiting).
                ; Called by NMI vector.

nmi             pha  ; push A, X, Y
                txa
                pha
                tya
                pha

                lda skip_nmi  ; if flag set, skip PPU stuff
                bne +

                jsr sprite_dma
                jsr flush_vram_buf

                lda_abs scroll_x_mirror         ; update PPU registers from mirrors
                sta ppu_scroll
                lda_abs scroll_y_mirror
                sta ppu_scroll
                copy ppu_ctrl_mirror, ppu_ctrl

+               copy_abs #1, nmi_done  ; set flag

                pla  ; pull Y, X, A
                tay
                pla
                tax
                pla

                rti

; -------------------------------------------------------------------------------------------------

draw_bg_graphic ; Copy a graphic (e.g. the Game Genie logo) to Name Table.
                ; In: A = id (see graphix_offsets), X/Y = X/Y position in tiles.
                ; Called by init_background, letter_input, check_button_b.

                stx graphic_x
                sty graphic_y
                jsr set_graphix_ptr  ; A = id

                ldy #0                ; get width and height in nybbles/tiles
                lda (graphics_ptr),y
                sta graphic_width
                iny
                lda (graphics_ptr),y
                sta graphic_height

                lda_abs graphics_ptr+0  ; advance pointer to start of data
                add #2
                sta_abs graphics_ptr+0
                lda_abs graphics_ptr+1
                adc #0
                sta_abs graphics_ptr+1

                copy #0, graphic_y_offs            ; copy nybbles to VRAM buffer
--              copy #0, graphic_x_offs            ; start Y loop
-               jsr graphic_nybble_to_vram_buffer  ; start X loop
                inc graphic_x_offs
                lda graphic_x_offs
                cmp graphic_width
                bne -
                inc graphic_y_offs
                lda graphic_y_offs
                cmp graphic_height
                bne --

                rts

graphic_nybble_to_vram_buffer
                ; Copy one nybble/tile of a graphic to VRAM buffer.
                ; In:
                ;     graphic_x/graphic_y:           position of top left in tiles
                ;     graphic_x_offs/graphic_y_offs: position inside graphic in tiles
                ;     vram_addr_hi:                  high byte of VRAM address
                ; Called by draw_bg_graphic.

                lda graphic_y_offs   ; offset to nybble -> nybble_offs
                ldx graphic_width
                jsr multiply         ; A = multiplicand, X = multiplier
                add graphic_x_offs
                sta_abs nybble_offs

                lsr                   ; graphic data byte -> temp1
                tay
                lda (graphics_ptr),y
                sta temp1

                lda_abs nybble_offs  ; nybble from byte -> graphic_nybble
                and #%00000001       ; if even offset, upper nybble; if odd, lower
                beq +
                lda temp1
                and #%00001111
                jmp ++
+               lda temp1
                lsr
                lsr
                lsr
                lsr
++              sta graphic_nybble

                ; vram_addr_hi * 256
                ; + (graphic_y + graphic_y_offs) * 32
                ; + graphic_x + graphic_x_offs - 4
                ; -> word(nybble_vram_hi, nybble_vram_lo)

                lda graphic_x       ; graphic_x + graphic_x_offs - 4 -> vram_block_x
                add graphic_x_offs
                sub #4
                sta vram_block_x

                lda graphic_y       ; (graphic_y + graphic_y_offs) * 32 + vram_addr_hi * 256
                add graphic_y_offs  ; -> word(nybble_vram_hi, nybble_vram_lo)
                sta vram_block_y
                lda vram_block_y
                asl
                asl
                asl
                sta nybble_vram_lo
                lda #0               ; start computing nybble_vram_hi
                asl nybble_vram_lo   ; 4th shift
                rol                  ; save overflowed bit
                asl nybble_vram_lo   ; 5th shift
                rol                  ; save overflowed bit
                add vram_addr_hi
                sta nybble_vram_hi

                lda nybble_vram_lo  ; word(nybble_vram_hi, nybble_vram_lo) += vram_block_x
                add vram_block_x
                sta nybble_vram_lo
                lda nybble_vram_hi
                adc #0
                sta nybble_vram_hi

                copy nybble_vram_hi, vram_block+1  ; set up VRAM block with data size 1
                copy nybble_vram_lo, vram_block+2
                copy graphic_nybble, vram_block+3
                copy #1,             vram_block+0

                jsr vram_blk_to_buf  ; copy to buffer
                rts

; -------------------------------------------------------------------------------------------------

multiply        ; Multiply (X * A -> A).
                ; Called by graphic_nybble_to_vram_buffer, draw_metaspr_gr.

                sta multiply_temp
                lda #0
                cpx #0
                beq +
                clc
-               adc multiply_temp
                dex
                bne -
+               rts

; -------------------------------------------------------------------------------------------------

sprite_dma      ; Copy interleaved sprite data to OAM. Called by nmi, init3.
                copy #$00, oam_addr
                copy #>sprite_data, oam_dma
                rts

; -------------------------------------------------------------------------------------------------

flush_vram_buf  ; Move as many blocks as possible from vram_buffer to VRAM.
                ; Each block in vram_buffer: data size, address high, address low, data.
                ; Called by nmi, vram_blk_to_buf.

                copy #100, vram_budget  ; maximum sum of (totalDataSize + blocks * 5) to copy

                ldy vram_buf_rd_pos
--              cpy vram_buf_wr_pos  ; exit if all blocks copied
                beq +
                lda vram_buffer,y    ; get data size; compute cost of copying it (dataSize + 5)
                tax
                add #5
                sta vram_block_cost
                lda vram_budget      ; remove cost from budget; exit if out of budget
                sub vram_block_cost
                bcc +
                sta vram_budget
                lda vram_buf_free    ; add total data size to number of free bytes
                clc
                adc vram_buffer,y
                adc #3
                sta vram_buf_free
                iny                  ; get and set VRAM address
                lda vram_buffer,y
                sta ppu_addr
                iny
                lda vram_buffer,y
                sta ppu_addr
                iny                  ; copy block data to VRAM
-               lda vram_buffer,y
                sta ppu_data
                iny
                dex
                bne -
                jmp --               ; next block

+               sty vram_buf_rd_pos  ; all blocks copied or out of budget
                rts

; -------------------------------------------------------------------------------------------------

vram_blk_to_buf ; Copy current VRAM block to VRAM buffer.
                ; Called by graphic_nybble_to_vram_buffer, update_attr_blk, init3, animate_color,
                ; hilite_inp_row.

                lda vram_block+0     ; total size of block -> temp1;
                add #3               ; does block fit in VRAM buffer?
                sta temp1
                lda vram_buf_free
                cmp temp1
                bcs +

                lda skip_nmi         ; does not fit;
                beq vram_blk_to_buf  ; if NMI is being skipped, flush VRAM buffer and restart sub;
                jsr flush_vram_buf   ; otherwise just restart sub
                jmp vram_blk_to_buf

+               sub temp1            ; fits;
                sta vram_buf_free    ; subtract total block size from free space
                ldx #0               ; copy VRAM block to VRAM buffer
                ldy vram_buf_wr_pos  ; (X/Y = source/destination index)
-               lda vram_block,x
                sta vram_buffer,y
                inx
                iny
                cpx temp1
                bne -
                sty vram_buf_wr_pos
                rts

; -------------------------------------------------------------------------------------------------

draw_metaspr_gr ; Draw a graphic (e.g. the hand cursor) as a metasprite.
                ; In: A = graphic id (see graphix_offsets), out: A = index to metaspr_indexes.
                ; Called by init3.

                sta graphic_id        ; set pointer to address of graphic
                jsr set_graphix_ptr

                ldy #0                ; get width and height
                lda (graphics_ptr),y
                sta metaspr_width
                tax
                iny
                lda (graphics_ptr),y
                sta metaspr_height

                jsr multiply         ; width * height (A * X) -> A, grafic_dataleft
                sta grafic_dataleft

                add #2               ; total size of graphic -> A

                ; find first free byte in metaspr_indexes;
                ; save to metaspr_index and X;
                ; add total size of graphic to all following bytes in metaspr_indexes
                jsr findfreemetaspr  ; A = value to add
                sta metaspr_index
                tax

                lda metaspr_indexes,x  ; index to metasprites -> X
                tax

                lda metaspr_width   ; save width and height of graphic to metasprite data
                sta metasprites,x
                lda metaspr_height
                sta metasprites+1,x

                inx  ; advance to indexes to individual sprites
                inx

                ldy #255                 ; assign grafic_dataleft free sprites to metasprite,
-               iny                      ; starting from first free sprite
                lda sprite_data_atr,y
                bne -
                tya
                sta metasprites,x
                inx
                dec grafic_dataleft
                bne -

                ldx metaspr_index    ; set up tiles and attributes for individual sprites
                lda graphic_id
                jsr init_metasprite  ; A = id, X = index to metaspr_indexes

                ldx metaspr_index    ; set up positions for individual sprites
                jsr update_metaspr   ; X = metasprite index

                lda metaspr_index    ; return metasprite index
                rts

; -------------------------------------------------------------------------------------------------

set_graphix_ptr ; Set graphics pointer.
                ; In: A = graphic id (see graphix_offsets), out: graphics_ptr = address of graphic.
                ; Called by draw_bg_graphic, draw_metaspr_gr, init_metasprite.

                sta temp1  ; id

                copy #<graphix_offsets, graphics_ptr+0  ; set pointer
                copy #>graphix_offsets, graphics_ptr+1

                lda temp1              ; word(graphics_ptr) += graphic_id >> 7
                bpl +
                inc graphics_ptr+1     ; never accessed
+               asl                    ; offset to offset
                tay
                lda (graphics_ptr),y   ; offset to graphic
                pha
                iny
                lda (graphics_ptr),y
                add #<graphix_offsets  ; address of graphic
                sta graphics_ptr+0
                pla
                adc #>graphix_offsets
                sta graphics_ptr+1

                rts

; -------------------------------------------------------------------------------------------------

update_metaspr  ; Update metasprite's position to its individual sprites' positions.
                ; In:
                ;   X: metasprite index (0 = hand cursor, 1 = revolving cursor, 2 = flying letter)
                ;   metasprite_x/metasprite_y: position of metasprite
                ; Called by draw_metaspr_gr, do_every_frame, update_revolv_cursor, input_effects,
                ; move_flying_ltr.

                lda metaspr_indexes,x  ; index to metasprite info
                tax

                lda metasprites,x    ; get metasprite width/height
                sta metaspr_width
                lda metasprites+1,x
                sta metaspr_height

                inx  ; start of target sprite indexes
                inx

                copy metasprite_y, sprite_y+0  ; init target sprites' Y position
                copy always_zero2, sprite_y+1

update_loop_y   lda sprite_y+1       ; hide row if beyond bottom edge of screen
                bne hide_sprite_row

                copy metaspr_width, nybbles_left_x  ; init loop counter
                copy metasprite_x, sprite_x+0       ; init target sprites' X position
                copy always_zero1, sprite_x+1

update_loop_x   lda sprite_x+1   ; hide if beyond right edge of screen
                bne hide_sprite

                lda metasprites,x  ; index to target sprite
                tay

                lda sprite_data_til,y  ; hide if target tile = 0
                beq hide_sprite

                lda sprite_x+0       ; set target sprite position
                sta sprite_data_x,y
                lda sprite_y+0
                sta sprite_data_y,y

                inx
                jmp next_sprite

hide_sprite     lda metasprites,x
                tay
                lda #255
                sta sprite_data_y,y
                inx

next_sprite     lda sprite_x+0
                add #8
                sta sprite_x+0
                lda sprite_x+1
                adc #0
                sta sprite_x+1

                dec nybbles_left_x
                bne update_loop_x

next_row        lda sprite_y+0
                add #8
                sta sprite_y+0
                lda sprite_y+1
                adc #0
                sta sprite_y+1

                dec metaspr_height
                bne update_loop_y

                rts

hide_sprite_row copy metaspr_width, nybbles_left_x  ; hide row of sprites
-               lda metasprites,x
                tay
                lda #255
                sta sprite_data_y,y
                inx
                dec nybbles_left_x
                bne -
                jmp next_row

; -------------------------------------------------------------------------------------------------

init_metasprite ; Set up tiles and attributes for individual sprites of a metasprite.
                ; In: A = graphic id (see graphix_offsets), X = index to metaspr_indexes.
                ; Called by draw_metaspr_gr, input_effects.

                sta graphic_id

                lda metaspr_indexes,x  ; index to metasprites
                add #2
                pha

                lda graphic_id       ; start reading graphic (A = id)
                jsr set_graphix_ptr

                ldy #0                ; get dimensions of graphic
                lda (graphics_ptr),y
                sta metaspr_width
                iny
                lda (graphics_ptr),y
                sta metaspr_height

                lda #0                 ; 2 -> nybble_offs
                ora #%00011100
                ldx unused1
                sta always_00011100
                lda #2
                ldy #1
                sty always_one
                sta nybble_offs
                copy #0, always_zero3

                pla  ; index to metasprites -> X
                tax

init_loop_y     ; copy all rows of graphics data to target sprites
                copy metaspr_width, nybbles_left_x  ; inner loop counter

init_loop_x     ; copy one row of graphics data to target sprites
                lda nybble_offs         ; graphics data byte -> temp1
                lsr
                add #1
                tay
                lda (graphics_ptr),y
                sta temp1

                lda nybble_offs  ; push high nybble if even offset, else low
                and #%00000001
                beq +
                lda temp1
                and #%00001111
                jmp ++
+               lda temp1
                lsr
                lsr
                lsr
                lsr
++              pha

                lda metasprites,x        ; nybble -> target sprite tile
                tay
                pla
                sta sprite_data_til,y
                lda always_00011100     ; %00011100 -> target sprite attribute
                sta sprite_data_atr,y

                inx  ; next target sprite

                lda nybble_offs  ; increment
                add always_one
                sta nybble_offs

                dec nybbles_left_x
                bne init_loop_x

                lda nybble_offs  ; do nothing
                add always_zero3
                sta nybble_offs

                dec metaspr_height
                bne init_loop_y

                rts

; -------------------------------------------------------------------------------------------------

convert_sprites ; Convert planar sprite data to interleaved.
                ; Sprites with attribute byte $00 will be hidden.
                ; Called by do_every_frame.

                ; ascending order every 2nd frame, descending order every 2nd frame
                lda odd_frame_flag1
                eor #%00000001
                sta odd_frame_flag1
                bne ++

                ; descending order: planar sprites 61-0 -> interleaved sprites 2-63
                ldy #(2*4)             ; target offset
                ldx #61                ; source offset
-               lda sprite_data_atr,x
                beq +                  ; hide if attributes are $00
                jsr convert_sprite     ; A = attribute byte, X = src index, Y = dst index
                dex
                bpl -
                rts
+               jsr hide_sprite_sub    ; Y = index
                dex
                bpl -
                rts  ; never accessed

++              ; ascending order: planar sprites 0-61 -> interleaved sprites 2-63
                ldy #(2*4)             ; target offset
                ldx #0                 ; source offset
-               lda sprite_data_atr,x
                beq +                  ; hide if attributes are $00
                jsr convert_sprite     ; A = attribute byte, X = src index, Y = dst index
                inx
                cpx #62
                bne -
                rts
+               jsr hide_sprite_sub    ; Y = index
                inx
                cpx #62
                bne -
                rts

convert_sprite  ; Convert one non-hidden sprite from planar to interleaved.
                ; In: A = attribute byte, X/Y = source/target index. Out: Y += 4
                ; Called by convert_sprites.

                sta sprite_data+2,y
                lda sprite_data_y,x
                sta sprite_data+0,y
                lda sprite_data_til,x
                sta sprite_data+1,y
                lda sprite_data_x,x
                sta sprite_data+3,y
                iny
                iny
                iny
                iny
                rts

hide_sprite_sub ; Hide a sprite in sprite_data. ; In: Y = index, out: Y += 4.
                ; Called by convert_sprites.

                lda #255
                sta sprite_data,y
                iny
                iny
                iny
                iny
                rts

; -------------------------------------------------------------------------------------------------

update_attr_blk ; Change the value of an attribute block (2 bits) within one attribute byte,
                ; preserving the other bits.
                ; In:
                ;     vram_block_x:        X position of attribute block (0-15)
                ;     vram_block_y:        Y position of attribute block (0-14)
                ;     attr_fill_byte: value of new block (which 2 bits are read depends on
                ;                          vram_block_x and vram_block_y)
                ; Called by update_atr_byte, fill_atrblkrows.

                lda vram_block_y  ; position of attribute block within attribute byte (0-3) -> Y
                and #%00000001
                asl
                sta temp1
                lda vram_block_x
                and #%00000001
                ora temp1
                tay

                lda vram_block_y  ; position of byte within Attribute Table (0-63) -> A, X
                asl
                asl
                and #%11111000
                sta temp1
                lda vram_block_x
                lsr
                add temp1
                tax

                ora #<vram_attrtable0  ; vram_attrtable0 + A -> VRAM block target address
                sta vram_block+2
                copy #>vram_attrtable0, vram_block+1

                copy #1, vram_block+0  ; size of data to copy: one byte

                lda attr_blk_masks,y  ; combine old and new bits of attribute byte:
                and attr_fill_byte    ; (newAttributeByte & bitmaskDeterminedByY)
                sta temp1             ; | (attr_data_copy,x & ~bitmaskDeterminedByY)
                lda attr_blk_masks,y  ; -> attr_data_copy,x and vram_block+3 (start of data)
                eor #%11111111
                and attr_data_copy,x
                ora temp1
                sta attr_data_copy,x
                sta vram_block+3

                jmp vram_blk_to_buf  ; ends with RTS

attr_blk_masks  ; Attribute block bitmasks. Read by: update_attr_blk.
                db %00000011, %00001100, %00110000, %11000000

; -------------------------------------------------------------------------------------------------

initial_palette ; Initial palette. 32 bytes. Read by init3.
                ; background
                db color_bg,      color_unused1, color_unused1, color_keyboard
                db color_unused1, color_unused1, color_unused1, color_anim_init
                db color_unused1, color_unused1, color_unused1, color_hilite
                db color_unused1, color_unused1, color_unused1, color_inputarea
                ; sprites
                db color_bg,      color_unused1, color_unused1, color_multiuse1
                db color_unused1, color_unused1, color_unused1, color_hand1
                db color_unused1, color_unused1, color_unused1, color_multiuse2
                db color_unused1, color_unused1, color_unused1, color_unused2

init3           ; Part 3/3 of initialization. Called by init2.

                copy #1, skip_nmi

                lda #>vram_nametable0   ; clear first Name & Attribute Table
                sta vram_addr_hi        ; (fill VRAM $2000-$23ff with $00)
                sta ppu_addr
                lda #<vram_nametable0
                sta ppu_addr
                ldx #4
                tay
-               sta ppu_data
                dey
                bne -
                dex
                bne -

                jsr init_background

                ldx #0                 ; clear metaspr_indexes (why? we just cleared all RAM)
                lda #$00
-               sta metaspr_indexes,x
                inx
                cpx #50
                bne -

                jsr sprite_dma

                lda #14               ; init hand cursor position
                sta_abs hand_x_pixel
                sta metasprite_x
                lda #60
                sta_abs hand_y_pixel
                sta metasprite_y

                copy_abs #128, revolv_x  ; init revolving cursor position
                copy_abs #150, revolv_y

                lda #20                  ; draw hand cursor (A = id)
                jsr draw_metaspr_gr
                sta_abs hand_metasprite
                lda #2                   ; draw revolving cursor (A = id)
                jsr draw_metaspr_gr
                sta_abs revolv_metaspr

                copy #255, metasprite_y

                lda #5                  ; init flying letter graphic
                jsr draw_metaspr_gr     ; (A = id = "P" but doesn't matter)
                sta_abs flying_metaspr

                ldx #(20-1)                  ; init sprite attribute data
-               lda initial_spr_attr_data,x
                sta sprite_data_atr,x
                dex
                bpl -

                copy #%00001001, sound_ctrl  ; enable pulse 1 and noise channels

                ; copy initial palette to VRAM block
                copy #32,            vram_block+0  ; data size
                copy #>vram_palette, vram_block+1  ; address high
                copy #<vram_palette, vram_block+2  ; address low
                ldy #0                             ; data
-               lda initial_palette,y
                sta vram_block+3,y
                iny
                cpy #32
                bne -
                jsr vram_blk_to_buf

                copy #0, skip_nmi
                jsr wait_nmi_done

                lda ppu_mask_mirror  ; show sprites & background
                ora #%00011000
                sta ppu_mask_mirror
                sta ppu_mask

                ; fall through to main_loop

main_loop       jsr do_every_frame  ; the main loop
                jsr wait_nmi_done
                jmp main_loop

wait_nmi_done   copy_abs #0, nmi_done  ; Wait until the NMI routine has run.
-               lda_abs nmi_done       ; Called by init3, main_loop.
                beq -
                rts

do_every_frame  jsr read_joypads          ; Stuff to do every time the main loop runs.
                jsr move_hand             ; Called by main_loop.
                jsr update_revolv_cursor
                jsr move_particles
                jsr move_flying_ltr
                jsr animate_color
                jsr check_button_a
                jsr check_button_b
                jsr check_sel_start

                lda_abs hand_x_pixel     ; update hand cursor metasprite
                sta metasprite_x
                lda_abs hand_y_pixel
                sta metasprite_y
                ldx_abs hand_metasprite
                jsr update_metaspr         ; X = metasprite index

                jsr convert_sprites
                rts

; -------------------------------------------------------------------------------------------------

check_arrows    ; Called by move_hand.

                lda_abs hand_y_spd_ptr+1  ; if Y speed pointer set, skip checking vertical arrows
                bne check_horiz

                lda joypad1_status   ; up pressed?
                and #pad_up
                beq check_down
                lda #pad_up          ; move hand if possible
                jsr set_hand_target  ; A = direction, out: A = success
                beq check_down

                copy_abs #<hand_spds_neg, hand_y_spd_ptr+0  ; negative hand speeds -> pointer
                copy_abs #>hand_spds_neg, hand_y_spd_ptr+1
                lda #pad_up                                 ; if moved from 3rd line to 2nd,
                jsr between_keyb_and_input_area             ; 32 -> hand_y_spd_offs,
                beq +                                       ; else 0 -> hand_y_spd_offs
                lda #32
                jmp ++
+               lda #0
++              sta_abs hand_y_spd_offs
                copy_abs #pad_up, last_y_inp_acc

                jmp check_horiz

check_down      lda joypad1_status  ; down pressed?
                and #pad_down
                beq check_horiz
                lda #pad_down        ; move hand if possible
                jsr set_hand_target  ; A = direction, out: A = success
                beq check_horiz

                copy_abs #<hand_spds_pos, hand_y_spd_ptr+0  ; positive hand speeds -> pointer
                copy_abs #>hand_spds_pos, hand_y_spd_ptr+1
                lda #pad_down                               ; if hand moved from 2nd to 3rd line,
                jsr between_keyb_and_input_area             ; 32 -> hand_y_spd_offs,
                beq +                                       ; else 0 -> hand_y_spd_offs
                lda #32
                jmp ++
+               lda #0
++              sta_abs hand_y_spd_offs
                copy_abs #pad_down, last_y_inp_acc

check_horiz     lda_abs hand_x_spd_ptr+1  ; if X speed pointer set, skip checking horizontal arrows
                bne arrows_checked

                lda joypad1_status   ; left pressed?
                and #pad_left
                beq check_right
                lda #pad_left        ; move hand if possible
                jsr set_hand_target  ; A = direction, out: A = success
                beq check_right

                copy_abs #<hand_spds_neg, hand_x_spd_ptr+0  ; negative hand speeds -> pointer
                copy_abs #>hand_spds_neg, hand_x_spd_ptr+1
                copy_abs #0,        hand_x_spd_offs
                copy_abs #pad_left, last_x_inp_acc

                jmp arrows_checked

check_right     lda joypad1_status   ; right pressed?
                and #pad_right
                beq arrows_checked
                lda #pad_right       ; move hand if possible
                jsr set_hand_target  ; A = direction, out: A = success
                beq arrows_checked

                copy_abs #<hand_spds_pos, hand_x_spd_ptr+0  ; positive hand speeds -> pointer
                copy_abs #>hand_spds_pos, hand_x_spd_ptr+1
                copy_abs #0,         hand_x_spd_offs
                copy_abs #pad_right, last_x_inp_acc

arrows_checked  rts

; -------------------------------------------------------------------------------------------------

findfreemetaspr ; Find the first free byte in metasprite indexes and add the specified value to all
                ; following bytes.
                ; In: A: the value to add
                ; Out: A: index to the first free byte
                ; Called by draw_metaspr_gr.

                sta temp1                ; value to add

                ldx #255                 ; find first free byte (same as the following byte)
-               inx
                lda metaspr_indexes+1,x
                cmp metaspr_indexes,x
                bne -

                stx first_free_byte  ; index to first free byte
                sta unused2          ; never used anywhere

                inx                    ; add value to all following bytes
-               lda temp1
                clc
                adc metaspr_indexes,x
                sta metaspr_indexes,x
                inx
                cpx #50
                bne -

                lda first_free_byte
                rts

; -------------------------------------------------------------------------------------------------

init_background ; Initialize background graphics. Called by init3.

                copy #0, scroll_x_mirror  ; center background graphics vertically
                copy #4, scroll_y_mirror

                ldaxy #1, #5, #3     ; the Game Genie logo (id, X, Y)
                jsr draw_bg_graphic

                ; draw virtual keyboard (2*8 letters)
                copy #3, keybd_graphic    ; "A"
                copy #4, keybd_graphic_x
                copy #8, keybd_graphic_y
-               ldaxy keybd_graphic, keybd_graphic_x, keybd_graphic_y  ; draw letter (id, X, Y)
                jsr draw_bg_graphic
                lda keybd_graphic_x       ; increment X position
                add #4
                sta keybd_graphic_x
                cmp #(9*4)                ; if at end of line, move to start of next line
                bne +
                copy #4, keybd_graphic_x
                lda keybd_graphic_y
                add #4
                sta keybd_graphic_y
+               inc keybd_graphic         ; loop until last letter (id 18 = "N") has been drawn
                lda keybd_graphic
                cmp #(18+1)
                bne -

                ; draw input area (3*8 dashes)
                copy #3,  keybd_graphic    ; "A" (note: only used as loop counter)
                copy #4,  keybd_graphic_x
                copy #18, keybd_graphic_y
-               ldaxy #19, keybd_graphic_x, keybd_graphic_y  ; draw dash (id, X, Y)
                jsr draw_bg_graphic
                lda keybd_graphic_x       ; increment X position
                add #4
                sta keybd_graphic_x
                cmp #(9*4)                ; if at end of line, move to start of next line
                bne +
                copy #4, keybd_graphic_x
                lda keybd_graphic_y
                add #4
                sta keybd_graphic_y
+               inc keybd_graphic         ; loop until last dash has been drawn
                lda keybd_graphic
                cmp #(3+24)
                bne -

                ; set attribute table data
                ldaxy #%01010101, #4, #0   ; "Game Genie" logo (byte, rows, first row)
                jsr fill_atrblkrows
                ldaxy #%00000000, #4, #4   ; virtual keyboard
                jsr fill_atrblkrows
                ldaxy #%10101010, #2, #9   ; 1st code
                jsr fill_atrblkrows
                ldaxy #%11111111, #4, #11  ; 2nd and 3rd code
                jsr fill_atrblkrows

                ldx #0               ; highlight top left letter on virtual keyboard
                ldy #0
                jsr hilite_atr_byte

                copy_abs #2, revolv_y_letr2  ; ? (no visible effect)
                rts

; -------------------------------------------------------------------------------------------------

move_hand       ; Called by do_every_frame.

                lda odd_frame_flag2  ; check arrows every 2nd frame
                eor #%00000001
                sta odd_frame_flag2
                bne +
                jsr check_arrows

+               lda hand_x_spd_ptr+1  ; horizontal movement
                beq move_hand_vert    ; not moving horizontally

                ldy hand_x_spd_offs     ; get speed (if offset modulo 16 = 15, terminator)
                lda (hand_x_spd_ptr),y
                cmp #$80
                bne +

                copy #0, hand_x_spd_ptr+1  ; stop hand by resetting pointer
                jmp ++

+               ldx #hand_x_pixel             ; no terminator
                jsr add_hand_spd_to_position  ; A = speed, X = address of position; out: Z
                beq move_hand_vert            ; every 2nd frame, skip rest of horizontal movement

++              tya             ; hand_x_spd_offs
                and #%00001111  ; get position on current line
                cmp #7
                bne +

                jsr update_hand_letr_position  ; hand_x_spd_offs modulo 16 = 7
                lda joypad1_status
                and last_x_inp_acc
                beq +

                lda last_x_inp_acc
                jsr set_hand_target  ; A = direction, out: A = success
                beq +

                ldy #(16-1)

+               iny
                sty hand_x_spd_offs

move_hand_vert  lda hand_y_spd_ptr+1  ; move vertically
                beq move_hand_exit    ; not moving vertically

                ldy hand_y_spd_offs     ; get speed (if offset modulo 16 = 15, terminator)
                lda (hand_y_spd_ptr),y
                cmp #$80
                bne +

                copy #0, hand_y_spd_ptr+1  ; stop hand by resetting pointer
                jmp ++

+               ldx #hand_y_pixel             ; no terminator
                jsr add_hand_spd_to_position  ; A = speed, X = address of position; out: Z
                beq move_hand_exit            ; every 2nd frame, skip rest of vertical movement

++              tya             ; hand_y_spd_offs
                and #%00001111  ; get position on current line
                cmp #7
                bne ++

                jsr update_hand_letr_position  ; hand_y_spd_offs modulo 16 = 7
                lda joypad1_status
                and last_y_inp_acc
                beq ++

                lda last_y_inp_acc
                jsr set_hand_target  ; A = direction, out: A = success
                beq ++

                lda last_y_inp_acc               ; success
                jsr between_keyb_and_input_area  ; A = direction
                beq +
                ldy #(3*16-1)
                jmp ++
+               ldy #(16-1)

++              iny
                sty hand_y_spd_offs

move_hand_exit  rts

                ; Hand cursor speeds. 8*16 bytes. $80 = terminator.
                ; Read indirectly by move_hand using hand_x_spd_ptr/hand_y_spd_ptr.
hand_spds_pos   ; positive speeds
                hex 01 01 02 02 03 03 04 04 03 03 02 02 02 01 ff 80  ; sum=32
                hex 05 04 04 03 03 04 05 04 03 03 02 02 02 01 ff 80  ; sum=44
                hex 02 03 04 05 06 06 06 04 03 03 02 02 02 01 ff 80  ; sum=48
                hex 05 06 07 08 07 06 05 04 03 03 02 02 02 01 ff 80  ; sum=60
hand_spds_neg   ; negative speeds (same as above but negated in two's complement)
                hex ff ff fe fe fd fd fc fc fd fd fe fe fe ff 01 80
                hex fb fc fc fd fd fc fb fc fd fd fe fe fe ff 01 80
                hex fe fd fc fb fa fa fa fc fd fd fe fe fe ff 01 80
                hex fb fa f9 f8 f9 fa fb fc fd fd fe fe fe ff 01 80

add_hand_spd_to_position
                ; Add hand cursor speed to position.
                ; In: A = speed (horizontal/vertical),
                ; X = address of position (hand_x_pixel/hand_y_pixel).
                ; Out: zero flag reflects odd_frame_flag2.
                ; Called by move_hand.

                pha  ; divide speed by 2, round down (toward -infinity)
                asl
                pla
                ror

                dec odd_frame_flag2  ; carry: 0 or LSB of original speed
                beq +
                clc
+               adc $00,x  ; add new speed and C to hand cursor position
                sta $00,x

                inc odd_frame_flag2
                rts

; -------------------------------------------------------------------------------------------------

between_keyb_and_input_area
                ; Is hand cursor moving between virtual keyboard and input area?
                ; In: A = direction (pad_up/pad_down), hand_y_letr_trg. Out: A (0 = no, 1 = yes).
                ; Called by check_arrows, move_hand.

                cmp #pad_down
                bne +
                lda hand_y_letr_trg
                cmp #2
                bne ++
                lda #1
                rts

+               cmp #pad_up
                bne ++
                lda hand_y_letr_trg
                cmp #1
                bne ++
                lda #1
                rts

++              lda #0
                rts

; -------------------------------------------------------------------------------------------------

set_hand_target ; Move hand cursor to specified direction if possible.
                ; In: A = direction (pad_right/pad_left/pad_down/pad_up)
                ; Out: hand_x_letr_trg/hand_y_letr_trg, A = success (1=yes, 0=no).
                ; Called by check_arrows, move_hand.

                cmp #pad_right       ; check right
                bne +
                lda hand_x_letr_trg  ; try to move
                cmp #7
                beq hand_move_fail
                inc hand_x_letr_trg
                jmp hand_move_succ

+               cmp #pad_left        ; check left
                bne +
                lda hand_x_letr_trg  ; try to move
                beq hand_move_fail
                dec hand_x_letr_trg
                jmp hand_move_succ

+               cmp #pad_down        ; check down
                bne +
                lda hand_y_letr_trg  ; try to move
                cmp #4
                beq hand_move_fail
                inc hand_y_letr_trg
                jmp hand_move_succ

+               cmp #pad_up          ; check up
                bne hand_move_succ
                lda hand_y_letr_trg  ; try to move
                beq hand_move_fail
                dec hand_y_letr_trg
                jmp hand_move_succ

hand_move_succ  lda #1  ; success
                rts
hand_move_fail  lda #0
                rts

; -------------------------------------------------------------------------------------------------

hilite_atr_byte ; Highlight an attribute byte. An alternate entry point for update_atr_byte.
                ; Called by: (see update_atr_byte).
                lda #%10101010

update_atr_byte ; Update an Attribute Table byte (2*2 attribute blocks).
                ; In: A = new attribute table byte,
                ; X/Y = position (X = 0-7; Y = 0-1 if virtual keyboard, 2-4 if input area).
                ; Alternate entry points: hilite_atr_byte, clear_atr_byte.
                ; Called by (incl. alternate entry points) init_background,
                ; update_hand_letr_position, hilite_inp_area.

                sta attr_fill_byte
                txa                  ; get attribute block X
                asl
                sta vram_block_x
                tya                  ; get attribute block Y
                asl
                add #4
                cmp #8
                bcc +                ; on virtual keyboard
                add #1
+               sta vram_block_y     ; update 2*2 attribute blocks
                jsr update_attr_blk
                inc vram_block_x
                jsr update_attr_blk
                inc vram_block_y
                jsr update_attr_blk
                dec vram_block_x
                jmp update_attr_blk  ; ends with RTS

clear_atr_byte  ; Clear an attribute type. An alternate entry point for update_atr_byte.
                ; Called by: (see update_atr_byte).
                lda #%00000000
                jmp update_atr_byte

; -------------------------------------------------------------------------------------------------

update_hand_letr_position
                ; Update letter position of hand cursor. Called by move_hand.

                tya  ; push Y
                pha

                ldx hand_x_keyboard  ; un-highlight the letter the cursor is leaving
                ldy hand_y_keyboard
                jsr clear_atr_byte
                lda hand_y_letr_trg  ; where's the target letter (virtual keyboard or input area)?
                cmp #2
                bcs +

                ; target letter is on virtual keyboard
                sta hand_y_keyboard  ; update actual hand position and last position on keyboard
                sta hand_y_letr
                lda hand_x_letr_trg
                sta hand_x_keyboard
                sta hand_x_letr
                ldx hand_x_keyboard  ; highlight the letter the cursor is on
                ldy hand_y_keyboard
                jsr hilite_atr_byte
                pla                  ; pull Y
                tay
                rts

+               ; target letter is on input area
                sta hand_y_letr                    ; update actual hand position
                copy hand_x_letr_trg, hand_x_letr
                pla  ; pull Y
                tay
                rts

; -------------------------------------------------------------------------------------------------

initial_spr_attr_data
                ; Initial sprite attribute data. Bytes are in reverse order.
                ; Bits: VHBUUUPP (Vflip, Hflip, Behind bg, Unimplemented, Palette).
                ; Read by init3.
                db %00011001, %00011001, %00011001, %00011001, %00011010  ; 19-15
                db %00011001, %00011001, %00011001, %00011001, %00011010  ; 14-10
                db %00011001, %00011001, %00011001, %00011001, %00011010  ;  9- 5
                db %00011001, %00011001, %00011001, %00011001, %00011010  ;  4- 0

; -------------------------------------------------------------------------------------------------

update_revolv_cursor_attributes
                ; Change attributes of revolving cursor's sprites (20-23).
                ; Called by update_revolv_cursor.

                lda sprite_data_atr+20  ; change subpalette between 0 and 2
                cmp #%00011010          ; (why not just EOR?)
                bne +
                lda #%00011000
                jmp ++
+               lda #%00011010

++              ldx_abs revolv_phase  ; put sprite behind background if phase is 0-6
                cpx #7
                bcs +
                ora #%00100000

+               sta sprite_data_atr+20
                sta sprite_data_atr+21
                sta sprite_data_atr+22
                sta sprite_data_atr+23
                rts

; -------------------------------------------------------------------------------------------------

letter_input    ; Button A went from off to on and hand cursor is on virtual keyboard.
                ; Called by check_button_a.

                lda hand_y_letr     ; push id of graphic to draw
                asl                 ; (hand_y_letr * 8 + hand_x_letr + 3; see graphix_offsets)
                asl
                asl
                add hand_x_letr
                adc #3
                pha
                lda revolv_y_letr1  ; Y position of revolving cursor in tiles -> Y
                asl                 ; (revolv_y_letr1 * 4 + 18)
                asl
                add #18
                tay
                lda revolv_x_letr1  ; X position of revolving cursor in tiles -> X
                asl                 ; (revolv_x_letr1 * 4 + 4)
                asl
                add #4
                tax

                pla
                pha
                jsr draw_bg_graphic  ; draw graphic (A = id, X/Y = X/Y position)
                jsr input_effects
                pla                  ; pull id
                jsr save_letter

                jmp +               ; why?
+               inc revolv_x_letr1  ; increment cursor X position (may be undone later)

                lda revolv_x_letr1  ; 6 letters on current line?
                cmp #6
                bne check_8letters

                lda revolv_y_letr1  ; 6 letters on current line;
                asl                 ; if 3rd one is A/P/Z/L/G/I/T/Y, code is complete
                asl
                asl
                add #(3-1)
                tax
                lda entered_letrs,x
                sub #3
                and #%00000001
                beq code_complete

                lda revolv_x_letr1  ; always 6; causes next branch to be always taken

check_8letters  cmp #8              ; 8 letters on current line?
                bne letr_input_end

code_complete   lda revolv_y_letr1       ; if not on last line, move to start of next line,
                cmp #2                   ; else undo cursor X increment
                beq +
                copy #0, revolv_x_letr1
                inc revolv_y_letr1
                jmp letr_input_end
+               dec revolv_x_letr1

letr_input_end  jmp hilite_inp_area  ; move highlight to new letter and RTS

; -------------------------------------------------------------------------------------------------

check_button_a  ; If status of button A has changed from off to on, input letter (if hand cursor on
                ; keyboard) or move revolving cursor (if hand cursor on input area).
                ; Called by do_every_frame.

                lda joypad1_status
                and #pad_a
                cmp prev_btn_a_stat
                bne +
                rts
+               sta prev_btn_a_stat
                cmp #pad_a
                beq +
                rts
+               lda hand_y_letr
                cmp #2
                bcs +
                jmp letter_input                 ; hand cursor is on keyboard   (sub ends with RTS)
+               jmp move_revolv_cursor_manually  ; hand cursor is on input area (sub ends with RTS)

; -------------------------------------------------------------------------------------------------

check_button_b  ; Called by do_every_frame.

                lda joypad1_status   ; if button B has gone from off to on, continue, else exit
                and #pad_b
                cmp prev_btn_b_stat
                bne +
                rts
+               sta prev_btn_b_stat
                cmp #pad_b
                beq +
                rts
+               jmp +                ; why?

+               lda revolv_x_letr1          ; continue depending on where revolving cursor is
                beq move_backwards_or_exit  ; on 1st column
                lda revolv_y_letr1
                cmp #2
                bne exit_if_no_letr         ; on 1st/2nd code, 2nd-8th column

                lda entered_letrs+2*8+2  ; non-first column, last code;
                sub #3                   ; maximum length of last code minus one -> A
                and #%00000001           ; (7 if 3rd letter is E/O/X/U/K/S/V/N, else 5)
                bne +
                lda #(6-1)
                bne ++
+               lda #(8-1)

++              cmp revolv_x_letr1
                bne exit_if_no_letr  ; cursor not on last possible letter of last code

                ; cursor is on last possible letter of last code (revolv_x_letr1 = 5/7);
                tax                      ; is code of maximum length?
                lda entered_letrs+2*8,x
                bne erase_letter         ; last code is maximum length (cursor is on last letter)
                ; 3rd code is maximum length minus one (cursor is on following position)

exit_if_no_letr ; cursor is on 2nd-8th column, except on last possible letter of 3rd code
                jsr get_letter      ; is letter zero? -> zero flag
                bne erase_letr_end

move_backwards_or_exit
                ; cursor is on a dash and/or on 1st column;
                ; if on first letter of first code, exit, otherwise move cursor and erase letter
                lda revolv_x_letr1
                ora revolv_y_letr1
                beq erase_letr_end
                jsr move_revolv_cursor_backwards

erase_letter    ; get position of letter to erase in tiles, store to X&Y
                lda revolv_y_letr1
                asl
                asl
                add #18
                tay
                lda revolv_x_letr1
                asl
                asl
                add #4
                tax

                ; spawn particles, draw dash on letter, mark letter as empty
                jsr spawn_particls1
                lda #19
                jsr draw_bg_graphic  ; A = id, X = X, Y = Y
                lda #0
                jmp save_letter  ; ends with RTS

erase_letr_end  jmp move_revolv_cursor_backwards  ; why?

move_revolv_cursor_backwards
                ; Called by check_button_b.

                ; if column > 0:          move backwards
                ; if column = 0, row > 0: move to end of previous line
                ; if column = 0, row = 0: do nothing
                ;
                dec revolv_x_letr1
                lda revolv_x_letr1
                cmp #$ff
                bne ++
                lda revolv_y_letr1
                beq +
                copy #7, revolv_x_letr1
                dec revolv_y_letr1
                jmp ++
+               inc revolv_x_letr1
++              jsr fix_revolv_x         ; move left to position after last letter

                jmp hilite_inp_area  ; ends with RTS

; -------------------------------------------------------------------------------------------------

move_revolv_cursor_manually
                ; Move revolving cursor manually. Called by check_button_a.

                lda hand_y_letr                   ; move revolving cursor to hand cursor
                sub #2
                sta revolv_y_letr1
                copy hand_x_letr, revolv_x_letr1
                jsr fix_revolv_x

                txa  ; push X, Y (why Y?)
                pha
                tya
                pha

                jsr hilite_inp_area

                pla  ; pull Y, X
                tay
                pla
                tax

                rts

; -------------------------------------------------------------------------------------------------

update_revolv_cursor
                ; Update position, phase and attributes of revolving cursor.
                ; Called by do_every_frame.

                lda revolv_y_letr1   ; revolv_y_letr1 * 32 + 152 -> revolv_y_target
                asl
                asl
                asl
                asl
                asl
                add #152
                sta revolv_y_target

                lda revolv_x_letr1   ; revolv_x1 * 32 + 10 -> revolv_x_target
                asl
                asl
                asl
                asl
                asl
                add #10
                sta revolv_x_target

                copy_abs revolv_x,        revolv_pos     ; compute horizontal speed
                copy_abs revolv_x_target, revolv_target
                jsr compute_revolv_cursor_spd
                add #128
                sta revolv_trg_spd
                lda revolv_spd_x
                jsr accel_revolv                         ; A = speed
                sta revolv_spd_x

                copy_abs revolv_y,        revolv_pos     ; compute vertical speed
                copy_abs revolv_y_target, revolv_target
                jsr compute_revolv_cursor_spd
                add #128
                sta revolv_trg_spd
                lda revolv_spd_y
                jsr accel_revolv                         ; A = speed
                sta revolv_spd_y

                lda revolv_x      ; add horizontal speed to position
                add revolv_spd_x
                sta revolv_x

                lda revolv_y      ; add vertical speed to position
                add revolv_spd_y
                sta revolv_y

                ldx revolv_phase       ; adjust horizontal position by phase
                lda revolv_x
                clc
                adc revolv_xoffsets,x
                sta metasprite_x

                lda revolv_y           ; adjust vertical position by phase
                clc
                adc revolv_yoffsets,x
                sta metasprite_y

                inx               ; increment phase
                cpx #16
                bne +
                ldx #0
+               stx revolv_phase

                ldx revolv_metaspr  ; update metasprite position (X = metasprite index)
                jsr update_metaspr

                ; update attributes of sprites (ends with RTS)
                jmp update_revolv_cursor_attributes

                ; X/Y offsets of revolving cursor by phase. Read by update_revolv_cursor.
revolv_xoffsets ; Sine wave in two's complement. 17 values, -10...+10.
                db 0, 4, 7, 9, 10, 9, 7, 4
                db 0, 256-4, 256-7, 256-9, 256-10, 256-9, 256-7, 256-4
                db 0                                                    ; never accessed
revolv_yoffsets ; Inverted cosine wave in two's complement. 17 values, -10...+10.
                db 256-10, 256-9, 256-7, 256-4, 0, 4, 7, 9
                db 10, 9, 7, 4, 0, 256-4, 256-7, 256-9
                db 256-10                                   ; never accessed

compute_revolv_cursor_spd
                ; Compute speed (X or Y) for revolving cursor. In: revolv_pos, revolv_target.
                ; Out: A = speed in pixels per frame as a signed integer.
                ; Called by update_revolv_cursor.

                lda revolv_pos
                cmp revolv_target
                bcc ++
                bne +
                lda #0             ; pos = target; 0 -> A
                rts

+               lda revolv_target  ; pos > target; floor((target - pos) / 8) -> A;
                sub revolv_pos     ; note: SEC & ROR is used to shift right with sign extension
                ldx #3
-               sec
                ror
                dex
                bne -
                rts

++              lda revolv_target  ; pos < target; floor((target - pos) / 8) + 1 -> A;
                sub revolv_pos     ; note: ceil((target - pos) / 8), i.e.,
                ldx #3             ; floor((target - pos + 7) / 8),
-               lsr                ; would be symmetrical with pos > target
                dex
                bne -
                add #1
                rts

accel_revolv    ; Accelerate revolving cursor by -1/0/+1 toward target speed.
                ; In:
                ;   A              = current speed (X or Y, pixels/frame, two's complement)
                ;   revolv_trg_spd = target  speed (X or Y, pixels/frame, excess-128)
                ; Out: A = new speed. Called by update_revolv_cursor.

                eor #%10000000      ; convert into excess-128 (%00000000 = -128, %11111111 = +127)
                cmp revolv_trg_spd
                bcc +               ; current < target (increment)
                beq ++              ; current = target (no change)
                sbc #1              ; current > target (decrement)
                jmp ++
+               adc #1
++              eor #%10000000      ; convert back into two's-complement
                rts

; -------------------------------------------------------------------------------------------------

spawn_particls2 ; Spawn one of two sets of particles.
                ; In: parti_set_flag = which set. Out: parti_set_flag flipped.
                ; Called by spawn_particls1.

                lda parti_set_flag
                beq +

                copy #0, parti_set_flag   ; flip flag, spawn 2nd set
                ldx #48
                jsr spawn_particls3       ; X = index to sprite data
                copy #1, parti_set2timer  ; init timer (this one counts up)
                rts

+               copy #1, parti_set_flag   ; flip flag, spawn 1st set
                ldx #32
                jsr spawn_particls3       ; X = index to sprite data
                copy #1, parti_set1timer  ; init timer (this one counts up)
                rts

                ; Initial horizontal/vertical speeds of particles.
                ; Four waves in two's complement. 8 values per wave. Read by spawn_particls3.
init_prt_spds_x db 0, 6, 8, 6, 0, 256-6, 256-8, 256-6  ; sine, outer ring (-8...+8)
                db 0, 3, 4, 3, 0, 256-3, 256-4, 256-3  ; sine, inner ring (-4...+4)
init_prt_spds_y db 256-8, 256-6, 0, 6, 8, 6, 0, 256-6  ; inverted cosine, outer ring (-8...+8)
                db 256-4, 256-3, 0, 3, 4, 3, 0, 256-3  ; inverted cosine, inner ring (-4...+4)

spawn_particls3 ; Write initial particle data (16 sprites). In: X = first index to sprite data.
                ; Called by spawn_particls2.

                ldy #0                 ; Y/X = source/target index
-               lda init_prt_spds_x,y  ; horizontal position
                add parti_start_x
                sta sprite_data_x,x
                lda init_prt_spds_y,y  ; vertical position
                add parti_start_y
                sta sprite_data_y,x
                lda init_prt_spds_x,y  ; horizontal speed
                sta parti_spds_x,x
                lda init_prt_spds_y,y  ; vertical speed
                sta parti_spds_y,x
                lda #$01               ; tile
                sta sprite_data_til,x
                lda #%00011010         ; attribute (unused bits %110, palette %10)
                sta sprite_data_atr,x
                inx                    ; end of loop
                iny
                cpy #16
                bne -

                copy #%00001110, noise_regs+2  ; make noise
                copy #%00000100, noise_regs+3
                copy #%00100101, noise_regs+0

                copy #24, particle_timer  ; set timer (this one counts down)
                rts

; -------------------------------------------------------------------------------------------------

move_particles  ; Move particles. Called by do_every_frame.

                lda particle_timer             ; stop noise if particle_timer goes to 0
                beq +
                dec particle_timer
                bne +
                copy #%00110000, noise_regs+0

+               lda parti_set1timer  ; process 1st set of particles (from every 2nd explosion)
                beq move_parti_set2
                ldx #32
                inc parti_set1timer
                lda parti_set1timer
                cmp #24
                beq hide_parti_set1
                jsr move_particles2  ; A = timer, X = index to sprite data

move_parti_set2 lda parti_set2timer  ; process 2nd set of particles (from every 2nd explosion)
                beq +
                ldx #48
                inc parti_set2timer
                lda parti_set2timer
                cmp #24
                beq hide_parti_set2
                jsr move_particles2  ; A = timer, X = index to sprite data
+               rts

hide_parti_set1 copy #0, parti_set1timer
                jsr hide_parti_set
                jmp move_parti_set2

hide_parti_set2 copy #0, parti_set2timer
                jmp hide_parti_set        ; ends with RTS

move_particles2 ; Move particles, part 2.
                ; In: A = timer (parti_set1timer/parti_set2timer), X = sprite data index (32/48).
                ; Called by move_particles.

                and #%00000111  ; timer modulo 8 -> temp1
                sta temp1

                ldy #16
parti_loop      lda sprite_data_y,x  ; if sprite is hidden, skip it
                cmp #255
                beq particle_moved

                lda sprite_data_atr,x  ; change palette
                eor #%00000010
                sta sprite_data_atr,x

                lda parti_spds_x,x   ; detect X position underflow/overflow
                bpl +
                clc                  ; moving left
                adc sprite_data_x,x
                bcs ++
                jmp hide_particle
+               clc                  ; moving right
                adc sprite_data_x,x
                bcc ++

hide_particle   lda #$ff               ; X/Y position underflow/overflow; hide sprite and move on
                sta sprite_data_y,x
                lda #$00
                sta sprite_data_atr,x
                jmp particle_moved

++              sta sprite_data_x,x

                lda parti_spds_y,x   ; detect Y position underflow/overflow
                bpl +
                clc                  ; moving up
                adc sprite_data_y,x
                bcs ++
                jmp hide_particle
+               clc                  ; moving down
                adc sprite_data_y,x
                bcs hide_particle

++              sta sprite_data_y,x

                lda temp1           ; slow particle down every 8th frame (temp1 = timer modulo 8)
                bne particle_moved
                lda parti_spds_x,x
                jsr dec_abs_value   ; A: value to decrement
                sta parti_spds_x,x
                lda parti_spds_y,x
                jsr dec_abs_value   ; A: value to decrement
                sta parti_spds_y,x

particle_moved  inx
                dey
                bne parti_loop
                rts

dec_abs_value   ; If number is nonzero, decrement absolute value in two's complement.
                ; In: A = number to decrement; negative/zero flags must reflect it.
                ; Called by move_particles2.

                beq ++
                bpl +
                add #1
                rts
+               sub #1
++              rts

hide_parti_set  ; Hide one set of particles (16 sprites starting from X). Called by move_particles.

                ldy #16
-               lda #%00000000
                sta sprite_data_atr,x
                lda #255
                sta sprite_data_y,x
                inx
                dey
                bne -
                rts

; -------------------------------------------------------------------------------------------------

spawn_particls1 ; If revolving cursor is on a letter, continue to prepare particles.
                ; In: X/Y = position of letter in tiles. Called by check_button_b.

                tya                ; push Y
                pha
                asl                ; Y * 8 + 10 -> parti_start_y
                asl
                asl
                add #10
                sta parti_start_y

                txa                ; push X
                pha
                sub #4             ; (X - 4) * 8 + 13 -> parti_start_x
                asl
                asl
                asl
                add #13
                sta parti_start_x

                jsr get_letter       ; spawn only if on a letter (why not check earlier?)
                beq +
                jsr spawn_particls2
+               pla                  ; pull X, Y
                tax
                pla
                tay

                rts

; -------------------------------------------------------------------------------------------------

save_letter     ; Save entered letter. In: A = letter. Called by letter_input, check_button_b.
                pha
                jsr get_revolv_pos  ; 0-23 -> X
                pla
                sta entered_letrs,x
                rts

; -------------------------------------------------------------------------------------------------

fix_revolv_x    ; Move revolving cursor left until it lies immediately after a letter, on
                ; a letter or on the first column.
                ; Out: X = letter position to highlight (index to entered_letrs), revolv_x_letr1.
                ; Called by check_button_b, move_revolv_cursor_manually.

                jsr get_revolv_pos   ; 0-23 -> X
                lda revolv_x_letr1   ; exit if cursor at column 0 or a letter
                beq ++
                lda entered_letrs,x
                bne ++
-               dex                  ; move cursor left until at column 0 or a letter
                lda entered_letrs,x
                bne ++
                dec revolv_x_letr1
                bne -
++              rts

; -------------------------------------------------------------------------------------------------

get_letter      ; Get letter at revolving cursor.
                ; Out: A = letter, X = cursor position (0-23), zero flag reflects A.
                ; Called by check_button_b, spawn_particls1.

                jsr get_revolv_pos
                lda entered_letrs,x
                rts

; -------------------------------------------------------------------------------------------------

get_revolv_pos  ; Get position of revolving cursor. Out: X = cursor position (0-23).
                ; Called by save_letter, fix_revolv_x, get_letter.

                lda revolv_y_letr1  ; revolv_y_letr1 * 8 + revolv_x_letr1
                asl
                asl
                asl
                add revolv_x_letr1
                tax
                rts

; -------------------------------------------------------------------------------------------------

input_effects   ; Letter input extra effects: spawn flying letter and play sound.
                ; Called by letter_input.

                lda hand_x_letr   ; hand_x_letr * 32 -> flying_x, metasprite_x
                asl
                asl
                asl
                asl
                asl
                sta flying_x
                sta metasprite_x
                lda hand_y_letr   ; hand_y_letr * 8 -> stack
                asl               ; hand_y_letr * 32 + 64 -> flying_y, metasprite_y
                asl
                asl
                pha
                asl
                asl
                add #64
                sta flying_y
                sta metasprite_y

                pla                   ; hand_y_letr * 8 + hand_x_letr -> entered_letr
                add hand_x_letr       ; (for playing sound)
                sta_abs entered_letr

                adc #3               ; spawn flying letter graphic;
                ldx flying_metaspr   ; A = id = hand_y_letr * 8 + hand_x_letr + 3;
                jsr init_metasprite  ; X = index to metaspr_indexes

                ldx flying_metaspr
                jsr update_metaspr   ; X = metasprite index

                lda revolv_y_letr1  ; compute Y speed of flying letter (always positive)
                asl                 ; floor((revolv_y_letr1 * 32 + 144 - flying_y) / 16)
                asl                 ; -> flying_y_spd
                asl
                asl
                asl
                add #144
                sub flying_y
                lsr
                lsr
                lsr
                lsr
                sta flying_y_spd

                lda revolv_x_letr1  ; compute X speed of flying letter (signed)
                sub hand_x_letr     ; (revolv_x_letr1 - hand_x_letr) * 2 -> flying_x_spd
                asl
                sta flying_x_spd

                copy #16, flying_timelft1

                lda_abs entered_letr            ; play sound depending on letter
                asl                             ; bits of entered_letr: 0000ABCD
                asl
                asl
                asl
                sta temp1
                lda #0
                asl temp1
                rol
                asl temp1
                rol                             ; bits: 000000AB
                add #2
                sta              pulse1_regs+3  ; bits: 00000A1B
                copy temp1,      pulse1_regs+2  ; bits: CD000000
                copy #%00100100, pulse1_regs+0
                copy #%11111001, pulse1_regs+1

                copy_abs #20, flying_timelft2
                rts

; -------------------------------------------------------------------------------------------------

move_flying_ltr ; Move flying letter. Called by do_every_frame.

                lda flying_timelft2
                beq +
                dec flying_timelft2
                bne +

                copy #%00110000, pulse1_regs+0  ; flying_timelft2 went from 1 to 0
+               lda flying_timelft1
                bne +
                rts

+               dec flying_timelft1
                bne +
                ldx flying_metaspr       ; flying_timelft went from 1 to 0
                copy #255, metasprite_y
                jmp update_metaspr       ; X = metasprite index; ends with RTS

+               lda flying_x      ; update flying letter X position
                add flying_x_spd
                sta flying_x
                sta metasprite_x
                lda flying_y      ; update flying letter Y position
                add flying_y_spd
                sta flying_y
                sta metasprite_y

                ldx flying_metaspr
                jmp update_metaspr  ; X = metasprite index; ends with RTS

; -------------------------------------------------------------------------------------------------

fill_atrblkrows ; Fill rows of blocks in attribute table.
                ; In: A = fill byte, X = number of rows, Y = first row.
                ; Called by init_background.

                sta attr_fill_byte
                sty vram_block_y
                stx rows_left

--              lda #$00
-               sta vram_block_x
                jsr update_attr_blk
                lda vram_block_x
                add #1
                cmp #16
                bne -
                inc vram_block_y
                dec rows_left
                bne --
                rts

; -------------------------------------------------------------------------------------------------

animate_color   ; Animate color 3 of background subpalette 1. Called by do_every_frame.

                ldx anim_colr_phase  ; increment anim_colr_delay every frame
                ldy anim_colr_delay  ; increment anim_colr_phase every 5th frame
                iny
                cpy #5
                bne +
                ldy #0
                inx
+               sty anim_colr_delay
                cpx #8
                bne +
                ldx #0
+               stx anim_colr_phase

                lda anim_colors,x                        ; set up VRAM block to update
                sta vram_block+3                         ; data
                copy #1, vram_block+0                    ; data size
                copy #>(vram_palette+4+3), vram_block+1  ; address
                copy #<(vram_palette+4+3), vram_block+2

                jmp vram_blk_to_buf  ; copy block to buffer (sub ends with RTS)

anim_colors     ; Phases of animated color. Read by animate_color.
                db color_anim0, color_anim1, color_anim2, color_anim3
                db color_anim4, color_anim5, color_anim6, color_anim7

; -------------------------------------------------------------------------------------------------

hilite_inp_area ; Move highlight to another letter on input area.
                ; In: revolv_x_letr1, revolv_x_letr2, revolv_y_letr1, revolv_y_letr2
                ; Called by letter_input, check_button_b, move_revolv_cursor_manually.

                ldx revolv_x_letr2   ; set attribute %10 to the letter the revolving cursor exits;
                ldy revolv_y_letr2   ; store old Y position
                sty revolv_y_ltr2pr
                lda #%10101010
                jsr update_atr_byte  ; A = byte, X = X, Y = Y

                lda revolv_y_letr1   ; update Y position
                add #2
                sta revolv_y_letr2

                copy revolv_x_letr1, revolv_x_letr2  ; update X position

                jsr hilite_inp_row

                ldx revolv_x_letr2   ; set attribute %01 to the letter the revolving cursor enters
                ldy revolv_y_letr2
                lda #%01010101
                jmp update_atr_byte  ; A = byte, X/Y = position; ends with RTS

inparearow_atrs ; Attribute table data to write when entering a row on input area (16 bytes).
                ; Read by hilite_inp_row.
                db %10101111, %10101111, %10101111, %10101111
                db %10101111, %10101111, %10101111, %10101111
                db %11111010, %11111010, %11111010, %11111010
                db %11111010, %11111010, %11111010, %11111010

hilite_inp_row  ; Highlight active row on input area using Attribute Table.
                ; In: revolv_y_letr2, revolv_y_ltr2pr. Called by hilite_inp_area.

                lda revolv_y_ltr2pr  ; exit if Y position of revolving cursor has not changed
                cmp revolv_y_letr2
                beq hilite_exit

                ; set up VRAM block to change attribute data of all rows to %11 (gray)
                copy #>(vram_attrtable0+4*8), vram_block+1  ; address
                copy #<(vram_attrtable0+4*8), vram_block+2
                ldy #(4*8-1)                                ; data: 32 bytes, all %11111111
-               lda #%11111111
                sta vram_block+3,y
                sta attr_data_copy+4*8,y
                dey
                bpl -
                copy #32, vram_block+0                      ; data size
                jsr vram_blk_to_buf                         ; copy

                ; set up VRAM block to change attribute data of active row to %10 (white)
                lda revolv_y_letr2           ; address: vram_attrtable0 + (2 + revolv_y_letr2) * 8
                sub #2
                asl
                asl
                asl
                add #<(vram_attrtable0+4*8)
                sta vram_block+2
                copy #>(vram_attrtable0+4*8), vram_block+1
                ldy #(2*8-1)                 ; data: 16 bytes from inparearow_atrs
-               lda inparearow_atrs,y
                sta vram_block+3,y
                dey
                bpl -
                copy #16, vram_block+0       ; data size
                jsr vram_blk_to_buf          ; copy

hilite_exit     rts

; -------------------------------------------------------------------------------------------------

check_sel_start ; If select or start pressed, decode entered codes and start game.
                ; Called by do_every_frame.

                ; If neither pressed, allow them next time and exit.
                ; If either pressed but not allowed, just exit.
                ; If either pressed and allowed, continue.
                lda joypad1_status
                and #(pad_select|pad_start)
                bne +
                copy #1, temp2               ; allow select/start
-               rts
+               lda temp2                    ; allow select/start?
                beq -

                lda #%00000000  ; disable NMI and rendering
                sta ppu_ctrl
                sta ppu_mask

                lda #<entered_letrs  ; set source pointer
                sta code_ptr+0
                lda #>entered_letrs
                sta code_ptr+1

                lda #<decoded_codes    ; set target pointer
                sta decoded_cod_ptr+0
                lda #>decoded_codes
                sta decoded_cod_ptr+1

                ldx #(16-1)          ; fill decoded_codes with $ff (why 16 bytes?)
                lda #$ff
-               sta decoded_codes,x
                dex
                bpl -

                copy #3, codes_left  ; number of codes to decode

                ; AND bitmask to enable 1st code in genie_ctrl_val;
                ; rotated left later for other codes
                copy #%11101111, code_enab_mask
                ; OR bitmask to enable compare value of 1st code in genie_ctrl_val;
                ; shifted left later for other codes
                copy #%00000010, comp_enab_mask

                ; will be written to genie_ctrl; bits: 0CBAcbaG
                ; (A/B/C = disable code, a/b/c = enable compare value, G = switch to game mode);
                ; start with all codes disabled
                copy #%01110001, genie_ctrl_val

decodeallcodes  ldy #%00000000  ; start the long outer loop
                sty temp2       ; LSB of previous letter

                ; Phase 1/2 of decoding: modify the letters of one code:
                ; - if 0 (no letter), exit loop
                ; - subtract 3 to get a 4-bit value
                ; - shift right once
                ; - copy 4th-least significant bit from least significant bit of previous value
                ;   (always 0 for the first value)
-               lda (code_ptr),y
                beq decoded
                sub #3
                and #%00001111
                lsr
                ora temp2         ; LSB of previous letter
                sta (code_ptr),y
                ;
                lda #$00  ; store the bit that just got shifted out, shifted to the 4th-least
                rol       ; significant position (%00000000 or %00001000)
                asl
                asl
                asl
                sta temp2
                ;
                iny
                cpy #8
                bne -

decoded         sty code_length  ; if code is not 6 or 8 letters, ignore it
                cpy #8
                beq long_code
                cpy #6
                beq short_code
                jmp next_code

long_code       ldy #3            ; regardless of code length, read 4th value and discard it
                lda (code_ptr),y
                jmp +
short_code      ldy #3
                lda (code_ptr),y

+               ldy #0            ; copy the bit that got shifted out from the last value, to the
                lda (code_ptr),y  ; 4th-least significant position of the 1st value; thus, the
                and #%00000111    ; values will have been rotated instead of shifted
                ora temp2         ; (temp2 = LSB of previous letter)
                sta (code_ptr),y

                ; Phase 2/2 of decoding: copy 8 nybbles from one semi-decoded code, in order
                ; specified by codekey, to 4 bytes in decoded_code.
                ldx #0              ; source nybble offset
                stx temp2           ; target byte offset
-               ldy codekey,x       ; value to high nybble
                lda (code_ptr),y
                asl
                asl
                asl
                asl
                inx                 ; value to low nybble
                ldy codekey,x
                ora (code_ptr),y
                ldy temp2           ; target byte offset
                sta decoded_code,y
                inc temp2           ; next target byte
                inx
                cpx #8              ; end of loop
                bne -

                lda decoded_code+0  ; clear MSB of address
                and #%01111111
                sta decoded_code+0

                ; compare address to codes stored in decoded_codes

                lda decoded_code+0  ; address high
                ldx decoded_code+1  ; address low

                cmp decoded_codes+0      ; ignore code if the address is same as in first code
                bne +
                cpx decoded_codes+1
                beq next_code
+               cmp decoded_codes+4      ; ignore code if the address is same as in second code
                bne +
                cpx decoded_codes+4+1
                beq next_code
+               cmp decoded_codes+2*4    ; ignore code if address is same as in third code
                bne +                    ; (useless; we always compare against $ffff)
                cpx decoded_codes+2*4+1  ; never accessed
                beq next_code            ; instruction never accessed

+               ldy #1                   ; store code to decoded_codes (note: replace/compare
-               lda decoded_code,y       ; value trade places)
                sta (decoded_cod_ptr),y  ; address
                dey
                bpl -
                ldy #3                   ; replace value
                lda decoded_code-1,y
                sta (decoded_cod_ptr),y
                dey                      ; compare value
                lda decoded_code+1,y
                sta (decoded_cod_ptr),y

                lda genie_ctrl_val  ; enable code by ANDing genie_ctrl_val with current
                and code_enab_mask  ; code_enab_mask; if code is 8 letters, also enable compare
                ldx code_length     ; value by ORing genie_ctrl_val with current comp_enab_mask
                cpx #8
                bne +
                ora comp_enab_mask
+               sta genie_ctrl_val

next_code       sec                 ; prepare for next code by rotating/shifting bitmasks left
                rol code_enab_mask
                asl comp_enab_mask

                lda code_ptr+0  ; advance source pointer;
                add #8          ; high byte never increments because low byte starts from only $6b
                sta code_ptr+0  ; (low byte of entered_letrs)
                bcc +
                inc code_ptr+1  ; never accessed

+               lda decoded_cod_ptr+0  ; advance target pointer;
                add #4                 ; high byte never increments because low byte starts from
                sta decoded_cod_ptr+0  ; only $90 (low byte of decoded_codes)
                bcc +
                inc decoded_cod_ptr+1  ; never accessed

+               dec codes_left      ; end of the long outer loop
                beq +
                jmp decodeallcodes

+               ldx #(codekey-ram_program+2-1)  ; copy a short program to RAM
-               lda ram_program,x               ; (and two extra bytes for some reason)
                sta ram_prog_target,x
                dex
                bpl -

                jmp ram_prog_target  ; execute the program in RAM

ram_program     ldx #(3*4-1)                     ; a short program that is copied to RAM and
-               lda decoded_codes,x              ; executed; copies decoded codes to Game Genie
                sta genie_codes,x                ; registers ($8001-$800c)
                dex
                bpl -
                copy genie_ctrl_val, genie_ctrl  ; switch to game mode with correct codes enabled
                copy #%00000000, genie_ctrl      ; for unknown reason
                jmp (reset_vector)               ; reset system

codekey         db 3, 5, 2, 4, 1, 0, 7, 6  ; How to descramble the codes. Read by check_sel_start.

; -------------------------------------------------------------------------------------------------

macro offs_to_graphic _addr
                db >(_addr-graphix_offsets)  ; emit 16-bit offset relative to graphix_offsets,
                db <(_addr-graphix_offsets)  ; high byte first
endm

graphix_offsets ; Offsets to actual graphics data (see below). 2 bytes each, high byte first.
                ; Read indirectly by set_graphix_ptr using graphics_ptr.

                offs_to_graphic graphic_unused  ;  0 (never accessed)
                offs_to_graphic graphic_logo    ;  1
                offs_to_graphic graphic_revolv  ;  2
                offs_to_graphic graphic_a       ;  3
                offs_to_graphic graphic_e       ;  4
                offs_to_graphic graphic_p       ;  5
                offs_to_graphic graphic_o       ;  6
                offs_to_graphic graphic_z       ;  7
                offs_to_graphic graphic_letr_x  ;  8
                offs_to_graphic graphic_l       ;  9
                offs_to_graphic graphic_u       ; 10
                offs_to_graphic graphic_g       ; 11
                offs_to_graphic graphic_k       ; 12
                offs_to_graphic graphic_i       ; 13
                offs_to_graphic graphic_s       ; 14
                offs_to_graphic graphic_t       ; 15
                offs_to_graphic graphic_v       ; 16
                offs_to_graphic graphic_letr_y  ; 17
                offs_to_graphic graphic_n       ; 18
                offs_to_graphic graphic_dash    ; 19
                offs_to_graphic graphic_hand    ; 20

                ; The actual graphics data.
                ; Read indirectly using graphics_ptr.
                ; Read by draw_bg_graphic, graphic_nybble_to_vram_buffer, draw_metaspr_gr,
                ; init_metasprite.
                ;
                ; Format of each graphic:
                ;   1 byte: width  in tiles
                ;   1 byte: height in tiles
                ;   width * height / 2 bytes: data:
                ;       1 nybble = 1 tile
                ;       bits in each nybble (3 = most significant) represent 2*2 virtual pixels:
                ;           23
                ;           01

graphic_unused  db 8, 2                  ; an invalid graphic (never accessed)
                db %00000001, %00100011, %01000101, %01100111
graphic_logo    db 30, 4                 ; "Game Genie" logo
                db %00101100, %11000000, %00000000, %00000000, %00000000  ; line 0 start
                db %00000000, %00000000, %00000010, %11001100, %00000000
                db %00000000, %00000000, %00001000, %00000000, %00000000
                db %01010010, %00110010, %11001100, %01011101, %10010110  ; line 1 start
                db %00101100, %11100100, %00000101, %00100011, %00101100
                db %11100100, %11011100, %01101010, %00101100, %11100100
                db %01100000, %00101010, %00000000, %01010101, %01011010  ; line 2 start
                db %10100000, %01000000, %00000110, %00000010, %10100000
                db %01000000, %01010000, %10101010, %10100000, %01000000
                db %00001100, %01000000, %11000000, %01000100, %01001000  ; line 3 start
                db %00001100, %11000100, %00000000, %11000100, %00001100
                db %11000100, %01000000, %10001000, %00001100, %11000100
graphic_revolv  db 2, 2                  ; revolving cursor
                db %10110001             ; line 0
                db %10000000             ; line 1
graphic_a       db 4, 4                  ; "A"
                db %00001010, %00000000  ; line 0
                db %00000101, %01010000  ; line 1
                db %10101100, %11100000  ; line 2
                db %11000100, %11000100  ; line 3
graphic_e       db 4, 4                  ; "E"
                db %10001101, %11100000  ; line 0
                db %00000111, %00010000  ; line 1
                db %00000101, %00100000  ; line 2
                db %10001100, %11000000  ; line 3
graphic_p       db 4, 4                  ; "P"
                db %10001101, %01100000  ; line 0
                db %00000111, %10010000  ; line 1
                db %00000101, %00000000  ; line 2
                db %10001100, %00000000  ; line 3
graphic_o       db 4, 4                  ; "O"
                db %00001001, %01100000  ; line 0
                db %10100000, %00000101  ; line 1
                db %10000001, %00100100  ; line 2
                db %00001000, %01000000  ; line 3
graphic_z       db 4, 4                  ; "Z"
                db %10101100, %11100000  ; line 0
                db %00000010, %01000000  ; line 1
                db %00100100, %00100000  ; line 2
                db %10001100, %11000000  ; line 3
graphic_letr_x  db 4, 4                  ; "X"
                db %11100100, %11100100  ; line 0
                db %00000110, %01000000  ; line 1
                db %00100100, %01100000  ; line 2
                db %11000100, %11000100  ; line 3
graphic_l       db 4, 4                  ; "L"
                db %10001101, %00000000  ; line 0
                db %00000101, %00000000  ; line 1
                db %00000101, %00100000  ; line 2
                db %10001100, %11000000  ; line 3
graphic_u       db 4, 4                  ; "U"
                db %11100100, %11100100  ; line 0
                db %10100000, %10100000  ; line 1
                db %10100000, %10100000  ; line 2
                db %00001100, %01000000  ; line 3
graphic_g       db 4, 4                  ; "G"
                db %00001001, %11000101  ; line 0
                db %10100000, %00110001  ; line 1
                db %10000001, %00000101  ; line 2
                db %00001000, %11000000  ; line 3
graphic_k       db 4, 4                  ; "K"
                db %10001101, %10100100  ; line 0
                db %00000111, %01000000  ; line 1
                db %00000101, %01100000  ; line 2
                db %10001100, %10000100  ; line 3
graphic_i       db 4, 4                  ; "I"
                db %00001110, %01000000  ; line 0
                db %00001010, %00000000  ; line 1
                db %00001010, %00000000  ; line 2
                db %00001100, %01000000  ; line 3
graphic_s       db 4, 4                  ; "S"
                db %00101100, %01110000  ; line 0
                db %10000111, %00010000  ; line 1
                db %00100000, %11100000  ; line 2
                db %00001100, %01000000  ; line 3
graphic_t       db 4, 4                  ; "T"
                db %10101110, %11100000  ; line 0
                db %00001010, %00000000  ; line 1
                db %00001010, %00000000  ; line 2
                db %00001100, %01000000  ; line 3
graphic_v       db 4, 4                  ; "V"
                db %11100100, %11100100  ; line 0
                db %10000001, %10010000  ; line 1
                db %00000110, %01000000  ; line 2
                db %00001000, %00000000  ; line 3
graphic_letr_y  db 4, 4                  ; "Y"
                db %11100100, %11100100  ; line 0
                db %00000110, %01000000  ; line 1
                db %00001010, %00000000  ; line 2
                db %00001100, %01000000  ; line 3
graphic_n       db 4, 4                  ; "N"
                db %11100000, %11100100  ; line 0
                db %10100110, %10100000  ; line 1
                db %10100000, %11100000  ; line 2
                db %11000100, %10000000  ; line 3
graphic_dash    db 4, 4                  ; "-" (placeholder for input area)
                db %00000000, %00000000  ; line 0
                db %00100011, %00110001  ; line 1
                db %00000000, %00000000  ; line 2
                db %00000000, %00000000  ; line 3
graphic_hand    db 5, 4                  ; hand cursor (the only graphic with an odd width)
                db %00000000, %01110000, %00000011, %00111011, %01110001  ; lines 0 & 1
                db %00001110, %11111111, %01010000, %00001100, %11000100  ; lines 2 & 3

                ; never accessed (partial duplicate of hand cursor)
                hex 03 3b 71 0e ff 50 0c c4 00 00 00 00 00 00 00 00 00 00 00 aa

; --- Interrupt vectors ---------------------------------------------------------------------------

                pad $fffa, $ff
                dw nmi    ; NMI
reset_vector    dw init1  ; reset
                dw $ffff  ; IRQ (never accessed)

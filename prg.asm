; Game Genie - PRG ROM

; --- Constants -----------------------------------------------------------------------------------

; CPU addresses - RAM (each variable takes one byte unless otherwise mentioned)
ram_clear_pointer        equ $00    ; 2 bytes (overlaps)
ppu_ctrl_mirror          equ $00
ppu_mask_mirror          equ $01
temp1                    equ $04    ; a temporary variable, many uses
vram_address_high        equ $05
joypad1_status           equ $07
joypad2_status           equ $08
skip_nmi                 equ $0c
graphic_width            equ $0d    ; in nybbles/tiles
graphic_height           equ $0e    ; in nybbles/tiles
ram_program_target       equ $10
graphic_x_offset         equ $15
graphic_y_offset         equ $16
graphic_x                equ $17
graphic_y                equ $18
graphic_nybble           equ $19
attribute_fill_byte      equ $1a
vram_block_x             equ $1e
vram_block_y             equ $1f
nybble_vram_low          equ $20
nybble_vram_high         equ $21
multiplication_temp      equ $27
vram_block_cost          equ $28
vram_buffer_next_read    equ $29
vram_buffer_next_write   equ $2a
vram_buffer_free_bytes   equ $2b
vram_budget              equ $2c
metasprite_x             equ $2d
always_zero1             equ $2e    ; 0
metasprite_y             equ $2f
always_zero2             equ $30    ; 0
graphic_id               equ $31
unused1                  equ $33
metasprite_width         equ $34    ; in nybbles/tiles
metasprite_height        equ $35    ; in nybbles/tiles
always_b00011100         equ $36    ; %00011100
graphic_data_left        equ $37
metasprite_to_create     equ $38    ; metasprite number
graphics_pointer         equ $39    ; 2 bytes
nybbles_left_x           equ $3b
sprite_y                 equ $3c    ; 2 bytes
sprite_x                 equ $3e    ; 2 bytes
nybble_offset            equ $40
always_one               equ $41    ; 1
always_zero3             equ $42    ; 0
odd_frame_flag1          equ $43
scroll_x_mirror          equ $45
scroll_y_mirror          equ $46
nmi_done                 equ $49
unused2                  equ $4a
first_free_byte          equ $4b
keyboard_graphic         equ $4c
keyboard_graphic_x       equ $4d
keyboard_graphic_y       equ $4e
hand_metasprite          equ $4f    ; hand cursor; metasprite number (0)
hand_x_pixel             equ $50    ; 14-238
hand_y_pixel             equ $51    ; 60-204
hand_x_letter_target     equ $52    ; 0-7
hand_y_letter_target     equ $53    ; 0-4
revolving_metasprite     equ $54    ; revolving cursor; metasprite number (1)
revolving_x              equ $55    ; 10-234
revolving_y              equ $56    ; 152-216
hand_x_speed_pointer     equ $57    ; 2 bytes
hand_y_speed_pointer     equ $59    ; 2 bytes
hand_y_speed_offset      equ $5b
hand_x_speed_offset      equ $5c
last_x_input_accepted    equ $5d    ; joypad_left/joypad_right
last_y_input_accepted    equ $5e    ; joypad_up/joypad_down
odd_frame_flag2          equ $5f
hand_x_keyboard          equ $60    ; last X position on keyboard (0-7)
hand_y_keyboard          equ $61    ; last Y position on keyboard (0-1)
hand_x_letter            equ $62    ; 0-7
hand_y_letter            equ $63    ; 0-4
revolving_x_letter1      equ $64    ; 0-7
revolving_y_letter1      equ $65    ; 0-2
prev_button_a_status     equ $66    ; previous
prev_button_b_status     equ $67
revolving_x_target       equ $68    ; 10-234
revolving_y_target       equ $69    ; 152-216 in increments of 32
revolving_target_speed   equ $6a    ; signed (excess-128)
revolving_speed_x        equ $6b    ; signed (two's-complement)
revolving_speed_y        equ $6c    ; signed (two's-complement)
revolving_phase          equ $6d    ; phase of animation (0-15)
revolving_pos            equ $6e    ; X or Y (arg for subroutine)
revolving_target         equ $6f    ; X or Y position (arg for subroutine)
particle_start_x         equ $70
particle_start_y         equ $71
particle_set_flag        equ $72    ; which particle set to spawn
particle_set1_timer      equ $73    ; counts up
particle_set2_timer      equ $74    ; counts up
particle_time_left       equ $75    ; counts down
flying_x                 equ $76    ; flying letter
flying_y                 equ $77
flying_x_speed           equ $78    ; signed (two's complement, -14...+14)
flying_y_speed           equ $79    ; 3...9
flying_time_left1        equ $7a
flying_metasprite        equ $7b    ; metasprite number (2)
flying_time_left2        equ $7c
entered_letter           equ $7d
rows_left                equ $7e    ; only in fill_attribute_table_rows
animated_color_phase     equ $7f    ; 0-7
code_pointer             equ $80    ; 2 bytes (overlaps)
animated_color_delay     equ $80    ; 0-4
revolving_x_letter2      equ $81
decoded_codes_pointer    equ $82    ; 2 bytes (overlaps)
revolving_y_letter2      equ $82    ; 2-4 (always revolving_y_letter1 + 2?)
revolving_y_letter2_prev equ $83    ; previous
temp2                    equ $84    ; a temporary variable, many uses
code_length              equ $85
code_enable_mask         equ $86
compare_enable_mask      equ $87
codes_left_to_decode     equ $88
genie_control_value      equ $89
decoded_code             equ $8a    ; 4 bytes; current decoded Game Genie code (see below)
decoded_codes            equ $90    ; 16 bytes (12 actually used); all decoded Game Genie codes
;                                     (see "arrays" below)
interleaved_sprite_data  equ $0200  ; 256 bytes; copied to OAM
vram_buffer              equ $0300  ; 256 bytes; several VRAM blocks to be copied to VRAM
vram_block               equ $0440  ; 35 bytes; a block of bytes to be copied to VRAM
;                                     (see "arrays" below)
sprite_attributes        equ $0463  ; 64 bytes; sprite data in planar format (see "arrays" below)
sprite_x_positions       equ $04a3  ; 64 bytes; sprite data in planar format (see "arrays" below)
sprite_y_positions       equ $04e3  ; 64 bytes; sprite data in planar format (see "arrays" below)
sprite_tiles             equ $0523  ; 64 bytes; sprite data in planar format (see "arrays" below)
metasprite_indexes       equ $0563  ; 50 bytes; indexes to metasprites
metasprites              equ $0595  ; 46 bytes; objects consisting of several hardware sprites
;                                     (see "arrays" below)
particle_speeds_x        equ $060b  ; 64 bytes; horizontal speeds of flying particles
;                                     signed, two's complement; only indexes 32-63 are accessed
particle_speeds_y        equ $062b  ; 64 bytes; vertical speeds of flying particles
;                                     signed, two's complement; only indexes 32-63 are accessed
entered_letters          equ $066b  ; 24 bytes; letters entered by user
;                                     (0 = none, 3-18 = AEPOZXLU GKISTVYN)

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
; sprite_attributes, sprite_x_positions, sprite_y_positions, sprite_tiles:
;      0-19: hand cursor      (5*4 sprites)
;     20-23: revolving cursor (2*2 sprites)
;     24-39: flying letter    (4*4 sprites)
;     32-47: 1st particle set (16 sprites; overlaps with flying letter)
;     48-63: 2nd particle set (16 sprites)
; metasprites:
;     2 + 5*4 bytes for hand cursor
;     2 + 2*2 bytes for revolving cursor
;     2 + 4*4 bytes for flying letter
;     for each one:
;         1 byte: width  in hardware sprites
;         1 byte: height in hardware sprites
;         width*height bytes: indexes to planar sprite data

; CPU addresses - NES registers
ppu_ctrl             equ $2000
ppu_mask             equ $2001
ppu_status           equ $2002
oam_addr             equ $2003
ppu_scroll           equ $2005
ppu_addr             equ $2006
ppu_data             equ $2007
snd_pulse1_ctrl      equ $4000
snd_pulse1_ramp_ctrl equ $4001
snd_pulse1_ft        equ $4002
snd_pulse1_ct        equ $4003
snd_noise_ctrl1      equ $400c
snd_noise_freq1      equ $400e
snd_noise_freq2      equ $400f
oam_dma              equ $4014
snd_clock            equ $4015
joypad1              equ $4016
joypad2              equ $4017

; CPU addresses - Game Genie registers
genie_master_control equ $8000
genie_values         equ $8001  ; ...$800c (12 bytes)
genie_unknown1       equ $fff0
genie_unknown2       equ $fff1

; PPU addresses
ppu_name_table      equ $2000
ppu_attribute_table equ $23c0
ppu_palettes        equ $3f00

; joypad bitmasks
joypad_a      equ 1 << 7
joypad_b      equ 1 << 6
joypad_select equ 1 << 5
joypad_start  equ 1 << 4
joypad_up     equ 1 << 3
joypad_down   equ 1 << 2
joypad_left   equ 1 << 1
joypad_right  equ 1 << 0

; color names
gray     equ $00
black    equ $0d  ; causes problems with some TVs
purple   equ $13
white    equ $20
sky_blue equ $21
pink1    equ $24
pink2    equ $25
red      equ $26
orange   equ $27
yellow   equ $28
green    equ $2b
cyan     equ $2c

; colors
color_animated0                   equ sky_blue
color_animated1                   equ cyan
color_animated2                   equ green
color_animated3                   equ yellow
color_animated4                   equ orange
color_animated5                   equ pink2
color_animated6                   equ pink1
color_animated7                   equ cyan
color_animated_initial            equ red  ; never seen
color_background                  equ black
color_hand1                       equ red
color_hand2_revolving2_particle2  equ white
color_highlight                   equ white
color_input_area                  equ gray
color_keyboard                    equ cyan
color_letter_revolving1_particle1 equ purple
color_unused1                     equ gray
color_unused2                     equ yellow

; --- Macros --------------------------------------------------------------------------------------

; Note: for clarity, don't use these if their intermediate results are used later.
; Hint: try: grep "^macro "

macro add _operand        ; add without carry
        clc
        adc _operand
endm

macro copy _src, _dst     ; copy value via A
        lda _src
        sta _dst
endm

macro dec_absolute _word  ; prevent zero page optimization of DEC $00xx
        db $ce
        dw _word
endm

macro inc_lda _mem        ; INC & LDA memory
        inc _mem
        lda _mem
endm

macro lda_absolute _word  ; prevent zero page optimization of LDA $00xx
        db $ad
        dw _word
endm

macro ldaxy _a, _x, _y    ; load A, X, Y
        lda _a
        ldx _x
        ldy _y
endm

macro ldx_absolute _word  ; prevent zero page optimization of LDX $00xx
        db $ae
        dw _word
endm

macro ldxy _x, _y         ; load X, Y
        ldx _x
        ldy _y
endm

macro phx                 ; push X via A
        txa
        pha
endm

macro phy                 ; push Y via A
        tya
        pha
endm

macro plx                 ; pull X via A
        pla
        tax
endm

macro ply                 ; pull Y via A
        pla
        tay
endm

macro sta_absolute _word  ; prevent zero page optimization of STA $00xx
        db $8d
        dw _word
endm

macro sub _operand        ; subtract without borrow
        sec
        sbc _operand
endm

; -------------------------------------------------------------------------------------------------

        ; last 4 KiB of CPU address space
        base $f000

init1   ; part 1/3 of initialization

        sei                        ; ignore IRQs
        cld                        ; disable decimal mode
        copy #%00000000, ppu_ctrl  ; disable NMI
        ldx #$ff
        txs                        ; initialize stack pointer

        ; do something to Game Genie registers
        lda #$00
        sta genie_unknown1
        jsr delay
        sta genie_unknown2
        jsr delay
        sta genie_unknown1

        jmp init2  ; continue initialization

; -------------------------------------------------------------------------------------------------

        ; Wait.
        ; Called by: init1

delay   ldx #96
        ldy #8
-       dex
        bne -
        dey
        bne -
        rts

; -------------------------------------------------------------------------------------------------

init2   ; part 2/3 of initialization

        ; wait for VBlank 10 times
        ldx #10
-       lda ppu_status
        bpl -
        dex
        bne -

        ldx #$ff
        txs       ; reinitialize stack pointer (why?)

        ; clear RAM (fill $0000...$07ff with $00)
        copy #$07, ram_clear_pointer + 1
        lda #$00
        sta ram_clear_pointer + 0
        tay
-       sta (ram_clear_pointer), y
        iny
        bne -
        dec ram_clear_pointer + 1
        bpl -

        ; hide sprites and background
        lda #%00000110
        sta ppu_mask_mirror
        sta ppu_mask

        ; enable NMI, use 8 * 8 pixel sprites, use Pattern Table 0, use Name Table 0,
        ; make the VRAM address increment by one
        lda #%10000000
        sta ppu_ctrl_mirror
        sta ppu_ctrl

        dec_absolute vram_buffer_free_bytes  ; set to 255

        jmp init3  ; continue initialization

; -------------------------------------------------------------------------------------------------

macro read_joypad_macro _src, _dst
        ldy #8    ; number of buttons to read
-       lda _src  ; read from memory-mapped register
        ror       ; LSB to carry
        rol _dst  ; carry to RAM
        dey
        bne -     ; next button
endm

read_joypads
        ; Read joypads.
        ; Out:
        ;     joypad1_status (bits: A, B, select, start, up, down, left, right)
        ;     joypad2_status (bits: A, B, select, start, up, down, left, right)
        ; Called by: do_every_frame

        ; initialize joypads
        copy #%00000001, joypad1
        copy #%00000000, joypad1

        ; read joypads
        read_joypad_macro joypad1, joypad1_status
        read_joypad_macro joypad2, joypad2_status

        rts

; -------------------------------------------------------------------------------------------------

        ; The non-maskable interrupt routine.
        ; In: skip_nmi: if nonzero, skip doing the usual stuff
        ; Out: nmi_done: set to nonzero when exiting

nmi     pha
        phx
        phy

        lda skip_nmi
        bne +

        jsr sprite_dma
        jsr vram_buffer_to_vram

        ; scroll the background
        lda_absolute scroll_x_mirror
        sta ppu_scroll
        lda_absolute scroll_y_mirror
        sta ppu_scroll

        copy ppu_ctrl_mirror, ppu_ctrl

+       lda #1
        sta_absolute nmi_done  ; set flag

        ply
        plx
        pla

        rti

; -------------------------------------------------------------------------------------------------

draw_graphic_on_background
        ; Draw a graphic (e.g. the Game Genie logo) on the Name Table.
        ; In:
        ;     A: graphic id (see graphics_offsets)
        ;     X: horizontal position, in tiles
        ;     Y: vertical   position, in tiles
        ; Called by: init_background, letter_input, check_button_b

        stx graphic_x
        sty graphic_y
        jsr set_graphics_pointer  ; A = id

        ; get width and height of graphic (in nybbles/tiles)
        ldy #0
        lda (graphics_pointer), y
        sta graphic_width
        iny
        lda (graphics_pointer), y
        sta graphic_height

        ; advance graphics pointer to start of data
        lda_absolute graphics_pointer + 0
        add #2
        sta_absolute graphics_pointer + 0
        lda_absolute graphics_pointer + 1
        adc #0
        sta_absolute graphics_pointer + 1

        ; copy nybbles to VRAM buffer
        copy #0, graphic_y_offset
--      copy #0, graphic_x_offset          ; Y loop
-       jsr graphic_nybble_to_vram_buffer  ; X loop
        inc_lda graphic_x_offset
        cmp graphic_width
        bne -
        inc_lda graphic_y_offset
        cmp graphic_height
        bne --
        rts

; -------------------------------------------------------------------------------------------------

graphic_nybble_to_vram_buffer
        ; Copy one nybble (one tile) of graphics data to VRAM buffer.
        ; In:
        ;     graphic_x:         horizontal position of graphic,     in tiles
        ;     graphic_y:         vertical   position of graphic,     in tiles
        ;     graphic_x_offset:  horizontal position inside graphic, in tiles
        ;     graphic_y_offset:  vertical   position inside graphic, in tiles
        ;     vram_address_high: high byte of VRAM address
        ; Called by: draw_graphic_on_background

        ; get offset of nybble
        lda graphic_y_offset
        ldx graphic_width
        jsr multiply  ; A = multiplicand, X = multiplier
        add graphic_x_offset
        sta_absolute nybble_offset

        ; graphic data byte -> temp1
        lsr
        tay
        lda (graphics_pointer), y
        sta temp1

        ; get nybble from byte
        lda_absolute nybble_offset
        and #%00000001
        beq +           ; get upper nybble
        lda temp1
        and #%00001111
        jmp ++
+       lda temp1
        lsr
        lsr
        lsr
        lsr
++      sta graphic_nybble

        ; vram_address_high * 256
        ; + (graphic_y + graphic_y_offset) * 32
        ; + graphic_x + graphic_x_offset - 4
        ; -> word(nybble_vram_high, nybble_vram_low)

        ; graphic_x + graphic_x_offset - 4 -> vram_block_x
        lda graphic_x
        add graphic_x_offset
        sub #4
        sta vram_block_x

        ; (graphic_y + graphic_y_offset) * 32 + vram_address_high * 256
        ; -> word(nybble_vram_high, nybble_vram_low)
        lda graphic_y
        add graphic_y_offset
        sta vram_block_y
        lda vram_block_y
        asl
        asl
        asl
        sta nybble_vram_low
        lda #0               ; will become nybble_vram_high
        asl nybble_vram_low  ; 4th shift
        rol                  ; save overflowed bit
        asl nybble_vram_low  ; 5th shift
        rol                  ; save overflowed bit
        add vram_address_high
        sta nybble_vram_high

        ; word(nybble_vram_high, nybble_vram_low) += vram_block_x
        lda nybble_vram_low
        add vram_block_x
        sta nybble_vram_low
        lda nybble_vram_high
        adc #0
        sta nybble_vram_high

        ; set up VRAM block
        copy nybble_vram_high, vram_block + 1  ; address high
        copy nybble_vram_low, vram_block + 2   ; address low
        copy graphic_nybble, vram_block + 3    ; data
        copy #1, vram_block + 0                ; data size

        jsr vram_block_to_buffer  ; copy to buffer
        rts

; -------------------------------------------------------------------------------------------------

multiply
        ; Multiply.
        ; In:
        ;     A: multiplicand
        ;     X: multiplier
        ; Out: A: product
        ; Called by: graphic_nybble_to_vram_buffer, assign_metasprite_to_graphic

        sta multiplication_temp
        lda #0
        cpx #0
        beq +
        clc
-       adc multiplication_temp
        dex
        bne -
+       rts

; -------------------------------------------------------------------------------------------------

sprite_dma
        ; Copy interleaved_sprite_data to OAM.
        ; Called by: nmi, init3

        copy #$00, oam_addr
        copy #>interleaved_sprite_data, oam_dma
        rts

; -------------------------------------------------------------------------------------------------

vram_buffer_to_vram
        ; Copy as much as possible from VRAM buffer to VRAM.
        ; Each block in buffer: number of bytes, address hi, address lo, bytes
        ; Called by: nmi, vram_block_to_buffer

        copy #100, vram_budget  ; maximum sum of (blocks * 5 + total_bytes) to copy
        ldy vram_buffer_next_read

vram_block_copy_loop
        ; exit if nothing left to copy
        cpy vram_buffer_next_write
        beq vram_copy_end

        ; get VRAM block size;
        ; compute the cost of copying the block, i.e. size + 5
        lda vram_buffer, y
        tax
        add #5
        sta vram_block_cost

        ; remove cost from budget;
        ; exit if out of budget
        lda vram_budget
        sub vram_block_cost
        bcc vram_copy_end
        sta vram_budget

        ; add total block size to vram_buffer_free_bytes
        lda vram_buffer_free_bytes
        clc
        adc vram_buffer, y
        adc #3
        sta vram_buffer_free_bytes

        ; get and set VRAM address
        iny
        lda vram_buffer, y
        sta ppu_addr
        iny
        lda vram_buffer, y
        sta ppu_addr

        ; copy block data to VRAM
        iny
-       lda vram_buffer, y
        sta ppu_data
        iny
        dex
        bne -

        jmp vram_block_copy_loop

vram_copy_end
        sty vram_buffer_next_read
        rts

; -------------------------------------------------------------------------------------------------

vram_block_to_buffer
        ; Copy current VRAM block to VRAM buffer.
        ; Called by: graphic_nybble_to_vram_buffer, update_attribute_block, init3, animate_color,
        ;     highlight_input_area_row

        ; VRAM block total size -> temp1
        lda vram_block + 0
        add #3
        sta temp1

        ; continue if vram_buffer_free_bytes >= temp1
        lda vram_buffer_free_bytes
        cmp temp1
        bcs +

        ; restart sub if NMI is being skipped
        lda skip_nmi
        beq vram_block_to_buffer

        ; copy some data out of VRAM buffer and restart sub
        jsr vram_buffer_to_vram
        jmp vram_block_to_buffer

        ; vram_buffer_free_bytes -= temp1
+       sub temp1
        sta vram_buffer_free_bytes

        ; copy VRAM block to buffer
        ldx #0                      ; index to read within block
        ldy vram_buffer_next_write
-       lda vram_block, x
        sta vram_buffer, y
        inx
        iny
        cpx temp1
        bne -

        sty vram_buffer_next_write
        rts

; -------------------------------------------------------------------------------------------------

assign_metasprite_to_graphic
        ; Draw a graphic (e.g. the hand cursor) as a metasprite.
        ; In: A: graphic id (see graphics_offsets)
        ; Out: A: index to metasprite_indexes
        ; Called by: init3

        ; start reading specified graphic
        sta graphic_id
        jsr set_graphics_pointer  ; A = id

        ; get width and height of graphic
        ldy #0
        lda (graphics_pointer), y
        sta metasprite_width
        tax
        iny
        lda (graphics_pointer), y
        sta metasprite_height

        ; width * height of graphic -> A, graphic_data_left
        jsr multiply  ; A = multiplicand, X = multiplier
        sta graphic_data_left

        ; total size of graphic -> A
        add #2

        ; find first free byte in metasprite_indexes;
        ; save to metasprite_to_create and X;
        ; add total size of graphic to all following bytes in metasprite_indexes
        jsr find_free_metasprite  ; A = value to add
        sta metasprite_to_create
        tax

        ; index to metasprites -> X
        lda metasprite_indexes, x
        tax

        ; save width and height of graphic to metasprite data
        lda metasprite_width
        sta metasprites, x
        lda metasprite_height
        sta metasprites + 1, x

        ; advance to indexes to individual sprites
        inx
        inx

        ; assign graphic_data_left free sprites to the metasprite,
        ; starting from the first free sprite
        ldy #255
-       iny
        lda sprite_attributes, y
        bne -
        tya
        sta metasprites, x
        inx
        dec graphic_data_left
        bne -

        ; set up tiles and attributes for individual sprites
        ldx metasprite_to_create
        lda graphic_id
        jsr init_metasprite  ; A = id, X = index to metasprite_indexes

        ; set up positions for individual sprites
        ldx metasprite_to_create
        jsr update_metasprite  ; X = metasprite index

        ; return metasprite index
        lda metasprite_to_create
        rts

; -------------------------------------------------------------------------------------------------

set_graphics_pointer
        ; Set graphics pointer.
        ; In: A: graphic id (see graphics_offsets)
        ; Out: graphics_pointer: the address of the graphic
        ; Called by: draw_graphic_on_background, assign_metasprite_to_graphic, init_metasprite

        sta temp1  ; graphic id

        ; set pointer
        copy #<graphics_offsets, graphics_pointer + 0
        copy #>graphics_offsets, graphics_pointer + 1

        ; word(graphics_pointer) += graphic_id >> 7
        lda temp1
        bpl +
        inc graphics_pointer + 1  ; never accessed

        ; get offset to offset
+       asl
        tay
        ; get offset to graphic
        lda (graphics_pointer), y
        pha
        iny
        lda (graphics_pointer), y
        ; get address of graphic
        add #<graphics_offsets
        sta graphics_pointer + 0
        pla
        adc #>graphics_offsets
        sta graphics_pointer + 1
        rts

; -------------------------------------------------------------------------------------------------

update_metasprite
        ; Update metasprite's position to its individual sprites' positions.
        ; In:
        ;   X: metasprite index (0 = hand cursor, 1 = revolving cursor, 2 = flying letter)
        ;   metasprite_x: horizontal position of metasprite
        ;   metasprite_y: vertical   position of metasprite
        ; Called by: assign_metasprite_to_graphic, do_every_frame, update_revolving_cursor,
        ;     letter_input_extra_effects, move_flying_letter

        lda metasprite_indexes, x              ; get index to metasprite info
        tax

        lda metasprites, x                     ; get metasprite dimensions
        sta metasprite_width
        lda metasprites + 1, x
        sta metasprite_height

        inx                                    ; go to start of target sprite indexes
        inx

        copy metasprite_y, sprite_y + 0        ; init target sprites' Y position
        copy always_zero2, sprite_y + 1

metasprite_loop_y
        lda sprite_y + 1                       ; hide row if beyond bottom edge of screen
        bne hiderow

        copy metasprite_width, nybbles_left_x  ; init loop counter

        copy metasprite_x, sprite_x + 0        ; init target sprites' X position
        copy always_zero1, sprite_x + 1

metasprite_loop_x
        lda sprite_x + 1                       ; hide if beyond right edge of screen
        bne hidespr

        lda metasprites, x                     ; get index to target sprite
        tay

        lda sprite_tiles, y                    ; hide if target tile = 0
        beq hidespr

        lda sprite_x + 0                       ; set target sprite position
        sta sprite_x_positions, y
        lda sprite_y + 0
        sta sprite_y_positions, y

        inx
        jmp nextspr

hidespr lda metasprites, x
        tay
        lda #255
        sta sprite_y_positions, y
        inx

nextspr lda sprite_x + 0
        add #8
        sta sprite_x + 0
        lda sprite_x + 1
        adc #0
        sta sprite_x + 1

        dec nybbles_left_x
        bne metasprite_loop_x

nextrow lda sprite_y + 0
        add #8
        sta sprite_y + 0
        lda sprite_y + 1
        adc #0
        sta sprite_y + 1

        dec metasprite_height
        bne metasprite_loop_y

        rts

hiderow copy metasprite_width, nybbles_left_x
-       lda metasprites, x
        tay
        lda #255
        sta sprite_y_positions, y
        inx
        dec nybbles_left_x
        bne -
        jmp nextrow

; -------------------------------------------------------------------------------------------------

init_metasprite
        ; Set up tiles and attributes for individual sprites of a metasprite.
        ; In:
        ;   A: graphic id (see graphics_offsets)
        ;   X: index to metasprite_indexes
        ; Called by: assign_metasprite_to_graphic, letter_input_extra_effects

        sta graphic_id

        lda metasprite_indexes, x  ; get index to metasprites
        add #2
        pha

        lda graphic_id             ; start reading graphic (A = id)
        jsr set_graphics_pointer

        ldy #0                     ; get dimensions of graphic
        lda (graphics_pointer), y
        sta metasprite_width
        iny
        lda (graphics_pointer), y
        sta metasprite_height

        lda #0                     ; 2 -> nybble_offset
        ora #%00011100
        ldx unused1                ; ignored later
        sta always_b00011100
        lda #2
        ldy #1
        sty always_one
        sta nybble_offset
        copy #0, always_zero3

        plx                        ; pull index to metasprites

        ; copy all rows of graphics data to target sprites

yloop   copy metasprite_width, nybbles_left_x  ; xloop counter

        ; copy one row of graphics data to target sprites

xloop   lda nybble_offset          ; graphics data byte -> temp1
        lsr
        add #1
        tay
        lda (graphics_pointer), y
        sta temp1

        lda nybble_offset          ; push high nybble if even offset, else low
        and #%00000001
        beq pshhiny
        lda temp1
        and #%00001111
        jmp +
pshhiny lda temp1
        lsr
        lsr
        lsr
        lsr
+       pha

        lda metasprites, x        ; nybble -> target sprite tile
        tay
        pla
        sta sprite_tiles, y
        lda always_b00011100      ; %00011100 -> target sprite attribute
        sta sprite_attributes, y

        inx                       ; next target sprite

        lda nybble_offset         ; increment
        add always_one
        sta nybble_offset

        dec nybbles_left_x
        bne xloop

        lda nybble_offset         ; do nothing
        add always_zero3
        sta nybble_offset

        dec metasprite_height
        bne yloop

        rts

; -------------------------------------------------------------------------------------------------

convert_sprites
        ; Convert planar sprite data to interleaved.
        ; Sprites with attribute byte $00 will be hidden.
        ; Called by: do_every_frame

        ; ascending order every 2nd frame, descending order every 2nd frame
        lda odd_frame_flag1
        eor #%00000001
        sta odd_frame_flag1
        bne ascending_order

        ; descending order: planar sprites 61...0 -> interleaved sprites 2...63
        ldy #2 * 4  ; target offset
        ldx #61     ; source offset
-       lda sprite_attributes, x
        beq hide1
        jsr convert_sprite  ; A = attribute byte, X = src index, Y = dst index
        dex
        bpl -
        rts
hide1   jsr hide_sprite  ; Y = index
        dex
        bpl -
        rts  ; never accessed

ascending_order
        ; ascending order: planar sprites 0...61 -> interleaved sprites 2...63
        ldy #2 * 4  ; target offset
        ldx #0      ; source offset
-       lda sprite_attributes, x
        beq hide2
        jsr convert_sprite  ; A = attribute byte, X = src index, Y = dst index
        inx
        cpx #62
        bne -
        rts
hide2   jsr hide_sprite  ; Y = index
        inx
        cpx #62
        bne -
        rts

; -------------------------------------------------------------------------------------------------

convert_sprite
        ; Convert one non-hidden sprite from planar to interleaved.
        ; In:
        ;     A: attribute byte
        ;     X: index to planar sprite data tables (source)
        ;     Y: index to interleaved_sprite_data (target)
        ; Out: Y += 4
        ; Called by: convert_sprites

        sta interleaved_sprite_data + 2, y
        lda sprite_y_positions, x
        sta interleaved_sprite_data + 0, y
        lda sprite_tiles, x
        sta interleaved_sprite_data + 1, y
        lda sprite_x_positions, x
        sta interleaved_sprite_data + 3, y

        iny
        iny
        iny
        iny

        rts

; -------------------------------------------------------------------------------------------------

hide_sprite
        ; Hide a sprite in interleaved_sprite_data.
        ; In: Y: index to interleaved_sprite_data
        ; Out: Y += 4
        ; Called by: convert_sprites

        lda #255
        sta interleaved_sprite_data, y

        iny
        iny
        iny
        iny

        rts

; -------------------------------------------------------------------------------------------------

update_attribute_block
        ; Change the value of an attribute block (2 bits) within one attribute
        ; byte, preserving the other 6 bits.
        ; In:
        ;     vram_block_x: horizontal position of attribute block (0-15)
        ;     vram_block_y: vertical   position of attribute block (0-14)
        ;     attribute_fill_byte: value of new block (which 2 bits are read depends on
        ;         vram_block_x and vram_block_y)
        ; Called by: update_attribute_byte, fill_attribute_table_rows

        ; position of attribute block within attribute byte (0-3) -> Y
        lda vram_block_y
        and #%00000001
        asl
        sta temp1
        lda vram_block_x
        and #%00000001
        ora temp1
        tay

        ; position of byte within Attribute Table (0-63) -> A, X
        lda vram_block_y
        asl
        asl
        and #%11111000
        sta temp1
        lda vram_block_x
        lsr
        add temp1
        tax

        ; ppu_attribute_table + A -> VRAM block target address
        ora #<ppu_attribute_table
        sta vram_block + 2
        copy #>ppu_attribute_table, vram_block + 1

        ; one byte to copy
        copy #1, vram_block + 0  ; data size

        ; combine old and new bits of attribute byte:
        ; (newAttributeByte & bitmaskDeterminedByY)
        ; | ($0400, x & ~bitmaskDeterminedByY)
        ; -> $0400, x and vram_block + 3 (start of data)
        lda attribute_block_bitmasks, y
        and attribute_fill_byte
        sta temp1
        lda attribute_block_bitmasks, y
        eor #%11111111
        and $0400, x
        ora temp1
        sta $0400, x
        sta vram_block + 3  ; start of data

        jmp vram_block_to_buffer  ; ends with rts

attribute_block_bitmasks
        ; Read by: update_attribute_block
        db %00000011, %00001100, %00110000, %11000000

; -------------------------------------------------------------------------------------------------

        ; Initial palette. 32 bytes.
        ; Read by: init3
initial_palette
        ; background
        db color_background, color_unused1, color_unused1, color_keyboard
        db color_unused1, color_unused1, color_unused1, color_animated_initial
        db color_unused1, color_unused1, color_unused1, color_highlight
        db color_unused1, color_unused1, color_unused1, color_input_area
        ; sprites
        db color_background, color_unused1, color_unused1, color_letter_revolving1_particle1
        db color_unused1, color_unused1, color_unused1, color_hand1
        db color_unused1, color_unused1, color_unused1, color_hand2_revolving2_particle2
        db color_unused1, color_unused1, color_unused1, color_unused2

init3   ; part 3/3 of initialization

        copy #1, skip_nmi

        ; clear first Name&Attribute Table (fill VRAM $2000...$23ff with $00)
        lda #>ppu_name_table
        sta vram_address_high
        sta ppu_addr
        lda #<ppu_name_table
        sta ppu_addr
        ldx #4
        tay
-       sta ppu_data
        dey
        bne -
        dex
        bne -

        jsr init_background

        ; clear metasprite_indexes (why? we just cleared the entire RAM)
        ldx #0
        lda #$00
-       sta metasprite_indexes, x
        inx
        cpx #50
        bne -

        jsr sprite_dma

        ; initial position of hand cursor
        lda #14
        sta_absolute hand_x_pixel
        sta metasprite_x
        lda #60
        sta_absolute hand_y_pixel
        sta metasprite_y

        ; initial position of revolving cursor
        lda #128
        sta_absolute revolving_x
        lda #150
        sta_absolute revolving_y

        ; draw hand cursor
        lda #20
        jsr assign_metasprite_to_graphic  ; A = id
        sta_absolute hand_metasprite

        ; draw revolving cursor
        lda #2
        jsr assign_metasprite_to_graphic  ; A = id
        sta_absolute revolving_metasprite

        copy #255, metasprite_y

        ; initialize the flying letter (only the dimensions of the graphic matter)
        lda #5                            ; letter "P"
        jsr assign_metasprite_to_graphic  ; A = id
        sta_absolute flying_metasprite

        ; copy sprite attribute data from ROM to RAM
        ldx #(20 - 1)
-       lda initial_sprite_attribute_data, x
        sta sprite_attributes, x
        dex
        bpl -

        copy #%00001001, snd_clock

        ; data size
        copy #32, vram_block + 0
        ; address
        copy #>ppu_palettes, vram_block + 1
        copy #<ppu_palettes, vram_block + 2

        ; copy initial palette from ROM to VRAM block
        ldy #0
-       lda initial_palette, y
        sta vram_block + 3, y  ; vram_block + 3 = start of data
        iny
        cpy #32
        bne -
        jsr vram_block_to_buffer

        copy #0, skip_nmi
        jsr wait_until_nmi_done

        ; show sprites & background
        lda ppu_mask_mirror
        ora #%00011000
        sta ppu_mask_mirror
        sta ppu_mask

        ; the main loop
-       jsr do_every_frame
        jsr wait_until_nmi_done
        jmp -

; -------------------------------------------------------------------------------------------------

wait_until_nmi_done
        ; Wait until the NMI routine has run.
        ; Called by: init3, main_loop

        ; clear the flag
        lda #0
        sta_absolute nmi_done
        ; wait until the flag gets set
-       lda_absolute nmi_done
        beq -
        rts

; -------------------------------------------------------------------------------------------------

do_every_frame
        ; Called by: main_loop

        jsr read_joypads
        jsr move_hand
        jsr update_revolving_cursor
        jsr move_particles
        jsr move_flying_letter
        jsr animate_color
        jsr check_button_a
        jsr check_button_b
        jsr check_select_and_start

        ; update the metasprite of the hand cursor
        lda_absolute hand_x_pixel
        sta metasprite_x
        lda_absolute hand_y_pixel
        sta metasprite_y
        ldx_absolute hand_metasprite
        jsr update_metasprite  ; X = metasprite index

        jsr convert_sprites
        rts

; -------------------------------------------------------------------------------------------------

check_arrows
        ; Called by: move_hand

        ; if hand_y_speed_pointer set, skip checking vertical arrows
        lda_absolute hand_y_speed_pointer + 1
        bne chk_hrz

        ; was up pressed?
        lda joypad1_status
        and #joypad_up
        beq chk_dn
        ; move hand if possible
        lda #joypad_up
        jsr set_hand_target  ; A = direction, out: A = success
        beq chk_dn
        ; hand successfully moved
        ; negative hand speeds -> hand_y_speed_pointer
        lda #<hand_speeds_negative
        sta_absolute hand_y_speed_pointer + 0
        lda #>hand_speeds_negative
        sta_absolute hand_y_speed_pointer + 1
        ; if hand moved from 3rd line to 2nd, 32 -> hand_y_speed_offset,
        ; else 0 -> hand_y_speed_offset
        lda #joypad_up
        jsr moving_between_keyboard_and_input_area  ; A = direction
        beq +  ; did not
        lda #32
        jmp ++
+       lda #0
++      sta_absolute hand_y_speed_offset
        lda #joypad_up
        sta_absolute last_y_input_accepted

        jmp chk_hrz

        ; was down pressed?
chk_dn  lda joypad1_status
        and #joypad_down
        beq chk_hrz
        ; move hand if possible
        lda #joypad_down
        jsr set_hand_target  ; A = direction, out: A = success
        beq chk_hrz
        ; hand successfully moved
        ; positive hand speeds -> hand_y_speed_pointer
        lda #<hand_speeds_positive
        sta_absolute hand_y_speed_pointer + 0
        lda #>hand_speeds_positive
        sta_absolute hand_y_speed_pointer + 1
        ; if hand moved from 2nd to 3rd line, 32 -> hand_y_speed_offset,
        ; else 0 -> hand_y_speed_offset
        lda #joypad_down
        jsr moving_between_keyboard_and_input_area  ; A = direction
        beq +  ; did not
        lda #32
        jmp ++
+       lda #0
++      sta_absolute hand_y_speed_offset
        lda #joypad_down
        sta_absolute last_y_input_accepted

        ; check horizontal arrows
        ;
        ; if hand_x_speed_pointer set, skip checking left/right button
chk_hrz lda_absolute hand_x_speed_pointer + 1
        bne arrdone
        ;
        ; was left pressed?
        lda joypad1_status
        and #joypad_left
        beq chk_rt
        ; move hand if possible
        lda #joypad_left
        jsr set_hand_target  ; A = direction, out: A = success
        beq chk_rt
        ; hand successfully moved
        ; negative hand speeds -> hand_x_speed_pointer
        lda #<hand_speeds_negative
        sta_absolute hand_x_speed_pointer + 0
        lda #>hand_speeds_negative
        sta_absolute hand_x_speed_pointer + 1
        ; 0 -> hand_x_speed_offset
        lda #0
        sta_absolute hand_x_speed_offset
        lda #joypad_left
        sta_absolute last_x_input_accepted

        jmp arrdone

        ; was right pressed?
chk_rt  lda joypad1_status
        and #joypad_right
        beq arrdone
        ; move hand if possible
        lda #joypad_right
        jsr set_hand_target  ; A = direction, out: A = success
        beq arrdone
        ; hand successfully moved
        ; positive hand speeds -> hand_x_speed_pointer
        lda #<hand_speeds_positive
        sta_absolute hand_x_speed_pointer + 0
        lda #>hand_speeds_positive
        sta_absolute hand_x_speed_pointer + 1
        ; 0 -> hand_x_speed_offset
        lda #0
        sta_absolute hand_x_speed_offset
        lda #joypad_right
        sta_absolute last_x_input_accepted

arrdone rts

; -------------------------------------------------------------------------------------------------

find_free_metasprite
        ; Find the first free byte in metasprite_indexes and add the specified
        ; value to all following bytes.
        ; In: A: the value to add
        ; Out: A: index to the first free byte
        ; Called by: assign_metasprite_to_graphic

        sta temp1  ; the value to add

        ; find first free byte (same as the following byte)
        ldx #255
-       inx
        lda metasprite_indexes + 1, x
        cmp metasprite_indexes, x
        bne -

        stx first_free_byte  ; index to first free byte
        sta unused2          ; never used anywhere

        ; add the value to all following bytes
        inx
-       lda temp1
        clc
        adc metasprite_indexes, x
        sta metasprite_indexes, x
        inx
        cpx #50
        bne -

        lda first_free_byte
        rts

; -------------------------------------------------------------------------------------------------

init_background
        ; Initialize background graphics.
        ; Called by: init3

        ; center background graphics vertically by scrolling
        copy #0, scroll_x_mirror
        copy #4, scroll_y_mirror

        ; the Game Genie logo
        ldaxy #1, #5, #3  ; id, X, Y
        jsr draw_graphic_on_background

        ; prepare to draw the virtual keyboard
        copy #3, keyboard_graphic  ; "A"
        copy #4, keyboard_graphic_x
        copy #8, keyboard_graphic_y

        ; virtual keyboard
-       ; draw letter
        ldaxy keyboard_graphic, keyboard_graphic_x, keyboard_graphic_y  ; id, X, Y
        jsr draw_graphic_on_background
        ; increment X position
        lda keyboard_graphic_x
        add #4
        sta keyboard_graphic_x
        ; if at end of line, move to start of next line
        cmp #(9 * 4)
        bne +
        copy #4, keyboard_graphic_x
        lda keyboard_graphic_y
        add #4
        sta keyboard_graphic_y
+       ; loop until the last letter ("N", id 18) has been drawn
        inc_lda keyboard_graphic
        cmp #(18 + 1)
        bne -

        ; prepare to draw the input area (dashes)
        copy #3, keyboard_graphic  ; used as the loop counter now
        copy #4, keyboard_graphic_x
        copy #18, keyboard_graphic_y

        ; draw the 24 (3 * 8) dashes of the input area
-       ; draw dash
        ldaxy #19, keyboard_graphic_x, keyboard_graphic_y  ; id, X, Y
        jsr draw_graphic_on_background
        ; increment X position
        lda keyboard_graphic_x
        add #4
        sta keyboard_graphic_x
        ; if at end of line, move to start of next line
        cmp #(9 * 4)
        bne +
        copy #4, keyboard_graphic_x
        lda keyboard_graphic_y
        add #4
        sta keyboard_graphic_y
+       ; loop until the last dash has been drawn
        inc_lda keyboard_graphic
        cmp #(3 + 24)
        bne -

        ; set attribute table data

        ; Game Genie logo (block rows 0-3)
        ldaxy #%01010101, #4, #0  ; byte, rows, first row
        jsr fill_attribute_table_rows

        ; virtual keyboard (block rows 4-7)
        ldaxy #%00000000, #4, #4  ; byte, rows, first row
        jsr fill_attribute_table_rows

        ; 1st code (block rows 9-10)
        ldaxy #%10101010, #2, #9  ; byte, rows, first row
        jsr fill_attribute_table_rows

        ; 2nd and 3rd code (block rows 11-14)
        ldaxy #%11111111, #4, #11  ; byte, rows, first row
        jsr fill_attribute_table_rows

        ldxy #0, #0
        jsr highlight_attribute_byte  ; use attribute byte %10101010

        ; no visible effect if replaced with another value
        lda #2
        sta_absolute revolving_y_letter2
        rts

; -------------------------------------------------------------------------------------------------

move_hand
        ; Called by: do_every_frame

        ; check arrows every 2nd frame
        lda odd_frame_flag2
        eor #%00000001
        sta odd_frame_flag2
        bne +
        jsr check_arrows

        ; horizontal movement
+       lda hand_x_speed_pointer + 1
        beq move_hand_vertically  ; not moving horizontally

        ; get speed (if offset modulo 16 = 15, terminator)
        ldy hand_x_speed_offset
        lda (hand_x_speed_pointer), y
        cmp #$80
        bne +

        ; stop hand by resetting pointer
        copy #0, hand_x_speed_pointer + 1
        jmp ++

+       ; no terminator
        ldx #hand_x_pixel
        jsr add_hand_speed_to_position  ; A = speed, X = address of position; out: Z
        ; every 2nd frame, skip the rest of horizontal movement
        beq move_hand_vertically

++      tya             ; hand_x_speed_offset
        and #%00001111  ; get position on current line
        cmp #7
        bne +

        ; hand_x_speed_offset mod 16 = 7

        jsr update_hand_letter_position
        lda joypad1_status
        and last_x_input_accepted
        beq +

        lda last_x_input_accepted
        jsr set_hand_target        ; A = direction, out: A = success
        beq +                      ; fail

        ldy #(16 - 1)  ; success

+       iny
        sty hand_x_speed_offset

move_hand_vertically
        lda hand_y_speed_pointer + 1
        beq move_hand_exit  ; not moving vertically

        ; get speed (if offset modulo 16 = 15, terminator)
        ldy hand_y_speed_offset
        lda (hand_y_speed_pointer), y
        cmp #$80
        bne +

        ; stop hand by resetting pointer
        copy #0, hand_y_speed_pointer + 1
        jmp ++

+       ; no terminator
        ldx #hand_y_pixel
        jsr add_hand_speed_to_position  ; A = speed, X = address of position; out: Z
        ; every 2nd frame, skip the rest of vertical movement
        beq move_hand_exit

++      tya             ; hand_y_speed_offset
        and #%00001111  ; get position on current line
        cmp #7
        bne ++

        ; hand_y_speed_offset mod 16 = 7

        jsr update_hand_letter_position
        lda joypad1_status
        and last_y_input_accepted
        beq ++

        lda last_y_input_accepted
        jsr set_hand_target        ; A = direction, out: A = success
        beq ++                     ; fail

        ; success
        lda last_y_input_accepted
        jsr moving_between_keyboard_and_input_area  ; A = direction
        beq +  ; no
        ldy #(3 * 16 - 1)
        jmp ++
+       ldy #(16 - 1)

++      iny
        sty hand_y_speed_offset

move_hand_exit
        rts

; -------------------------------------------------------------------------------------------------

        ; Hand cursor speeds. 8 * 16 bytes. $80 = terminator.
        ; Read indirectly by move_hand using hand_x_speed_pointer and hand_y_speed_pointer.
        ; positive speeds
hand_speeds_positive
        hex 01 01 02 02 03 03 04 04 03 03 02 02 02 01 ff 80  ; sum=32
        hex 05 04 04 03 03 04 05 04 03 03 02 02 02 01 ff 80  ; sum=44
        hex 02 03 04 05 06 06 06 04 03 03 02 02 02 01 ff 80  ; sum=48
        hex 05 06 07 08 07 06 05 04 03 03 02 02 02 01 ff 80  ; sum=60
hand_speeds_negative  ; same as above but negated in two's complement
        hex ff ff fe fe fd fd fc fc fd fd fe fe fe ff 01 80
        hex fb fc fc fd fd fc fb fc fd fd fe fe fe ff 01 80
        hex fe fd fc fb fa fa fa fc fd fd fe fe fe ff 01 80
        hex fb fa f9 f8 f9 fa fb fc fd fd fe fe fe ff 01 80

; -------------------------------------------------------------------------------------------------

add_hand_speed_to_position
        ; Add hand cursor speed to position.
        ; In:
        ;     A: speed of hand cursor (horizontal/vertical)
        ;     X: address of hand cursor position (hand_x_pixel/hand_y_pixel)
        ; Out: Z: reflects odd_frame_flag2
        ; Called by: move_hand

        ; divide speed by 2, round down (toward -infinity)
        pha
        asl
        pla
        ror

        ; carry: 0 or LSB of original speed
        dec odd_frame_flag2
        beq +
        clc
+       ; add new speed and C to hand cursor position
        adc $00, x
        sta $00, x

        inc odd_frame_flag2
        rts

; -------------------------------------------------------------------------------------------------

moving_between_keyboard_and_input_area
        ; Are we moving between virtual keyboard and input area? That is,
        ;     (A = joypad_down and hand_y_letter_target == 2)
        ;     or (A = joypad_up and hand_y_letter_target == 1)
        ; In:
        ;     A: direction (joypad_up/joypad_down)
        ;     hand_y_letter_target
        ; Out: A (0 = no, 1 = yes)
        ; Called by: check_arrows, move_hand

        cmp #joypad_down
        bne +
        lda hand_y_letter_target
        cmp #2
        bne ++
        lda #1
        rts

+       ; check up direction
        cmp #joypad_up
        bne ++
        lda hand_y_letter_target
        cmp #1
        bne ++
        lda #1
        rts

++      lda #0
        rts

; -------------------------------------------------------------------------------------------------

set_hand_target
        ; Move hand cursor to specified direction if possible.
        ; In: A: direction (joypad_right/_left/_down/_up)
        ; Out:
        ;     hand_x_letter_target or hand_y_letter_target: changed
        ;     A: success (1 = yes, 0 = no)
        ; Called by: check_arrows, move_hand

        cmp #joypad_right
        bne +
        ; try to move right
        lda hand_x_letter_target
        cmp #7
        beq hand_move_fail
        inc hand_x_letter_target
        jmp hand_move_success

+       ; check left direction
        cmp #joypad_left
        bne +
        ; try to move left
        lda hand_x_letter_target
        beq hand_move_fail
        dec hand_x_letter_target
        jmp hand_move_success

+       ; check down direction
        cmp #joypad_down
        bne +
        ; try to move down
        lda hand_y_letter_target
        cmp #4
        beq hand_move_fail
        inc hand_y_letter_target
        jmp hand_move_success

+       ; check up direction
        cmp #joypad_up
        bne hand_move_success
        ; try to move up
        lda hand_y_letter_target
        beq hand_move_fail
        dec hand_y_letter_target
        jmp hand_move_success

hand_move_success
        lda #1
        rts
hand_move_fail
        lda #0
        rts

; -------------------------------------------------------------------------------------------------

highlight_attribute_byte
        ; Alternative entry point for update_attribute_byte.
        ; Called by: (see update_attribute_byte)

        lda #%10101010

update_attribute_byte
        ; Update an Attribute Table byte (2 * 2 attribute blocks).
        ; In:
        ;     A: new attribute table byte
        ;     X: horizontal position (0-7)
        ;     Y: vertical   position (0-1 = virtual keyboard, 2-4 = input area)
        ; Alternative entry points: highlight_attribute_byte, clear_attribute_byte
        ; Called by (incl. alternative entry points): init_background, update_hand_letter_position,
        ;     highlight_input_area_letter

        sta attribute_fill_byte
        ; get attribute block X
        txa
        asl
        sta vram_block_x
        ; get attribute block Y
        tya
        asl
        add #4
        cmp #8
        bcc +  ; on the virtual keyboard
        add #1
+       ; update 2 * 2 attribute blocks
        sta vram_block_y
        jsr update_attribute_block
        inc vram_block_x
        jsr update_attribute_block
        inc vram_block_y
        jsr update_attribute_block
        dec vram_block_x
        jmp update_attribute_block  ; ends with rts

clear_attribute_byte
        ; Alternative entry point for update_attribute_byte.
        ; Called by: (see update_attribute_byte)

        lda #%00000000
        jmp update_attribute_byte

; -------------------------------------------------------------------------------------------------

update_hand_letter_position
        ; Called by: move_hand

        phy

        ; un-highlight the letter the cursor is leaving
        ldxy hand_x_keyboard, hand_y_keyboard
        jsr clear_attribute_byte
        ; where is the target letter (virtual keyboard or input area)?
        lda hand_y_letter_target
        cmp #2
        bcs target_on_input_area

        ; target letter is on virtual keyboard
        ; update actual hand position and last position on keyboard
        sta hand_y_keyboard
        sta hand_y_letter
        lda hand_x_letter_target
        sta hand_x_keyboard
        sta hand_x_letter
        ; highlight the letter the cursor is on
        ldxy hand_x_keyboard, hand_y_keyboard
        jsr highlight_attribute_byte

        ply
        rts

target_on_input_area
        ; target letter is on input area
        ; update actual hand position
        sta hand_y_letter
        copy hand_x_letter_target, hand_x_letter

        ply
        rts

; -------------------------------------------------------------------------------------------------

initial_sprite_attribute_data
        ; bits: VHBUUUPP (Vflip, Hflip, Behind bg, Unimplemented, Palette);
        ; in reverse order
        db %00011001, %00011001, %00011001, %00011001, %00011010  ; 19...15
        db %00011001, %00011001, %00011001, %00011001, %00011010  ; 14...10
        db %00011001, %00011001, %00011001, %00011001, %00011010  ;  9... 5
        db %00011001, %00011001, %00011001, %00011001, %00011010  ;  4... 0

; -------------------------------------------------------------------------------------------------

update_revolving_cursor_attributes
        ; change attributes of revolving cursor's sprites (20-23)

        ; change subpalette from 0 to 2 or vice versa
        lda sprite_attributes + 20
        cmp #%00011010  ; VHBUUUPP
        bne +
        lda #%00011000
        jmp ++
+       lda #%00011010  ; use the 3rd subpalette

        ; put sprite behind background if phase is 0-6
++      ldx_absolute revolving_phase
        cpx #7
        bcs +
        ora #%00100000
+

        sta sprite_attributes + 20
        sta sprite_attributes + 21
        sta sprite_attributes + 22
        sta sprite_attributes + 23
        rts

; -------------------------------------------------------------------------------------------------

letter_input
        ; Button A went from off to on, with the hand on virtual keyboard.
        ; Called by: check_button_A

        ; graphic id to draw (see graphics_offsets) -> stack
        ; (hand_y_letter * 8 + hand_x_letter + 3)
        lda hand_y_letter
        asl
        asl
        asl
        add hand_x_letter
        adc #3
        pha

        ; Y position of the revolving cursor in tiles -> Y
        ; (revolving_y_letter1 * 4 + 18)
        lda revolving_y_letter1
        asl
        asl
        add #18
        tay

        ; X position of the revolving cursor in tiles -> X
        ; (revolving_x_letter1 * 4 + 4)
        lda revolving_x_letter1
        asl
        asl
        add #4
        tax

        pla
        pha
        jsr draw_graphic_on_background  ; A = id, X = X, Y = Y
        jsr letter_input_extra_effects
        pla
        jsr save_entered_letter

        jmp +  ; why?
+

        ; increment cursor X position (may be undone later)
        inc revolving_x_letter1

        ; six letters on current line?
        lda revolving_x_letter1
        cmp #6
        bne +  ; no

        ; six letters on current line

        ; third letter on current line -> X
        lda revolving_y_letter1
        asl
        asl
        asl
        add #(3 - 1)
        tax

        ; if A/P/Z/L/G/I/T/Y, the code is complete
        lda entered_letters, x
        sub #3
        and #%00000001
        beq code_complete

        lda revolving_x_letter1  ; always six, so the next branch is always taken
+       ; eight letters entered?
        cmp #8
        bne letter_input_end  ; no

code_complete
        ; if not on last line, move to start of next line; otherwise, undo cursor X increment
        lda revolving_y_letter1
        cmp #2
        beq +
        copy #0, revolving_x_letter1
        inc revolving_y_letter1
        jmp letter_input_end
+       dec revolving_x_letter1

letter_input_end
        ; move the highlight to the new letter and "rts"
        jmp highlight_input_area_letter

; -------------------------------------------------------------------------------------------------

check_button_a
        ; If status of button A has changed from off to on, input a letter (if hand is on keyboard)
        ; or move revolving cursor (if hand is on input area).
        ; Called by: do_every_frame

        lda joypad1_status
        and #joypad_a
        cmp prev_button_a_status
        bne +
        rts
+       sta prev_button_a_status
        cmp #joypad_a
        beq +
        rts
+       lda hand_y_letter
        cmp #2
        bcs +
        ; hand is on keyboard
        jmp letter_input  ; ends with rts
+       ; hand is on input area
        jmp move_revolving_cursor_manually  ; ends with rts

; -------------------------------------------------------------------------------------------------

check_button_b
        ; Called by: do_every_frame

        ; if status of button B has changed from off to on, continue, otherwise exit
        lda joypad1_status
        and #joypad_b
        cmp prev_button_b_status
        bne +
        rts
+       sta prev_button_b_status
        cmp #joypad_b
        beq +
        rts
+       jmp +  ; why?

+       ; continue depending on where the revolving cursor is
        lda revolving_x_letter1
        beq move_backwards_or_exit  ; on 1st column
        lda revolving_y_letter1
        cmp #2
        bne exit_if_no_letter  ; on 1st/2nd code, 2nd-8th column

        ; non-first column, last code;
        ; maximum length of last code minus one -> A (7 if 3rd letter is E/O/X/U/K/S/V/N, else 5)
        lda entered_letters + 2 * 8 + 2
        sub #3
        and #%00000001
        bne +
        lda #(6 - 1)
        bne ++
+       lda #(8 - 1)
++

        cmp revolving_x_letter1
        bne exit_if_no_letter  ; cursor not on last possible letter of last code

        ; cursor is on last possible letter of last code (revolving_x_letter1 = 5/7);
        ; is the code of maximum length?
        tax
        lda entered_letters + 2 * 8, x
        bne erase_letter  ; last code is maximum length (cursor is on last letter)
        ; 3rd code is maximum length minus one (cursor is on following position)

exit_if_no_letter
        ; cursor is on 2nd-8th column, except on last possible letter of 3rd code
        jsr get_letter_at_revolving_cursor  ; is letter zero -> Z
        bne erase_letter_end

move_backwards_or_exit
        ; cursor is on a dash and/or on 1st column;
        ; if on first letter of first code, exit, otherwise move cursor and erase letter
        lda revolving_x_letter1
        ora revolving_y_letter1
        beq erase_letter_end
        jsr move_revolving_cursor_backwards

erase_letter
        ; get position of letter to erase in tiles, store to X&Y
        lda revolving_y_letter1
        asl
        asl
        add #18
        tay
        lda revolving_x_letter1
        asl
        asl
        add #4
        tax

        ; spawn particles, draw dash on letter, mark letter as empty
        jsr spawn_particles1
        lda #19
        jsr draw_graphic_on_background  ; A = id, X = X, Y = Y
        lda #0
        jmp save_entered_letter  ; ends with rts

erase_letter_end
        jmp move_revolving_cursor_backwards  ; why?

; -------------------------------------------------------------------------------------------------

move_revolving_cursor_backwards
        ; Called by: check_button_b

        ; if column > 0:          move backwards
        ; if column = 0, row > 0: move to end of previous line
        ; if column = 0, row = 0: do nothing
        ;
        dec revolving_x_letter1
        lda revolving_x_letter1
        cmp #$ff
        bne ++
        lda revolving_y_letter1
        beq +
        copy #7, revolving_x_letter1
        dec revolving_y_letter1
        jmp ++
+       inc revolving_x_letter1
++      jsr fix_revolving_cursor_x  ; move left to position after last letter

        jmp highlight_input_area_letter  ; ends with rts

; -------------------------------------------------------------------------------------------------

move_revolving_cursor_manually
        ; Called by: check_button_a

        ; move revolving cursor to hand cursor
        lda hand_y_letter
        sub #2
        sta revolving_y_letter1
        copy hand_x_letter, revolving_x_letter1
        jsr fix_revolving_cursor_x

        phx
        phy  ; why?

        jsr highlight_input_area_letter

        ply
        plx

        rts

; -------------------------------------------------------------------------------------------------

update_revolving_cursor
        ; Update horizontal and vertical position, phase and attributes of the revolving cursor.
        ; Called by: do_every_frame

        ; revolving_y_letter1 * 32 + 152 -> revolving_y_target
        lda revolving_y_letter1
        asl
        asl
        asl
        asl
        asl
        add #152
        sta revolving_y_target

        ; revolving_x1 * 32 + 10 -> revolving_x_target
        lda revolving_x_letter1
        asl
        asl
        asl
        asl
        asl
        add #10
        sta revolving_x_target

        ; compute horizontal speed
        lda revolving_x
        sta_absolute revolving_pos
        lda revolving_x_target
        sta_absolute revolving_target
        jsr compute_revolving_cursor_speed
        add #128
        sta revolving_target_speed
        lda revolving_speed_x
        jsr accelerate_revolving_cursor  ; A = speed
        sta revolving_speed_x

        ; compute vertical speed
        lda revolving_y
        sta_absolute revolving_pos
        lda revolving_y_target
        sta_absolute revolving_target
        jsr compute_revolving_cursor_speed
        add #128
        sta revolving_target_speed
        lda revolving_speed_y
        jsr accelerate_revolving_cursor  ; A = speed
        sta revolving_speed_y

        ; add horizontal speed to horizontal position
        lda revolving_x
        add revolving_speed_x
        sta revolving_x

        ; add vertical speed to vertical position
        lda revolving_y
        add revolving_speed_y
        sta revolving_y

        ; adjust X position by phase
        ldx revolving_phase
        lda revolving_x
        clc
        adc revolving_cursor_x_offsets, x
        sta metasprite_x

        ; adjust Y position by phase
        lda revolving_y
        clc
        adc revolving_cursor_y_offsets, x
        sta metasprite_y

        ; increment phase
        inx
        cpx #16
        bne +
        ldx #0
+       stx revolving_phase

        ; update metasprite position
        ldx revolving_metasprite
        jsr update_metasprite  ; X = metasprite index

        ; update attributes of sprites
        jmp update_revolving_cursor_attributes  ; ends with rts

revolving_cursor_x_offsets
        ; Sine wave in two's complement. 17 values, -10...+10.
        ; Read by: update_revolving_cursor

        db 0, 4, 7, 9, 10, 9, 7, 4
        db 0, 256 - 4, 256 - 7, 256 - 9, 256 - 10, 256 - 9, 256 - 7, 256 - 4
        db 0  ; never accessed

revolving_cursor_y_offsets
        ; Inverted cosine wave in two's complement. 17 values, -10...+10.
        ; Read by: update_revolving_cursor

        db 256 - 10, 256 - 9, 256 - 7, 256 - 4, 0, 4, 7, 9
        db 10, 9, 7, 4, 0, 256 - 4, 256 - 7, 256 - 9
        db 256 - 10  ; never accessed

; -------------------------------------------------------------------------------------------------

compute_revolving_cursor_speed
        ; Compute speed for revolving cursor in X or Y direction.
        ; In:
        ;     revolving_pos
        ;     revolving_target
        ; Out: A: speed in pixels per frame as a signed integer:
        ;     if pos < target: 1 + floor((target - pos) / 8)
        ;     if pos = target: 0
        ;     if pos > target: ((target - pos) >> 3) | %11100000
        ; Called by: update_revolving_cursor

        lda revolving_pos
        cmp revolving_target
        bcc ++  ; too small
        bne +   ; too large

        lda #0
        rts

+       ; too large
        lda revolving_target
        sub revolving_pos
        ; shift negative number right (sign extension); why not just three SEC & ROR?
        ldx #3
-       sec
        ror
        dex
        bne -
        rts

        ; too small
++      lda revolving_target
        sub revolving_pos
        ; why not just three LSR?
        ldx #3
-       lsr
        dex
        bne -
        add #1
        rts

; -------------------------------------------------------------------------------------------------

accelerate_revolving_cursor
        ; Accelerate the revolving cursor by -1/0/+1 towards the target speed.
        ; In:
        ;   A: revolving cursor speed
        ;       (horizontal/vertical, pixels/frame, two's complement)
        ;   revolving_target_speed: target speed
        ;       (horizontal/vertical, pixels/frame, excess-128)
        ; Out: A: revolving cursor speed
        ; Called by: update_revolving_cursor

        eor #%10000000
        cmp revolving_target_speed
        bcc +   ; speed too small
        beq ++
        sbc #1
        jmp ++
+       adc #1
++      eor #%10000000
        rts

; -------------------------------------------------------------------------------------------------

spawn_particles2
        ; Spawn one of two sets of particles.
        ; In:  particle_set_flag: which set to spawn
        ; Out: particle_set_flag: flipped
        ; Called by: spawn_particles1

        lda particle_set_flag
        beq +

        ; flip flag, spawn set #1
        copy #0, particle_set_flag
        ldx #48
        jsr spawn_particles3  ; X = index to sprite data
        copy #1, particle_set2_timer
        rts

+       ; flip flag, spawn set #0
        copy #1, particle_set_flag
        ldx #32
        jsr spawn_particles3  ; X = index to sprite data
        copy #1, particle_set1_timer
        rts

; -------------------------------------------------------------------------------------------------

        ; Four waves in two's complement. 8 values per wave. Read by spawn_particles3.
initial_particle_speeds_x
        db 0, 6, 8, 6, 0, 256 - 6, 256 - 8, 256 - 6  ; sine, outer ring (-8...+8)
        db 0, 3, 4, 3, 0, 256 - 3, 256 - 4, 256 - 3  ; sine, inner ring (-4...+4)
initial_particle_speeds_y
        db 256 - 8, 256 - 6, 0, 6, 8, 6, 0, 256 - 6  ; inverted cosine, outer ring (-8...+8)
        db 256 - 4, 256 - 3, 0, 3, 4, 3, 0, 256 - 3  ; inverted cosine, inner ring (-4...+4)

spawn_particles3
        ; Write initial particle data.
        ; In: X: first index to sprite data
        ; Called by: spawn_particles2

        ldy #0  ; particle index
-       ; X position
        lda initial_particle_speeds_x, y
        add particle_start_x
        sta sprite_x_positions, x
        ; Y position
        lda initial_particle_speeds_y, y
        add particle_start_y
        sta sprite_y_positions, x
        ; X speed
        lda initial_particle_speeds_x, y
        sta particle_speeds_x, x
        ; Y speed
        lda initial_particle_speeds_y, y
        sta particle_speeds_y, x
        ; tile
        lda #$01
        sta sprite_tiles, x
        ; attribute (unused bits %110, palette %10)
        lda #%00011010
        sta sprite_attributes, x
        ; loop counters
        inx
        iny
        cpy #16
        bne -

        ; make noise
        copy #%00001110, snd_noise_freq1
        copy #%00000100, snd_noise_freq2
        copy #%00100101, snd_noise_ctrl1

        copy #24, particle_time_left  ; set timer
        rts

; -------------------------------------------------------------------------------------------------

move_particles
        ; Called by: do_every_frame

        ; stop noise if particle_time_left goes to 0
        lda particle_time_left
        beq +
        dec particle_time_left
        bne +
        copy #%00110000, snd_noise_ctrl1

+       ; process 1st set of particles (from every 2nd explosion)
        lda particle_set1_timer
        beq process_particle_set2
        ldx #32
        inc_lda particle_set1_timer
        cmp #24
        beq hide_particle_set1
        jsr move_particles2  ; A = timer, X = index to sprite data

process_particle_set2
        ; process 2nd set of particles (from every 2nd explosion)
        lda particle_set2_timer
        beq +
        ldx #48
        inc_lda particle_set2_timer
        cmp #24
        beq hide_particle_set2
        jsr move_particles2  ; A = timer, X = index to sprite data
+       rts

hide_particle_set1
        copy #0, particle_set1_timer
        jsr hide_particle_set
        jmp process_particle_set2

hide_particle_set2
        copy #0, particle_set2_timer
        jmp hide_particle_set  ; ends with rts

; -------------------------------------------------------------------------------------------------

move_particles2
        ; In:
        ;     A: timer (particle_set1_timer/particle_set2_timer)
        ;     X: index to sprite data (32/48)
        ; Called by: move_particles

        ; timer modulo 8 -> temp1
        and #%00000111
        sta temp1

        ldy #16
particle_loop
        ; if sprite is hidden, skip it
        lda sprite_y_positions, x
        cmp #255
        beq particle_processed

        ; change palette
        lda sprite_attributes, x
        eor #%00000010
        sta sprite_attributes, x

        ; detect underflow/overflow of X position
        lda particle_speeds_x, x
        bpl +
        ; moving left
        clc
        adc sprite_x_positions, x
        bcs ++
        jmp hide_particle
+       ; moving right
        clc
        adc sprite_x_positions, x
        bcc ++
hide_particle
        ; X/Y position underflow/overflow; hide sprite and move on
        lda #$ff
        sta sprite_y_positions, x
        lda #$00
        sta sprite_attributes, x
        jmp particle_processed

++      sta sprite_x_positions, x

        ; detect Y position underflow/overflow
        lda particle_speeds_y, x
        bpl +
        ; moving up
        clc
        adc sprite_y_positions, x
        bcs particle_y_checked
        jmp hide_particle
+       ; moving down
        clc
        adc sprite_y_positions, x
        bcs hide_particle
particle_y_checked
        sta sprite_y_positions, x

        ; slow particle down every 8th frame
        lda temp1  ; timer modulo 8
        bne particle_processed
        lda particle_speeds_x, x
        jsr decrement_absolute_value  ; A: value to decrement
        sta particle_speeds_x, x
        lda particle_speeds_y, x
        jsr decrement_absolute_value  ; A: value to decrement
        sta particle_speeds_y, x

particle_processed
        inx
        dey
        bne particle_loop
        rts

; -------------------------------------------------------------------------------------------------

decrement_absolute_value
        ; If number is nonzero, decrement absolute value in two's complement.
        ; In:
        ;     A: number to decrement
        ;     N, Z: reflect A
        ; Called by: move_particles2

        beq ++
        bpl +
        add #1
        rts
+       sub #1
++      rts

; -------------------------------------------------------------------------------------------------

hide_particle_set
        ; Hide one set of particles (16 sprites starting from X).
        ; Called by: move_particles

        ldy #16
-       lda #%00000000
        sta sprite_attributes, x
        lda #255
        sta sprite_y_positions, x
        inx
        dey
        bne -
        rts

; -------------------------------------------------------------------------------------------------

spawn_particles1
        ; If there is a letter at revolving cursor, continue to prepare particles.
        ; In:
        ;     X: horizontal position of letter, in tiles
        ;     Y: vertical   position of letter, in tiles
        ; Called by: check_button_b

        tya
        pha

        ; Y * 8 + 10 -> particle_start_y
        asl
        asl
        asl
        add #10
        sta particle_start_y

        txa
        pha

        ; (X - 4) * 8 + 13 -> particle_start_x
        sub #4
        asl
        asl
        asl
        add #13
        sta particle_start_x

        ; continue spawning only if revolving cursor is on a letter
        ; (why not check this earlier?)
        jsr get_letter_at_revolving_cursor  ; letter -> A, position -> X
        beq +
        jsr spawn_particles2

+       plx
        ply

        rts

; -------------------------------------------------------------------------------------------------

save_entered_letter
        ; Save entered letter to RAM.
        ; Called by: letter_input, check_button_b

        pha
        jsr get_revolving_cursor_position  ; 0...23 -> X
        pla
        sta entered_letters, x
        rts

; -------------------------------------------------------------------------------------------------

fix_revolving_cursor_x
        ; Move revolving cursor left until it lies immediately after a letter, on
        ; a letter or on the first column.
        ; Out:
        ;     X: letter position to highlight (index to entered_letters)
        ;     revolving_x_letter1
        ; Called by: check_button_b, move_revolving_cursor_manually

        jsr get_revolving_cursor_position  ; 0...23 -> X
        ; exit if revolving cursor at column 0 or letter at cursor
        lda revolving_x_letter1
        beq ++
        lda entered_letters, x
        bne ++
        ; move revolving cursor left until at column 0 or letter at cursor
-       dex
        lda entered_letters, x
        bne ++
        dec revolving_x_letter1
        bne -
++      rts

; -------------------------------------------------------------------------------------------------

get_letter_at_revolving_cursor
        ; Out:
        ;     A: letter
        ;     X: cursor position (0...23)
        ;     Z: reflects A
        ; Called by: check_button_b, spawn_particles1

        jsr get_revolving_cursor_position
        lda entered_letters, x
        rts

; -------------------------------------------------------------------------------------------------

get_revolving_cursor_position
        ; Out:
        ;     X: cursor position (0...23)
        ; Called by: save_entered_letter, fix_revolving_cursor_x, get_letter_at_revolving_cursor

        ; revolving_y_letter1 * 8 + revolving_x_letter1
        lda revolving_y_letter1
        asl
        asl
        asl
        add revolving_x_letter1
        tax
        rts

; -------------------------------------------------------------------------------------------------

letter_input_extra_effects
        ; Spawn flying letter and play sound (after drawing a letter).
        ; Called by: letter_input

        ; hand_x_letter * 32 -> flying_x, metasprite_x
        lda hand_x_letter
        asl
        asl
        asl
        asl
        asl
        sta flying_x
        sta metasprite_x

        ; hand_y_letter * 8 -> stack
        ; hand_y_letter * 32 + 64 -> flying_y, metasprite_y
        lda hand_y_letter
        asl
        asl
        asl
        pha
        asl
        asl
        add #64
        sta flying_y
        sta metasprite_y

        ; hand_y_letter * 8 + hand_x_letter -> entered_letter
        ; (for playing sound)
        pla
        add hand_x_letter
        sta_absolute entered_letter

        ; spawn flying letter
        ; graphic id: hand_y_letter * 8 + hand_x_letter + 3
        ; (3 is the id for the first letter, "A")
        adc #3
        ldx flying_metasprite
        jsr init_metasprite  ; A = id, X = index to metasprite_indexes

        ldx flying_metasprite
        jsr update_metasprite  ; X = metasprite index

        ; compute Y speed of flying letter (+3...+9 in increments of 2):
        ; (revolving_y_letter1 * 32 + 144 - flying_y) / 16 -> flying_y_speed
        lda revolving_y_letter1
        asl
        asl
        asl
        asl
        asl
        add #144
        sub flying_y
        lsr
        lsr
        lsr
        lsr
        sta flying_y_speed

        ; compute X speed of flying letter (-14...+14 in increments of 2):
        ; (revolving_x_letter1 - hand_x_letter) * 2 -> flying_x_speed
        lda revolving_x_letter1
        sub hand_x_letter
        asl
        sta flying_x_speed

        copy #16, flying_time_left1

        ; play sound depending on letter

        ; bits of entered_letter: 0000abcd
        ; 000000ab -> A, cd000000 -> temp1
        lda_absolute entered_letter
        asl
        asl
        asl
        asl
        sta temp1
        lda #$00
        asl temp1
        rol
        asl temp1
        rol

        add #2
        sta snd_pulse1_ct
        copy temp1, snd_pulse1_ft
        copy #%00100100, snd_pulse1_ctrl
        copy #%11111001, snd_pulse1_ramp_ctrl

        lda #20
        sta_absolute flying_time_left2
        rts

; -------------------------------------------------------------------------------------------------

move_flying_letter
        ; Called by: do_every_frame

        lda flying_time_left2
        beq +
        dec flying_time_left2
        bne +
        ; flying_time_left2 went from 1 to 0
        copy #%00110000, snd_pulse1_ctrl
+       lda flying_time_left1
        bne +
        rts

+       dec flying_time_left1
        bne +
        ; flying_time_left went from 1 to 0
        ldx flying_metasprite
        copy #255, metasprite_y
        jmp update_metasprite  ; X = metasprite index; ends with rts

+       ; update flying cursor X position
        lda flying_x
        add flying_x_speed
        sta flying_x
        sta metasprite_x
        ; update flying cursor Y position
        lda flying_y
        add flying_y_speed
        sta flying_y
        sta metasprite_y

        ldx flying_metasprite
        jmp update_metasprite  ; X = metasprite index; ends with rts

; -------------------------------------------------------------------------------------------------

fill_attribute_table_rows
        ; Fill attribute block rows.
        ; In:
        ;     A: fill byte
        ;     X: number of rows
        ;     Y: first row
        ; Called by: init_background

        sta attribute_fill_byte
        sty vram_block_y
        stx rows_left
--      ; fill one row
        lda #$00
-       sta vram_block_x
        jsr update_attribute_block
        lda vram_block_x
        add #1
        cmp #16
        bne -
        ; next row
        inc vram_block_y
        dec rows_left
        bne --
        rts

; -------------------------------------------------------------------------------------------------

animate_color
        ; Animate color 3 of background subpalette 1.
        ; Called by: do_every_frame

        ; increment animated_color_delay every frame
        ; increment animated_color_phase every 5th frame
        ldx animated_color_phase
        ldy animated_color_delay
        iny
        cpy #5
        bne +
        ldy #0
        inx
+       sty animated_color_delay
        cpx #8
        bne +
        ldx #0
+       stx animated_color_phase

        ; set up VRAM block to update
        ; data
        lda animated_colors, x
        sta vram_block + 3
        ; data size
        copy #1, vram_block + 0
        ; address
        copy #>(ppu_palettes + 4 + 3), vram_block + 1
        copy #<(ppu_palettes + 4 + 3), vram_block + 2

        ; copy block to buffer
        jmp vram_block_to_buffer  ; ends with rts

animated_colors
        ; the phases of the animated color
        db color_animated0, color_animated1, color_animated2, color_animated3
        db color_animated4, color_animated5, color_animated6, color_animated7

; -------------------------------------------------------------------------------------------------

highlight_input_area_letter
        ; Move highlight to another letter on input area.
        ; In: revolving_x_letter1, revolving_x_letter2, revolving_y_letter1, revolving_y_letter2
        ; Called by: letter_input, check_button_b, move_revolving_cursor_manually

        ; set attribute %10 to the letter the revolving cursor exits; store old Y position
        ldxy revolving_x_letter2, revolving_y_letter2
        sty revolving_y_letter2_prev
        lda #%10101010
        jsr update_attribute_byte  ; A = byte, X = X, Y = Y

        ; update Y position
        lda revolving_y_letter1
        add #2
        sta revolving_y_letter2

        ; update X position
        copy revolving_x_letter1, revolving_x_letter2

        jsr highlight_input_area_row

        ; set attribute %01 to the letter the revolving cursor enters
        ldxy revolving_x_letter2, revolving_y_letter2
        lda #%01010101
        jmp update_attribute_byte  ; A = byte, X = X, Y = Y; ends with RTS

; -------------------------------------------------------------------------------------------------

input_area_row_attributes
        ; Attribute table data to write when entering a row on input area (16 bytes).
        ; Read by: highlight_input_area_row

        db %10101111, %10101111, %10101111, %10101111
        db %10101111, %10101111, %10101111, %10101111
        db %11111010, %11111010, %11111010, %11111010
        db %11111010, %11111010, %11111010, %11111010

highlight_input_area_row
        ; Highlight active row on input area using the Attribute Table.
        ; In: revolving_y_letter2, revolving_y_letter2_prev
        ; Called by: highlight_input_area_letter

        ; exit if Y position of revolving cursor has not changed
        lda revolving_y_letter2_prev
        cmp revolving_y_letter2
        beq highlight_exit

        ; set up VRAM block to change attribute data of all rows to %11 (gray)

        ; address
        copy #>(ppu_attribute_table + 4 * 8), vram_block + 1
        copy #<(ppu_attribute_table + 4 * 8), vram_block + 2
        ; data: 32 bytes, all %11111111
        ldy #(4 * 8 - 1)
-       lda #%11111111
        sta vram_block + 3, y  ; vram_block + 3 = start of data
        sta $0400 + 32, y
        dey
        bpl -
        ; data size
        copy #32, vram_block + 0
        ; copy
        jsr vram_block_to_buffer

        ; set up VRAM block to change attribute data of active row to %10 (white)

        ; address: ppu_attribute_table + (2 + revolving_y_letter2) * 8
        lda revolving_y_letter2
        sub #2
        asl
        asl
        asl
        add #<(ppu_attribute_table + 4 * 8)
        sta vram_block + 2
        copy #>(ppu_attribute_table + 4 * 8), vram_block + 1
        ; data: 16 bytes from input_area_row_attributes
        ldy #(2 * 8 - 1)
-       lda input_area_row_attributes, y
        sta vram_block + 3, y
        dey
        bpl -
        copy #16, vram_block + 0  ; data size
        jsr vram_block_to_buffer  ; copy

highlight_exit
        rts

; -------------------------------------------------------------------------------------------------

check_select_and_start
        ; If select or start pressed, decode entered codes and start game.
        ; Called by: do_every_frame

        ; If neither pressed, allow them next time and exit.
        ; If either pressed but not allowed, just exit.
        ; If either pressed and allowed, continue.
        lda joypad1_status
        and #(joypad_select | joypad_start)
        bne +
        copy #1, temp2  ; allow select/start
-       rts
+       lda temp2  ; allow select/start?
        beq -

        ; disable rendering
        lda #%00000000
        sta ppu_ctrl
        sta ppu_mask

        ; set source pointer
        lda #<entered_letters
        sta code_pointer + 0
        lda #>entered_letters
        sta code_pointer + 1

        ; set target pointer
        lda #<decoded_codes
        sta decoded_codes_pointer + 0
        lda #>decoded_codes
        sta decoded_codes_pointer + 1

        ; fill decoded_codes with $ff (why 16 bytes?)
        ldx #(16 - 1)
        lda #$ff
-       sta decoded_codes, x
        dex
        bpl -

        ; number of codes to decode
        copy #3, codes_left_to_decode

        ; bitmasks
        ; %11101111/%11011111/%10111111 for 1st/2nd/3rd code, respectively
        copy #%11101111, code_enable_mask
        ; %00000010/%00000100/%00001000 for 1st/2nd/3rd code, respectively
        copy #%00000010, compare_enable_mask

        ; will be written to genie_master_control; bits: 0ABCabc1
        ; (A/B/C = disable code, a/b/c = compare enable);
        ; start with all codes disabled
        copy #%01110001, genie_control_value

        ; start the long outer loop
all_codes_decode_loop
        ldy #%00000000
        sty temp2  ; LSB of previous letter

        ; Phase 1/2 of decoding: modify the letters of one code:
        ; - if 0 (no letter), exit loop
        ; - subtract 3 to get a 4-bit value
        ; - shift right once
        ; - copy 4th-least significant bit from least significant bit of previous
        ;   value (always 0 for the first value)
-       lda (code_pointer), y
        beq decoded
        sub #3
        and #%00001111
        lsr
        ora temp2  ; LSB of previous letter
        sta (code_pointer), y
        ;
        ; store the bit that just got shifted out, shifted to the 4th-least
        ; significant position (%00000000 or %00001000)
        lda #$00
        rol
        asl
        asl
        asl
        sta temp2
        ;
        iny
        cpy #8
        bne -

        ; if the code is not 6 or 8 letters, ignore it
decoded sty code_length
        cpy #8
        beq longcod
        cpy #6
        beq shorcod
        jmp nextcod

        ; regardless of code length, read the fourth value and do nothing with it
longcod ldy #3
        lda (code_pointer), y
        jmp +
shorcod ldy #3
        lda (code_pointer), y

+       ; copy the bit that got shifted out from the last value, to the 4th-least
        ; significant position of the 1st value; thus, the values will have been
        ; rotated instead of shifted
        ldy #0
        lda (code_pointer), y
        and #%00000111
        ora temp2  ; LSB of previous letter
        sta (code_pointer), y

        ; Phase 2/2 of decoding:
        ; copy 8 nybbles from one semi-decoded code, in order specified by
        ; codekey, to 4 bytes in decoded_code.
        ldx #0    ; source nybble offset
        stx temp2  ; target byte offset
        ; value to high nybble
-       ldy codekey, x
        lda (code_pointer), y
        asl
        asl
        asl
        asl
        ; value to low nybble
        inx
        ldy codekey, x
        ora (code_pointer), y
        ; store
        ldy temp2  ; target byte offset
        sta decoded_code, y
        ; next target byte
        inc temp2  ; target byte offset
        inx
        ; end of loop
        cpx #8
        bne -

        ; clear MSB of address
        lda decoded_code + 0
        and #%01111111
        sta decoded_code + 0

        ; compare the address to codes stored in decoded_codes

        lda decoded_code + 0  ; address high
        ldx decoded_code + 1  ; address low

        ; ignore the code if the address is the same as in the first code
        cmp decoded_codes + 0
        bne +
        cpx decoded_codes + 1
        beq nextcod  ; ignore the code

+       ; ignore the code if the address is the same as in the second code
        cmp decoded_codes + 4
        bne +
        cpx decoded_codes + 4 + 1
        beq nextcod  ; ignore the code

+       ; ignore the code if the address is the same as in the third code
        ; (The code is never ignored here because the address in decoded_codes
        ; is always $ffff at this stage.)
        cmp decoded_codes + 2 * 4
        bne +
        cpx decoded_codes + 2 * 4 + 1  ; never accessed
        beq nextcod       ; ignore the code (never accessed)

+       ; store the code to decoded_codes
        ; (note: the replace value and the compare value trade places)

        ; address
        ldy #1
-       lda decoded_code, y
        sta (decoded_codes_pointer), y
        dey
        bpl -
        ; replace value
        ldy #3
        lda decoded_code - 1, y
        sta (decoded_codes_pointer), y
        ; compare value
        dey
        lda decoded_code + 1, y
        sta (decoded_codes_pointer), y

        ; Enable this code by ANDing genie_control_value with code_enable_mask.
        ; If the code is 8 letters, also enable the compare value by ORing
        ; genie_control_value with compare_enable_mask.
        lda genie_control_value
        and code_enable_mask
        ldx code_length
        cpx #8
        bne +
        ora compare_enable_mask  ; 8-letter code
+       sta genie_control_value

        ; prepare for the next code

        ; change control value masks
nextcod sec
        rol code_enable_mask
        asl compare_enable_mask

        ; advance source pointer
        ; The high byte is never incremented because the low byte starts from
        ; only $6b (the low byte of entered_letters).
        lda code_pointer + 0
        add #8
        sta code_pointer + 0
        bcc +
        inc code_pointer + 1  ; never accessed

+       ; advance target pointer
        ; The high byte is never incremented because the low byte starts from
        ; only $90 (the low byte of decoded_codes).
        lda decoded_codes_pointer + 0
        add #4
        sta decoded_codes_pointer + 0
        bcc +
        inc decoded_codes_pointer + 1  ; never accessed

+       ; the end of the long outer loop
        dec codes_left_to_decode
        beq +
        jmp all_codes_decode_loop

+       ; copy a short program from ROM to RAM
        ; (for some reason, two extra bytes are copied)
        ldx #(ram_program_end - ram_program + 2 - 1)  ; bytes to copy, minus one
-       lda ram_program, x
        sta ram_program_target, x
        dex
        bpl -

        ; execute the program in RAM
        jmp ram_program_target

; -------------------------------------------------------------------------------------------------

        ; a short program that is copied to RAM and executed
        ; copy decoded codes to Game Genie registers ($8001-$800c)
ram_program
        ldx #(3 * 4 - 1)
-       lda decoded_codes, x
        sta genie_values, x
        dex
        bpl -
        ; tell the hardware which codes are enabled and whether they use compare
        ; values, then switch to game mode and reset the system
        copy genie_control_value, genie_master_control
        copy #%00000000, genie_master_control
        jmp (reset_vector)
ram_program_end

; -------------------------------------------------------------------------------------------------

        ; How to descramble the codes.
        ; Read by: check_select_and_start
codekey db 3, 5, 2, 4, 1, 0, 7, 6

; -------------------------------------------------------------------------------------------------

macro offset_to_graphic _addr
        ; Emit a 16-bit offset relative to graphics_offsets, high byte first.
        db >(_addr - graphics_offsets), <(_addr - graphics_offsets)
endm

graphics_offsets
        ; Offsets to actual graphics data (see below). 2 bytes each, high byte first.
        ; Read indirectly by set_graphics_pointer using graphics_pointer.

        offset_to_graphic graphic_unused    ;  0 (never accessed)
        offset_to_graphic graphic_logo      ;  1
        offset_to_graphic graphic_revolv    ;  2
        offset_to_graphic graphic_a         ;  3
        offset_to_graphic graphic_e         ;  4
        offset_to_graphic graphic_p         ;  5
        offset_to_graphic graphic_o         ;  6
        offset_to_graphic graphic_z         ;  7
        offset_to_graphic graphic_letter_x  ;  8
        offset_to_graphic graphic_l         ;  9
        offset_to_graphic graphic_u         ; 10
        offset_to_graphic graphic_g         ; 11
        offset_to_graphic graphic_k         ; 12
        offset_to_graphic graphic_i         ; 13
        offset_to_graphic graphic_s         ; 14
        offset_to_graphic graphic_t         ; 15
        offset_to_graphic graphic_v         ; 16
        offset_to_graphic graphic_letter_y  ; 17
        offset_to_graphic graphic_n         ; 18
        offset_to_graphic graphic_dash      ; 19
        offset_to_graphic graphic_hand      ; 20

; -------------------------------------------------------------------------------------------------

; The actual graphics data.
; Read indirectly using graphics_pointer.
; Read by: draw_graphic_on_background, graphic_nybble_to_vram_buffer, assign_metasprite_to_graphic,
;     init_metasprite

; Format of each graphic:
;   1 byte: width in tiles
;   1 byte: height in tiles
;   width * height / 2 bytes: data:
;       1 nybble = 1 tile
;       bits in each nybble (3 = MSB) represent 2 * 2 virtual pixels:
;           23
;           01

graphic_unused  ; an invalid graphic (never accessed)
        db 8, 2
        db %00000001, %00100011, %01000101, %01100111

graphic_logo  ; the "Game Genie" logo
        db 30, 4
        ; line 0
        db %00101100, %11000000, %00000000, %00000000, %00000000
        db %00000000, %00000000, %00000010, %11001100, %00000000
        db %00000000, %00000000, %00001000, %00000000, %00000000
        ; line 1
        db %01010010, %00110010, %11001100, %01011101, %10010110
        db %00101100, %11100100, %00000101, %00100011, %00101100
        db %11100100, %11011100, %01101010, %00101100, %11100100
        ; line 2
        db %01100000, %00101010, %00000000, %01010101, %01011010
        db %10100000, %01000000, %00000110, %00000010, %10100000
        db %01000000, %01010000, %10101010, %10100000, %01000000
        ; line 3
        db %00001100, %01000000, %11000000, %01000100, %01001000
        db %00001100, %11000100, %00000000, %11000100, %00001100
        db %11000100, %01000000, %10001000, %00001100, %11000100

graphic_revolv  ; the revolving cursor
        db 2, 2
        db %10110001  ; line 0
        db %10000000  ; line 1

graphic_a  ; the letter A
        db 4, 4
        db %00001010, %00000000  ; line 0
        db %00000101, %01010000  ; line 1
        db %10101100, %11100000  ; line 2
        db %11000100, %11000100  ; line 3

graphic_e  ; the letter E
        db 4, 4
        db %10001101, %11100000  ; line 0
        db %00000111, %00010000  ; line 1
        db %00000101, %00100000  ; line 2
        db %10001100, %11000000  ; line 3

graphic_p  ; the letter P
        db 4, 4
        db %10001101, %01100000  ; line 0
        db %00000111, %10010000  ; line 1
        db %00000101, %00000000  ; line 2
        db %10001100, %00000000  ; line 3

graphic_o  ; the letter O
        db 4, 4
        db %00001001, %01100000  ; line 0
        db %10100000, %00000101  ; line 1
        db %10000001, %00100100  ; line 2
        db %00001000, %01000000  ; line 3

graphic_z  ; the letter Z
        db 4, 4
        db %10101100, %11100000  ; line 0
        db %00000010, %01000000  ; line 1
        db %00100100, %00100000  ; line 2
        db %10001100, %11000000  ; line 3

graphic_letter_x  ; the letter X
        db 4, 4
        db %11100100, %11100100  ; line 0
        db %00000110, %01000000  ; line 1
        db %00100100, %01100000  ; line 2
        db %11000100, %11000100  ; line 3

graphic_l  ; the letter L
        db 4, 4
        db %10001101, %00000000  ; line 0
        db %00000101, %00000000  ; line 1
        db %00000101, %00100000  ; line 2
        db %10001100, %11000000  ; line 3

graphic_u  ; the letter U
        db 4, 4
        db %11100100, %11100100  ; line 0
        db %10100000, %10100000  ; line 1
        db %10100000, %10100000  ; line 2
        db %00001100, %01000000  ; line 3

graphic_g  ; the letter G
        db 4, 4
        db %00001001, %11000101  ; line 0
        db %10100000, %00110001  ; line 1
        db %10000001, %00000101  ; line 2
        db %00001000, %11000000  ; line 3

graphic_k  ; the letter K
        db 4, 4
        db %10001101, %10100100  ; line 0
        db %00000111, %01000000  ; line 1
        db %00000101, %01100000  ; line 2
        db %10001100, %10000100  ; line 3

graphic_i  ; the letter I
        db 4, 4
        db %00001110, %01000000  ; line 0
        db %00001010, %00000000  ; line 1
        db %00001010, %00000000  ; line 2
        db %00001100, %01000000  ; line 3

graphic_s  ; the letter S
        db 4, 4
        db %00101100, %01110000  ; line 0
        db %10000111, %00010000  ; line 1
        db %00100000, %11100000  ; line 2
        db %00001100, %01000000  ; line 3

graphic_t  ; the letter T
        db 4, 4
        db %10101110, %11100000  ; line 0
        db %00001010, %00000000  ; line 1
        db %00001010, %00000000  ; line 2
        db %00001100, %01000000  ; line 3

graphic_v  ; the letter V
        db 4, 4
        db %11100100, %11100100  ; line 0
        db %10000001, %10010000  ; line 1
        db %00000110, %01000000  ; line 2
        db %00001000, %00000000  ; line 3

graphic_letter_y  ; the letter Y
        db 4, 4
        db %11100100, %11100100  ; line 0
        db %00000110, %01000000  ; line 1
        db %00001010, %00000000  ; line 2
        db %00001100, %01000000  ; line 3

graphic_n  ; the letter N
        db 4, 4
        db %11100000, %11100100  ; line 0
        db %10100110, %10100000  ; line 1
        db %10100000, %11100000  ; line 2
        db %11000100, %10000000  ; line 3

graphic_dash  ; "-", placeholder for input area
        db 4, 4
        db %00000000, %00000000  ; line 0
        db %00100011, %00110001  ; line 1
        db %00000000, %00000000  ; line 2
        db %00000000, %00000000  ; line 3

graphic_hand  ; hand cursor (the only graphic with an odd width)
        db 5, 4
        db %00000000, %01110000, %00000011, %00111011, %01110001  ; lines 0&1
        db %00001110, %11111111, %01010000, %00001100, %11000100  ; lines 2&3

        ; same as lines 1-3 of the hand cursor (never accessed)
        db %00000011, %00111011, %01110001
        db %00001110, %11111111, %01010000, %00001100, %11000100

        ; never accessed
        db %00000000, %00000000, %00000000, %00000000
        db %00000000, %00000000, %00000000, %00000000
        db %00000000, %00000000, %00000000, %10101010

; --- Interrupt vectors ---------------------------------------------------------------------------

        pad $fffa, $ff
        dw nmi    ; NMI
reset_vector
        dw init1  ; reset
        dw $ffff  ; IRQ (never accessed)

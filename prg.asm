    .include "constants.asm"
    .include "macros.asm"

; --------------------------------------------------------------------------------------------------

    ; last 4 KiB of CPU memory space
    org $f000

initialization1:
    ; Part 1/3 of initialization.

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

    jmp initialization2  ; continue initialization

; --------------------------------------------------------------------------------------------------

delay:
    ; Wait.
    ; Called by:
    ;   initialization1

    ldx #96
    ldy #8
-   dex
    bne -
    dey
    bne -
    rts

; --------------------------------------------------------------------------------------------------

initialization2:
    ; Part 2/3 of initialization.

    ; wait for VBlank 10 times
    ldx #10
-   lda ppu_status
    bpl -
    dex
    bne -

    ldx #$ff
    txs       ; reinitialize stack pointer (why?)

    ; clear RAM (fill $0000...$07ff with $00)
    copy #$07, ram_clear_pointer+1
    lda #$00
    sta ram_clear_pointer+0
    tay
-   sta (ram_clear_pointer),y
    iny
    bne -
    dec ram_clear_pointer+1
    bpl -

    ; hide sprites and background
    lda #%00000110
    sta ppu_mask_mirror
    sta ppu_mask

    ; enable NMI, use 8*8-pixel sprites, use Pattern Table 0, use Name Table 0,
    ; make the VRAM address increment by one
    lda #%10000000
    sta ppu_ctrl_mirror
    sta ppu_ctrl

    dec_absolute vram_buffer_free_bytes  ; set to 255

    jmp initialization3  ; continue initialization

; --------------------------------------------------------------------------------------------------

read_joypads:
    ; Read joypads (both, for some reason).
    ; Out:
    ;   joypad1_status (bits: A, B, select, start, up, down, left, right)
    ;   joypad2_status (bits: A, B, select, start, up, down, left, right)
    ; Called by:
    ;   do_every_frame

    ; initialize the joypads
    copy #%00000001, joypad1
    copy #%00000000, joypad1

    ; read joypad 1
    ldy #8              ; number of buttons to read
-   lda joypad1         ; read from memory-mapped register
    ror                 ; LSB to carry
    rol joypad1_status  ; carry to RAM
    dey
    bne -               ; next button

    ; read joypad 2
    ldy #8
-   lda joypad2
    ror
    rol joypad2_status
    dey
    bne -
    rts

; --------------------------------------------------------------------------------------------------

nmi:
    ; The non-maskable interrupt routine.
    ; Args:
    ;   skip_nmi: if nonzero, skip doing the usual stuff
    ; Out:
    ;   nmi_done: set to nonzero when exiting

    pha
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

+   lda #1
    sta_absolute nmi_done  ; set flag

    ply
    plx
    pla
    rti

; --------------------------------------------------------------------------------------------------

draw_graphic_on_background:
    ; Draw a graphic (e.g. the Game Genie logo) on the Name Table.
    ; Args:
    ;   A: graphic id (see graphics_offsets)
    ;   X: horizontal position, in tiles
    ;   Y: vertical position, in tiles
    ; Called by:
    ;   set_up_background
    ;   letter_input
    ;   check_button_b

    stx graphic_x
    sty graphic_y
    jsr set_graphics_pointer  ; A = id

    ; get width and height of graphic (in nybbles/tiles)
    ldy #0
    lda (graphics_pointer),y
    sta graphic_width
    iny
    lda (graphics_pointer),y
    sta graphic_height

    ; advance graphics pointer to start of data
    lda_absolute graphics_pointer+0
    add #2
    sta_absolute graphics_pointer+0
    lda_absolute graphics_pointer+1
    adc #0
    sta_absolute graphics_pointer+1

    ; copy nybbles to VRAM buffer
    copy #0, graphic_y_offset
--  copy #0, graphic_x_offset          ; Y loop
-   jsr graphic_nybble_to_vram_buffer  ; X loop
    inc graphic_x_offset
    lda graphic_x_offset
    cmp graphic_width
    bne -
    inc graphic_y_offset
    lda graphic_y_offset
    cmp graphic_height
    bne --
    rts

; --------------------------------------------------------------------------------------------------

graphic_nybble_to_vram_buffer:
    ; Copy one nybble (one tile) of graphics data to VRAM buffer.
    ; Args:
    ;   graphic_x: horizontal position of graphic, in tiles
    ;   graphic_y: vertical position of graphic, in tiles
    ;   graphic_x_offset: horizontal position inside graphic, in tiles
    ;   graphic_y_offset: vertical position inside graphic, in tiles
    ;   vram_address_high: high byte of VRAM address
    ; Called by:
    ;   draw_graphic_on_background

    ; get offset of nybble
    lda graphic_y_offset
    ldx graphic_width
    jsr multiply  ; A/X = multiplicand/multiplier
    add graphic_x_offset
    sta_absolute nybble_offset

    ; graphic data byte -> temp1
    lsr
    tay
    lda (graphics_pointer),y
    sta temp1

    ; get nybble from byte
    lda_absolute nybble_offset
    and #%00000001
    beq +           ; get upper nybble
    lda temp1
    and #%00001111
    jmp ++
+   lda temp1
    rept 4
        lsr
    endr
++  sta graphic_nybble

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
    rept 3
        asl
    endr
    sta nybble_vram_low
    lda #0               ; will become nybble_vram_high
    asl nybble_vram_low  ; 4th shift
    rol                  ; save overflown bit
    asl nybble_vram_low  ; 5th shift
    rol                  ; save overflown bit
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
    copy nybble_vram_high, vram_block+1  ; address high
    copy nybble_vram_low, vram_block+2   ; address low
    copy graphic_nybble, vram_block+3    ; data
    copy #1, vram_block+0                ; data size

    jsr vram_block_to_buffer  ; copy to buffer
    rts

; --------------------------------------------------------------------------------------------------

multiply:
    ; Multiply.
    ; Args:
    ;   A: multiplicand
    ;   X: multiplier
    ; Out:
    ;   A: product
    ; Called by:
    ;   graphic_nybble_to_vram_buffer
    ;   assign_metasprite_to_graphic

    sta multiplication_temp
    lda #0
    cpx #0
    beq +
    clc
-   adc multiplication_temp
    dex
    bne -
+   rts

; --------------------------------------------------------------------------------------------------

sprite_dma:
    ; Copy interleaved_sprite_data to OAM.
    ; Called by:
    ;   nmi
    ;   initialization3

    copy #$00, oam_addr
    copy #>interleaved_sprite_data, oam_dma
    rts

; --------------------------------------------------------------------------------------------------

vram_buffer_to_vram:
    ; Copy as much as possible from VRAM buffer to VRAM.
    ; Each block in buffer: number of bytes, address hi, address lo, bytes
    ; Called by:
    ;   nmi
    ;   vram_block_to_buffer

    copy #100, vram_budget  ; maximum sum of (blocks * 5 + total_bytes) to copy
    ldy vram_buffer_next_read

vram_block_copy_loop:
    ; exit if nothing left to copy
    cpy vram_buffer_next_write
    beq vram_copy_end

    ; get VRAM block size;
    ; compute the cost of copying the block, i.e. size + 5
    lda vram_buffer,y
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
    adc vram_buffer,y
    adc #3
    sta vram_buffer_free_bytes

    ; get and set VRAM address
    iny
    lda vram_buffer,y
    sta ppu_addr
    iny
    lda vram_buffer,y
    sta ppu_addr

    ; copy block data to VRAM
    iny
-   lda vram_buffer,y
    sta ppu_data
    iny
    dex
    bne -

    jmp vram_block_copy_loop

vram_copy_end:
    sty vram_buffer_next_read
    rts

; --------------------------------------------------------------------------------------------------

vram_block_to_buffer:
    ; Copy current VRAM block to VRAM buffer.
    ; Called by:
    ;   graphic_nybble_to_vram_buffer
    ;   update_attribute_block
    ;   initialization3
    ;   animate_color
    ;   highlight_input_area_row

    ; VRAM block total size -> temp1
    lda vram_block+0
    add #3
    sta temp1

    ; continue if vram_buffer_free_bytes >= temp1
    lda vram_buffer_free_bytes
    cmp temp1
    bcs continue_vram_block_to_buffer

    ; restart sub if NMI is being skipped
    lda skip_nmi
    beq vram_block_to_buffer

    ; copy some data out of VRAM buffer and restart sub
    jsr vram_buffer_to_vram
    jmp vram_block_to_buffer

continue_vram_block_to_buffer:
    ; vram_buffer_free_bytes -= temp1
    sub temp1
    sta vram_buffer_free_bytes

    ; copy VRAM block to buffer
    ldx #0                      ; index to read within block
    ldy vram_buffer_next_write
-   lda vram_block,x
    sta vram_buffer,y
    inx
    iny
    cpx temp1
    bne -

    sty vram_buffer_next_write
    rts

; --------------------------------------------------------------------------------------------------

assign_metasprite_to_graphic:
    ; Draw a graphic (e.g. the hand cursor) as a metasprite.
    ; Args:
    ;   A: graphic id (see graphics_offsets)
    ; Out:
    ;   A: index to metasprite_indexes
    ; Called by:
    ;   initialization3

    ; start reading specified graphic
    sta graphic_id
    jsr set_graphics_pointer  ; A = id

    ; get width and height of graphic
    ldy #0
    lda (graphics_pointer),y
    sta metasprite_width
    tax
    iny
    lda (graphics_pointer),y
    sta metasprite_height

    ; width * height of graphic -> A, graphic_data_left
    jsr multiply  ; A/X = multiplicand/multiplier
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
    lda metasprite_indexes,x
    tax

    ; save width and height of graphic to metasprite data
    lda metasprite_width
    sta metasprites,x
    lda metasprite_height
    sta metasprites+1,x

    ; advance to indexes to individual sprites
    inx
    inx

    ; assign graphic_data_left free sprites to the metasprite,
    ; starting from the first free sprite
    ldy #255
-   iny
    lda sprite_attributes,y
    bne -
    tya
    sta metasprites,x
    inx
    dec graphic_data_left
    bne -

    ; set up tiles and attributes for individual sprites
    ldx metasprite_to_create
    lda graphic_id
    jsr set_up_metasprite  ; A/X = id, index to metasprite_indexes

    ; set up positions for individual sprites
    ldx metasprite_to_create
    jsr update_metasprite  ; X = metasprite index

    ; return metasprite index
    lda metasprite_to_create
    rts

; --------------------------------------------------------------------------------------------------

set_graphics_pointer:
    ; Set graphics pointer.
    ; Args:
    ;   A: graphic id (see graphics_offsets)
    ; Out:
    ;   graphics_pointer: the address of the graphic
    ; Called by:
    ;   draw_graphic_on_background
    ;   assign_metasprite_to_graphic
    ;   set_up_metasprite

    sta temp1  ; graphic id

    ; set pointer
    lda #<graphics_offsets
    sta graphics_pointer + 0
    lda #>graphics_offsets
    sta graphics_pointer + 1

    ; word(graphics_pointer) += graphic_id >> 7
    lda temp1
    bpl +
    inc graphics_pointer + 1  ; never accessed
+

    ; get offset to offset
    asl
    tay
    ; get offset to graphic
    lda (graphics_pointer),y
    pha
    iny
    lda (graphics_pointer),y
    ; get address of graphic
    add #<graphics_offsets
    sta graphics_pointer+0
    pla
    adc #>graphics_offsets
    sta graphics_pointer+1
    rts

; --------------------------------------------------------------------------------------------------

update_metasprite:
    ; Update metasprite's position to its individual sprites' positions.
    ; Args:
    ;   X: metasprite index:
    ;       0: hand cursor
    ;       1: revolving cursor
    ;       2: flying letter
    ;   metasprite_x: horizontal position of metasprite
    ;   metasprite_y: vertical position of metasprite
    ; Called by:
    ;   assign_metasprite_to_graphic
    ;   do_every_frame
    ;   update_revolving_cursor
    ;   letter_input_extra_effects
    ;   move_flying_letter

    ; get index to metasprite info (width, height, individual sprite indexes)
    lda metasprite_indexes,x
    tax

    ; get width and height of metasprite
    lda metasprites, x
    sta metasprite_width
    lda metasprites + 1, x
    sta metasprite_height

    ; advance to start of individual sprite indexes
    inx
    inx

    ; Y positions of individual sprites start from metasprite Y position
    copy metasprite_y, sprite_y + 0
    copy always_zero2, sprite_y + 1

update_metasprite_loop:
    ; if sprite is beyond bottom edge of screen, hide this row of sprites
    lda sprite_y + 1
    bne hide_row_of_sprites

    ; initialize loop counter with number of nybbles/tiles per row
    copy metasprite_width, nybbles_left_x

    ; X positions of individual sprites start from metasprite X position
    copy metasprite_x, sprite_x + 0
    copy always_zero1, sprite_x + 1

update_metasprite_loop_x:
    ; if sprite is beyond right edge of screen, hide it
    lda sprite_x+1
    bne hide_individual_sprite

    ; get index to individual target sprite
    lda metasprites,x
    tay

    ; if tile of target sprite = 0, hide it
    lda sprite_tiles,y
    beq hide_individual_sprite

    ; set position of target sprite
    lda sprite_x+0
    sta sprite_x_positions,y
    lda sprite_y+0
    sta sprite_y_positions,y

    ; next sprite
    inx
    jmp sprite_processed

hide_individual_sprite:
    ; hide target sprite
    lda metasprites,x
    tay
    lda #255
    sta sprite_y_positions,y

    ; next sprite
    inx

sprite_processed:
    ; increase current sprite X position by 8
    lda sprite_x+0
    add #8
    sta sprite_x+0
    lda sprite_x+1
    adc #0
    sta sprite_x+1

    ; end of inner loop
    dec nybbles_left_x
    bne update_metasprite_loop_x

sprite_row_processed:
    ; word[sprite_y] += 8
    lda sprite_y+0
    add #8
    sta sprite_y+0
    lda sprite_y+1
    adc #0
    sta sprite_y+1

    ; end of outer loop
    dec metasprite_height
    bne update_metasprite_loop
    rts

hide_row_of_sprites:
    ; hide metasprite_width sprites
    copy metasprite_width, nybbles_left_x
-   lda metasprites,x
    tay
    lda #255
    sta sprite_y_positions,y
    inx
    dec nybbles_left_x
    bne -
    jmp sprite_row_processed

; --------------------------------------------------------------------------------------------------

set_up_metasprite:
    ; Set up tiles and attributes for individual sprites of a metasprite.
    ; Args:
    ;   A: graphic id (see graphics_offsets)
    ;   X: index to metasprite_indexes
    ; Called by:
    ;   assign_metasprite_to_graphic
    ;   letter_input_extra_effects

    sta graphic_id

    ; get index to metasprites where indexes to individual sprites are
    lda metasprite_indexes,x
    add #2
    pha

    ; start reading specified graphic
    lda graphic_id
    jsr set_graphics_pointer  ; A = id

    ; get width and height of graphic
    ldy #0
    lda (graphics_pointer),y
    sta metasprite_width
    iny
    lda (graphics_pointer),y
    sta metasprite_height

    ; 2 -> nybble_offset
    lda #0
    ora #%00011100
    ldx unused1  ; the value is ignored later
    sta always_b00011100
    lda #2
    ldy #1
    sty always_one
    sta nybble_offset
    copy #0, always_zero3

    plx  ; index to metasprites

    ; copy all rows of graphics data to metasprite's individual sprites
loop_y:
    copy metasprite_width, nybbles_left_x  ; loop_x counter

    ; copy one row of graphics data to metasprite's individual sprites
loop_x:
    ; graphic data byte -> temp1
    lda nybble_offset
    lsr
    add #1
    tay
    lda (graphics_pointer),y
    sta temp1

    ; push upper/lower nybble (if nybble_offset is even/odd, respectively)
    lda nybble_offset
    and #%00000001
    beq +           ; upper nybble
    lda temp1
    and #%00001111
    jmp ++
+   lda temp1
    rept 4
        lsr
    endr
++  pha

    ; nybble    -> tile of individual sprite
    ; %00011100 -> attribute of individual sprite
    lda metasprites,x
    tay
    pla
    sta sprite_tiles,y
    lda always_b00011100
    sta sprite_attributes,y

    ; next individual sprite
    inx

    ; nybble_offset += 1
    lda nybble_offset
    add always_one
    sta nybble_offset

    ; end of loop_x
    dec nybbles_left_x
    bne loop_x

    ; do nothing
    lda nybble_offset
    add always_zero3
    sta nybble_offset

    ; end of loop_y
    dec metasprite_height
    bne loop_y
    rts

; --------------------------------------------------------------------------------------------------

convert_sprites:
    ; Convert planar sprite data to interleaved.
    ; Sprites with attribute byte $00 will be hidden.
    ; Called by:
    ;   do_every_frame

    ; ascending order every 2nd frame, descending order every 2nd frame
    lda odd_frame_flag1
    eor #%00000001
    sta odd_frame_flag1
    bne ascending_order

    ; descending order: planar sprites 61...0 -> interleaved sprites 2...63
    ldy #2*4  ; target offset
    ldx #61   ; source offset
-   lda sprite_attributes,x
    beq +  ; hide sprite
    jsr convert_sprite  ; A/X/Y = attribute byte, source index, target index
    dex
    bpl -
    rts
+   jsr hide_sprite  ; Y = index
    dex
    bpl -
    rts  ; never accessed

ascending_order:
    ; ascending order: planar sprites 0...61 -> interleaved sprites 2...63
    ldy #2*4  ; target offset
    ldx #0    ; source offset
-   lda sprite_attributes,x
    beq +  ; hide sprite
    jsr convert_sprite  ; A/X/Y = attribute byte, source index, target index
    inx
    cpx #62
    bne -
    rts
+   jsr hide_sprite  ; Y = index
    inx
    cpx #62
    bne -
    rts

; --------------------------------------------------------------------------------------------------

convert_sprite:
    ; Convert one non-hidden sprite from planar to interleaved.
    ; Args:
    ;   A: attribute byte
    ;   X: index to planar sprite data tables (source)
    ;   Y: index to interleaved_sprite_data (target)
    ; Out:
    ;   Y += 4
    ; Called by:
    ;   convert_sprites

    sta interleaved_sprite_data+2,y
    lda sprite_y_positions,x
    sta interleaved_sprite_data+0,y
    lda sprite_tiles,x
    sta interleaved_sprite_data+1,y
    lda sprite_x_positions,x
    sta interleaved_sprite_data+3,y
    rept 4
        iny
    endr
    rts

; --------------------------------------------------------------------------------------------------

hide_sprite:
    ; Hide a sprite in interleaved_sprite_data.
    ; Args:
    ;   Y: index to interleaved_sprite_data
    ; Out:
    ;   Y += 4
    ; Called by:
    ;   convert_sprites

    lda #255
    sta interleaved_sprite_data,y
    rept 4
        iny
    endr
    rts

; --------------------------------------------------------------------------------------------------

update_attribute_block:
    ; Change the value of an attribute block (2 bits) within one attribute
    ; byte, preserving the other 6 bits.
    ; Args:
    ;   vram_block_x: horizontal position of attribute block (0-15)
    ;   vram_block_y: vertical position of attribute block (0-14)
    ;   attribute_fill_byte: value of new block (which 2 bits are read depends
    ;       on vram_block_x and vram_block_y)
    ; Called by:
    ;   update_attribute_byte
    ;   fill_attribute_table_rows

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
    sta vram_block+2
    copy #>ppu_attribute_table, vram_block+1

    ; one byte to copy
    copy #1, vram_block+0  ; data size

    ; combine old and new bits of attribute byte:
    ; (newAttributeByte & bitmaskDeterminedByY)
    ; | ($0400,x & ~bitmaskDeterminedByY)
    ; -> $0400,x and vram_block+3 (start of data)
    lda attribute_block_bitmasks,y
    and attribute_fill_byte
    sta temp1
    lda attribute_block_bitmasks,y
    eor #%11111111
    and $0400,x
    ora temp1
    sta $0400,x
    sta vram_block+3  ; start of data

    jmp vram_block_to_buffer  ; ends with rts

; --------------------------------------------------------------------------------------------------

attribute_block_bitmasks:
    ; Read by:
    ;   update_attribute_block

    db %00000011, %00001100, %00110000, %11000000

; --------------------------------------------------------------------------------------------------

initial_palette:
    ; Initial palette. 32 bytes.
    ; Read by:
    ;   initialization3

    ; background subpalette 0
    db color_background, color_unused1, color_unused1, color_keyboard
    ; background subpalette 1
    db color_unused1, color_unused1, color_unused1, color_animated_initial
    ; background subpalette 2
    db color_unused1, color_unused1, color_unused1, color_highlight
    ; background subpalette 3
    db color_unused1, color_unused1, color_unused1, color_input_area

    ; sprite subpalette 0
    db color_background, color_unused1, color_unused1
    db color_letter_revolving1_particle1
    ; sprite subpalette 1
    db color_unused1, color_unused1, color_unused1, color_hand1
    ; sprite subpalette 2
    db color_unused1, color_unused1, color_unused1
    db color_hand2_revolving2_particle2
    ; sprite subpalette 3
    db color_unused1, color_unused1, color_unused1, color_unused2

; --------------------------------------------------------------------------------------------------

initialization3:
    ; Part 3/3 of initialization.

    copy #1, skip_nmi

    ; clear first Name&Attribute Table (fill VRAM $2000...$23ff with $00)
    lda #>ppu_name_table
    sta vram_address_high
    sta ppu_addr
    lda #<ppu_name_table
    sta ppu_addr
    ldx #4
    tay
-   sta ppu_data
    dey
    bne -
    dex
    bne -

    jsr set_up_background

    ; clear metasprite_indexes (why? we just cleared the entire RAM)
    ldx #0
    lda #$00
-   sta metasprite_indexes,x
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
    ldx #20-1
-   lda initial_sprite_attribute_data,x
    sta sprite_attributes,x
    dex
    bpl -

    copy #%00001001, snd_clock

    ; data size
    copy #32, vram_block+0
    ; address
    copy #>ppu_palettes, vram_block+1
    copy #<ppu_palettes, vram_block+2

    ; copy initial palette from ROM to VRAM block
    ldy #0
-   lda initial_palette,y
    sta vram_block+3,y  ; vram_block+3 = start of data
    iny
    cpy #32
    bne -
    jsr vram_block_to_buffer

    copy #0, skip_nmi
    jsr wait_until_nmi_done

    ; show sprites&background
    lda ppu_mask_mirror
    ora #%00011000
    sta ppu_mask_mirror
    sta ppu_mask

    ; the main loop
-   jsr do_every_frame
    jsr wait_until_nmi_done
    jmp -

; --------------------------------------------------------------------------------------------------

wait_until_nmi_done:
    ; Wait until the NMI routine has run.
    ; Called by:
    ;   initialization3
    ;   main_loop

    ; clear the flag
    lda #0
    sta_absolute nmi_done
    ; wait until the flag gets set
-   lda_absolute nmi_done
    beq -
    rts

; --------------------------------------------------------------------------------------------------

do_every_frame:
    ; Called by:
    ;   main_loop

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

; --------------------------------------------------------------------------------------------------

check_arrows:
    ; Called by:
    ;   move_hand

    ; if hand_y_speed_pointer set, skip checking vertical arrows
    lda_absolute hand_y_speed_pointer+1
    bne check_horizontal_arrows

    ; was up pressed?
    lda joypad1_status
    and #joypad_up
    beq check_down
    ; move hand if possible
    lda #joypad_up
    jsr set_hand_target  ; A = direction, out: A = success
    beq check_down
    ; hand successfully moved
    ; hand_speeds_negative -> hand_y_speed_pointer
    lda #<hand_speeds_negative
    sta_absolute hand_y_speed_pointer+0
    lda #>hand_speeds_negative
    sta_absolute hand_y_speed_pointer+1
    ; if hand moved from 3rd line to 2nd, 32 -> hand_y_speed_offset,
    ; else 0 -> hand_y_speed_offset
    lda #joypad_up
    jsr moving_between_keyboard_and_input_area  ; A = direction
    beq +  ; did not
    lda #32
    jmp ++
+   lda #0
++  sta_absolute hand_y_speed_offset
    lda #joypad_up
    sta_absolute last_y_input_accepted

    jmp check_horizontal_arrows

check_down:
    ; was down pressed?
    lda joypad1_status
    and #joypad_down
    beq check_horizontal_arrows
    ; move hand if possible
    lda #joypad_down
    jsr set_hand_target  ; A = direction, out: A = success
    beq check_horizontal_arrows
    ; hand successfully moved
    ; hand_speeds_positive -> hand_y_speed_pointer
    lda #<hand_speeds_positive
    sta_absolute hand_y_speed_pointer+0
    lda #>hand_speeds_positive
    sta_absolute hand_y_speed_pointer+1
    ; if hand moved from 2nd to 3rd line, 32 -> hand_y_speed_offset,
    ; else 0 -> hand_y_speed_offset
    lda #joypad_down
    jsr moving_between_keyboard_and_input_area  ; A = direction
    beq +  ; did not
    lda #32
    jmp ++
+   lda #0
++  sta_absolute hand_y_speed_offset
    lda #joypad_down
    sta_absolute last_y_input_accepted

check_horizontal_arrows:
    ; if hand_x_speed_pointer set, skip checking left/right button
    lda_absolute hand_x_speed_pointer+1
    bne arrow_check_exit

    ; was left pressed?
    lda joypad1_status
    and #joypad_left
    beq check_right
    ; move hand if possible
    lda #joypad_left
    jsr set_hand_target  ; A = direction, out: A = success
    beq check_right
    ; hand successfully moved
    ; hand_speeds_negative -> hand_x_speed_pointer
    lda #<hand_speeds_negative
    sta_absolute hand_x_speed_pointer+0
    lda #>hand_speeds_negative
    sta_absolute hand_x_speed_pointer+1
    ; 0 -> hand_x_speed_offset
    lda #0
    sta_absolute hand_x_speed_offset
    lda #joypad_left
    sta_absolute last_x_input_accepted

    jmp arrow_check_exit

check_right:
    ; was right pressed?
    lda joypad1_status
    and #joypad_right
    beq arrow_check_exit
    ; move hand if possible
    lda #joypad_right
    jsr set_hand_target  ; A = direction, out: A = success
    beq arrow_check_exit
    ; hand successfully moved
    ; hand_speeds_positive -> hand_x_speed_pointer
    lda #<hand_speeds_positive
    sta_absolute hand_x_speed_pointer+0
    lda #>hand_speeds_positive
    sta_absolute hand_x_speed_pointer+1
    ; 0 -> hand_x_speed_offset
    lda #0
    sta_absolute hand_x_speed_offset
    lda #joypad_right
    sta_absolute last_x_input_accepted

arrow_check_exit:
    rts

; --------------------------------------------------------------------------------------------------

find_free_metasprite:
    ; Find the first free byte in metasprite_indexes and add the specified
    ; value to all following bytes.
    ; Args:
    ;   A: the value to add
    ; Out:
    ;   A: index to the first free byte
    ; Called by:
    ;   assign_metasprite_to_graphic

    sta temp1  ; the value to add

    ; find first free byte (same as the following byte)
    ldx #255
-   inx
    lda metasprite_indexes+1,x
    cmp metasprite_indexes,x
    bne -

    stx first_free_byte  ; index to first free byte
    sta unused2          ; never used anywhere

    ; add the value to all following bytes
    inx
-   lda temp1
    clc
    adc metasprite_indexes,x
    sta metasprite_indexes,x
    inx
    cpx #50
    bne -

    lda first_free_byte
    rts

; --------------------------------------------------------------------------------------------------

set_up_background:
    ; Initialize background graphics.
    ; Called by:
    ;   initialization3

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
-   ; draw letter
    ldaxy keyboard_graphic, keyboard_graphic_x, keyboard_graphic_y  ; id, X, Y
    jsr draw_graphic_on_background
    ; increment X position
    lda keyboard_graphic_x
    add #4
    sta keyboard_graphic_x
    ; if at end of line, move to start of next line
    cmp #9*4
    bne +
    copy #4, keyboard_graphic_x
    lda keyboard_graphic_y
    add #4
    sta keyboard_graphic_y
+   ; loop until the last letter ("N", id 18) has been drawn
    inc keyboard_graphic
    lda keyboard_graphic
    cmp #18+1
    bne -

    ; prepare to draw the input area (dashes)
    copy #3, keyboard_graphic  ; used as the loop counter now
    copy #4, keyboard_graphic_x
    copy #18, keyboard_graphic_y

    ; draw the 24 (3*8) dashes of the input area
-   ; draw dash
    ldaxy #19, keyboard_graphic_x, keyboard_graphic_y  ; id, X, Y
    jsr draw_graphic_on_background
    ; increment X position
    lda keyboard_graphic_x
    add #4
    sta keyboard_graphic_x
    ; if at end of line, move to start of next line
    cmp #9*4
    bne +
    copy #4, keyboard_graphic_x
    lda keyboard_graphic_y
    add #4
    sta keyboard_graphic_y
+   ; loop until the last dash has been drawn
    inc keyboard_graphic
    lda keyboard_graphic
    cmp #3+24
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

    ldx #0
    ldy #0
    jsr highlight_attribute_byte  ; use attribute byte %10101010

    ; no visible effect if replaced with another value
    lda #2
    sta_absolute revolving_y_letter2
    rts

; --------------------------------------------------------------------------------------------------

move_hand:
    ; Called by:
    ;   do_every_frame

    ; check arrows every 2nd frame
    lda odd_frame_flag2
    eor #%00000001
    sta odd_frame_flag2
    bne +
    jsr check_arrows

    ; horizontal movement
+   lda hand_x_speed_pointer+1
    beq move_hand_vertically  ; not moving horizontally

    ; get speed (if offset modulo 16 = 15, terminator)
    ldy hand_x_speed_offset
    lda (hand_x_speed_pointer),y
    cmp #$80
    bne +

    ; stop hand by resetting pointer
    copy #0, hand_x_speed_pointer+1
    jmp ++

+   ; no terminator
    ldx #hand_x_pixel
    jsr add_hand_speed_to_position  ; A = speed, X = address of position; out: Z
    ; every 2nd frame, skip the rest of horizontal movement
    beq move_hand_vertically

++  tya             ; hand_x_speed_offset
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

    ldy #16-1  ; success

+   iny
    sty hand_x_speed_offset

move_hand_vertically:
    lda hand_y_speed_pointer+1
    beq hand_moved  ; not moving vertically

    ; get speed (if offset modulo 16 = 15, terminator)
    ldy hand_y_speed_offset
    lda (hand_y_speed_pointer),y
    cmp #$80
    bne +

    ; stop hand by resetting pointer
    copy #0, hand_y_speed_pointer+1
    jmp ++

+   ; no terminator
    ldx #hand_y_pixel
    jsr add_hand_speed_to_position  ; A = speed, X = address of position; out: Z
    ; every 2nd frame, skip the rest of vertical movement
    beq hand_moved

++  tya             ; hand_y_speed_offset
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
    ldy #3*16-1
    jmp ++
+   ldy #16-1

++  iny
    sty hand_y_speed_offset

hand_moved:
    rts

; --------------------------------------------------------------------------------------------------

    ; read indirectly using hand_x_speed_pointer and hand_y_speed_pointer
hand_speeds_positive:
    ; 4 * 16 bytes; $80 is the terminator
    db 1, 1, 2, 2, 3, 3, 4, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=32
    db 5, 4, 4, 3, 3, 4, 5, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=44
    db 2, 3, 4, 5, 6, 6, 6, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=48
    db 5, 6, 7, 8, 7, 6, 5, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=60
hand_speeds_negative:
    ; same values as above, except negated in two's complement
    db $ff,$ff,$fe,$fe,$fd,$fd,$fc,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80
    db $fb,$fc,$fc,$fd,$fd,$fc,$fb,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80
    db $fe,$fd,$fc,$fb,$fa,$fa,$fa,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80
    db $fb,$fa,$f9,$f8,$f9,$fa,$fb,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80

; --------------------------------------------------------------------------------------------------

add_hand_speed_to_position:
    ; Add hand cursor speed to position.
    ; Args:
    ;   A: speed of hand cursor (horizontal/vertical)
    ;   X: address of hand cursor position (hand_x_pixel/hand_y_pixel)
    ; Return:
    ;   Z: reflects odd_frame_flag2
    ; Called by:
    ;   move_hand

    ; divide speed by 2, round down (toward -infinity)
    pha
    asl
    pla
    ror

    ; carry: 0 or LSB of original speed
    dec odd_frame_flag2
    beq +
    clc
+   ; add new speed and C to hand cursor position
    adc $00,x
    sta $00,x

    inc odd_frame_flag2
    rts

; --------------------------------------------------------------------------------------------------

moving_between_keyboard_and_input_area:
    ; Are we moving between virtual keyboard and input area?
    ;   That is, (A = joypad_down and hand_y_letter_target == 2)
    ;   or (A = joypad_up and hand_y_letter_target == 1)
    ; Args:
    ;   A: direction (joypad_up/joypad_down)
    ;   hand_y_letter_target
    ; Out:
    ;   A: 0 = no, 1 = yes
    ; Called by:
    ;   check_arrows
    ;   move_hand

    cmp #joypad_down
    bne +
    lda hand_y_letter_target
    cmp #2
    bne ++
    lda #1
    rts

+   ; check up direction
    cmp #joypad_up
    bne ++
    lda hand_y_letter_target
    cmp #1
    bne ++
    lda #1
    rts

++  lda #0
    rts

; --------------------------------------------------------------------------------------------------

set_hand_target:
    ; Move hand cursor to specified direction if possible.
    ; Args:
    ;   A: direction (joypad_right/_left/_down/_up)
    ; Out:
    ;   hand_x_letter_target or hand_y_letter_target: changed
    ;   A: success (1 = yes, 0 = no)
    ; Called by:
    ;   check_arrows
    ;   move_hand

    cmp #joypad_right
    bne +
    ; try to move right
    lda hand_x_letter_target
    cmp #7
    beq hand_move_fail
    inc hand_x_letter_target
    jmp hand_move_success

+   ; check left direction
    cmp #joypad_left
    bne +
    ; try to move left
    lda hand_x_letter_target
    beq hand_move_fail
    dec hand_x_letter_target
    jmp hand_move_success

+   ; check down direction
    cmp #joypad_down
    bne +
    ; try to move down
    lda hand_y_letter_target
    cmp #4
    beq hand_move_fail
    inc hand_y_letter_target
    jmp hand_move_success

+   ; check up direction
    cmp #joypad_up
    bne hand_move_success
    ; try to move up
    lda hand_y_letter_target
    beq hand_move_fail
    dec hand_y_letter_target
    jmp hand_move_success

hand_move_success:
    lda #1
    rts
hand_move_fail:
    lda #0
    rts

; --------------------------------------------------------------------------------------------------

highlight_attribute_byte:
    ; Alternative entry point for update_attribute_byte.
    ; Called by: (see update_attribute_byte)

    lda #%10101010

update_attribute_byte:
    ; Update an Attribute Table byte (2*2 attribute blocks).
    ; Args:
    ;   A: new attribute table byte
    ;   X: horizontal position (0-7)
    ;   Y: vertical position (0-4; 0-1 = virtual keyboard, 2-4 = input area)
    ; alternative entry points:
    ;   highlight_attribute_byte
    ;   clear_attribute_byte
    ; called by (incl. alternative entry points):
    ;   set_up_background
    ;   update_hand_letter_position
    ;   highlight_input_area_letter

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
+   ; update 2*2 attribute blocks
    sta vram_block_y
    jsr update_attribute_block
    inc vram_block_x
    jsr update_attribute_block
    inc vram_block_y
    jsr update_attribute_block
    dec vram_block_x
    jmp update_attribute_block  ; ends with rts

clear_attribute_byte:
    ; Alternative entry point for update_attribute_byte.

    lda #%00000000
    jmp update_attribute_byte

; --------------------------------------------------------------------------------------------------

update_hand_letter_position:
    ; Called by:
    ;   move_hand

    phy

    ; un-highlight the letter the cursor is leaving
    ldx hand_x_keyboard
    ldy hand_y_keyboard
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
    ldx hand_x_keyboard
    ldy hand_y_keyboard
    jsr highlight_attribute_byte
    ply
    rts

target_on_input_area:
    ; target letter is on input area
    ; update actual hand position
    sta hand_y_letter
    copy hand_x_letter_target, hand_x_letter
    ply
    rts

; --------------------------------------------------------------------------------------------------

initial_sprite_attribute_data:
    ; bits: VHBUUUPP (Vflip, Hflip, Behind bg, Unimplemented, Palette);
    ; in reverse order
    db %00011001, %00011001, %00011001, %00011001, %00011010  ; 19...15
    db %00011001, %00011001, %00011001, %00011001, %00011010  ; 14...10
    db %00011001, %00011001, %00011001, %00011001, %00011010  ;  9... 5
    db %00011001, %00011001, %00011001, %00011001, %00011010  ;  4... 0

; --------------------------------------------------------------------------------------------------

update_revolving_cursor_attributes:
    ; change attributes of revolving cursor's sprites (20-23)

    ; change subpalette from 0 to 2 or vice versa
    lda sprite_attributes+20
    cmp #%00011010  ; VHBUUUPP
    bne +
    lda #%00011000
    jmp ++
+   lda #%00011010  ; use the 3rd subpalette
++

    ; put sprite behind background if phase is 0-6
    ldx_absolute revolving_phase
    cpx #7
    bcs +
    ora #%00100000
+

    sta sprite_attributes+20
    sta sprite_attributes+21
    sta sprite_attributes+22
    sta sprite_attributes+23
    rts

; --------------------------------------------------------------------------------------------------

letter_input:
    ; Button A went from off to on, with the hand on virtual keyboard.
    ; Called by:
    ;   check_button_A

    ; graphic id to draw (see graphics_offsets) -> stack
    ; (hand_y_letter * 8 + hand_x_letter + 3)
    lda hand_y_letter
    rept 3
        asl
    endr
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
    jsr draw_graphic_on_background  ; A/X/Y = id/X/Y
    jsr letter_input_extra_effects
    pla
    jsr save_entered_letter

    jmp +  ; a useless jump
+

    ; increment cursor X position (may be undone later)
    inc revolving_x_letter1

    ; six letters on current line?
    lda revolving_x_letter1
    cmp #6
    bne is_line_full  ; no

    ; six letters on current line

    ; third letter on current line -> X
    lda revolving_y_letter1
    rept 3
        asl
    endr
    add #3-1
    tax

    ; if A/P/Z/L/G/I/T/Y, the code is complete
    lda entered_letters,x
    sub #3
    and #%00000001
    beq complete_code_entered

    lda revolving_x_letter1  ; always six, so the next branch is always taken
is_line_full:
    ; eight letters entered?
    cmp #8
    bne letter_input_end  ; no
complete_code_entered:

    ; a code (six or eight letters) is complete
    ; if not on last line, move to start of next line;
    ; otherwise, undo cursor X increment
    lda revolving_y_letter1
    cmp #2
    beq +
    copy #0, revolving_x_letter1
    inc revolving_y_letter1
    jmp letter_input_end
+   dec revolving_x_letter1  ; undo cursor X increment

letter_input_end:
    ; move the highlight to the new letter
    jmp highlight_input_area_letter  ; ends with rts

; --------------------------------------------------------------------------------------------------

check_button_a:
    ; Called by:
    ;   do_every_frame

    lda joypad1_status
    and #joypad_a
    cmp prev_button_a_status
    bne button_a_status_changed
    rts

button_a_status_changed:
    sta prev_button_a_status
    cmp #joypad_a
    beq button_a_off_to_on
    rts

button_a_off_to_on:
    lda hand_y_letter
    cmp #2
    bcs hand_on_input_area

    jmp letter_input  ; ends with rts

hand_on_input_area:
    jmp move_revolving_cursor_manually  ; ends with rts

; --------------------------------------------------------------------------------------------------

check_button_b:
    ; Called by:
    ;   do_every_frame

    lda joypad1_status
    and #joypad_b
    cmp prev_button_b_status
    bne button_b_status_changed
    rts

button_b_status_changed:
    sta prev_button_b_status
    cmp #joypad_b
    beq button_b_off_to_on
    rts

button_b_off_to_on:
    jmp erase_letter_if_there_is_one  ; a useless jump

erase_letter_if_there_is_one:
    lda revolving_x_letter1
    beq first_column
    lda revolving_y_letter1
    cmp #2
    bne non_final_row

    ; 3rd code, non-first letter
    ; length of third code minus one -> A
    ; (7 if 3rd letter is E/O/X/U/K/S/V/N, otherwise 5)
    lda entered_letters+2*8+2
    sub #3
    and #%00000001
    bne eight_letters
    lda #5
    bne six_letters
eight_letters:
    lda #7
six_letters:

    cmp revolving_x_letter1
    bne non_final_row  ; cursor not at last letter

    ; cursor on last letter of 3rd code
    tax  ; length of 3rd code - 1
    lda entered_letters+2*8,x  ; last letter of 3rd code
    bne erase_letter  ; is a letter
    ; last letter of 3rd code isn't a letter??

non_final_row:
    ; revolving cursor at last letter of 3rd code
    jsr get_letter_at_revolving_cursor  ; letter -> A, position -> X
    bne erase_letter_end  ; is a letter

first_column:
    lda revolving_x_letter1
    ora revolving_y_letter1
    beq erase_letter_end  ; at 1st letter of 1st code, no letter to erase
    jsr move_revolving_cursor_backwards

erase_letter:
    ; get X&Y position of letter to erase, in tiles

    ; revolving_y_letter1 * 4 + 18 -> Y
    lda revolving_y_letter1
    asl
    asl
    add #18
    tay

    ; revolving_x_letter1 * 4 + 4 -> X
    lda revolving_x_letter1
    asl
    asl
    add #4
    tax

    jsr spawn_particles1  ; X/Y = X/Y position in tiles

    ; draw dash
    lda #19
    jsr draw_graphic_on_background  ; A/X/Y = id/X/Y

    lda #0  ; no letter
    jmp save_entered_letter  ; A = letter; ends with rts

erase_letter_end:
    jmp move_revolving_cursor_backwards  ; a useless jump

; --------------------------------------------------------------------------------------------------

move_revolving_cursor_backwards:
    ; Called by:
    ;   check_button_b

    ; if at column >0:        move backwards
    ; if at column 0, row >0: move to end of previous line
    ; if at column 0, row 0:  do nothing
    dec revolving_x_letter1
    lda revolving_x_letter1
    cmp #$ff
    bne revolving_cursor_moved_backwards
    lda revolving_y_letter1
    beq undo_movement_backwards
    copy #7, revolving_x_letter1
    dec revolving_y_letter1
    jmp revolving_cursor_moved_backwards
undo_movement_backwards:
    inc revolving_x_letter1
revolving_cursor_moved_backwards:
    jsr fix_revolving_cursor_x  ; move left to position after last letter

    jmp highlight_input_area_letter  ; ends with rts

; --------------------------------------------------------------------------------------------------

move_revolving_cursor_manually:
    ; Called by:
    ;   check_button_a

    ; move revolving cursor to hand cursor
    lda hand_y_letter
    sub #2
    sta revolving_y_letter1
    copy hand_x_letter, revolving_x_letter1
    jsr fix_revolving_cursor_x

    phx
    phy  ; why preserve Y?
    jsr highlight_input_area_letter
    ply
    plx
    rts

; --------------------------------------------------------------------------------------------------

update_revolving_cursor:
    ; Update horizontal and vertical position, phase and attributes of the
    ; revolving cursor.
    ; Called by:
    ;   do_every_frame

    ; revolving_y_letter1 * 32 + 152 -> revolving_y_target
    lda revolving_y_letter1
    rept 5
        asl
    endr
    add #152
    sta revolving_y_target

    ; revolving_x1 * 32 + 10 -> revolving_x_target
    lda revolving_x_letter1
    rept 5
        asl
    endr
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
    adc revolving_cursor_x_offsets,x
    sta metasprite_x

    ; adjust Y position by phase
    lda revolving_y
    clc
    adc revolving_cursor_y_offsets,x
    sta metasprite_y

    ; increment phase
    inx
    cpx #16
    bne phase_incremented
    ldx #0
phase_incremented:
    stx revolving_phase

    ; update metasprite position
    ldx revolving_metasprite
    jsr update_metasprite  ; X = metasprite index

    ; update attributes of sprites
    jmp update_revolving_cursor_attributes  ; ends with rts

; --------------------------------------------------------------------------------------------------

revolving_cursor_x_offsets:
    ; Sine wave in two's complement.
    ; Values: 17.
    ; Amplitude: 10.
    ; Read by:
    ;   update_revolving_cursor

    db 0, 4, 7, 9, 10, 9, 7, 4
    db 0, 256-4, 256-7, 256-9, 256-10, 256-9, 256-7, 256-4
    db 0  ; never accessed

revolving_cursor_y_offsets:
    ; Inverted cosine wave in two's complement.
    ; Values: 17.
    ; Amplitude: 10.
    ; Read by:
    ;   update_revolving_cursor

    db 256-10, 256-9, 256-7, 256-4, 0, 4, 7, 9
    db 10, 9, 7, 4, 0, 256-4, 256-7, 256-9
    db 256-10  ; never accessed

; --------------------------------------------------------------------------------------------------

compute_revolving_cursor_speed:
    ; Compute speed for revolving cursor in X or Y direction.
    ; Args:
    ;   revolving_pos
    ;   revolving_target
    ; Out:
    ;   A: speed in pixels per frame as a signed integer:
    ;     if pos < target: 1 + floor((target - pos) / 8)
    ;     if pos = target: 0
    ;     if pos > target: ((target - pos) >> 3) | %11100000
    ; Called by:
    ;   update_revolving_cursor

    lda revolving_pos
    cmp revolving_target
    bcc too_small
    bne too_large

    lda #0
    rts

too_large:
    lda revolving_target
    sub revolving_pos
    ldx #3
shift_negative_right_loop:
    sec
    ror
    dex
    bne shift_negative_right_loop
    rts

too_small:
    lda revolving_target
    sub revolving_pos
    ldx #3
shift_positive_right_loop:
    lsr
    dex
    bne shift_positive_right_loop
    add #1
    rts

; --------------------------------------------------------------------------------------------------

accelerate_revolving_cursor:
    ; Accelerate the revolving cursor by -1/0/+1 towards the target speed.
    ; Args:
    ;   A: revolving cursor speed
    ;       (horizontal/vertical, pixels/frame, two's complement)
    ;   revolving_target_speed: target speed
    ;       (horizontal/vertical, pixels/frame, excess-128)
    ; Out:
    ;   A: revolving cursor speed
    ; Called by:
    ;   update_revolving_cursor

    eor #%10000000
    cmp revolving_target_speed
    bcc speed_too_small
    beq speed_adjusted
    sbc #1
    jmp speed_adjusted
speed_too_small:
    adc #1
speed_adjusted:
    eor #%10000000
    rts

; --------------------------------------------------------------------------------------------------

spawn_particles2:
    ; Spawn one of two sets of particles.
    ; Args:
    ;   particle_set_flag: which set to spawn
    ; Out:
    ;   particle_set_flag: flipped
    ; Called by:
    ;   spawn_particles1

    lda particle_set_flag
    beq spawn_set0

    ; flip flag, spawn set #1
    copy #0, particle_set_flag
    ldx #48
    jsr spawn_particles3  ; X = index to sprite data
    copy #1, particle_set2_timer
    rts

spawn_set0:
    ; flip flag, spawn set #0
    copy #1, particle_set_flag
    ldx #32
    jsr spawn_particles3  ; X = index to sprite data
    copy #1, particle_set1_timer
    rts

; --------------------------------------------------------------------------------------------------

initial_particle_speeds_x:
    ; Two sine waves in two's complement. Values: 8/wave.
    ; Read by:
    ;   spawn_particles3

    db 0, 6, 8, 6, 0, 256-6, 256-8, 256-6  ; outer ring (amplitude 8)
    db 0, 3, 4, 3, 0, 256-3, 256-4, 256-3  ; inner ring (amplitude 4)

initial_particle_speeds_y:
    ; Two inverted cosine waves in two's complement. Values: 8/wave.
    ; Read by:
    ;   spawn_particles3

    db 256-8, 256-6, 0, 6, 8, 6, 0, 256-6  ; outer ring (amplitude 8)
    db 256-4, 256-3, 0, 3, 4, 3, 0, 256-3  ; inner ring (amplitude 4)

; --------------------------------------------------------------------------------------------------

spawn_particles3:
    ; Write initial data of particles.
    ; Args:
    ;   X: first index to sprite data
    ; Called by:
    ;   spawn_particles2

    ldy #0  ; particle index
particle_loop:
    ; X position
    lda initial_particle_speeds_x,y
    add particle_start_x
    sta sprite_x_positions,x

    ; Y position
    lda initial_particle_speeds_y,y
    add particle_start_y
    sta sprite_y_positions,x

    ; X speed
    lda initial_particle_speeds_x,y
    sta particle_speeds_x,x

    ; Y speed
    lda initial_particle_speeds_y,y
    sta particle_speeds_y,x

    ; tile
    lda #$01
    sta sprite_tiles,x

    ; attribute (unused bits %110, palette %10)
    lda #%00011010
    sta sprite_attributes,x

    inx
    iny
    cpy #16
    bne particle_loop

    ; make noise
    copy #%00001110, snd_noise_freq1
    copy #%00000100, snd_noise_freq2
    copy #%00100101, snd_noise_ctrl1

    ; set timer
    copy #24, particle_time_left
    rts

; --------------------------------------------------------------------------------------------------

move_particles:
    ; Called by:
    ;   do_every_frame

    ; stop noise if particle_time_left goes to 0
    lda particle_time_left
    beq process_particle_set1
    dec particle_time_left
    bne process_particle_set1
    copy #%00110000, snd_noise_ctrl1
process_particle_set1:
    ; process 1st set of particles (from every 2nd explosion)
    lda particle_set1_timer
    beq process_particle_set2
    ldx #32
    inc particle_set1_timer
    lda particle_set1_timer
    cmp #24
    beq hide_particle_set1
    jsr move_particles2  ; A/X = timer, index to sprite data
process_particle_set2:
    ; process 2nd set of particles (from every 2nd explosion)
    lda particle_set2_timer
    beq move_particles_exit
    ldx #48
    inc particle_set2_timer
    lda particle_set2_timer
    cmp #24
    beq hide_particle_set2
    jsr move_particles2  ; A/X = timer, index to sprite data

move_particles_exit:
    rts

hide_particle_set1:
    copy #0, particle_set1_timer
    jsr hide_particle_set
    jmp process_particle_set2

hide_particle_set2:
    copy #0, particle_set2_timer
    jmp hide_particle_set  ; ends with rts

; --------------------------------------------------------------------------------------------------

move_particles2:
    ; Args:
    ;   A: timer (particle_set1_timer/particle_set2_timer)
    ;   X: index to sprite data (32/48)
    ; Called by:
    ;   move_particles

    ; timer modulo 8 -> temp1
    and #%00000111
    sta temp1

    ldy #16
particle_sprite_loop:
    ; if sprite is hidden, skip it
    lda sprite_y_positions,x
    cmp #255
    beq particle_sprite_processed

    ; change palette
    lda sprite_attributes,x
    eor #%00000010
    sta sprite_attributes,x

    ; detect underflow/overflow of X position
    lda particle_speeds_x,x
    bpl particle_moving_right
    ; particle is moving left
    clc
    adc sprite_x_positions,x
    bcs particle_x_checked
    jmp hide_particle
particle_moving_right:
    clc
    adc sprite_x_positions,x
    bcc particle_x_checked
hide_particle:
    ; underflow/overflow of X/Y position; hide sprite and move on
    lda #$ff
    sta sprite_y_positions,x
    lda #$00
    sta sprite_attributes,x
    jmp particle_sprite_processed
particle_x_checked:
    sta sprite_x_positions,x

    ; detect underflow/overflow of Y position
    lda particle_speeds_y,x
    bpl particle_moving_down
    ; particle is moving up
    clc
    adc sprite_y_positions,x
    bcs particle_y_checked
    jmp hide_particle
particle_moving_down:
    clc
    adc sprite_y_positions,x
    bcs hide_particle
particle_y_checked:
    sta sprite_y_positions,x

    ; slow particle down every 8th frame
    lda temp1  ; timer modulo 8
    bne particle_sprite_processed
    lda particle_speeds_x,x
    jsr decrement_absolute_value  ; A: value to decrement
    sta particle_speeds_x,x
    lda particle_speeds_y,x
    jsr decrement_absolute_value  ; A: value to decrement
    sta particle_speeds_y,x

particle_sprite_processed:
    inx
    dey
    bne particle_sprite_loop
    rts

; --------------------------------------------------------------------------------------------------

decrement_absolute_value:
    ; If A is nonzero, decrement its absolute value.
    ; Called by:
    ;   move_particles2

    beq ++
    bpl +
    add #1
    rts
+   sub #1
++  rts

; --------------------------------------------------------------------------------------------------

hide_particle_set:
    ; Hide one set of particles (16 sprites starting from X).
    ; Called by:
    ;   move_particles

    ldy #16
hide_particle_set_loop:
    lda #%00000000
    sta sprite_attributes,x
    lda #255
    sta sprite_y_positions,x
    inx
    dey
    bne hide_particle_set_loop
    rts

; --------------------------------------------------------------------------------------------------

spawn_particles1:
    ; If there is a letter at revolving cursor, continue to prepare particles.
    ; Args:
    ;   X: horizontal position of letter, in tiles
    ;   Y: vertical position of letter, in tiles
    ; Called by:
    ;   check_button_b

    tya
    pha

    ; Y * 8 + 10 -> particle_start_y
    rept 3
        asl
    endr
    add #10
    sta particle_start_y

    txa
    pha

    ; (X - 4) * 8 + 13 -> particle_start_x
    sub #4
    rept 3
        asl
    endr
    add #13
    sta particle_start_x

    ; continue spawning only if revolving cursor is on a letter
    ; (why not check this earlier?)
    jsr get_letter_at_revolving_cursor  ; letter -> A, position -> X
    beq +
    jsr spawn_particles2
+   plx
    ply
    rts

; --------------------------------------------------------------------------------------------------

save_entered_letter:
    ; Save entered letter to RAM.
    ; Called by:
    ;   letter_input
    ;   check_button_b

    pha
    jsr get_revolving_cursor_position  ; 0...23 -> X
    pla
    sta entered_letters,x
    rts

; --------------------------------------------------------------------------------------------------

fix_revolving_cursor_x:
    ; Move revolving cursor left until it lies immediately after a letter, on
    ; a letter or on the first column.
    ; Out:
    ;   X: letter position to highlight (index to entered_letters)
    ;   revolving_x_letter1
    ; Called by:
    ;   check_button_b
    ;   move_revolving_cursor_manually

    jsr get_revolving_cursor_position  ; 0...23 -> X
    ; exit if revolving cursor at left column or there's a letter at cursor
    lda revolving_x_letter1
    beq revolving_cursor_x_fixed
    lda entered_letters,x
    bne revolving_cursor_x_fixed
    ; move revolving cursor left until at column 0 or letter at cursor
read_letters_loop:
    dex
    lda entered_letters,x
    bne revolving_cursor_x_fixed
    dec revolving_x_letter1
    bne read_letters_loop
revolving_cursor_x_fixed:
    rts

; --------------------------------------------------------------------------------------------------

get_letter_at_revolving_cursor:
    ; Out:
    ;   A: letter
    ;   X: cursor position (0...23)
    ; Called by:
    ;   check_button_b
    ;   spawn_particles1

    jsr get_revolving_cursor_position
    lda entered_letters,x
    rts

; --------------------------------------------------------------------------------------------------

get_revolving_cursor_position:
    ; Out:
    ;   X: cursor position (0...23)
    ; Called by:
    ;   save_entered_letter
    ;   fix_revolving_cursor_x
    ;   get_letter_at_revolving_cursor

    ; revolving_y_letter1 * 8 + revolving_x_letter1
    lda revolving_y_letter1
    rept 3
        asl
    endr
    add revolving_x_letter1
    tax
    rts

; --------------------------------------------------------------------------------------------------

letter_input_extra_effects:
    ; Spawn flying letter and play sound (after drawing a letter).
    ; Called by:
    ;   letter_input

    ; hand_x_letter * 32 -> flying_x, metasprite_x
    lda hand_x_letter
    rept 5
        asl
    endr
    sta flying_x
    sta metasprite_x

    ; hand_y_letter * 8 -> stack
    ; hand_y_letter * 32 + 64 -> flying_y, metasprite_y
    lda hand_y_letter
    rept 3
        asl
    endr
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
    jsr set_up_metasprite  ; A = id, X = index to metasprite_indexes

    ldx flying_metasprite
    jsr update_metasprite  ; X = metasprite index

    ; compute Y speed of flying letter (+3...+9 in increments of 2):
    ; (revolving_y_letter1 * 32 + 144 - flying_y) / 16 -> flying_y_speed
    lda revolving_y_letter1
    rept 5
        asl
    endr
    add #144
    sub flying_y
    rept 4
        lsr
    endr
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
    rept 4
        asl
    endr
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

; --------------------------------------------------------------------------------------------------

move_flying_letter:
    ; Called by:
    ;   do_every_frame

    lda flying_time_left2
    beq move_flying_letter_bra1
    dec flying_time_left2
    bne move_flying_letter_bra1
    ; flying_time_left2 went from 1 to 0
    copy #%00110000, snd_pulse1_ctrl
move_flying_letter_bra1:
    lda flying_time_left1
    bne move_flying_letter_bra2
    rts

move_flying_letter_bra2:
    dec flying_time_left1
    bne move_flying_letter_bra3
    ; flying_time_left went from 1 to 0
    ldx flying_metasprite
    copy #255, metasprite_y
    jmp update_metasprite  ; X = metasprite index; ends with rts

move_flying_letter_bra3:
    ; update flying cursor X position
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

; --------------------------------------------------------------------------------------------------

fill_attribute_table_rows:
    ; Fill attribute block rows.
    ; Args:
    ;   A: fill byte
    ;   X: number of rows
    ;   Y: first row
    ; Called by:
    ;   set_up_background

    sta attribute_fill_byte
    sty vram_block_y
    stx rows_left
fill_attribute_table_rows_loop:
    lda #$00
fill_attribute_table_row_loop:
    sta vram_block_x
    jsr update_attribute_block
    lda vram_block_x
    add #1
    cmp #16
    bne fill_attribute_table_row_loop
    ; next row
    inc vram_block_y
    dec rows_left
    bne fill_attribute_table_rows_loop
    rts

; --------------------------------------------------------------------------------------------------

animate_color:
    ; Animate color 3 of background subpalette 1.
    ; Called by:
    ;   do_every_frame

    ; increment animated_color_delay every frame
    ; increment animated_color_phase every 5th frame
    ldx animated_color_phase
    ldy animated_color_delay
    iny
    cpy #5
    bne color_change_delay_incremented
    ldy #0
    inx
color_change_delay_incremented:
    sty animated_color_delay
    cpx #8
    bne color_phase_incremented
    ldx #0
color_phase_incremented:
    stx animated_color_phase

    ; set up VRAM block to update
    ; data
    lda animated_colors,x
    sta vram_block+3
    ; data size
    copy #1, vram_block+0
    ; address
    copy #>(ppu_palettes + 4 + 3), vram_block + 1
    copy #<(ppu_palettes + 4 + 3), vram_block + 2

    ; copy block to buffer
    jmp vram_block_to_buffer  ; ends with rts

animated_colors:
    ; the phases of the animated color
    db color_animated0, color_animated1, color_animated2, color_animated3
    db color_animated4, color_animated5, color_animated6, color_animated7

; --------------------------------------------------------------------------------------------------

highlight_input_area_letter:
    ; Move highlight to another letter on input area.
    ; Args:
    ;   revolving_x_letter1
    ;   revolving_x_letter2
    ;   revolving_y_letter1
    ;   revolving_y_letter2
    ; Called by:
    ;   letter_input
    ;   check_button_b
    ;   move_revolving_cursor_manually

    ; set attribute %10 to the letter the revolving cursor exits;
    ; store old Y position
    ldx revolving_x_letter2
    ldy revolving_y_letter2
    sty revolving_y_letter2_prev
    lda #%10101010
    jsr update_attribute_byte  ; A/X/Y = byte/X/Y

    ; update Y position
    lda revolving_y_letter1
    add #2
    sta revolving_y_letter2

    ; update X position
    copy revolving_x_letter1, revolving_x_letter2

    jsr highlight_input_area_row

    ; set attribute %01 to the letter the revolving cursor enters
    ldx revolving_x_letter2
    ldy revolving_y_letter2
    lda #%01010101
    jmp update_attribute_byte  ; A/X/Y = byte/X/Y; ends with rts

; --------------------------------------------------------------------------------------------------

input_area_row_attributes:
    ; Attribute table data to write when entering a row on input area
    ;   (16 bytes).
    ; Read by:
    ;   highlight_input_area_row

    db %10101111, %10101111, %10101111, %10101111
    db %10101111, %10101111, %10101111, %10101111
    db %11111010, %11111010, %11111010, %11111010
    db %11111010, %11111010, %11111010, %11111010

; --------------------------------------------------------------------------------------------------

highlight_input_area_row:
    ; Highlight active row on input area using the Attribute Table.
    ; Args:
    ;   revolving_y_letter2
    ;   revolving_y_letter2_prev
    ; Called by:
    ;   highlight_input_area_letter

    ; exit if Y position of revolving cursor has not changed
    lda revolving_y_letter2_prev
    cmp revolving_y_letter2
    beq highlight_input_area_row_exit

    ; set up VRAM block to change attribute data of all rows to %11 (gray)

    ; address
    copy #>(ppu_attribute_table + 4 * 8), vram_block+1
    copy #<(ppu_attribute_table + 4 * 8), vram_block+2
    ; data: 32 bytes, all %11111111
    ldy #4*8-1
data_setup_loop:
    lda #%11111111
    sta vram_block+3,y  ; vram_block+3 = start of data
    sta $0400+32,y
    dey
    bpl data_setup_loop
    ; data size
    copy #32, vram_block+0
    ; copy
    jsr vram_block_to_buffer

    ; set up VRAM block to change attribute data of active row to %10 (white)

    ; address: ppu_attribute_table + (2 + revolving_y_letter2) * 8
    lda revolving_y_letter2
    sub #2
    rept 3
        asl
    endr
    add #<(ppu_attribute_table + 4 * 8)
    sta vram_block+2
    copy #>(ppu_attribute_table + 4 * 8), vram_block+1
    ; data: 16 bytes from input_area_row_attributes
    ldy #2*8-1
data_setup_loop2:
    lda input_area_row_attributes,y
    sta vram_block+3,y
    dey
    bpl data_setup_loop2
    copy #16, vram_block+0    ; data size
    jsr vram_block_to_buffer  ; copy

highlight_input_area_row_exit:
    rts

; --------------------------------------------------------------------------------------------------

check_select_and_start:
    ; Called by:
    ;   do_every_frame

    ; If neither pressed, allow them next time and return.
    ; If either pressed but not allowed, just return.
    ; If either pressed and allowed, decode entered codes and start game.
    lda joypad1_status
    and #joypad_select|joypad_start
    bne select_or_start_pressed
    copy #1, temp2  ; allow select/start

check_select_and_start_exit:
    rts

select_or_start_pressed:
    lda temp2  ; allow select/start?
    beq check_select_and_start_exit

    ; disable rendering
    lda #%00000000
    sta ppu_ctrl
    sta ppu_mask

    ; set source pointer
    lda #<entered_letters
    sta code_pointer+0
    lda #>entered_letters
    sta code_pointer+1

    ; set target pointer
    lda #<decoded_codes
    sta decoded_codes_pointer+0
    lda #>decoded_codes
    sta decoded_codes_pointer+1

    ; fill decoded_codes with $ff (why 16 bytes?)
    ldx #16-1
    lda #$ff
decoded_codes_clear_loop:
    sta decoded_codes,x
    dex
    bpl decoded_codes_clear_loop

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
all_codes_decode_loop:
    ldy #%00000000
    sty temp2  ; LSB of previous letter

    ; Phase 1/2 of decoding:
    ; Modify the letters of one code (up to but not including the first
    ; non-letter):
    ;   - subtract 3 from each letter to get 4-bit values
    ;   - shift all values right one bit (a value's least significant bit
    ;     becomes the next value's 4th-least significant bit)
code_decode_loop:
    ; - if 0 (no letter), exit loop
    ; - subtract 3
    ; - shift right once
    ; - copy 4th-least significant bit from least significant bit of previous
    ;   value (always 0 for the first value)
    lda (code_pointer),y
    beq code_decoded
    sub #3
    and #%00001111
    lsr
    ora temp2  ; LSB of previous letter
    sta (code_pointer),y

    ; store the bit that just got shifted out, shifted to the 4th-least
    ; significant position (%00000000 or %00001000)
    lda #$00
    rol
    rept 3
        asl
    endr
    sta temp2  ; LSB of previous letter

    ; end of first inner loop
    iny
    cpy #8
    bne code_decode_loop

code_decoded:
    ; if the code is not 6 or 8 letters, ignore it
    sty code_length
    cpy #8
    beq eight_letter_code
    cpy #6
    beq six_letter_code
    jmp code_processed

    ; regardless of code length, read the fourth value and do nothing with it
eight_letter_code:
    ldy #3
    lda (code_pointer),y
    jmp valid_code_length
six_letter_code:
    ldy #3
    lda (code_pointer),y

valid_code_length:
    ; copy the bit that got shifted out from the last value, to the 4th-least
    ; significant position of the 1st value; thus, the values will have been
    ; rotated instead of shifted
    ldy #0
    lda (code_pointer),y
    and #%00000111
    ora temp2  ; LSB of previous letter
    sta (code_pointer),y

    ; Phase 2/2 of decoding:
    ; copy 8 nybbles from one semi-decoded code, in order specified by
    ; code_descramble_key, to 4 bytes in decoded_code.
    ldx #0    ; source nybble offset
    stx temp2  ; target byte offset
nybbles_to_bytes_loop:
    ; value to high nybble
    ldy code_descramble_key,x
    lda (code_pointer),y
    rept 4
        asl
    endr
    ; value to low nybble
    inx
    ldy code_descramble_key,x
    ora (code_pointer),y
    ; store
    ldy temp2  ; target byte offset
    sta decoded_code,y
    ; next target byte
    inc temp2  ; target byte offset
    inx
    ; end of loop
    cpx #8
    bne nybbles_to_bytes_loop

    ; clear MSB of address
    lda decoded_code+0
    and #%01111111
    sta decoded_code+0

    ; compare the address to codes stored in decoded_codes

    lda decoded_code+0  ; address high
    ldx decoded_code+1  ; address low

    ; ignore the code if the address is the same as in the first code
    cmp decoded_codes+0
    bne compare_to_second_code
    cpx decoded_codes+1
    beq code_processed  ; ignore the code

compare_to_second_code:
    ; ignore the code if the address is the same as in the second code
    cmp decoded_codes+4
    bne compare_to_third_code
    cpx decoded_codes+4+1
    beq code_processed  ; ignore the code

compare_to_third_code:
    ; ignore the code if the address is the same as in the third code
    ; (The code is never ignored here because the address in decoded_codes
    ; is always $ffff at this stage.)
    cmp decoded_codes+2*4
    bne accept_code
    cpx decoded_codes+2*4+1  ; never accessed
    beq code_processed       ; ignore the code (never accessed)

accept_code:
    ; store the code to decoded_codes
    ; (note: the replace value and the compare value trade places)

    ; address
    ldy #1
copy_address_loop:
    lda decoded_code,y
    sta (decoded_codes_pointer),y
    dey
    bpl copy_address_loop
    ; replace value
    ldy #3
    lda decoded_code-1,y
    sta (decoded_codes_pointer),y
    ; compare value
    dey
    lda decoded_code+1,y
    sta (decoded_codes_pointer),y

    ; Enable this code by ANDing genie_control_value with code_enable_mask.
    ; If the code is 8 letters, also enable the compare value by ORing
    ; genie_control_value with compare_enable_mask.
    lda genie_control_value
    and code_enable_mask
    ldx code_length
    cpx #8
    bne not_eight_letter_code
    ora compare_enable_mask
not_eight_letter_code:
    sta genie_control_value

code_processed:
    ; prepare for the next code

    ; change control value masks
    sec
    rol code_enable_mask
    asl compare_enable_mask

    ; advance source pointer
    ; The high byte is never incremented because the low byte starts from
    ; only $6b (the low byte of entered_letters).
    lda code_pointer+0
    add #8
    sta code_pointer+0
    bcc code_pointer_incremented
    inc code_pointer+1  ; never accessed

code_pointer_incremented:
    ; advance target pointer
    ; The high byte is never incremented because the low byte starts from
    ; only $90 (the low byte of decoded_codes).
    lda decoded_codes_pointer+0
    add #4
    sta decoded_codes_pointer+0
    bcc decoded_codes_pointer_incremented
    inc decoded_codes_pointer+1  ; never accessed
decoded_codes_pointer_incremented:

    ; the end of the long outer loop
    dec codes_left_to_decode
    beq all_codes_decoded
    jmp all_codes_decode_loop
all_codes_decoded:

    ; copy a short program from ROM to RAM
    ; (for some reason, two extra bytes are copied)
    ldx #ram_program_end-ram_program_source+2-1  ; bytes to copy, minus one
ram_program_copy_loop:
    lda ram_program_source,x
    sta ram_program_target,x
    dex
    bpl ram_program_copy_loop

    ; execute the program in RAM
    jmp ram_program_target

; --------------------------------------------------------------------------------------------------

    ; a short program that is copied to RAM and executed
ram_program_source:
    ; copy decoded codes to Game Genie registers ($8001-$800c)
    ldx #3*4-1
copy_to_genie_regs_loop:
    lda decoded_codes,x
    sta genie_values,x
    dex
    bpl copy_to_genie_regs_loop
    ; tell the hardware which codes are enabled and whether they use compare
    ; values, then switch to game mode
    copy genie_control_value, genie_master_control
    copy #%00000000, genie_master_control
    ; reset the system
    jmp (reset_vector)
ram_program_end:

; --------------------------------------------------------------------------------------------------

code_descramble_key:
    ; how to descramble the codes
    db 3, 5, 2, 4, 1, 0, 7, 6

; --------------------------------------------------------------------------------------------------

graphics_offsets:
    ; Offsets to actual graphics data (see below).
    ; 2 bytes each, high byte first.
    ; Read indirectly using graphics_pointer.
    ; Read by:
    ;   set_graphics_pointer

    dwbe graphic_unused   - graphics_offsets  ;  0 (never accessed)
    dwbe graphic_logo     - graphics_offsets  ;  1
    dwbe graphic_revolv   - graphics_offsets  ;  2
    dwbe graphic_a        - graphics_offsets  ;  3
    dwbe graphic_e        - graphics_offsets  ;  4
    dwbe graphic_p        - graphics_offsets  ;  5
    dwbe graphic_o        - graphics_offsets  ;  6
    dwbe graphic_z        - graphics_offsets  ;  7
    dwbe graphic_letter_x - graphics_offsets  ;  8
    dwbe graphic_l        - graphics_offsets  ;  9
    dwbe graphic_u        - graphics_offsets  ; 10
    dwbe graphic_g        - graphics_offsets  ; 11
    dwbe graphic_k        - graphics_offsets  ; 12
    dwbe graphic_i        - graphics_offsets  ; 13
    dwbe graphic_s        - graphics_offsets  ; 14
    dwbe graphic_t        - graphics_offsets  ; 15
    dwbe graphic_v        - graphics_offsets  ; 16
    dwbe graphic_letter_y - graphics_offsets  ; 17
    dwbe graphic_n        - graphics_offsets  ; 18
    dwbe graphic_dash     - graphics_offsets  ; 19
    dwbe graphic_hand     - graphics_offsets  ; 20

; --------------------------------------------------------------------------------------------------

; The actual graphics data.
; Read indirectly using graphics_pointer.
; Read by:
;   draw_graphic_on_background
;   graphic_nybble_to_vram_buffer
;   assign_metasprite_to_graphic
;   set_up_metasprite

; Format of each graphic:
;   1 byte: width in tiles
;   1 byte: height in tiles
;   width*height/2 bytes: data:
;       1 nybble = 1 tile
;       bits in each nybble (3 = MSB) represent 2*2 virtual pixels:
;           23
;           01

graphic_unused:  ; an invalid graphic (never accessed)
    db 8, 2
    db %00000001, %00100011, %01000101, %01100111

graphic_logo:  ; the "Game Genie" logo
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

graphic_revolv:  ; the revolving cursor
    db 2, 2
    db %10110001  ; line 0
    db %10000000  ; line 1

graphic_a:  ; the letter A
    db 4, 4
    db %00001010, %00000000  ; line 0
    db %00000101, %01010000  ; line 1
    db %10101100, %11100000  ; line 2
    db %11000100, %11000100  ; line 3

graphic_e:  ; the letter E
    db 4, 4
    db %10001101, %11100000  ; line 0
    db %00000111, %00010000  ; line 1
    db %00000101, %00100000  ; line 2
    db %10001100, %11000000  ; line 3

graphic_p:  ; the letter P
    db 4, 4
    db %10001101, %01100000  ; line 0
    db %00000111, %10010000  ; line 1
    db %00000101, %00000000  ; line 2
    db %10001100, %00000000  ; line 3

graphic_o:  ; the letter O
    db 4, 4
    db %00001001, %01100000  ; line 0
    db %10100000, %00000101  ; line 1
    db %10000001, %00100100  ; line 2
    db %00001000, %01000000  ; line 3

graphic_z:  ; the letter Z
    db 4, 4
    db %10101100, %11100000  ; line 0
    db %00000010, %01000000  ; line 1
    db %00100100, %00100000  ; line 2
    db %10001100, %11000000  ; line 3

graphic_letter_x:  ; the letter X
    db 4, 4
    db %11100100, %11100100  ; line 0
    db %00000110, %01000000  ; line 1
    db %00100100, %01100000  ; line 2
    db %11000100, %11000100  ; line 3

graphic_l:  ; the letter L
    db 4, 4
    db %10001101, %00000000  ; line 0
    db %00000101, %00000000  ; line 1
    db %00000101, %00100000  ; line 2
    db %10001100, %11000000  ; line 3

graphic_u:  ; the letter U
    db 4, 4
    db %11100100, %11100100  ; line 0
    db %10100000, %10100000  ; line 1
    db %10100000, %10100000  ; line 2
    db %00001100, %01000000  ; line 3

graphic_g:  ; the letter G
    db 4, 4
    db %00001001, %11000101  ; line 0
    db %10100000, %00110001  ; line 1
    db %10000001, %00000101  ; line 2
    db %00001000, %11000000  ; line 3

graphic_k:  ; the letter K
    db 4, 4
    db %10001101, %10100100  ; line 0
    db %00000111, %01000000  ; line 1
    db %00000101, %01100000  ; line 2
    db %10001100, %10000100  ; line 3

graphic_i:  ; the letter I
    db 4, 4
    db %00001110, %01000000  ; line 0
    db %00001010, %00000000  ; line 1
    db %00001010, %00000000  ; line 2
    db %00001100, %01000000  ; line 3

graphic_s:  ; the letter S
    db 4, 4
    db %00101100, %01110000  ; line 0
    db %10000111, %00010000  ; line 1
    db %00100000, %11100000  ; line 2
    db %00001100, %01000000  ; line 3

graphic_t:  ; the letter T
    db 4, 4
    db %10101110, %11100000  ; line 0
    db %00001010, %00000000  ; line 1
    db %00001010, %00000000  ; line 2
    db %00001100, %01000000  ; line 3

graphic_v:  ; the letter V
    db 4, 4
    db %11100100, %11100100  ; line 0
    db %10000001, %10010000  ; line 1
    db %00000110, %01000000  ; line 2
    db %00001000, %00000000  ; line 3

graphic_letter_y:  ; the letter Y
    db 4, 4
    db %11100100, %11100100  ; line 0
    db %00000110, %01000000  ; line 1
    db %00001010, %00000000  ; line 2
    db %00001100, %01000000  ; line 3

graphic_n:  ; the letter N
    db 4, 4
    db %11100000, %11100100  ; line 0
    db %10100110, %10100000  ; line 1
    db %10100000, %11100000  ; line 2
    db %11000100, %10000000  ; line 3

graphic_dash:  ; "-", placeholder for input area
    db 4, 4
    db %00000000, %00000000  ; line 0
    db %00100011, %00110001  ; line 1
    db %00000000, %00000000  ; line 2
    db %00000000, %00000000  ; line 3

graphic_hand:  ; hand cursor (the only graphic with an odd width)
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

; --------------------------------------------------------------------------------------------------
; Interrupt vectors

    pad $10000-6

    dw nmi              ; NMI
reset_vector:
    dw initialization1  ; reset
    dw $ffff            ; IRQ (never accessed)

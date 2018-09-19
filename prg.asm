; PRG-ROM (4 KiB)

; data segment (output disallowed)
    .data
    .include "constants.asm"
    .include "macros.asm"

; text segment (output allowed)
    .text
    .org $f000  ; last 4 KiB of CPU memory space

initialization1:
    ; Part 1/3 of initialization.

    sei  ; ignore IRQs
    cld  ; disable decimal mode
    ; disable NMI
    lda #%00000000
    sta ppu_ctrl
    ; initialize stack pointer
    ldx #$ff
    txs

    ; do something to Game Genie registers
    lda #$00
    sta genie_unknown1
    jsr delay
    sta genie_unknown2
    jsr delay
    sta genie_unknown1

    ; continue initialization
    jmp initialization2

; -----------------------------------------------------------------------------

delay:
    ; Wait.
    ; Called by:
    ;   initialization1

    ldx #96
    ldy #8
delay_loop:
    dex
    bne delay_loop
    dey
    bne delay_loop
    rts

; -----------------------------------------------------------------------------

initialization2:
    ; Part 2/3 of initialization.

    ; wait for VBlank 10 times
    ldx #10
wait_vblank_loop:
    lda ppu_status
    bpl wait_vblank_loop
    dex
    bne wait_vblank_loop

    ; reinitialize stack pointer (why?)
    ldx #$ff
    txs

    ; clear RAM (fill $0000...$07ff with $00)
    lda #$07
    sta ram_clear_pointer+1
    lda #$00
    sta ram_clear_pointer
    tay
ram_clear_loop:
    sta (ram_clear_pointer),y
    iny
    bne ram_clear_loop
    dec ram_clear_pointer+1
    bpl ram_clear_loop

    ; hide sprites and background
    lda #%00000110
    sta ppu_mask_mirror
    sta ppu_mask

    ; enable NMI, use 8*8-pixel sprites, use Pattern Table 0, use Name Table 0,
    ; make the VRAM address increment by one
    lda #%10000000
    sta ppu_ctrl_mirror
    sta ppu_ctrl

    ; $ff -> vram_buffer_free_bytes
    `dec_abs vram_buffer_free_bytes

    ; continue initialization
    jmp initialization3

; -----------------------------------------------------------------------------

read_joypads:
    ; Read joypads (both, for some reason).
    ; Out:
    ;   joypad1_status (bits: A, B, select, start, up, down, left, right)
    ;   joypad2_status (bits: A, B, select, start, up, down, left, right)
    ; Called by:
    ;   do_every_frame

    ; initialize the joypads
    lda #%00000001
    sta joypad1
    lda #%00000000
    sta joypad1

    ; read joypad 1
    ldy #8              ; number of buttons to read
joypad1_loop:
    lda joypad1         ; read from memory-mapped register
    ror                 ; LSB to carry
    rol joypad1_status  ; carry to RAM
    dey
    bne joypad1_loop

    ; read joypad 2
    ldy #8
joypad2_loop:
    lda joypad2
    ror
    rol joypad2_status
    dey
    bne joypad2_loop

    rts

; -----------------------------------------------------------------------------

nmi:
    ; The non-maskable interrupt routine.
    ; Args:
    ;   skip_nmi: if nonzero, skip doing the usual stuff
    ; Out:
    ;   nmi_done: set to nonzero when exiting

    ; push A, X, Y
    pha
    txa
    pha
    tya
    pha

    lda skip_nmi
    bne nmi_skip

    jsr sprite_dma
    jsr vram_buffer_to_vram

    `lda_abs scroll_x_mirror
    sta ppu_scroll
    `lda_abs scroll_y_mirror
    sta ppu_scroll

    lda ppu_ctrl_mirror
    sta ppu_ctrl

nmi_skip:
    lda #1
    `sta_abs nmi_done

    ; pull Y, X, A
    pla
    tay
    pla
    tax
    pla

    rti

; -----------------------------------------------------------------------------

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

    stx gfx_x
    sty gfx_y
    jsr set_graphics_pointer

    ; get width and height of graphic (in nybbles/tiles)
    ldy #0
    lda (graphics_pointer),y
    sta gfx_width
    iny
    lda (graphics_pointer),y
    sta gfx_height

    ; advance graphics pointer to start of data
    `lda_abs graphics_pointer
    clc
    adc #2
    `sta_abs graphics_pointer
    `lda_abs graphics_pointer+1
    adc #0
    `sta_abs graphics_pointer+1

    ; copy nybbles to VRAM buffer
    lda #0
    sta gfx_y_offset
graphic_copy_loop_y:
    lda #0
    sta gfx_x_offset
graphic_copy_loop_x:
    jsr graphic_nybble_to_vram_buffer
    inc gfx_x_offset
    lda gfx_x_offset
    cmp gfx_width
    bne graphic_copy_loop_x
    inc gfx_y_offset
    lda gfx_y_offset
    cmp gfx_height
    bne graphic_copy_loop_y
    rts

; -----------------------------------------------------------------------------

graphic_nybble_to_vram_buffer:
    ; Copy one nybble (one tile) of graphics data to VRAM buffer.
    ; Args:
    ;   gfx_x: horizontal position of graphic, in tiles
    ;   gfx_y: vertical position of graphic, in tiles
    ;   gfx_x_offset: horizontal position inside graphic, in tiles
    ;   gfx_y_offset: vertical position inside graphic, in tiles
    ;   vram_address_high: high byte of VRAM address
    ; Called by:
    ;   draw_graphic_on_background

    ; get offset of nybble
    lda gfx_y_offset
    ldx gfx_width
    jsr multiply
    clc
    adc gfx_x_offset
    `sta_abs nybble_offset

    ; read graphics byte
    lsr
    tay
    lda (graphics_pointer),y
    sta gfx_data_byte

    ; get nybble from byte
    `lda_abs nybble_offset
    and #%00000001
    beq get_upper_nybble1
    lda gfx_data_byte
    and #%00001111
    jmp got_nybble1
get_upper_nybble1:
    lda gfx_data_byte
    lsr
    lsr
    lsr
    lsr
got_nybble1:
    sta gfx_nybble

    ; vram_address_high * 256
    ; + (gfx_y + gfx_y_offset) * 32
    ; + gfx_x + gfx_x_offset - 4
    ; -> word(nybble_vram_high, nybble_vram_low)

    ; gfx_x + gfx_x_offset - 4 -> vram_block_x
    lda gfx_x
    clc
    adc gfx_x_offset
    sec
    sbc #4
    sta vram_block_x

    ; (gfx_y + gfx_y_offset) * 32 + vram_address_high * 256
    ; -> word(nybble_vram_high, nybble_vram_low)
    lda gfx_y
    clc
    adc gfx_y_offset
    sta vram_block_y
    lda vram_block_y
    asl
    asl
    asl
    sta nybble_vram_low
    lda #0               ; will become nybble_vram_high
    asl nybble_vram_low  ; 4th shift
    rol                  ; save overflown bit
    asl nybble_vram_low  ; 5th shift
    rol                  ; save overflown bit
    clc
    adc vram_address_high
    sta nybble_vram_high

    ; word(nybble_vram_high, nybble_vram_low) += vram_block_x
    lda nybble_vram_low
    clc
    adc vram_block_x
    sta nybble_vram_low
    lda nybble_vram_high
    adc #0
    sta nybble_vram_high

    ; set up VRAM block (1 byte of data)
    lda nybble_vram_high
    sta vram_block+1      ; address high
    lda nybble_vram_low
    sta vram_block+2      ; address low
    lda gfx_nybble
    sta vram_block+3      ; data
    lda #1
    sta vram_block+0      ; data size

    ; copy VRAM block to VRAM buffer
    jsr vram_block_to_buffer

    rts

; -----------------------------------------------------------------------------

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
    beq skip_multiplication
    clc
multiplication_loop:
    adc multiplication_temp
    dex
    bne multiplication_loop
skip_multiplication:
    rts

; -----------------------------------------------------------------------------

sprite_dma:
    ; Copy interleaved_sprite_data to OAM.
    ; Called by:
    ;   nmi
    ;   initialization3

    lda #$00
    sta oam_addr
    lda #>interleaved_sprite_data
    sta oam_dma
    rts

; -----------------------------------------------------------------------------

vram_buffer_to_vram:
    ; Copy as much as possible from VRAM buffer to VRAM.
    ; Each block in buffer: number of bytes, address hi, address lo, bytes
    ; Called by:
    ;   nmi
    ;   vram_block_to_buffer

    ; the budget, i.e., the maximum sum of (blocks*5 + total_bytes) to copy
    lda #100
    sta vram_budget

    ldy vram_buffer_next_read

vram_block_copy_loop:
    ; exit if nothing left to copy
    cpy vram_buffer_next_write
    beq vram_copy_end

    ; get VRAM block size;
    ; compute the cost of copying the block, i.e. size + 5
    lda vram_buffer,y
    tax
    clc
    adc #5
    sta vram_block_cost

    ; remove cost from budget;
    ; exit if out of budget
    lda vram_budget
    sec
    sbc vram_block_cost
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
copy_vram_byte:
    lda vram_buffer,y
    sta ppu_data
    iny
    dex
    bne copy_vram_byte

    jmp vram_block_copy_loop

vram_copy_end:
    sty vram_buffer_next_read
    rts

; -----------------------------------------------------------------------------

vram_block_to_buffer:
    ; Copy current VRAM block to VRAM buffer.
    ; Called by:
    ;   graphic_nybble_to_vram_buffer
    ;   update_attribute_block
    ;   initialization3
    ;   animate_color
    ;   highlight_input_area_row

    ; get total block size
    lda vram_block+0
    clc
    adc #3
    sta vram_block_total_size

    ; continue if vram_buffer_free_bytes >= vram_block_total_size
    lda vram_buffer_free_bytes
    cmp vram_block_total_size
    bcs continue_vram_block_to_buffer

    ; restart sub if NMI is being skipped
    lda skip_nmi
    beq vram_block_to_buffer

    ; copy some data out of VRAM buffer and restart sub
    jsr vram_buffer_to_vram
    jmp vram_block_to_buffer

continue_vram_block_to_buffer:
    ; vram_buffer_free_bytes -= vram_block_total_size
    sec
    sbc vram_block_total_size
    sta vram_buffer_free_bytes

    ; copy VRAM block to buffer
    ldx #0                      ; index to read within block
    ldy vram_buffer_next_write
block_to_buffer_loop:
    lda vram_block,x
    sta vram_buffer,y
    inx
    iny
    cpx vram_block_total_size
    bne block_to_buffer_loop

    sty vram_buffer_next_write
    rts

; -----------------------------------------------------------------------------

assign_metasprite_to_graphic:
    ; Draw a graphic (e.g. the hand cursor) as a metasprite.
    ; Args:
    ;   A: graphic id (see graphics_offsets)
    ; Out:
    ;   A: index to metasprite_indexes
    ; Called by:
    ;   initialization3

    ; start reading specified graphic
    sta gfx_to_draw
    jsr set_graphics_pointer

    ; get width and height of graphic
    ldy #0
    lda (graphics_pointer),y
    sta metasprite_width
    tax
    iny
    lda (graphics_pointer),y
    sta metasprite_height

    ; width * height of graphic -> A, gfx_data_bytes_left
    jsr multiply
    sta gfx_data_bytes_left

    ; total size of graphic -> A
    clc
    adc #2

    ; find first free byte in metasprite_indexes;
    ; save to metasprite_to_create and X;
    ; add total size of graphic to all following bytes in metasprite_indexes
    jsr find_free_metasprite
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

    ; assign gfx_data_bytes_left free sprites to the metasprite,
    ; starting from the first free sprite
    ldy #255
find_sprite_loop:
    iny
    lda sprite_attributes,y
    bne find_sprite_loop
    tya
    sta metasprites,x
    inx
    dec gfx_data_bytes_left
    bne find_sprite_loop

    ; set up tiles and attributes for individual sprites
    ldx metasprite_to_create
    lda gfx_to_draw
    jsr set_up_metasprite

    ; set up positions for individual sprites
    ldx metasprite_to_create
    jsr update_metasprite

    ; return metasprite index
    lda metasprite_to_create
    rts

; -----------------------------------------------------------------------------

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

    sta gfx_index

    ; word(graphics_offsets) -> word(graphics_pointer)
    lda #<graphics_offsets
    sta graphics_pointer
    lda #>graphics_offsets
    sta graphics_pointer+1

    ; word(graphics_pointer) += gfx_index >> 7
    lda gfx_index
    bpl graphics_pointer_initialized
    inc graphics_pointer+1
graphics_pointer_initialized:

    ; get offset to offset
    asl
    tay
    ; get offset to graphic
    lda (graphics_pointer),y
    pha
    iny
    lda (graphics_pointer),y
    ; get address of graphic
    clc
    adc #<graphics_offsets
    sta graphics_pointer
    pla
    adc #>graphics_offsets
    sta graphics_pointer+1
    rts

; -----------------------------------------------------------------------------

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
    lda metasprites,x
    sta metasprite_width
    lda metasprites+1,x
    sta metasprite_height

    ; advance to start of individual sprite indexes
    inx
    inx

    ; Y positions of individual sprites start from metasprite Y position
    lda metasprite_y
    sta sprite_y
    lda always_zero2
    sta sprite_y+1

update_metasprite_loop:
    ; if sprite is beyond bottom edge of screen, hide this row of sprites
    lda sprite_y+1
    bne hide_row_of_sprites

    ; initialize loop counter with number of nybbles/tiles per row
    lda metasprite_width
    sta nybbles_left_x

    ; X positions of individual sprites start from metasprite X position
    lda metasprite_x
    sta sprite_x
    lda always_zero1
    sta sprite_x+1

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
    lda sprite_x
    sta sprite_x_positions,y
    lda sprite_y
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
    lda sprite_x
    clc
    adc #8
    sta sprite_x
    lda sprite_x+1
    adc #0
    sta sprite_x+1

    ; end of inner loop
    dec nybbles_left_x
    bne update_metasprite_loop_x

sprite_row_processed:
    ; word[sprite_y] += 8
    lda sprite_y
    clc
    adc #8
    sta sprite_y
    lda sprite_y+1
    adc #0
    sta sprite_y+1

    ; end of outer loop
    dec metasprite_height
    bne update_metasprite_loop

    rts

hide_row_of_sprites:
    ; hide metasprite_width sprites and go to sprite_row_processed
    lda metasprite_width
    sta nybbles_left_x
hide_row_of_sprites_loop:
    lda metasprites,x
    tay
    lda #255
    sta sprite_y_positions,y
    inx
    dec nybbles_left_x
    bne hide_row_of_sprites_loop
    jmp sprite_row_processed

; -----------------------------------------------------------------------------

set_up_metasprite:
    ; Set up tiles and attributes for individual sprites of a metasprite.
    ; Args:
    ;   A: graphic id (see graphics_offsets)
    ;   X: index to metasprite_indexes
    ; Called by:
    ;   assign_metasprite_to_graphic
    ;   letter_input_extra_effects

    sta gfx_to_draw

    ; get index to metasprites where indexes to individual sprites are
    lda metasprite_indexes,x
    clc
    adc #2
    pha

    ; start reading specified graphic
    lda gfx_to_draw
    jsr set_graphics_pointer

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
    lda #0
    sta always_zero3

    ; X: index to metasprites
    pla
    tax

    ; copy all rows of graphics data to metasprite's individual sprites
loop_y:
    lda metasprite_width
    sta nybbles_left_x  ; loop_x counter

    ; copy one row of graphics data to metasprite's individual sprites
loop_x:
    ; read byte from graphics data
    lda nybble_offset
    lsr
    clc
    adc #1
    tay
    lda (graphics_pointer),y
    sta gfx_data_byte

    ; push upper/lower nybble (if nybble_offset is even/odd, respectively)
    lda nybble_offset
    and #%00000001
    beq get_upper_nybble2
    lda gfx_data_byte
    and #%00001111
    jmp got_nybble2
get_upper_nybble2:
    lda gfx_data_byte
    lsr
    lsr
    lsr
    lsr
got_nybble2:
    pha

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
    clc
    adc always_one
    sta nybble_offset

    ; end of loop_x
    dec nybbles_left_x
    bne loop_x

    ; do nothing
    lda nybble_offset
    clc
    adc always_zero3
    sta nybble_offset

    ; end of loop_y
    dec metasprite_height
    bne loop_y

    rts

; -----------------------------------------------------------------------------

convert_sprites:
    ; Convert planar sprite data to interleaved.
    ; Sprites with attribute byte $00 will be hidden.
    ; Called by:
    ;   do_every_frame

    ; ascending order every 2nd frame, descending order every 2nd frame
    lda odd_frame_flag1
    eor #%00000001
    sta odd_frame_flag1
    bne convert_sprites_ascending

    ; descending order: planar sprites 61...0 -> interleaved sprites 2...63
    ldy #2*4  ; target offset
    ldx #61   ; source offset
convert_sprites_descending_loop:
    lda sprite_attributes,x
    beq sprite_setup_descending_hide
    jsr convert_sprite
    dex
    bpl convert_sprites_descending_loop
    rts
sprite_setup_descending_hide:
    jsr hide_sprite
    dex
    bpl convert_sprites_descending_loop
    rts  ; unaccessed

    ; ascending order: planar sprites 0...61 -> interleaved sprites 2...63
convert_sprites_ascending:
    ldy #2*4  ; target offset
    ldx #0    ; source offset
convert_sprites_ascending_loop:
    lda sprite_attributes,x
    beq sprite_setup_ascending_hide
    jsr convert_sprite
    inx
    cpx #62
    bne convert_sprites_ascending_loop
    rts
sprite_setup_ascending_hide:
    jsr hide_sprite
    inx
    cpx #62
    bne convert_sprites_ascending_loop
    rts

; -----------------------------------------------------------------------------

convert_sprite:
    ; Convert one non-hidden sprite from planar to interleaved.
    ; Args:
    ;   A: attributes
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

    iny
    iny
    iny
    iny
    rts

; -----------------------------------------------------------------------------

hide_sprite:
    ; Hide a sprite in interleaved_sprite_data.
    ; Args:
    ;   Y: offset in interleaved_sprite_data
    ; Out:
    ;   Y += 4
    ; Called by:
    ;   convert_sprites

    ; set Y position outside screen
    lda #255
    sta interleaved_sprite_data,y

    iny
    iny
    iny
    iny
    rts

; -----------------------------------------------------------------------------

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
    sta attribute_temp
    lda vram_block_x
    and #%00000001
    ora attribute_temp
    tay

    ; position of byte within Attribute Table (0-63) -> A, X
    lda vram_block_y
    asl
    asl
    and #%11111000
    sta attribute_temp
    lda vram_block_x
    lsr
    clc
    adc attribute_temp
    tax

    ; (start address of Attribute Table) + A -> target address of VRAM block
    ora #<ppu_attribute_table
    sta vram_block+2  ; address low
    lda #>ppu_attribute_table
    sta vram_block+1  ; address high

    ; one byte to copy
    lda #1
    sta vram_block+0  ; data size

    ; combine old and new bits of attribute byte:
    ; (newAttributeByte & bitmaskDeterminedByY)
    ; | ($0400,x & ~bitmaskDeterminedByY)
    ; -> $0400,x and vram_block+3 (start of data)
    lda attribute_block_bitmasks,y
    and attribute_fill_byte
    sta gfx_data_bit_pair
    lda attribute_block_bitmasks,y
    eor #%11111111
    and $0400,x
    ora gfx_data_bit_pair
    sta $0400,x
    sta vram_block+3  ; start of data

    jmp vram_block_to_buffer  ; ends with rts

; -----------------------------------------------------------------------------

attribute_block_bitmasks:
    ; Read by:
    ;   update_attribute_block

    .byte %00000011, %00001100, %00110000, %11000000

; -----------------------------------------------------------------------------

initial_palette:
    ; Initial palette. 32 bytes.
    ; Read by:
    ;   initialization3

    ; background subpalette 0
    .byte color_background, color_unused1, color_unused1, color_keyboard
    ; background subpalette 1
    .byte color_unused1, color_unused1, color_unused1, color_animated_initial
    ; background subpalette 2
    .byte color_unused1, color_unused1, color_unused1, color_highlight
    ; background subpalette 3
    .byte color_unused1, color_unused1, color_unused1, color_input_area

    ; sprite subpalette 0
    .byte color_background, color_unused1, color_unused1
    .byte color_letter_revolving1_particle1
    ; sprite subpalette 1
    .byte color_unused1, color_unused1, color_unused1, color_hand1
    ; sprite subpalette 2
    .byte color_unused1, color_unused1, color_unused1
    .byte color_hand2_revolving2_particle2
    ; sprite subpalette 3
    .byte color_unused1, color_unused1, color_unused1, color_unused2

; -----------------------------------------------------------------------------

initialization3:
    ; Part 3/3 of initialization.

    lda #1
    sta skip_nmi

    ; clear first Name&Attribute Table (fill VRAM $2000...$23ff with $00)
    lda #>ppu_name_table
    sta vram_address_high
    sta ppu_addr
    lda #<ppu_name_table
    sta ppu_addr
    ldx #4
    tay
vram_clear_loop:
    sta ppu_data
    dey
    bne vram_clear_loop
    dex
    bne vram_clear_loop

    jsr set_up_background

    ; clear metasprite_indexes (why? we just cleared the entire RAM)
    ldx #0
    lda #$00
clear_loop:
    sta metasprite_indexes,x
    inx
    cpx #50
    bne clear_loop

    jsr sprite_dma

    ; initial position of hand cursor
    lda #14
    `sta_abs hand_x_px
    sta metasprite_x
    lda #60
    `sta_abs hand_y_px
    sta metasprite_y

    ; initial position of revolving cursor
    lda #128
    `sta_abs revolving_x
    lda #150
    `sta_abs revolving_y

    ; draw hand cursor
    lda #graphic_id_hand
    jsr assign_metasprite_to_graphic
    `sta_abs hand_metasprite

    ; draw revolving cursor
    lda #graphic_id_revolv
    jsr assign_metasprite_to_graphic
    `sta_abs revolving_metasprite

    lda #255
    sta metasprite_y

    ; initialize the letter that flies from virtual keyboard to input area
    ; (only the dimensions matter; drawing e.g. the hand cursor will cause
    ; the flying letter to have wrong dimensions)
    lda #graphic_id_p
    jsr assign_metasprite_to_graphic
    `sta_abs flying_metasprite

    ; copy sprite attribute data from ROM to RAM
    ldx #19
sprite_attr_copy_loop:
    lda initial_sprite_attribute_data,x
    sta sprite_attributes,x
    dex
    bpl sprite_attr_copy_loop

    lda #%00001001
    sta snd_clock

    lda #32
    sta vram_block+0  ; data size
    lda #>ppu_palettes
    sta vram_block+1  ; address high
    lda #<ppu_palettes
    sta vram_block+2  ; address low

    ; copy initial palette from ROM to VRAM block
    ldy #0
uncond23_loop4:
    lda initial_palette,y
    sta vram_block+3,y  ; vram_block+3 = start of data
    iny
    cpy #32
    bne uncond23_loop4
    jsr vram_block_to_buffer

    lda #0
    sta skip_nmi

    jsr wait_until_nmi_done

    ; show sprites&background
    lda ppu_mask_mirror
    ora #%00011000
    sta ppu_mask_mirror
    sta ppu_mask

; -----------------------------------------------------------------------------

main_loop:
    jsr do_every_frame
    jsr wait_until_nmi_done
    jmp main_loop

; -----------------------------------------------------------------------------

wait_until_nmi_done:
    ; Wait until the NMI routine has run.
    ; Called by:
    ;   initialization3
    ;   main_loop

    ; clear the flag
    lda #0
    `sta_abs nmi_done
    ; wait until the flag gets set
nmi_done_wait_loop:
    `lda_abs nmi_done
    beq nmi_done_wait_loop
    rts

; -----------------------------------------------------------------------------

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
    `lda_abs hand_x_px
    sta metasprite_x
    `lda_abs hand_y_px
    sta metasprite_y
    `ldx_abs hand_metasprite
    jsr update_metasprite

    jsr convert_sprites
    rts

; -----------------------------------------------------------------------------

check_arrows:
    ; Called by:
    ;   move_hand

    ; if hand_y_speed_pointer set, skip checking vertical arrows
    `lda_abs hand_y_speed_pointer+1
    bne check_horizontal_arrows

    ; was up pressed?
    lda joypad1_status
    and #joypad_up
    beq check_down
    ; move hand if possible
    lda #joypad_up
    jsr set_hand_target  ; args: A=direction, out: A=success
    beq check_down
    ; hand successfully moved
    ; hand_speeds_negative -> hand_y_speed_pointer
    lda #<hand_speeds_negative
    `sta_abs hand_y_speed_pointer
    lda #>hand_speeds_negative
    `sta_abs hand_y_speed_pointer+1
    ; if hand moved from 3rd line to 2nd, 32 -> hand_y_speed_offset,
    ; else 0 -> hand_y_speed_offset
    lda #joypad_up
    jsr moving_between_keyboard_and_input_area
    beq did_not_move_to_line2
    lda #32
    jmp up_checked
did_not_move_to_line2:
    lda #0
up_checked:
    `sta_abs hand_y_speed_offset
    lda #joypad_up
    `sta_abs last_y_input_accepted

    jmp check_horizontal_arrows

check_down:
    ; was down pressed?
    lda joypad1_status
    and #joypad_down
    beq check_horizontal_arrows
    ; move hand if possible
    lda #joypad_down
    jsr set_hand_target  ; args: A=direction, out: A=success
    beq check_horizontal_arrows
    ; hand successfully moved
    ; hand_speeds_positive -> hand_y_speed_pointer
    lda #<hand_speeds_positive
    `sta_abs hand_y_speed_pointer
    lda #>hand_speeds_positive
    `sta_abs hand_y_speed_pointer+1
    ; if hand moved from 2nd to 3rd line, 32 -> hand_y_speed_offset,
    ; else 0 -> hand_y_speed_offset
    lda #joypad_down
    jsr moving_between_keyboard_and_input_area
    beq did_not_move_to_line3
    lda #32
    jmp down_checked
did_not_move_to_line3:
    lda #0
down_checked:
    `sta_abs hand_y_speed_offset
    lda #joypad_down
    `sta_abs last_y_input_accepted

check_horizontal_arrows:
    ; if hand_x_speed_pointer set, skip checking left/right button
    `lda_abs hand_x_speed_pointer+1
    bne arrow_check_exit

    ; was left pressed?
    lda joypad1_status
    and #joypad_left
    beq check_right
    ; move hand if possible
    lda #joypad_left
    jsr set_hand_target  ; args: A=direction, out: A=success
    beq check_right
    ; hand successfully moved
    ; hand_speeds_negative -> hand_x_speed_pointer
    lda #<hand_speeds_negative
    `sta_abs hand_x_speed_pointer
    lda #>hand_speeds_negative
    `sta_abs hand_x_speed_pointer+1
    ; 0 -> hand_x_speed_offset
    lda #0
    `sta_abs hand_x_speed_offset
    lda #joypad_left
    `sta_abs last_x_input_accepted

    jmp arrow_check_exit

check_right:
    ; was right pressed?
    lda joypad1_status
    and #joypad_right
    beq arrow_check_exit
    ; move hand if possible
    lda #joypad_right
    jsr set_hand_target  ; args: A=direction, out: A=success
    beq arrow_check_exit
    ; hand successfully moved
    ; hand_speeds_positive -> hand_x_speed_pointer
    lda #<hand_speeds_positive
    `sta_abs hand_x_speed_pointer
    lda #>hand_speeds_positive
    `sta_abs hand_x_speed_pointer+1
    ; 0 -> hand_x_speed_offset
    lda #0
    `sta_abs hand_x_speed_offset
    lda #joypad_right
    `sta_abs last_x_input_accepted

arrow_check_exit:
    rts

; -----------------------------------------------------------------------------

find_free_metasprite:
    ; Find the first free byte in metasprite_indexes and add the specified
    ; value to all following bytes.
    ; Args:
    ;   A: the value to add
    ; Out:
    ;   A: index to the first free byte
    ; Called by:
    ;   assign_metasprite_to_graphic

    sta value_to_add

    ; find first free byte (same as the following byte)
    ldx #255
find_byte_loop:
    inx
    lda metasprite_indexes+1,x
    cmp metasprite_indexes,x
    bne find_byte_loop

    stx first_free_byte  ; index to first free byte
    sta unused2          ; never used anywhere

    ; add the value to all following bytes
    inx
add_value_loop:
    lda value_to_add
    clc
    adc metasprite_indexes,x
    sta metasprite_indexes,x
    inx
    cpx #50
    bne add_value_loop

    lda first_free_byte
    rts

; -----------------------------------------------------------------------------

set_up_background:
    ; Initialize background graphics.
    ; Called by:
    ;   initialization3

    ; center background graphics vertically by scrolling
    lda #0
    sta scroll_x_mirror
    lda #4
    sta scroll_y_mirror

    ; the Game Genie logo
    lda #graphic_id_logo
    ldx #5  ; X
    ldy #3  ; Y
    jsr draw_graphic_on_background

    ; prepare to draw the virtual keyboard
    lda #graphic_id_a
    sta keyboard_graphic
    lda #4
    sta keyboard_graphic_x
    lda #8
    sta keyboard_graphic_y

    ; virtual keyboard
draw_keyboard_loop:
    ; draw letter
    lda keyboard_graphic
    ldx keyboard_graphic_x
    ldy keyboard_graphic_y
    jsr draw_graphic_on_background
    ; increment X position
    lda keyboard_graphic_x
    clc
    adc #4
    sta keyboard_graphic_x
    ; at end of line?
    cmp #9*4
    bne next_keyboard_letter
    ; yes; move to start of next line
    lda #4
    sta keyboard_graphic_x
    lda keyboard_graphic_y
    clc
    adc #4
    sta keyboard_graphic_y
next_keyboard_letter:
    ; loop until the last letter (N) has been drawn
    inc keyboard_graphic
    lda keyboard_graphic
    cmp #graphic_id_n+1
    bne draw_keyboard_loop

    ; prepare to draw the input area (dashes)
    lda #3
    sta keyboard_graphic  ; used as the loop counter now
    lda #4
    sta keyboard_graphic_x
    lda #18
    sta keyboard_graphic_y

    ; draw the 24 (3*8) dashes of the input area
draw_input_area_loop:
    ; draw the dash
    lda #graphic_id_dash
    ldx keyboard_graphic_x
    ldy keyboard_graphic_y
    jsr draw_graphic_on_background
    ; increment X position
    lda keyboard_graphic_x
    clc
    adc #4
    sta keyboard_graphic_x
    ; at end of line?
    cmp #9*4
    bne draw_next_dash
    ; yes; move to start of next line
    lda #4
    sta keyboard_graphic_x
    lda keyboard_graphic_y
    clc
    adc #4
    sta keyboard_graphic_y
draw_next_dash:
    ; loop until last dash has been drawn
    inc keyboard_graphic
    lda keyboard_graphic
    cmp #3+24
    bne draw_input_area_loop

    ; set attribute table data

    ; Game Genie logo (block rows 0-3)
    lda #%01010101
    ldx #4
    ldy #0
    jsr fill_attribute_table_rows

    ; "keyboard" (block rows 4-7)
    lda #%00000000
    ldx #4
    ldy #4
    jsr fill_attribute_table_rows

    ; 1st code (block rows 9-10)
    lda #%10101010
    ldx #2
    ldy #9
    jsr fill_attribute_table_rows

    ; 2nd and 3rd code (block rows 11-14)
    lda #%11111111
    ldx #4
    ldy #11
    jsr fill_attribute_table_rows

    ldx #0
    ldy #0
    jsr highlight_attribute_byte  ; use attribute byte %10101010

    lda #2  ; no visible effect if replaced with different value
    `sta_abs revolving_y_letter2
    rts

; -----------------------------------------------------------------------------

move_hand:
    ; Called by:
    ;   do_every_frame

    ; check arrows every 2nd frame
    lda odd_frame_flag2
    eor #%00000001
    sta odd_frame_flag2
    bne move_hand_horizontally
    jsr check_arrows

move_hand_horizontally:
    lda hand_x_speed_pointer+1
    beq move_hand_vertically  ; hand not moving horizontally

    ; get hand speed (if offset modulo 16 = 15, terminator)
    ldy hand_x_speed_offset
    lda (hand_x_speed_pointer),y
    cmp #$80
    bne hand_moving_horizontally

    ; terminator; stop hand by resetting pointer
    lda #0
    sta hand_x_speed_pointer+1
    jmp move_hand_bra1

hand_moving_horizontally:
    ; no terminator
    ldx #hand_x_px
    jsr add_hand_speed_to_position  ; A = speed, X = address of variable
    ; every 2nd frame, skip the rest of horizontal movement
    beq move_hand_vertically

    ; continue towards update_x_speed_offset
move_hand_bra1:
    tya             ; hand_x_speed_offset
    and #%00001111  ; get position on current line
    cmp #7
    bne update_x_speed_offset

    ; hand_x_speed_offset mod 16 = 7

    jsr update_hand_letter_position
    lda joypad1_status
    and last_x_input_accepted
    beq update_x_speed_offset

    lda last_x_input_accepted
    jsr set_hand_target        ; args: A=direction, out: A=success
    beq update_x_speed_offset  ; fail

    ; success
    ldy #16-1

update_x_speed_offset:
    iny
    sty hand_x_speed_offset

move_hand_vertically:
    lda hand_y_speed_pointer+1
    beq move_hand_exit  ; hand not moving vertically

    ; get hand speed (if offset modulo 16 = 15, terminator)
    ldy hand_y_speed_offset
    lda (hand_y_speed_pointer),y
    cmp #$80
    bne hand_moving_vertically

    ; terminator; stop hand by resetting pointer
    lda #0
    sta hand_y_speed_pointer+1
    jmp move_hand_bra2

hand_moving_vertically:
    ; no terminator
    ldx #hand_y_px
    jsr add_hand_speed_to_position  ; A = speed, X = address of variable
    ; every 2nd frame, skip the rest of vertical movement
    beq move_hand_exit

move_hand_bra2:
    tya             ; hand_y_speed_offset
    and #%00001111  ; get position on current line
    cmp #7
    bne update_y_speed_offset

    ; hand_y_speed_offset mod 16 = 7

    jsr update_hand_letter_position
    lda joypad1_status
    and last_y_input_accepted
    beq update_y_speed_offset

    lda last_y_input_accepted
    jsr set_hand_target        ; args: A=direction, out: A=success
    beq update_y_speed_offset  ; fail

    ; success
    lda last_y_input_accepted
    jsr moving_between_keyboard_and_input_area
    beq not_moving_between_keyboard_and_input_area
    ldy #3*16-1
    jmp update_y_speed_offset
not_moving_between_keyboard_and_input_area:
    ldy #16-1

update_y_speed_offset:
    iny
    sty hand_y_speed_offset
move_hand_exit:
    rts

; -----------------------------------------------------------------------------

    ; read with hand_x_speed_pointer and hand_y_speed_pointer
hand_speeds_positive:
    ; 4 * 16 bytes; $80 is the terminator
    .byte 1, 1, 2, 2, 3, 3, 4, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=32
    .byte 5, 4, 4, 3, 3, 4, 5, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=44
    .byte 2, 3, 4, 5, 6, 6, 6, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=48
    .byte 5, 6, 7, 8, 7, 6, 5, 4, 3, 3, 2, 2, 2, 1, 256-1, $80  ; sum=60
hand_speeds_negative:
    ; same values as above, except negated in two's complement
    .byte $ff,$ff,$fe,$fe,$fd,$fd,$fc,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80
    .byte $fb,$fc,$fc,$fd,$fd,$fc,$fb,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80
    .byte $fe,$fd,$fc,$fb,$fa,$fa,$fa,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80
    .byte $fb,$fa,$f9,$f8,$f9,$fa,$fb,$fc,$fd,$fd,$fe,$fe,$fe,$ff, 1, $80

; -----------------------------------------------------------------------------

add_hand_speed_to_position:
    ; Add hand cursor speed to position.
    ; Args:
    ;   A: speed of hand cursor (horizontal/vertical)
    ;   X: address of hand cursor position (hand_x_px/hand_y_px)
    ; Called by:
    ;   move_hand

    ; divide speed by 2, round down (toward -infinity)
    pha
    asl
    pla
    ror

    ; on even frame: 0 -> C
    ; on odd  frame: LSB of original speed -> C
    dec odd_frame_flag2
    beq odd_frame
    clc
odd_frame:

    ; add new speed and C to hand cursor position
    adc $00,x
    sta $00,x

    inc odd_frame_flag2
    rts

; -----------------------------------------------------------------------------

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
    bne input_area_check_up
    lda hand_y_letter_target
    cmp #2
    bne input_area_exit
    lda #1
    rts
input_area_check_up:
    cmp #joypad_up
    bne input_area_exit
    lda hand_y_letter_target
    cmp #1
    bne input_area_exit
    lda #1
    rts
input_area_exit:
    lda #0
    rts

; -----------------------------------------------------------------------------

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
    bne move_hand_check_left
    ; try to move right
    lda hand_x_letter_target
    cmp #7
    beq hand_move_fail
    inc hand_x_letter_target
    jmp hand_move_success

move_hand_check_left:
    cmp #joypad_left
    bne move_hand_check_down
    ; try to move left
    lda hand_x_letter_target
    beq hand_move_fail
    dec hand_x_letter_target
    jmp hand_move_success

move_hand_check_down:
    cmp #joypad_down
    bne move_hand_check_up
    ; try to move down
    lda hand_y_letter_target
    cmp #4
    beq hand_move_fail
    inc hand_y_letter_target
    jmp hand_move_success

move_hand_check_up:
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

; -----------------------------------------------------------------------------

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
    clc
    adc #4
    cmp #8
    bcc on_virtual_keyboard
    clc
    adc #1
on_virtual_keyboard:
    ; update 2*2 attribute blocks
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
    ; Called by:
    ;   (see update_attribute_byte)

    lda #%00000000
    jmp update_attribute_byte

; -----------------------------------------------------------------------------

update_hand_letter_position:
    ; Called by:
    ;   move_hand

    ; preserve original Y
    tya
    pha

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
    ; return
    pla
    tay
    rts

target_on_input_area:
    ; target letter is on input area
    ; update actual hand position
    sta hand_y_letter
    lda hand_x_letter_target
    sta hand_x_letter
    ; return
    pla
    tay
    rts

; -----------------------------------------------------------------------------

initial_sprite_attribute_data:
    ; bits: VHBUUUPP (Vflip, Hflip, Behind bg, Unimplemented, Palette);
    ; in reverse order
    .byte %00011001, %00011001, %00011001, %00011001, %00011010  ; 19...15
    .byte %00011001, %00011001, %00011001, %00011001, %00011010  ; 14...10
    .byte %00011001, %00011001, %00011001, %00011001, %00011010  ;  9... 5
    .byte %00011001, %00011001, %00011001, %00011001, %00011010  ;  4... 0

update_revolving_cursor_attributes:
    ; change attributes of revolving cursor's sprites (20-23)

    ; change subpalette from 0 to 2 or vice versa
    lda sprite_attributes+20
    cmp #%00011010  ; VHBUUUPP
    bne use_3rd_subpalette
    lda #%00011000
    jmp subpalette_changed
use_3rd_subpalette:
    lda #%00011010
subpalette_changed:

    ; put sprite behind background if phase is 0-6
    `ldx_abs revolving_phase
    cpx #7
    bcs revolving_cursor_priority_bit_adjusted
    ora #%00100000
revolving_cursor_priority_bit_adjusted:

    sta sprite_attributes+20
    sta sprite_attributes+21
    sta sprite_attributes+22
    sta sprite_attributes+23
    rts

; -----------------------------------------------------------------------------

letter_input:
    ; Button A went from off to on, with hand on virtual keyboard.

    ; graphic id to draw (see graphics_offsets)
    ; hand_y_letter * 8 + hand_x_letter + 3 -> stack
    lda hand_y_letter
    asl
    asl
    asl
    clc
    adc hand_x_letter
    adc #3
    pha

    ; revolving_y_letter1 * 4 + 18 -> Y
    ; (Y position in tiles)
    lda revolving_y_letter1
    asl
    asl
    clc
    adc #18
    tay

    ; revolving_x_letter1 * 4 + 4 -> X
    ; (X position in tiles)
    lda revolving_x_letter1
    asl
    asl
    clc
    adc #4
    tax

    pla
    pha
    jsr draw_graphic_on_background  ; A/X/Y = tile/X/Y
    jsr letter_input_extra_effects
    pla
    jsr save_entered_letter

    jmp useless_label  ; a useless jump
useless_label:

    ; increment cursor X position (may be undone later)
    inc revolving_x_letter1

    ; six letters on current line?
    lda revolving_x_letter1
    cmp #6
    bne is_line_full  ; no

    ; six letters on current line

    ; third letter on current line -> X
    lda revolving_y_letter1
    asl
    asl
    asl
    clc
    adc #3-1
    tax

    ; if A/P/Z/L/G/I/T/Y, the code is complete
    lda entered_letters,x
    sec
    sbc #3
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
    beq undo_cursor_x_increment
    lda #0
    sta revolving_x_letter1
    inc revolving_y_letter1
    jmp letter_input_end
undo_cursor_x_increment:
    dec revolving_x_letter1

letter_input_end:
    ; move the highlight to the new letter
    jmp highlight_input_area_letter  ; ends with rts

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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
    sec
    sbc #3
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
    clc
    adc #18
    tay

    ; revolving_x_letter1 * 4 + 4 -> X
    lda revolving_x_letter1
    asl
    asl
    clc
    adc #4
    tax

    jsr spawn_particles1

    lda #graphic_id_dash
    jsr draw_graphic_on_background  ; A/X/Y = tile/X/Y

    lda #0  ; no letter
    jmp save_entered_letter  ; A = letter; ends with rts

erase_letter_end:
    jmp move_revolving_cursor_backwards  ; a useless jump

; -----------------------------------------------------------------------------

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
    lda #7
    sta revolving_x_letter1
    dec revolving_y_letter1
    jmp revolving_cursor_moved_backwards
undo_movement_backwards:
    inc revolving_x_letter1
revolving_cursor_moved_backwards:
    jsr fix_revolving_cursor_x  ; move left to position after last letter

    jmp highlight_input_area_letter  ; ends with rts

; -----------------------------------------------------------------------------

move_revolving_cursor_manually:
    ; Called by:
    ;   check_button_a

    ; move revolving cursor to hand cursor
    lda hand_y_letter
    sec
    sbc #2
    sta revolving_y_letter1
    lda hand_x_letter
    sta revolving_x_letter1
    jsr fix_revolving_cursor_x

    ; push X, Y (there seems to be no reason to preserve Y)
    txa
    pha
    tya
    pha

    jsr highlight_input_area_letter

    ; pull Y, X
    pla
    tay
    pla
    tax

    rts

; -----------------------------------------------------------------------------

update_revolving_cursor:
    ; Update horizontal and vertical position, phase and attributes of the
    ; revolving cursor.
    ; Called by:
    ;   do_every_frame

    ; revolving_y_letter1 * 32 + 152 -> revolving_y_target
    lda revolving_y_letter1
    asl
    asl
    asl
    asl
    asl
    clc
    adc #152
    sta revolving_y_target

    ; revolving_x1 * 32 + 10 -> revolving_x_target
    lda revolving_x_letter1
    asl
    asl
    asl
    asl
    asl
    clc
    adc #10
    sta revolving_x_target

    ; compute horizontal speed
    lda revolving_x
    `sta_abs revolving_pos
    lda revolving_x_target
    `sta_abs revolving_target
    jsr compute_revolving_cursor_speed
    clc
    adc #128
    sta revolving_target_speed
    lda revolving_speed_x
    jsr accelerate_revolving_cursor
    sta revolving_speed_x

    ; compute vertical speed
    lda revolving_y
    `sta_abs revolving_pos
    lda revolving_y_target
    `sta_abs revolving_target
    jsr compute_revolving_cursor_speed
    clc
    adc #128
    sta revolving_target_speed
    lda revolving_speed_y
    jsr accelerate_revolving_cursor
    sta revolving_speed_y

    ; add horizontal speed to horizontal position
    lda revolving_x
    clc
    adc revolving_speed_x
    sta revolving_x

    ; add vertical speed to vertical position
    lda revolving_y
    clc
    adc revolving_speed_y
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
    jsr update_metasprite

    ; update attributes of sprites
    jmp update_revolving_cursor_attributes  ; ends with rts

; -----------------------------------------------------------------------------

revolving_cursor_x_offsets:
    ; Sine wave in two's complement.
    ; Values: 17 (16 accessed).
    ; Amplitude: 10.
    ; Read by:
    ;   update_revolving_cursor

    .byte 0, 4, 7, 9, 10, 9, 7, 4
    .byte 0, 256-4, 256-7, 256-9, 256-10, 256-9, 256-7, 256-4
    .byte 0  ; unaccessed

revolving_cursor_y_offsets:
    ; Inverted cosine wave in two's complement.
    ; Values: 17 (16 accessed).
    ; Amplitude: 10.
    ; Read by:
    ;   update_revolving_cursor

    .byte 256-10, 256-9, 256-7, 256-4, 0, 4, 7, 9
    .byte 10, 9, 7, 4, 0, 256-4, 256-7, 256-9
    .byte 256-10  ; unaccessed

; -----------------------------------------------------------------------------

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
    sec
    sbc revolving_pos
    ldx #3
shift_negative_right_loop:
    sec
    ror
    dex
    bne shift_negative_right_loop
    rts
too_small:
    lda revolving_target
    sec
    sbc revolving_pos
    ldx #3
shift_positive_right_loop:
    lsr
    dex
    bne shift_positive_right_loop
    clc
    adc #1
    rts

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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
    lda #0
    sta particle_set_flag
    ldx #48
    jsr spawn_particles3
    lda #1
    sta particle_set2_timer
    rts

spawn_set0:
    ; flip flag, spawn set #0
    lda #1
    sta particle_set_flag
    ldx #32
    jsr spawn_particles3
    lda #1
    sta particle_set1_timer
    rts

; -----------------------------------------------------------------------------

initial_particle_speeds_x:
    ; Two sine waves in two's complement. Values: 8/wave.
    ; Read by:
    ;   spawn_particles3

    .byte 0, 6, 8, 6, 0, 256-6, 256-8, 256-6  ; outer ring (amplitude 8)
    .byte 0, 3, 4, 3, 0, 256-3, 256-4, 256-3  ; inner ring (amplitude 4)

initial_particle_speeds_y:
    ; Two inverted cosine waves in two's complement. Values: 8/wave.
    ; Read by:
    ;   spawn_particles3

    .byte 256-8, 256-6, 0, 6, 8, 6, 0, 256-6  ; outer ring (amplitude 8)
    .byte 256-4, 256-3, 0, 3, 4, 3, 0, 256-3  ; inner ring (amplitude 4)

; -----------------------------------------------------------------------------

spawn_particles3:
    ; Write initial data of particles.
    ; Args:
    ;   X: first position in sprite data
    ; Called by:
    ;   spawn_particles2

    ldy #0  ; particle index
particle_loop:
    ; X position
    lda initial_particle_speeds_x,y
    clc
    adc particle_start_x
    sta sprite_x_positions,x

    ; Y position
    lda initial_particle_speeds_y,y
    clc
    adc particle_start_y
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
    lda #%00001110
    sta snd_noise_freq1
    lda #%00000100
    sta snd_noise_freq2
    lda #%00100101
    sta snd_noise_ctrl1

    ; set timer
    lda #24
    sta particle_time_left
    rts

; -----------------------------------------------------------------------------

move_particles:
    ; Called by:
    ;   do_every_frame

    ; stop noise if particle_time_left goes to 0
    lda particle_time_left
    beq process_particle_set1
    dec particle_time_left
    bne process_particle_set1
    lda #%00110000
    sta snd_noise_ctrl1
process_particle_set1:
    ; process 1st set of particles (from every 2nd explosion)
    lda particle_set1_timer
    beq process_particle_set2
    ldx #32
    inc particle_set1_timer
    lda particle_set1_timer
    cmp #24
    beq hide_particle_set1
    jsr move_particles2
process_particle_set2:
    ; process 2nd set of particles (from every 2nd explosion)
    lda particle_set2_timer
    beq move_particles_exit
    ldx #48
    inc particle_set2_timer
    lda particle_set2_timer
    cmp #24
    beq hide_particle_set2
    jsr move_particles2
move_particles_exit:
    rts

hide_particle_set1:
    lda #0
    sta particle_set1_timer
    jsr hide_particle_set
    jmp process_particle_set2

hide_particle_set2:
    lda #0
    sta particle_set2_timer
    jmp hide_particle_set  ; ends with rts

; -----------------------------------------------------------------------------

move_particles2:
    ; Args:
    ;   X: 32/48
    ;   A: particle_set1_timer/particle_set2_timer
    ; Called by:
    ;   move_particles

    and #%00000111
    sta particle_timer_mod8

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
    lda particle_timer_mod8
    bne particle_sprite_processed
    lda particle_speeds_x,x
    jsr decrement_absolute_value
    sta particle_speeds_x,x
    lda particle_speeds_y,x
    jsr decrement_absolute_value
    sta particle_speeds_y,x

particle_sprite_processed:
    inx
    dey
    bne particle_sprite_loop
    rts

; -----------------------------------------------------------------------------

decrement_absolute_value:
    ; If the number is nonzero, decrement its absolute value.
    ; Args:
    ;   A
    ; Out:
    ;   A
    ; Called by:
    ;   move_particles2

    beq decrement_absolute_done
    bpl decrement
    clc
    adc #1
    rts
decrement:
    sec
    sbc #1
decrement_absolute_done:
    rts

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

spawn_particles1:
    ; If there is a letter at revolving cursor, continue to prepare particles.
    ; Args:
    ;   X: horizontal position of letter, in tiles
    ;   Y: vertical position of letter, in tiles
    ; Called by:
    ;   check_button_b

    ; push Y
    tya
    pha

    ; Y * 8 + 10 -> particle_start_y
    asl
    asl
    asl
    clc
    adc #10
    sta particle_start_y

    ; push X
    txa
    pha

    ; (X - 4) * 8 + 13 -> particle_start_x
    sec
    sbc #4
    asl
    asl
    asl
    clc
    adc #13
    sta particle_start_x

    ; continue spawning only if revolving cursor is on a letter
    ; (why not check this earlier?)
    jsr get_letter_at_revolving_cursor  ; letter -> A, position -> X
    beq spawn_particles1_end
    jsr spawn_particles2

spawn_particles1_end:
    ; pull X, Y
    pla
    tax
    pla
    tay
    rts

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

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

; -----------------------------------------------------------------------------

get_revolving_cursor_position:
    ; Out:
    ;   X: cursor position (0...23)
    ; Called by:
    ;   save_entered_letter
    ;   fix_revolving_cursor_x
    ;   get_letter_at_revolving_cursor

    ; revolving_y_letter1 * 8 + revolving_x_letter1
    lda revolving_y_letter1
    asl
    asl
    asl
    clc
    adc revolving_x_letter1
    tax
    rts

; -----------------------------------------------------------------------------

letter_input_extra_effects:
    ; Spawn flying letter and play sound (after drawing a letter).
    ; Called by:
    ;   letter_input

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
    clc
    adc #64
    sta flying_y
    sta metasprite_y

    ; hand_y_letter * 8 + hand_x_letter -> entered_letter
    ; (for playing sound)
    pla
    clc
    adc hand_x_letter
    `sta_abs entered_letter

    ; spawn flying letter
    ; graphic id (see graphics_offsets):
    ;   hand_y_letter * 8 + hand_x_letter + 3
    adc #graphic_id_a
    ldx flying_metasprite
    jsr set_up_metasprite

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
    clc
    adc #144
    sec
    sbc flying_y
    lsr
    lsr
    lsr
    lsr
    sta flying_y_speed

    ; compute X speed of flying letter (-14...+14 in increments of 2):
    ; (revolving_x_letter1 - hand_x_letter) * 2 -> flying_x_speed
    lda revolving_x_letter1
    sec
    sbc hand_x_letter
    asl
    sta flying_x_speed

    lda #16
    sta flying_time_left1

    ; play sound depending on letter

    ; bits of entered_letter: 0000abcd
    ; 000000ab -> A, cd000000 -> sound_temp
    `lda_abs entered_letter
    asl
    asl
    asl
    asl
    sta sound_temp
    lda #$00
    asl sound_temp
    rol
    asl sound_temp
    rol

    ; A + 2      -> snd_pulse1_ct
    ; sound_temp -> snd_pulse1_ft
    ; %00100100  -> snd_pulse1_ctrl
    ; %11111001  -> snd_pulse1_ramp_ctrl
    clc
    adc #2
    sta snd_pulse1_ct
    lda sound_temp
    sta snd_pulse1_ft
    lda #%00100100
    sta snd_pulse1_ctrl
    lda #%11111001
    sta snd_pulse1_ramp_ctrl

    lda #20
    `sta_abs flying_time_left2

    rts

; -----------------------------------------------------------------------------

move_flying_letter:
    ; Called by:
    ;   do_every_frame

    lda flying_time_left2
    beq move_flying_letter_bra1
    dec flying_time_left2
    bne move_flying_letter_bra1
    ; flying_time_left2 went from 1 to 0
    lda #%00110000
    sta snd_pulse1_ctrl
move_flying_letter_bra1:
    lda flying_time_left1
    bne move_flying_letter_bra2
    rts
move_flying_letter_bra2:
    dec flying_time_left1
    bne move_flying_letter_bra3
    ; flying_time_left went from 1 to 0
    ldx flying_metasprite
    lda #255
    sta metasprite_y
    jmp update_metasprite  ; X = metasprite index; ends with rts
move_flying_letter_bra3:
    ; update flying cursor X position
    lda flying_x
    clc
    adc flying_x_speed
    sta flying_x
    sta metasprite_x
    ; update flying cursor Y position
    lda flying_y
    clc
    adc flying_y_speed
    sta flying_y
    sta metasprite_y

    ldx flying_metasprite
    jmp update_metasprite  ; ends with rts

; -----------------------------------------------------------------------------

fill_attribute_table_rows:
    ; Fill attribute block rows.
    ; Args:
    ;   Y: first row
    ;   X: number of rows
    ;   A: fill byte
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
    clc
    adc #1
    cmp #16
    bne fill_attribute_table_row_loop
    ; next row
    inc vram_block_y
    dec rows_left
    bne fill_attribute_table_rows_loop
    rts

; -----------------------------------------------------------------------------

animate_color:
    ; Animate color 3 of background subpalette 1.
    ; Called by:
    ;   do_every_frame

    ; increment anim_color_delay every frame
    ; increment anim_color_phase every 5th frame
    ldx anim_color_phase
    ldy anim_color_delay
    iny
    cpy #5
    bne color_change_delay_incremented
    ldy #0
    inx
color_change_delay_incremented:
    sty anim_color_delay
    cpx #8
    bne color_phase_incremented
    ldx #0
color_phase_incremented:
    stx anim_color_phase

    ; set up VRAM block to update
    lda animated_colors,x
    sta vram_block+3        ; data
    lda #1
    sta vram_block+0        ; data size
    lda #>[ppu_palettes+7]
    sta vram_block+1        ; address high
    lda #<[ppu_palettes+7]
    sta vram_block+2        ; address low

    ; copy block to buffer
    jmp vram_block_to_buffer  ; ends with rts

animated_colors:
    ; the phases of the animated color
    .byte color_animated0, color_animated1, color_animated2, color_animated3
    .byte color_animated4, color_animated5, color_animated6, color_animated7

; -----------------------------------------------------------------------------

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
    jsr update_attribute_byte

    ; update Y position
    lda revolving_y_letter1
    clc
    adc #2
    sta revolving_y_letter2

    ; update X position
    lda revolving_x_letter1
    sta revolving_x_letter2

    jsr highlight_input_area_row

    ; set attribute %01 to the letter the revolving cursor enters
    ldx revolving_x_letter2
    ldy revolving_y_letter2
    lda #%01010101
    jmp update_attribute_byte  ; ends with rts

; -----------------------------------------------------------------------------

input_area_row_attributes:
    ; Attribute table data to write when entering a row on input area
    ;   (16 bytes).
    ; Read by:
    ;   highlight_input_area_row

    .byte %10101111, %10101111, %10101111, %10101111
    .byte %10101111, %10101111, %10101111, %10101111
    .byte %11111010, %11111010, %11111010, %11111010
    .byte %11111010, %11111010, %11111010, %11111010

; -----------------------------------------------------------------------------

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

    ; address: ppu_attribute_table + 4 * 8
    lda #>[ppu_attribute_table+4*8]
    sta vram_block+1
    lda #<[ppu_attribute_table+4*8]
    sta vram_block+2
    ; data: 32 bytes, all %11111111
    ldy #4*8-1
data_setup_loop:
    lda #%11111111
    sta vram_block+3,y  ; vram_block+3 = start of data
    sta $0400+32,y
    dey
    bpl data_setup_loop
    ; data size
    lda #32
    sta vram_block+0
    ; copy
    jsr vram_block_to_buffer

    ; set up VRAM block to change attribute data of active row to %10 (white)

    ; address: ppu_attribute_table + (2 + revolving_y_letter2) * 8
    lda revolving_y_letter2
    sec
    sbc #2
    asl
    asl
    asl
    clc
    adc #<[ppu_attribute_table+4*8]
    sta vram_block+2
    lda #>[ppu_attribute_table+4*8]
    sta vram_block+1
    ; data: 16 bytes from input_area_row_attributes
    ldy #2*8-1
data_setup_loop2:
    lda input_area_row_attributes,y
    sta vram_block+3,y
    dey
    bpl data_setup_loop2
    ; data size
    lda #16
    sta vram_block+0
    ; copy
    jsr vram_block_to_buffer

highlight_input_area_row_exit:
    rts

; -----------------------------------------------------------------------------

check_select_and_start:
    ; Called by:
    ;   do_every_frame

    ; If neither pressed, allow them next time and return.
    ; If either pressed but not allowed, just return.
    ; If either pressed and allowed, decode entered codes and start game.
    lda joypad1_status
    and #joypad_select|joypad_start
    bne select_or_start_pressed
    lda #1
    sta allow_select_start
check_select_and_start_exit:
    rts
select_or_start_pressed:
    lda allow_select_start
    beq check_select_and_start_exit

    ; disable rendering
    lda #%00000000
    sta ppu_ctrl
    sta ppu_mask

    ; pointer to entered_letters
    lda #<entered_letters
    sta code_pointer
    lda #>entered_letters
    sta code_pointer+1

    ; pointer to decoded_codes
    lda #<decoded_codes
    sta decoded_codes_pointer
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
    lda #3
    sta codes_left_to_decode

    ; bitmasks
    ; %11101111/%11011111/%10111111 for 1st/2nd/3rd code, respectively
    lda #%11101111
    sta code_enable_mask
    ; %00000010/%00000100/%00001000 for 1st/2nd/3rd code, respectively
    lda #%00000010
    sta compare_enable_mask

    ; will be written to genie_master_control; bits: 0ABCabc1
    ; (A/B/C = disable code, a/b/c = compare enable);
    ; start with all codes disabled
    lda #%01110001
    sta genie_control_value

    ; start the long outer loop
all_codes_decode_loop:
    ldy #%00000000
    sty previous_letter_lsb

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
    sec
    sbc #3
    and #%00001111
    lsr
    ora previous_letter_lsb
    sta (code_pointer),y

    ; store the bit that just got shifted out, shifted to the 4th-least
    ; significant position (%00000000 or %00001000)
    lda #$00
    rol
    asl
    asl
    asl
    sta previous_letter_lsb

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

    ; regardless of code length, read 4th value and do nothing with it
eight_letter_code:
    ldy #3
    lda (code_pointer),y
    jmp valid_code_length
six_letter_code:
    ldy #3
    lda (code_pointer),y

valid_code_length:
    ; copy the bit that got shifted out from the last value, to the 4th-least
    ; significant position of the 1st value; thus, the values have been rotated
    ; instead of shifted
    ldy #0
    lda (code_pointer),y
    and #%00000111
    ora previous_letter_lsb
    sta (code_pointer),y

    ; Phase 2/2 of decoding:
    ; copy 8 nybbles from one semi-decoded code, in order specified by
    ; code_descramble_key, to 4 bytes in decoded_code.
    ldx #0             ; source nybble offset
    stx target_offset  ; target byte offset
nybbles_to_bytes_loop:
    ; value to high nybble
    ldy code_descramble_key,x
    lda (code_pointer),y
    asl
    asl
    asl
    asl
    ; value to low nybble
    inx
    ldy code_descramble_key,x
    ora (code_pointer),y
    ; store
    ldy target_offset
    sta decoded_code,y
    ; next target byte
    inc target_offset
    inx
    ; end of loop
    cpx #8
    bne nybbles_to_bytes_loop

    ; clear MSB of address
    lda decoded_code+0
    and #%01111111
    sta decoded_code+0

    ; compare address to codes stored in decoded_codes

    lda decoded_code
    ldx decoded_code+1

    ; compare address to 1st code in decoded_codes
    cmp decoded_codes+0
    bne compare_to_2nd_code  ; different, proceed
    cpx decoded_codes+1
    beq code_processed  ; same, ignore code

compare_to_2nd_code:
    ; compare address to 2nd code in decoded_codes
    cmp decoded_codes+4
    bne compare_to_3rd_code  ; different, proceed
    cpx decoded_codes+4+1
    beq code_processed  ; same, ignore code

compare_to_3rd_code:
    ; compare address to 3rd code in decoded_codes
    cmp decoded_codes+2*4
    bne accept_code  ; different, proceed
    cpx decoded_codes+2*4+1
    beq code_processed  ; same, ignore code

accept_code:
    ; The code has been decoded and validated.

    ; store the code to decoded_codes

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
    ; One code has been either accepted or rejected. Prepare for next code.

    ; change control value masks
    sec
    rol code_enable_mask
    asl compare_enable_mask

    ; advance source pointer
    lda code_pointer
    clc
    adc #8
    sta code_pointer
    bcc code_pointer_incremented
    inc code_pointer+1
code_pointer_incremented:

    ; advance target pointer
    lda decoded_codes_pointer
    clc
    adc #4
    sta decoded_codes_pointer
    bcc decoded_codes_pointer_incremented
    inc decoded_codes_pointer+1
decoded_codes_pointer_incremented:

    ; end of the long outer loop
    dec codes_left_to_decode
    beq all_codes_decoded
    jmp all_codes_decode_loop
all_codes_decoded:

    ; copy a short program from ROM to RAM
    ; (for some reason, two extra bytes are copied)
    ldx #ram_program_end-ram_program_source+1  ; bytes to copy, minus one
ram_program_copy_loop:
    lda ram_program_source,x
    sta ram_program_target,x
    dex
    bpl ram_program_copy_loop

    ; execute the program in RAM
    jmp ram_program_target

    ; a short program that is copied to RAM and executed
ram_program_source:
    ; copy decoded codes to Game Genie registers ($8001-$800c)
    ldx #3*4-1
copy_to_genie_regs_loop:
    lda decoded_codes,x
    sta genie_values,x
    dex
    bpl copy_to_genie_regs_loop
    ; tell the hardware which codes are enabled and if they use compare values,
    ; then switch to game mode
    lda genie_control_value
    sta genie_master_control
    lda #%00000000
    sta genie_master_control
    ; reset the system
    jmp (reset_vector)
ram_program_end:

code_descramble_key:
    ; how to descramble codes
    .byte 3, 5, 2, 4, 1, 0, 7, 6

; -----------------------------------------------------------------------------

graphics_offsets:
    ; Offsets to actual graphics data (see below).
    ; 2 bytes each, high byte first.
    ; The constants (graphic_id_logo etc.) are used for accessing each graphic.
    ; Read by:
    ;   set_graphics_pointer

    .wordbe graphic_unused - graphics_offsets

    .alias graphic_id_logo 1
    .wordbe graphic_logo - graphics_offsets

    .alias graphic_id_revolv 2
    .wordbe graphic_revolv - graphics_offsets

    .alias graphic_id_a 3
    .wordbe graphic_a - graphics_offsets

    .alias graphic_id_e 4
    .wordbe graphic_e - graphics_offsets

    .alias graphic_id_p 5
    .wordbe graphic_p - graphics_offsets

    .alias graphic_id_o 6
    .wordbe graphic_o - graphics_offsets

    .alias graphic_id_z 7
    .wordbe graphic_z - graphics_offsets

    .alias graphic_id_x 8
    .wordbe graphic_x - graphics_offsets

    .alias graphic_id_l 9
    .wordbe graphic_l - graphics_offsets

    .alias graphic_id_u 10
    .wordbe graphic_u - graphics_offsets

    .alias graphic_id_g 11
    .wordbe graphic_g - graphics_offsets

    .alias graphic_id_k 12
    .wordbe graphic_k - graphics_offsets

    .alias graphic_id_i 13
    .wordbe graphic_i - graphics_offsets

    .alias graphic_id_s 14
    .wordbe graphic_s - graphics_offsets

    .alias graphic_id_t 15
    .wordbe graphic_t - graphics_offsets

    .alias graphic_id_v 16
    .wordbe graphic_v - graphics_offsets

    .alias graphic_id_y 17
    .wordbe graphic_y - graphics_offsets

    .alias graphic_id_n 18
    .wordbe graphic_n - graphics_offsets

    .alias graphic_id_dash 19
    .wordbe graphic_dash - graphics_offsets

    .alias graphic_id_hand 20
    .wordbe graphic_hand - graphics_offsets

; -----------------------------------------------------------------------------

; The actual graphics data.
; Read indirectly via graphics_pointer.
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

graphic_unused:  ; unaccessed (and an invalid graphic)
    .byte 8, 2
    .byte %00000001, %00100011, %01000101, %01100111

graphic_logo:  ; the "Game Genie" logo
    .byte 30, 4
    ; line 0
    .byte %00101100, %11000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000010, %11001100, %00000000
    .byte %00000000, %00000000, %00001000, %00000000, %00000000
    ; line 1
    .byte %01010010, %00110010, %11001100, %01011101, %10010110
    .byte %00101100, %11100100, %00000101, %00100011, %00101100
    .byte %11100100, %11011100, %01101010, %00101100, %11100100
    ; line 2
    .byte %01100000, %00101010, %00000000, %01010101, %01011010
    .byte %10100000, %01000000, %00000110, %00000010, %10100000
    .byte %01000000, %01010000, %10101010, %10100000, %01000000
    ; line 3
    .byte %00001100, %01000000, %11000000, %01000100, %01001000
    .byte %00001100, %11000100, %00000000, %11000100, %00001100
    .byte %11000100, %01000000, %10001000, %00001100, %11000100

graphic_revolv:  ; the revolving cursor
    .byte 2, 2
    .byte %10110001  ; line
    .byte %10000000  ; line

graphic_a:  ; the letter A
    .byte 4, 4
    .byte %00001010, %00000000  ; line 0
    .byte %00000101, %01010000  ; line 1
    .byte %10101100, %11100000  ; line 2
    .byte %11000100, %11000100  ; line 3

graphic_e:  ; the letter E
    .byte 4, 4
    .byte %10001101, %11100000  ; line 0
    .byte %00000111, %00010000  ; line 1
    .byte %00000101, %00100000  ; line 2
    .byte %10001100, %11000000  ; line 3

graphic_p:  ; the letter P
    .byte 4, 4
    .byte %10001101, %01100000  ; line 0
    .byte %00000111, %10010000  ; line 1
    .byte %00000101, %00000000  ; line 2
    .byte %10001100, %00000000  ; line 3

graphic_o:  ; the letter O
    .byte 4, 4
    .byte %00001001, %01100000  ; line 0
    .byte %10100000, %00000101  ; line 1
    .byte %10000001, %00100100  ; line 2
    .byte %00001000, %01000000  ; line 3

graphic_z:  ; the letter Z
    .byte 4, 4
    .byte %10101100, %11100000  ; line 0
    .byte %00000010, %01000000  ; line 1
    .byte %00100100, %00100000  ; line 2
    .byte %10001100, %11000000  ; line 3

graphic_x:  ; the letter X
    .byte 4, 4
    .byte %11100100, %11100100  ; line 0
    .byte %00000110, %01000000  ; line 1
    .byte %00100100, %01100000  ; line 2
    .byte %11000100, %11000100  ; line 3

graphic_l:  ; the letter L
    .byte 4, 4
    .byte %10001101, %00000000  ; line 0
    .byte %00000101, %00000000  ; line 1
    .byte %00000101, %00100000  ; line 2
    .byte %10001100, %11000000  ; line 3

graphic_u:  ; the letter U
    .byte 4, 4
    .byte %11100100, %11100100  ; line 0
    .byte %10100000, %10100000  ; line 1
    .byte %10100000, %10100000  ; line 2
    .byte %00001100, %01000000  ; line 3

graphic_g:  ; the letter G
    .byte 4, 4
    .byte %00001001, %11000101  ; line 0
    .byte %10100000, %00110001  ; line 1
    .byte %10000001, %00000101  ; line 2
    .byte %00001000, %11000000  ; line 3

graphic_k:  ; the letter K
    .byte 4, 4
    .byte %10001101, %10100100  ; line 0
    .byte %00000111, %01000000  ; line 1
    .byte %00000101, %01100000  ; line 2
    .byte %10001100, %10000100  ; line 3

graphic_i:  ; the letter I
    .byte 4, 4
    .byte %00001110, %01000000  ; line 0
    .byte %00001010, %00000000  ; line 1
    .byte %00001010, %00000000  ; line 2
    .byte %00001100, %01000000  ; line 3

graphic_s:  ; the letter S
    .byte 4, 4
    .byte %00101100, %01110000  ; line 0
    .byte %10000111, %00010000  ; line 1
    .byte %00100000, %11100000  ; line 2
    .byte %00001100, %01000000  ; line 3

graphic_t:  ; the letter T
    .byte 4, 4
    .byte %10101110, %11100000  ; line 0
    .byte %00001010, %00000000  ; line 1
    .byte %00001010, %00000000  ; line 2
    .byte %00001100, %01000000  ; line 3

graphic_v:  ; the letter V
    .byte 4, 4
    .byte %11100100, %11100100  ; line 0
    .byte %10000001, %10010000  ; line 1
    .byte %00000110, %01000000  ; line 2
    .byte %00001000, %00000000  ; line 3

graphic_y:  ; the letter Y
    .byte 4, 4
    .byte %11100100, %11100100  ; line 0
    .byte %00000110, %01000000  ; line 1
    .byte %00001010, %00000000  ; line 2
    .byte %00001100, %01000000  ; line 3

graphic_n:  ; the letter N
    .byte 4, 4
    .byte %11100000, %11100100  ; line 0
    .byte %10100110, %10100000  ; line 1
    .byte %10100000, %11100000  ; line 2
    .byte %11000100, %10000000  ; line 3

graphic_dash:  ; hyphen, "-", placeholder for input area
    .byte 4, 4
    .byte %00000000, %00000000  ; line 0
    .byte %00100011, %00110001  ; line 1
    .byte %00000000, %00000000  ; line 2
    .byte %00000000, %00000000  ; line 3

graphic_hand:  ; hand cursor (the only graphic with an odd width)
    .byte 5, 4
    .byte %00000000, %01110000, %00000011, %00111011, %01110001  ; lines 0&1
    .byte %00001110, %11111111, %01010000, %00001100, %11000100  ; lines 2&3

    ; unaccessed (same as lines 1-3 of the hand cursor)
    .byte %00000011, %00111011, %01110001
    .byte %00001110, %11111111, %01010000, %00001100, %11000100

    ; unaccessed
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %00000000
    .byte %00000000, %00000000, %00000000, %10101010

; -----------------------------------------------------------------------------

; interrupt vectors (at the end of the PRG-ROM and the CPU memory space)
    .advance $fffa
    .word nmi              ; NMI
reset_vector:
    .word initialization1  ; reset
    .word $ffff            ; IRQ (unused)

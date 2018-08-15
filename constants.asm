; constants for "prg.asm"

; Abbreviations used in constants:
;    "act"    actual (position), opposite of target (position)
;    "addr"   address
;    "anim"   animation; animated
;    "at"     Attribute Table
;    "attr"   attribute
;    "blk"    block
;    "ctrl"   control
;    "flag"   1 = yes, 0 = no
;    "fly"    flying letter (see above)
;    "gfx"    graphics
;    "hand"   hand cursor (see above)
;    "hi"     more significant byte
;    "kbd"    virtual keyboard (see above)
;    "lo"     less significant byte
;    "ltr"    letter (32*32 pixels)
;    "nt"     Name Table
;    "oam"    Object Attribute Memory (sprite RAM)
;    "off"    offset
;    "pal"    palette
;    "parti"  particle (see above)
;    "ppu"    Picture Processing Unit (the NES video chip)
;    "prev"   previous
;    "ptr"    memory pointer (2 bytes, less significant first)
;    "px"     pixel
;    "revo"   revolving cursor (see above)
;    "snd"    sound
;    "spd"    speed
;    "tbl"    table
;    "temp"   temporary variable
;    "trg"    target (opposite of actual)
;    "x"      horizontal (position)
;    "y"      vertical (position)

; --- Memory-mapped registers -------------------------------------------------

; CPU address space (from NESDev Wiki)
.alias ppu_ctrl             $2000
.alias ppu_mask             $2001
.alias ppu_status           $2002
.alias oam_addr             $2003
.alias ppu_scroll           $2005
.alias ppu_addr             $2006
.alias ppu_data             $2007
.alias snd_pulse1_ctrl      $4000
.alias snd_pulse1_ramp_ctrl $4001
.alias snd_pulse1_ft        $4002
.alias snd_pulse1_ct        $4003
.alias snd_noise_ctrl1      $400c
.alias snd_noise_freq1      $400e
.alias snd_noise_freq2      $400f
.alias oam_dma              $4014
.alias snd_clock            $4015
.alias joypad1              $4016
.alias joypad2              $4017
.alias genie_master_control $8000
.alias genie_values         $8001  ; ...$800c (12 bytes)
.alias genie_unknown1       $fff0
.alias genie_unknown2       $fff1

; --- PPU addresses -----------------------------------------------------------

.alias ppu_name_table      $2000
.alias ppu_attribute_table $23c0
.alias ppu_palettes        $3f00

; --- RAM - single bytes ------------------------------------------------------

.alias allow_select_start       $84  ; 0/1
.alias always_b00011100         $36  ; %00011100
.alias always_one               $41  ; 1
.alias always_zero1             $2e  ; 0
.alias always_zero2             $30  ; 0
.alias always_zero3             $42  ; 0
.alias anim_color_delay         $80  ; 0-4
.alias anim_color_phase         $7f  ; 0-7
.alias attribute_fill_byte      $1a
.alias attribute_temp           $04
.alias code_enable_mask         $86
.alias code_length              $85
.alias codes_left_to_decode     $88
.alias compare_enable_mask      $87
.alias entered_letter           $7d
.alias first_free_byte          $4b
.alias flying_metasprite        $7b  ; metasprite number (2)
.alias flying_time_left1        $7a
.alias flying_time_left2        $7c
.alias flying_x                 $76
.alias flying_x_speed           $78  ; signed (two's-complement, -14...+14)
.alias flying_y                 $77
.alias flying_y_speed           $79  ; 3...9
.alias genie_control_value      $89
.alias gfx_data_bit_pair        $04
.alias gfx_data_byte            $04
.alias gfx_data_bytes_left      $37
.alias gfx_height               $0e  ; in nybbles/tiles
.alias gfx_index                $04
.alias gfx_nybble               $19
.alias gfx_to_draw              $31
.alias gfx_width                $0d  ; in nybbles/tiles
.alias gfx_x                    $17
.alias gfx_x_offset             $15
.alias gfx_y                    $18
.alias gfx_y_offset             $16
.alias hand_metasprite          $4f  ; metasprite number (0)
.alias hand_x_keyboard          $60  ; last X position on keyboard (0-7)
.alias hand_x_letter            $62  ; 0-7
.alias hand_x_letter_target     $52  ; 0-7
.alias hand_x_px                $50  ; 14-238
.alias hand_x_speed_offset      $5c
.alias hand_y_keyboard          $61  ; last Y position on keyboard (0-1)
.alias hand_y_letter            $63  ; 0-4
.alias hand_y_letter_target     $53  ; 0-4
.alias hand_y_px                $51  ; 60-204
.alias hand_y_speed_offset      $5b
.alias last_x_input_accepted    $5d  ; joypad_left/joypad_right
.alias last_y_input_accepted    $5e  ; joypad_up/joypad_down
.alias joypad1_status           $07
.alias joypad2_status           $08
.alias keyboard_graphic         $4c
.alias keyboard_graphic_x       $4d
.alias keyboard_graphic_y       $4e
.alias metasprite_height        $35  ; in nybbles/tiles
.alias metasprite_to_create     $38  ; metasprite number
.alias metasprite_width         $34  ; in nybbles/tiles
.alias metasprite_x             $2d
.alias metasprite_y             $2f
.alias multiplication_temp      $27
.alias nmi_done_flag            $49
.alias nybble_offset            $40
.alias nybble_vram_high         $21
.alias nybble_vram_low          $20
.alias nybbles_left_x           $3b
.alias odd_frame_flag1          $43
.alias odd_frame_flag2          $5f
.alias particle_set1_timer      $73  ; counts up
.alias particle_set2_timer      $74  ; counts up
.alias particle_set_flag        $72  ; which particle set to spawn
.alias particle_start_x         $70
.alias particle_start_y         $71
.alias particle_time_left       $75  ; counts down
.alias particle_timer_mod8      $04
.alias ppu_ctrl_mirror          $00
.alias ppu_mask_mirror          $01
.alias prev_button_a_status     $66
.alias prev_button_b_status     $67
.alias previous_letter_lsb      $84
.alias ram_program_target       $10
.alias revolving_phase          $6d  ; phase of animation (0-15)
.alias revolving_metasprite     $54  ; metasprite number (1)
.alias revolving_pos            $6e  ; X or Y (argument for subroutine)
.alias revolving_target         $6f  ; X or Y position (argument for subroutine)
.alias revolving_target_speed   $6a  ; signed (excess-128)
.alias revolving_speed_x        $6b  ; signed (two's-complement)
.alias revolving_speed_y        $6c  ; signed (two's-complement)
.alias revolving_x_letter1      $64  ; 0-7
.alias revolving_x_letter2      $81  ; only in update_keyboard_letter_attribute_data
.alias revolving_x              $55  ; 10-234
.alias revolving_x_target       $68  ; 10-234
.alias revolving_y_letter1      $65  ; 0-2
.alias revolving_y_letter2      $82  ; 2-4 (always revolving_y_letter1 + 2?)
.alias revolving_y_letter2_prev $83
.alias revolving_y              $56  ; 152-216
.alias revolving_y_target       $69  ; 152-216 in increments of 32
.alias rows_left                $7e  ; only in fill_attribute_table_rows
.alias scroll_x_mirror          $45
.alias scroll_y_mirror          $46
.alias skip_nmi_flag            $0c
.alias sound_temp               $04
.alias target_offset            $84
.alias unused1                  $33
.alias unused2                  $4a
.alias value_to_add             $04
.alias vram_address_high        $05
.alias vram_block_cost          $28
.alias vram_block_total_size    $04
.alias vram_block_x             $1e
.alias vram_block_y             $1f
.alias vram_budget              $2c
.alias vram_buffer_read_pos1    $29  ; position of last byte read + 1
.alias vram_buffer_read_pos2    $2b  ; position of last byte read (not sure of purpose)
.alias vram_buffer_write_pos    $2a  ; position of last byte written + 1

; --- RAM - words (2 bytes each, low first) -----------------------------------

.alias code_ptr          $80
.alias decoded_codes_ptr $82
.alias gfx_ptr           $39
.alias hand_x_speed_ptr  $57
.alias hand_y_speed_ptr  $59
.alias ram_clear_ptr     $00
.alias sprite_x          $3e
.alias sprite_y          $3c

; --- RAM - blocks ------------------------------------------------------------

; current decoded Game Genie code
; 4 bytes: address high, address low, replace value, compare value
.alias decoded_code $8a  ; ...$8d

; all decoded Game Genie codes
; 16 bytes initialized, 12 bytes actually used
; 4 bytes for each code: address high, address low, compare value, replace
; value (note: different order compared to decoded_code)
.alias decoded_codes $90  ; ...$9f

; 24 bytes; 0 = no letter, 3-18 = letter
.alias entered_letters $066b  ; ...$0682

; Index to each metasprite in the metasprites table.
; Value 46 (size of the metasprites table) = no metasprite.
; 50 bytes are cleared in init3.
.alias metasprite_indexes $0563  ; ...$0594

; Metasprites, i.e. objects consisting of more than one sprite, i.e. the hand
; cursor, the revolving cursor and the flying letter.
; Data for each metasprite:
;     - 1 byte: width in sprites
;     - 1 byte: height in sprites
;     - width*height bytes: each byte = sprite index in planar sprite data
; Only modified during initialization in assign_metasprite_to_graphic.
; 46 bytes total:
;     2 + 20 (5*4) for the hand cursor
;     2 +  4 (2*2) for the revolving cursor
;     2 + 16 (4*4) for the flying letter
.alias metasprites $0595  ; ...$05c2

; horizontal and vertical speeds of particles
; signed, two's complement
; only indexes 32-63 are accessed
.alias particle_speeds_x $060b  ; indexes 32-63 = $062b...$064a
.alias particle_speeds_y $062b  ; indexes 32-63 = $064b...$066a

; sprite data in planar format (64 bytes each)
; sprites  0-19: hand cursor (5*4 sprites)
; sprites 20-23: revolving cursor (2*2 sprites)
; sprites 24-39: flying letter (4*4 sprites)
; sprites 32-47: 1st particle set (16 sprites; overlaps with the flying letter)
; sprites 48-63: 2nd particle set (16 sprites)
; Note: maybe they planned to use the residual hand graphic at the end of the
; graphics data to avoid the overlap.
.alias sprite_attributes  $0463  ; ...$04a2
.alias sprite_x_positions $04a3  ; ...$04e2
.alias sprite_y_positions $04e3  ; ...$0522
.alias sprite_tiles       $0523  ; ...$0562

; final sprite data, copied to OAM (256 bytes)
.alias interleaved_sprite_data $0200  ; ...$02ff

; contiguous block of bytes to be copied to VRAM (35 bytes)
; first 3 bytes: data size (1-32), address high, address low
; the rest: payload (1-32 bytes)
.alias vram_block $0440  ; ...$0462

; several VRAM blocks (256 bytes)
.alias vram_buffer $0300  ; ...$03ff

; --- Non-address constants ---------------------------------------------------

; joypad bitmasks
.alias joypad_a      %10000000
.alias joypad_b      %01000000
.alias joypad_select %00100000
.alias joypad_start  %00010000
.alias joypad_up     %00001000
.alias joypad_down   %00000100
.alias joypad_left   %00000010
.alias joypad_right  %00000001

; background palette colors
.alias pal_bg_bg        $0d  ; black (background)
.alias pal_bg_kbd       $2c  ; cyan
.alias pal_bg_input     $00  ; gray (input area)
.alias pal_bg_hilite    $20  ; white (highlighted items)
.alias pal_bg_anim_init $26  ; red (initial animated color, never visible)
.alias pal_bg_anim0     $21  ; sky blue
.alias pal_bg_anim1     $2c  ; cyan
.alias pal_bg_anim2     $2b  ; green
.alias pal_bg_anim3     $28  ; yellow
.alias pal_bg_anim4     $27  ; orange
.alias pal_bg_anim5     $25  ; pink 1
.alias pal_bg_anim6     $24  ; pink 2
.alias pal_bg_anim7     $2c  ; cyan
.alias pal_bg_unused    $00  ; gray

; sprite palette colors
.alias pal_spr_letter_and_revo1_and_parti1      $13  ; purple
.alias pal_spr_hand_skin                        $26  ; red
.alias pal_spr_hand_sleeve_and_revo2_and_parti2 $20  ; white
.alias pal_spr_unused1                          $00  ; gray
.alias pal_spr_unused2                          $28  ; yellow

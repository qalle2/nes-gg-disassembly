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
;    "fly"    flying letter
;    "gfx"    graphics
;    "hand"   hand cursor
;    "hi"     more significant byte
;    "kbd"    virtual keyboard
;    "lo"     less significant byte
;    "ltr"    letter (32*32 pixels)
;    "nt"     Name Table
;    "oam"    Object Attribute Memory (sprite RAM)
;    "off"    offset
;    "pal"    palette
;    "parti"  particle
;    "ppu"    Picture Processing Unit (the NES video chip)
;    "prev"   previous
;    "px"     pixel
;    "revo"   revolving cursor
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
.alias nmi_done                 $49
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
.alias revolving_x_letter2      $81
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
.alias skip_nmi                 $0c
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
.alias vram_buffer_free_bytes   $2b
.alias vram_buffer_next_read    $29
.alias vram_buffer_next_write   $2a

; --- RAM - words (2 bytes each, low first) -----------------------------------

.alias code_pointer          $80
.alias decoded_codes_pointer $82
.alias graphics_pointer      $39
.alias hand_x_speed_pointer  $57
.alias hand_y_speed_pointer  $59
.alias ram_clear_pointer     $00
.alias sprite_x              $3e
.alias sprite_y              $3c

; --- RAM - blocks - graphics -------------------------------------------------

; interleaved sprite data
; 256 bytes
; copied to OAM
.alias interleaved_sprite_data $0200  ; ...$02ff

; sprite data in planar format
; each table is 64 bytes
; sprite indexes:
;    0-19: hand cursor (5*4 sprites)
;   20-23: revolving cursor (2*2 sprites)
;   24-39: flying letter (4*4 sprites)
;   32-47: 1st particle set (16 sprites; overlaps with the flying letter)
;   48-63: 2nd particle set (16 sprites)
.alias sprite_attributes  $0463  ; ...$04a2
.alias sprite_x_positions $04a3  ; ...$04e2
.alias sprite_y_positions $04e3  ; ...$0522
.alias sprite_tiles       $0523  ; ...$0562

; metasprites (objects consisting of more than one hardware sprite)
; i.e., the hand cursor, the revolving cursor and the flying letter
; only modified during initialization
; for each metasprite:
;     - 1 byte: width (in hardware sprites)
;     - 1 byte: height (in hardware sprites)
;     - width*height bytes: indexes to individual sprites in planar sprite data
; 46 bytes total:
;     - 2 + 5*4 for the hand cursor
;     - 2 + 2*2 for the revolving cursor
;     - 2 + 4*4 for the flying letter
.alias metasprites $0595  ; ...$05c2

; index to each metasprite in the metasprites table
; size: 50 bytes
.alias metasprite_indexes $0563  ; ...$0594

; horizontal and vertical speeds of flying particles
; signed, two's complement
; only indexes 32-63 are accessed
.alias particle_speeds_x $060b  ; indexes 32-63 = $062b...$064a
.alias particle_speeds_y $062b  ; indexes 32-63 = $064b...$066a

; a block of bytes to be copied to VRAM
; total size: 35 bytes
; structure:
;   - 1 byte: data size (1-32)
;   - 1 byte: address high
;   - 1 byte: address low
;   - 1-32 bytes: payload
.alias vram_block $0440  ; ...$0462

; several VRAM blocks to be copied to VRAM
; size: 256 bytes
.alias vram_buffer $0300  ; ...$03ff

; --- RAM - blocks - other ----------------------------------------------------

; letters entered by the user
; 24 bytes
; 0 = no letter
; 3-18 = letter AEPOZXLU GKISTVYN, respectively
.alias entered_letters $066b  ; ...$0682

; current decoded Game Genie code
; 4 bytes: address high, address low, replace value, compare value
.alias decoded_code $8a  ; ...$8d

; all decoded Game Genie codes
; 16 bytes initialized, 12 bytes actually used
; 4 bytes for each code:
; address high, address low, compare value, replace value
; (note: the order is different from decoded_code)
.alias decoded_codes $90  ; ...$9f

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

; color names
.alias gray     $00
.alias black    $0d  ; causes problems with some TVs
.alias purple   $13
.alias white    $20
.alias sky_blue $21
.alias pink1    $24
.alias pink2    $25
.alias red      $26
.alias orange   $27
.alias yellow   $28
.alias green    $2b
.alias cyan     $2c

; colors
.alias color_animated0                   sky_blue
.alias color_animated1                   cyan
.alias color_animated2                   green
.alias color_animated3                   yellow
.alias color_animated4                   orange
.alias color_animated5                   pink2
.alias color_animated6                   pink1
.alias color_animated7                   cyan
.alias color_animated_initial            red  ; never seen
.alias color_background                  black
.alias color_hand1                       red
.alias color_hand2_revolving2_particle2  white
.alias color_highlight                   white
.alias color_input_area                  gray
.alias color_keyboard                    cyan
.alias color_letter_revolving1_particle1 purple
.alias color_unused1                     gray
.alias color_unused2                     yellow

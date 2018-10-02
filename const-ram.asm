; Main RAM address constants

; Glossary:
;    "flying":    the flying letter
;    "hand":      the hand cursor
;    "prev":      previous
;    "revolving": the revolving cursor
;    "temp":      temporary

; Each variable takes one byte unless otherwise mentioned.

.alias ram_clear_pointer        $00    ; 2 bytes (overlaps)
.alias ppu_ctrl_mirror          $00
.alias ppu_mask_mirror          $01
.alias temp1                    $04    ; a temporary variable, many uses
.alias vram_address_high        $05
.alias joypad1_status           $07
.alias joypad2_status           $08
.alias skip_nmi                 $0c
.alias graphic_width            $0d    ; in nybbles/tiles
.alias graphic_height           $0e    ; in nybbles/tiles
.alias ram_program_target       $10
.alias graphic_x_offset         $15
.alias graphic_y_offset         $16
.alias graphic_x                $17
.alias graphic_y                $18
.alias graphic_nybble           $19
.alias attribute_fill_byte      $1a
.alias vram_block_x             $1e
.alias vram_block_y             $1f
.alias nybble_vram_low          $20
.alias nybble_vram_high         $21
.alias multiplication_temp      $27
.alias vram_block_cost          $28
.alias vram_buffer_next_read    $29
.alias vram_buffer_next_write   $2a
.alias vram_buffer_free_bytes   $2b
.alias vram_budget              $2c
.alias metasprite_x             $2d
.alias always_zero1             $2e    ; 0
.alias metasprite_y             $2f
.alias always_zero2             $30    ; 0
.alias graphic_id               $31
.alias unused1                  $33
.alias metasprite_width         $34    ; in nybbles/tiles
.alias metasprite_height        $35    ; in nybbles/tiles
.alias always_b00011100         $36    ; %00011100
.alias graphic_data_left        $37
.alias metasprite_to_create     $38    ; metasprite number
.alias graphics_pointer         $39    ; 2 bytes
.alias nybbles_left_x           $3b
.alias sprite_y                 $3c    ; 2 bytes
.alias sprite_x                 $3e    ; 2 bytes
.alias nybble_offset            $40
.alias always_one               $41    ; 1
.alias always_zero3             $42    ; 0
.alias odd_frame_flag1          $43
.alias scroll_x_mirror          $45
.alias scroll_y_mirror          $46
.alias nmi_done                 $49
.alias unused2                  $4a
.alias first_free_byte          $4b
.alias keyboard_graphic         $4c
.alias keyboard_graphic_x       $4d
.alias keyboard_graphic_y       $4e
.alias hand_metasprite          $4f    ; metasprite number (0)
.alias hand_x_pixel             $50    ; 14-238
.alias hand_y_pixel             $51    ; 60-204
.alias hand_x_letter_target     $52    ; 0-7
.alias hand_y_letter_target     $53    ; 0-4
.alias revolving_metasprite     $54    ; metasprite number (1)
.alias revolving_x              $55    ; 10-234
.alias revolving_y              $56    ; 152-216
.alias hand_x_speed_pointer     $57    ; 2 bytes
.alias hand_y_speed_pointer     $59    ; 2 bytes
.alias hand_y_speed_offset      $5b
.alias hand_x_speed_offset      $5c
.alias last_x_input_accepted    $5d    ; joypad_left/joypad_right
.alias last_y_input_accepted    $5e    ; joypad_up/joypad_down
.alias odd_frame_flag2          $5f
.alias hand_x_keyboard          $60    ; last X position on keyboard (0-7)
.alias hand_y_keyboard          $61    ; last Y position on keyboard (0-1)
.alias hand_x_letter            $62    ; 0-7
.alias hand_y_letter            $63    ; 0-4
.alias revolving_x_letter1      $64    ; 0-7
.alias revolving_y_letter1      $65    ; 0-2
.alias prev_button_a_status     $66
.alias prev_button_b_status     $67
.alias revolving_x_target       $68    ; 10-234
.alias revolving_y_target       $69    ; 152-216 in increments of 32
.alias revolving_target_speed   $6a    ; signed (excess-128)
.alias revolving_speed_x        $6b    ; signed (two's-complement)
.alias revolving_speed_y        $6c    ; signed (two's-complement)
.alias revolving_phase          $6d    ; phase of animation (0-15)
.alias revolving_pos            $6e    ; X or Y (arg for subroutine)
.alias revolving_target         $6f    ; X or Y position (arg for subroutine)
.alias particle_start_x         $70
.alias particle_start_y         $71
.alias particle_set_flag        $72    ; which particle set to spawn
.alias particle_set1_timer      $73    ; counts up
.alias particle_set2_timer      $74    ; counts up
.alias particle_time_left       $75    ; counts down
.alias flying_x                 $76
.alias flying_y                 $77
.alias flying_x_speed           $78    ; signed (two's-complement, -14...+14)
.alias flying_y_speed           $79    ; 3...9
.alias flying_time_left1        $7a
.alias flying_metasprite        $7b    ; metasprite number (2)
.alias flying_time_left2        $7c
.alias entered_letter           $7d
.alias rows_left                $7e    ; only in fill_attribute_table_rows
.alias animated_color_phase     $7f    ; 0-7
.alias code_pointer             $80    ; 2 bytes (overlaps)
.alias animated_color_delay     $80    ; 0-4
.alias revolving_x_letter2      $81
.alias decoded_codes_pointer    $82    ; 2 bytes (overlaps)
.alias revolving_y_letter2      $82    ; 2-4 (always revolving_y_letter1 + 2?)
.alias revolving_y_letter2_prev $83
.alias temp2                    $84    ; a temporary variable, many uses
.alias code_length              $85
.alias code_enable_mask         $86
.alias compare_enable_mask      $87
.alias codes_left_to_decode     $88
.alias genie_control_value      $89
.alias decoded_code             $8a    ; 4 bytes (see below)
.alias decoded_codes            $90    ; 16 bytes (see below)
.alias interleaved_sprite_data  $0200  ; 256 bytes (see below)
.alias vram_buffer              $0300  ; 256 bytes (see below)
.alias vram_block               $0440  ; 35 bytes (see below)
.alias sprite_attributes        $0463  ; 64 bytes (see below)
.alias sprite_x_positions       $04a3  ; 64 bytes (see below)
.alias sprite_y_positions       $04e3  ; 64 bytes (see below)
.alias sprite_tiles             $0523  ; 64 bytes (see below)
.alias metasprite_indexes       $0563  ; 50 bytes (see below)
.alias metasprites              $0595  ; 46 bytes (see below)
.alias particle_speeds_x        $060b  ; 64 bytes (see below)
.alias particle_speeds_y        $062b  ; 64 bytes (see below)
.alias entered_letters          $066b  ; 24 bytes (see below)

; decoded_code:
;   current decoded Game Genie code
;   bytes: address high, address low, replace value, compare value

; decoded_codes:
;   all decoded Game Genie codes
;   16 bytes initialized, 12 bytes actually used
;   bytes for each code: address hi, address lo, compare value, replace value
;   (note: the order is different from decoded_code)

; interleaved_sprite_data:
;   copied to OAM

; vram_buffer:
;   several VRAM blocks to be copied to VRAM

; vram_block:
;   a block of bytes to be copied to VRAM
;   structure:
;       1 byte: payload size (1-32)
;       1 byte: address high
;       1 byte: address low
;       1-32 bytes: payload

; sprite_attributes, sprite_x_positions, sprite_y_positions, sprite_tiles:
;   sprite data in planar format
;   indexes in each table:
;        0-19: hand cursor (5*4 sprites)
;       20-23: revolving cursor (2*2 sprites)
;       24-39: flying letter (4*4 sprites)
;       32-47: 1st particle set (16 sprites; overlaps with the flying letter)
;       48-63: 2nd particle set (16 sprites)

; metasprite_indexes:
;   index to each metasprite in metasprites

; metasprites:
;   objects consisting of more than one hardware sprite
;   i.e., the hand cursor, the revolving cursor and the flying letter
;   only modified during initialization
;   for each metasprite:
;       1 byte: width (in hardware sprites)
;       1 byte: height (in hardware sprites)
;       width*height bytes: indexes to individual sprites in planar sprite data
;   structure:
;       2 + 5*4 bytes for the hand cursor
;       2 + 2*2 bytes for the revolving cursor
;       2 + 4*4 bytes for the flying letter

; particle_speeds_x, particle_speeds_y:
;   horizontal and vertical speeds of flying particles
;   signed, two's complement
;   only indexes 32-63 are accessed

; entered_letters:
;   letters entered by the user
;   0 = no letter
;   3-18 = letter AEPOZXLU GKISTVYN, respectively

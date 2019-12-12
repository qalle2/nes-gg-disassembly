; Memory-mapped registers

; CPU
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

; Game Genie
genie_master_control equ $8000
genie_values         equ $8001  ; ...$800c (12 bytes)
genie_unknown1       equ $fff0
genie_unknown2       equ $fff1

; --------------------------------------------------------------------------------------------------
; VRAM

ppu_name_table      equ $2000
ppu_attribute_table equ $23c0
ppu_palettes        equ $3f00

; --------------------------------------------------------------------------------------------------
; RAM

; Glossary:
;    "flying":    the flying letter
;    "hand":      the hand cursor
;    "prev":      previous
;    "revolving": the revolving cursor
;    "temp":      temporary

; Each variable takes one byte unless otherwise mentioned.

ram_clear_pointer        equ $00  ; 2 bytes (overlaps)
ppu_ctrl_mirror          equ $00
ppu_mask_mirror          equ $01
temp1                    equ $04  ; a temporary variable, many uses
vram_address_high        equ $05
joypad1_status           equ $07
joypad2_status           equ $08
skip_nmi                 equ $0c
graphic_width            equ $0d  ; in nybbles/tiles
graphic_height           equ $0e  ; in nybbles/tiles
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
always_zero1             equ $2e  ; 0
metasprite_y             equ $2f
always_zero2             equ $30  ; 0
graphic_id               equ $31
unused1                  equ $33
metasprite_width         equ $34  ; in nybbles/tiles
metasprite_height        equ $35  ; in nybbles/tiles
always_b00011100         equ $36  ; %00011100
graphic_data_left        equ $37
metasprite_to_create     equ $38  ; metasprite number
graphics_pointer         equ $39  ; 2 bytes
nybbles_left_x           equ $3b
sprite_y                 equ $3c  ; 2 bytes
sprite_x                 equ $3e  ; 2 bytes
nybble_offset            equ $40
always_one               equ $41  ; 1
always_zero3             equ $42  ; 0
odd_frame_flag1          equ $43
scroll_x_mirror          equ $45
scroll_y_mirror          equ $46
nmi_done                 equ $49
unused2                  equ $4a
first_free_byte          equ $4b
keyboard_graphic         equ $4c
keyboard_graphic_x       equ $4d
keyboard_graphic_y       equ $4e
hand_metasprite          equ $4f  ; metasprite number (0)
hand_x_pixel             equ $50  ; 14-238
hand_y_pixel             equ $51  ; 60-204
hand_x_letter_target     equ $52  ; 0-7
hand_y_letter_target     equ $53  ; 0-4
revolving_metasprite     equ $54  ; metasprite number (1)
revolving_x              equ $55  ; 10-234
revolving_y              equ $56  ; 152-216
hand_x_speed_pointer     equ $57  ; 2 bytes
hand_y_speed_pointer     equ $59  ; 2 bytes
hand_y_speed_offset      equ $5b
hand_x_speed_offset      equ $5c
last_x_input_accepted    equ $5d  ; joypad_left/joypad_right
last_y_input_accepted    equ $5e  ; joypad_up/joypad_down
odd_frame_flag2          equ $5f
hand_x_keyboard          equ $60  ; last X position on keyboard (0-7)
hand_y_keyboard          equ $61  ; last Y position on keyboard (0-1)
hand_x_letter            equ $62  ; 0-7
hand_y_letter            equ $63  ; 0-4
revolving_x_letter1      equ $64  ; 0-7
revolving_y_letter1      equ $65  ; 0-2
prev_button_a_status     equ $66
prev_button_b_status     equ $67
revolving_x_target       equ $68  ; 10-234
revolving_y_target       equ $69  ; 152-216 in increments of 32
revolving_target_speed   equ $6a  ; signed (excess-128)
revolving_speed_x        equ $6b  ; signed (two's-complement)
revolving_speed_y        equ $6c  ; signed (two's-complement)
revolving_phase          equ $6d  ; phase of animation (0-15)
revolving_pos            equ $6e  ; X or Y (arg for subroutine)
revolving_target         equ $6f  ; X or Y position (arg for subroutine)
particle_start_x         equ $70
particle_start_y         equ $71
particle_set_flag        equ $72  ; which particle set to spawn
particle_set1_timer      equ $73  ; counts up
particle_set2_timer      equ $74  ; counts up
particle_time_left       equ $75  ; counts down
flying_x                 equ $76
flying_y                 equ $77
flying_x_speed           equ $78  ; signed (two's-complement, -14...+14)
flying_y_speed           equ $79  ; 3...9
flying_time_left1        equ $7a
flying_metasprite        equ $7b  ; metasprite number (2)
flying_time_left2        equ $7c
entered_letter           equ $7d
rows_left                equ $7e  ; only in fill_attribute_table_rows
animated_color_phase     equ $7f  ; 0-7
code_pointer             equ $80  ; 2 bytes (overlaps)
animated_color_delay     equ $80  ; 0-4
revolving_x_letter2      equ $81
decoded_codes_pointer    equ $82  ; 2 bytes (overlaps)
revolving_y_letter2      equ $82  ; 2-4 (always revolving_y_letter1 + 2?)
revolving_y_letter2_prev equ $83
temp2                    equ $84  ; a temporary variable, many uses
code_length              equ $85
code_enable_mask         equ $86
compare_enable_mask      equ $87
codes_left_to_decode     equ $88
genie_control_value      equ $89
decoded_code             equ $8a  ; 4 bytes (see below)
decoded_codes            equ $90  ; 16 bytes (see below)

interleaved_sprite_data  equ $0200  ; 256 bytes (see below)
vram_buffer              equ $0300  ; 256 bytes (see below)
vram_block               equ $0440  ; 35 bytes (see below)
sprite_attributes        equ $0463  ; 64 bytes (see below)
sprite_x_positions       equ $04a3  ; 64 bytes (see below)
sprite_y_positions       equ $04e3  ; 64 bytes (see below)
sprite_tiles             equ $0523  ; 64 bytes (see below)
metasprite_indexes       equ $0563  ; 50 bytes (see below)
metasprites              equ $0595  ; 46 bytes (see below)
particle_speeds_x        equ $060b  ; 64 bytes (see below)
particle_speeds_y        equ $062b  ; 64 bytes (see below)
entered_letters          equ $066b  ; 24 bytes (see below)

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

; --------------------------------------------------------------------------------------------------
; Non-address constants

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

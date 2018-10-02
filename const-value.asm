; Non-address constants

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

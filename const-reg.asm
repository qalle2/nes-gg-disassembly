; Memory-mapped register constants

; CPU
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

; Game Genie
.alias genie_master_control $8000
.alias genie_values         $8001  ; ...$800c (12 bytes)
.alias genie_unknown1       $fff0
.alias genie_unknown2       $fff1

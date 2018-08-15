; macros for "prg.asm"

; used for overriding the automatic zero page optimization performed by Ophis
; (the Game Genie ROM needlessly accesses some zero page addresses using
; absolute addressing)

.macro dec_absolute
    .byte $ce, _1, $00
.macend

.macro lda_absolute
    .byte $ad, _1, $00
.macend

.macro ldx_absolute
    .byte $ae, _1, $00
.macend

.macro sta_absolute
    .byte $8d, _1, $00
.macend

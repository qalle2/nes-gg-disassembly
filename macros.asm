; macros for "prg.asm"

; used for overriding the automatic zero page optimization performed by Ophis
; (the Game Genie ROM needlessly accesses some zero page addresses using
; absolute addressing)

.macro dec_absolute
    .byte $ce  ; opcode
    .word _1
.macend

.macro lda_absolute
    .byte $ad  ; opcode
    .word _1
.macend

.macro ldx_absolute
    .byte $ae  ; opcode
    .word _1
.macend

.macro sta_absolute
    .byte $8d  ; opcode
    .word _1
.macend

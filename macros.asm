; Macros for "prg.asm" in alphabetical order

.macro dec_abs
    ; DEC absolute
    ; (for overriding the zero page auto-optimization performed by Ophis)
    .byte $ce  ; opcode
    .word _1   ; operand
.macend

.macro lda_abs
    ; LDA absolute
    ; (for overriding the zero page auto-optimization performed by Ophis)
    .byte $ad  ; opcode
    .word _1   ; operand
.macend

.macro ldx_abs
    ; LDX absolute
    ; (for overriding the zero page auto-optimization performed by Ophis)
    .byte $ae  ; opcode
    .word _1   ; operand
.macend

.macro sta_abs
    ; STA absolute
    ; (for overriding the zero page auto-optimization performed by Ophis)
    .byte $8d  ; opcode
    .word _1   ; operand
.macend

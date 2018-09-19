; Macros for "prg.asm" in alphabetical order

.macro add_imm
    ; add immediate to accumulator without carry
    clc
    adc #_1
.macend

.macro add_mem
    ; add memory to accumulator without carry
    clc
    adc _1
.macend

.macro asl2
    asl
    asl
.macend

.macro asl3
    asl
    asl
    asl
.macend

.macro asl4
    asl
    asl
    asl
    asl
.macend

.macro asl5
    asl
    asl
    asl
    asl
    asl
.macend

.macro copy_imm
    ; copy immediate to memory via accumulator
    ; (this macro is only used if the value is not needed afterwards)
    lda #_1
    sta _2
.macend

.macro copy_imm_to_abs
    ; copy immediate to memory (absolute) via accumulator
    ; (override the zero page auto-optimization performed by Ophis)
    ; (this macro is only used if the value is not needed afterwards)
    lda #_1
    .byte $8d  ; opcode for STA absolute
    .word _2   ; operand
.macend

.macro copy_mem
    ; copy memory to memory via accumulator
    ; (this macro is only used if the value is not needed afterwards)
    lda _1
    sta _2
.macend

.macro copy_mem_to_abs
    ; copy memory to memory (absolute) via accumulator
    ; (override the zero page auto-optimization performed by Ophis)
    ; (this macro is only used if the value is not needed afterwards)
    lda _1
    .byte $8d  ; opcode for STA absolute
    .word _2   ; operand
.macend

.macro dec_abs
    ; DEC absolute
    ; (for overriding the zero page auto-optimization performed by Ophis)
    .byte $ce  ; opcode
    .word _1   ; operand
.macend

.macro inx2
    inx
    inx
.macend

.macro iny4
    iny
    iny
    iny
    iny
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

.macro lsr4
    lsr
    lsr
    lsr
    lsr
.macend

.macro phx
    ; push X
    ; (this macro is only used if the value of A is not needed afterwards)
    txa
    pha
.macend

.macro phy
    ; push Y
    ; (this macro is only used if the value of A is not needed afterwards)
    tya
    pha
.macend

.macro plx
    ; pull X
    ; (this macro is only used if the value of A is not needed afterwards)
    pla
    tax
.macend

.macro ply
    ; pull Y
    ; (this macro is only used if the value of A is not needed afterwards)
    pla
    tay
.macend

.macro sta_abs
    ; STA absolute
    ; (for overriding the zero page auto-optimization performed by Ophis)
    .byte $8d  ; opcode
    .word _1   ; operand
.macend

.macro sub_imm
    ; subtract immediate from accumulator without borrow
    sec
    sbc #_1
.macend

.macro sub_mem
    ; subtract memory from accumulator without borrow
    sec
    sbc _1
.macend

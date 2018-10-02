; Macros (in alphabetical order)

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

.macro dec_absolute
    ; for overriding the zero page auto-optimization performed by Ophis
    .byte $ce  ; opcode for DEC absolute
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

.macro lda_absolute
    ; for overriding the zero page auto-optimization performed by Ophis
    .byte $ad  ; opcode for LDA absolute
    .word _1   ; operand
.macend

.macro ldx_absolute
    ; for overriding the zero page auto-optimization performed by Ophis
    .byte $ae  ; opcode for LDX absolute
    .word _1   ; operand
.macend

.macro lsr4
    lsr
    lsr
    lsr
    lsr
.macend

.macro sta_absolute
    ; for overriding the zero page auto-optimization performed by Ophis
    .byte $8d  ; opcode for STA absolute
    .word _1   ; operand
.macend

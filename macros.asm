macro dec_absolute address
    ; dec absolute (even for zero page operands)
    db $ce
    dw address
endm

macro lda_absolute address
    ; lda absolute (even for zero page operands)
    db $ad
    dw address
endm

macro ldx_absolute address
    ; ldx absolute (even for zero page operands)
    db $ae
    dw address
endm

macro sta_absolute address
    ; sta absolute (even for zero page operands)
    db $8d
    dw address
endm

macro dwbe word
    ; Emit a big-endian word.
    db >(word), <(word)
endm

; --------------------------------------------------------------------------------------------------
; Synthetic instructions (for clarity, these are only used when they would behave like the "real"
; ones; e.g. don't replace txa&pha with phx if the value of A is accessed later)

macro phx
    ; push X
    txa
    pha
endm

macro phy
    ; push Y
    tya
    pha
endm

macro plx
    ; pull X
    pla
    tax
endm

macro ply
    ; pull Y
    pla
    tay
endm

macro ldaxy a_value, x_value, y_value
    ; load A, X, Y
    lda a_value
    ldx x_value
    ldy y_value
endm

macro add operand
    ; add without carry
    clc
    adc operand
endm

macro sub operand
    ; subtract without carry
    sec
    sbc operand
endm

macro copy from, to
    ; copy
    lda from
    sta to
endm

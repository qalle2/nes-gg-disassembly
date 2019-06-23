; NES Game Genie disassembly by qalle

; -----------------------------------------------------------------------------
; iNES header

    ; identifier
    .byte "NES", $1a
    ; PRG ROM size (16 KiB)
    .byte 1
    ; CHR ROM size (8 KiB)
    .byte 1
    ; mapper (0, NROM), mirroring (horizontal)
    .byte %00000000, %00000000
    ; reserved
    .byte $00, $00, $00, $00, $00, $00, $00, $00

; -----------------------------------------------------------------------------
; PRG ROM

    ; The same 4096-byte PRG ROM (assembled prg.asm) is repeated four times.
    .incbin "genie.prg"
    .incbin "genie.prg"
    .incbin "genie.prg"
    .incbin "genie.prg"

; -----------------------------------------------------------------------------
; CHR ROM

    ; The same 256-byte CHR ROM is repeated 32 times.
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"
    .incbin "original.chr"

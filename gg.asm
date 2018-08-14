    ; Game Genie ROM disassembly by Kalle (http://qalle.net)
    ; Assembles with Ophis.

    ; iNES header
    .byte "NES", $1a  ; identifier
    .byte 1  ; 1*16 KiB PRG-ROM
    .byte 1  ; 1*8 KiB CHR-ROM
    .byte %00000000, %00000000  ; mapper 0, horizontal mirroring
    .byte $00, $00, $00, $00, $00, $00, $00, $00  ; reserved

    ; The same 4096-byte PRG-ROM (assembled prg.asm) is repeated four times,
    ; for a total of 16 KiB.
    .incbin "prg.bin"
    .incbin "prg.bin"
    .incbin "prg.bin"
    .incbin "prg.bin"

    ; The same 256-byte CHR-ROM (not included) is repeated 32 times,
    ; for a total of 8 KiB.
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"
    .incbin "original-chr.bin"

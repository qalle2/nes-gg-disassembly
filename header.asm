; iNES header

    .byte "NES", $1a            ; identifier
    .byte 1                     ; 1*16 KiB PRG-ROM
    .byte 1                     ; 1*8 KiB CHR-ROM
    .byte %00000000, %00000000  ; mapper 0 (NROM), horizontal mirroring
    .byte $00, $00, $00, $00    ; reserved
    .byte $00, $00, $00, $00    ; reserved

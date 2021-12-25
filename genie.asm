; NES Game Genie disassembly (ASM6)

        ; iNES header; see https://wiki.nesdev.org/w/index.php/INES
        base $0000
        db "NES", $1a            ; file id
        db 1, 1                  ; 16 KiB PRG ROM, 8 KiB CHR ROM
        db %00000000, %00000000  ; mapper 0 (NROM), horizontal mirroring
        pad $0010, $00           ; unused

        ; PRG ROM
        rept 4
        incbin "genie.prg"  ; 4 KiB each
        endr

        ; CHR ROM
        rept 32
        incbin "original.chr"  ; 256 bytes each
        endr

; NES Game Genie disassembly (ASM6)

; --- iNES header ----------------------------------------------------------------------------------

        ; https://wiki.nesdev.com/w/index.php/INES

        base $0000
        db "NES", $1a            ; file id
        db 1, 1                  ; 16 KiB PRG ROM, 8 KiB CHR ROM
        db %00000000, %00000000  ; mapper 0 (NROM), horizontal mirroring
        pad $0010, $00           ; unused

; --- PRG ROM --------------------------------------------------------------------------------------

        ; 4 * 4 KiB
        base $0000
rept 4
        incbin "genie.prg"
endr
        pad $4000

; --- CHR ROM --------------------------------------------------------------------------------------

        ; 32 * 256 bytes
        base $0000
rept 32
        incbin "original.chr"
endr
        pad $2000

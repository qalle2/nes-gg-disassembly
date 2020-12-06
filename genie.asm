; NES Game Genie disassembly (ASM6)

; --- iNES header ---------------------------------------------------------------------------------

    ; https://wiki.nesdev.com/w/index.php/INES

    base 0
    db "NES", $1a            ; file id
    db 1, 1                  ; 16 KiB PRG ROM, 8 KiB CHR ROM
    db %00000000, %00000000  ; NROM mapper, horizontal mirroring
    pad $0010, 0             ; unused

; --- PRG ROM -------------------------------------------------------------------------------------

    ; 4 KiB, repeated 4 times
rept 4
    incbin genie.prg
endr

; --- CHR ROM -------------------------------------------------------------------------------------

    ; 256 bytes, repeated 32 times
rept 32
    incbin "original.chr"
endr


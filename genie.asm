; NES Game Genie disassembly (ASM6)

                ; iNES header; see https://www.nesdev.org/wiki/INES
                base $0000
                db "NES", $1a            ; file id
                db 1, 1                  ; 16 KiB PRG ROM, 8 KiB CHR ROM
                db %00000000, %00000000  ; mapper 0 (NROM), horiz. mirroring
                pad $0010, $00           ; unused

                ; PRG ROM (4 * 4 KiB)
rept 4
                incbin "prg.bin"
endr

                ; CHR ROM (32 * 256 bytes)
rept 32
                incbin "chr.bin"
endr

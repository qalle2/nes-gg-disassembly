    ; the byte to fill unused space with
    fillvalue $ff

    ; iNES header
    inesprg 1  ; PRG ROM size: 1 * 16 KiB
    ineschr 1  ; CHR ROM size: 1 * 8 KiB
    inesmap 0  ; mapper: 0 (NROM)
    inesmir 0  ; name table mirroring: horizontal

    ; PRG ROM (4,096 bytes, repeated four times)
rept 4
    incbin genie.prg
endr

    ; CHR ROM (256 bytes, repeated 32 times)
rept 32
    .incbin "original.chr"
endr

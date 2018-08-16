; nes-gg-disassembly by qalle2 (http://github.com/qalle2/nes-gg-disassembly)

    .include "header.asm"  ; iNES header

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

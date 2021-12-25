# Warning: this script DELETES files. Run at your own risk.
rm -f prg.bin genie.nes
asm6 prg.asm prg.bin
asm6 genie.asm genie.nes
rm prg.bin
diff -q original.nes genie.nes

# Warning: this script deletes files. Run at your own risk.
rm -f prg.bin genie.nes
asm6 prg.asm prg.bin
asm6 genie.asm genie.nes
md5sum -c --quiet hashes.md5

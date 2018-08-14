@echo off
cls
rem delete original prg.bin (required to detect assembly failure;
rem Ophis does not seem to support errorlevel)
if exist prg.bin del prg.bin
rem assemble PRG-ROM to prg.bin, be verbose
ophis -v -o prg.bin prg.asm
rem exit if assembly failed
if not exist prg.bin goto error
rem compare original PRG-ROM (not included) to assembled file
fc /b original-prg.bin prg.bin
rem assemble entire file to gg.nes, be verbose
ophis -v -o gg.nes gg.asm
rem compare original file (not included) to assembled file
fc /b original.nes gg.nes
rem success
goto end
:error
echo assemble.bat: error detected!
:end

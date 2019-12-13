@echo off
choice /c NY /m "This batch file will DELETE some files. Continue"
if not errorlevel 2 goto end

cls

echo === assemble.bat: assembling prg.asm ===
rem export FCEUX-compatible .nl files
asm6f -n prg.asm genie.prg
if errorlevel 1 goto error
if exist genie.nes.ram.nl del genie.nes.ram.nl
echo.

echo === assemble.bat: comparing original.prg to genie.prg ===
if exist original.prg goto hasprg
echo Warning: original.prg not found; cannot compare.
goto assemblerom
:hasprg
fc /b original.prg genie.prg

:assemblerom
echo === assemble.bat: assembling genie.asm ===
if exist original.chr goto haschr
echo error: original.chr not found; cannot assemble
goto end
:haschr
asm6f genie.asm genie.nes
if errorlevel 1 goto error
echo.

echo === assemble.bat: comparing ROM to original ===
if exist original.nes goto hasnes
echo Warning: original.nes not found; cannot compare
goto end
:hasnes
fc /b original.nes genie.nes

goto end

:error
echo === assemble.bat: an error was detected ===

:end

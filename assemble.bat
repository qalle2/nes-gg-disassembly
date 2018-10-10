@echo off
cls

echo Assembling prg.asm to genie.prg...
ophis -v -o genie.prg prg.asm
echo.

echo Comparing original.prg to genie.prg...
if exist original.prg goto hasprg
echo Warning: original.prg not found; cannot compare
echo.
goto assemblerom
:hasprg
fc /b original.prg genie.prg

:assemblerom
echo Assembling genie.asm to genie.nes...
if exist original.chr goto haschr
echo error: original.chr not found; cannot assemble
goto end
:haschr
ophis -v -o genie.nes genie.asm
echo.

echo Comparing original.nes to genie.nes...
if exist original.nes goto hasnes
echo Warning: original.nes not found; cannot compare
goto end
:hasnes
fc /b original.nes genie.nes

:end

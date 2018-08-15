@echo off
cls

rem user must have original CHR-ROM
if not exist original-chr.bin goto nochr

echo === assemble.bat: assembling ===
echo.
ophis -v -o prg.bin prg.asm
echo.
if errorlevel 1 goto assemblyerror
ophis -v -o gg.nes gg.asm
echo.
if errorlevel 1 goto assemblyerror

rem user must have original PRG-ROM and entire ROM to compare
if not exist original-prg.bin goto cannotcompare
if not exist original.nes goto cannotcompare

echo === assemble.bat: comparing original files to assembled files ===
echo.
fc /b original-prg.bin prg.bin
echo.
if errorlevel 2 goto fcerror
fc /b original.nes gg.nes
echo.
if errorlevel 2 goto fcerror

rem success
goto end

:nochr
echo assemble.bat: error: "original-chr.bin" not found; see readme for how to get it
goto end

:assemblyerror
echo assemble.bat: error: assembly failed
goto end

:cannotcompare
echo assemble.bat: warning: cannot compare to original files ("original-prg.bin", "original.nes") because they do not exist
goto end

:fcerror
echo assemble.bat: error: FC returned an error
goto end

:end

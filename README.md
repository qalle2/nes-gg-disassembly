# nes-gg-disassembly

A disassembly of the ROM of the [Nintendo Entertainment System](http://en.wikipedia.org/wiki/Nintendo_Entertainment_System) (NES) version of the cheat cartridge [*Game Genie*](http://en.wikipedia.org/wiki/Game_Genie).

## Resources and programs used

* [NESDev Wiki](http://wiki.nesdev.com)
  * e.g. [the Game Genie article](http://wiki.nesdev.com/w/index.php/Game_Genie)
* [NES Game Genie Code Format DOC](http://nesdev.com/nesgg.txt)
* [FCEUX](http://www.fceux.com) (Debugger, Code/Data Logger, etc.)
* [`nes-sprites.lua`](http://forums.nesdev.com/viewtopic.php?f=2&t=13255) for FCEUX by tokumaru
* a quick&dirty disassembler I wrote myself

However, I have *not* used earlier Game Genie disassemblies, e.g. [game-genie-disassembly by Kevin Selwyn](http://github.com/kevinselwyn/game-genie-disassembly) (I discovered it just before publishing my disassembly)

## How to assemble

1. install [Ophis](http://michaelcmartin.github.io/Ophis/) (an assembler)
1. get the files described below
1. run `assemble.bat` (only works on Windows)

### Getting the Game Genie ROM

The Game Genie ROM (`genie.nes`) is required but not included.

1. download the compressed ROM (`genie.zip`) from [NESDev](http://nesdev.com/archive.html) or directly from [`genie.zip`](http://nesdev.com/genie.zip) (the site claims: *Galoob has ... given permission for free distribution*)
1. extract the file (`genie.nes`)
1. rename the file to `original.nes` (`assemble.bat` expects to find it)
1. the file size should be 24,592 bytes
1. the MD5 hash of the file should be `e354fb5b20e1b9fe4e5ca330f9b3391a`

### Getting the original PRG-ROM data

1. download and extract the Game Genie ROM (`genie.nes`) as described earlier
1. open the file in a hex editor
1. go to offset 16 (i.e., skip the first 16 bytes)
1. copy the following 4,096 bytes (4 KiB) to a new file
1. save the new file as `original-prg.bin` (`assemble.bat` expects to find it)
1. the MD5 hash of the new file should be `8d699c97d164d406c2912aece164cd32`

### Getting the original CHR-ROM data

1. download and extract the Game Genie ROM (`genie.nes`) as described earlier
1. open the file in a hex editor
1. copy the last 256 bytes to a new file
1. save the new file as `original-chr.bin` (`assemble.bat` expects to find it)
1. the MD5 hash of the new file should be `0a0b0b2ed4f45699a0d27cd6ddb4d906`

## Screenshots

### Normal screenshot

* NTSC mode (256×224 pixels)

![normal](screenshot-ntsc.png)

### Attribute Table byte boundaries

* PAL mode (256×240 pixels)
* background scrolling disabled (by hacking the ROM)
* sprites hidden (using emulator settings)
* a red 32×32-pixel grid and text added (photoshopped)

![attribute byte boundaries](screenshot-pal,no_scroll,no_sprites,grid.png)

## Parts of the Game Genie screen

* *flying letter*: a letter that flies from the virtual keyboard to the input area when a letter is entered
* *hand cursor*: looks like a hand, moved with arrow keys, on the input area or on the virtual keyboard
* *input area*: where the entered codes appear; initially filled with dashes (`-`)
* *particles*: small squares that fly when a letter is deleted
* *revolving cursor*: revolves around a letter or dash (`-`) on the input area; looks like a plus sign (`+`)
* *virtual keyboard*: the letters `AEPOZXLU GKISTVYN`

## Bugs

* Sometimes, the letter or dash (`-`) under the revolving cursor on the input area does not flash.
  * How to reproduce:
    1. boot up the ROM
    1. observe the first dash (it does not flash)
    1. press B
    1. observe the first dash (it flashes)
* You can sometimes delete non-final letters of the code.
  * How to reproduce:
    1. enter `AAAAAAAA` on the last line (you need to move the cursor manually from the 6th to the 7th letter)
    1. press B three times
    1. observe the code (`AAAAA-AA`)
* The bottom half of the flying letter sometimes looks corrupt and flashing.
  * How to reproduce (use pause&frame advance on an emulator for best results):
    1. enter any two letters (e.g. `UU`)
    1. immediately delete the last letter (press B)
    1. immediately delete another letter (press B)
    1. observe the bottom half of the flying letter (it is corrupt and flashes)
* A variation of the previous bug: sometimes, the bottom half of the flying letter flashes and some flying particles disappear.
  * How to reproduce (use pause&frame advance on an emulator for best results):
    1. enter two letters or more (e.g. `UUU`)
    1. delete the last letter (press B)
    1. immediately delete another letter (press B)
    1. immediately enter another letter (e.g. `U`)
    1. observe the bottom half of the flying letter (it flashes) and note the missing particles

The sprite-related bugs seem to be caused by some sprites being assigned to both the flying letter and the flying particles.

## Speculation

The Game Genie may have been originally designed to support four codes instead of three:
* At `$fdb0`, `decoded_codes` (`$0090`) is initialized to length 16 instead of 12.
* At `$fe43`, the address of the code that was just decoded is compared to the first three codes, not just two.
* There is space for 32 letters in `entered_letters` (`$066b`).
* Even on an NTSC TV, there would be just enough space vertically for four codes (4 tiles for logo, 8 tiles for virtual keyboard, 16 tiles for the codes).

However:
* There are not enough bits for four codes in `genie_master_control` (`$8000`).
* There are no unused registers immediately after `genie_master_control`.

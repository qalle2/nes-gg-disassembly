# nes-gg-disassembly
A disassembly of the ROM of the [NES](http://en.wikipedia.org/wiki/Nintendo_Entertainment_System) cheat cartridge [*Game Genie*](http://en.wikipedia.org/wiki/Game_Genie). Assembles with [ASM6](https://github.com/qalle2/asm6/).

## How to assemble
* Download the original Game Genie ROM and rename it to **`original.nes`**.
  * One location: [NESDev](http://nesdev.com/archive.html) (go to the link *Game Genie by Codemasters* and download `genie.zip`). The site claims: *Galoob has kindly given permission for free distribution.*
* Copy the CHR ROM data from the original Game Genie ROM to a new file, **`original.chr`**.
  * Either use `ines_split.py` from my [NES utilities](https://github.com/qalle2/nes-util)&hellip;
  * &hellip;or open the ROM in a hex editor and copy the last 256 bytes to a new file.
* Assemble and verify that the reassembled file is identical to the original:
  * Either run the Linux script `assemble.sh`&hellip;
  * &hellip;or run:
    * `asm6 prg.asm genie.prg`
    * `asm6 genie.asm genie.nes`
    * `diff original.nes genie.nes` (on Linux)
    * `fc /b original.nes genie.nes` (on Windows)

## Screenshots

### Normal screenshot
* NTSC mode (256&times;224 pixels)

![normal](screenshot-ntsc.png)

### Attribute Table byte boundaries
* PAL mode (256&times;240 pixels)
* background scrolling disabled (by hacking the ROM)
* sprites hidden (using emulator settings)
* a red 32&times;32-pixel grid and text added (photoshopped)

![attribute byte boundaries](screenshot-pal,no_scroll,no_sprites,grid.png)

### Sprites
* NTSC mode (256&times;224 pixels)
* background hidden (using emulator settings)
* the hand cursor, the revolving cursor, a flying letter (`L`) and flying particles can be seen

![sprites](screenshot-sprites.png)

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
    * boot up the ROM
    * observe the first dash (it does not flash)
    * press B
    * observe the first dash (it flashes)
* You can sometimes delete non-final letters of the code.
  * How to reproduce:
    * enter `AAAAAAAA` on the last line (you need to move the cursor manually from the 6th to the 7th letter)
    * press B three times
    * observe the code (`AAAAA-AA`)
* The bottom half of the flying letter sometimes looks corrupt and flashing.
  * How to reproduce (use pause&frame advance on an emulator for best results):
    * enter any two letters (e.g. `UU`)
    * immediately delete the last letter (press B)
    * immediately delete another letter (press B)
    * observe the bottom half of the flying letter (it is corrupt and flashes)
* A variation of the previous bug: sometimes, the bottom half of the flying letter flashes and some flying particles disappear.
  * How to reproduce (use pause&frame advance on an emulator for best results):
    * enter two letters or more (e.g. `UUU`)
    * delete the last letter (press B)
    * immediately delete another letter (press B)
    * immediately enter another letter (e.g. `U`)
    * observe the bottom half of the flying letter (it flashes) and note the missing particles

The sprite-related bugs seem to be caused by some sprites being assigned to both the flying letter and the flying particles.

## FCEUX Code/Data log
I created a `.cdl` file of the ROM by using FCEUX Code/Data Logger.
Then I converted it to a human-readable format using my [`cdl-summary`](http://github.com/qalle2/cdl-summary) and edited it slightly.
The columns are: start address (hexadecimal), end address (hexadecimal), length (decimal), description.
Note that there are only 41 unaccessed bytes (excluding the unused IRQ vector).

```
f000-f245 (582): code
f246-f247 (  2): unaccessed
f248-f374 (301): code
f375-f375 (  1): unaccessed
f376-f3fa (133): code
f3fb-f41e ( 36): data
f41f-f6fe (736): code
f6ff-f77e (128): data (indirectly accessed)
f77f-f842 (196): code
f843-f856 ( 20): data
f857-fa0c (438): code
fa0d-fa1c ( 16): data
fa1d-fa1d (  1): unaccessed
fa1e-fa2d ( 16): data
fa2e-fa2e (  1): unaccessed
fa2f-fa87 ( 89): code
fa88-faa7 ( 32): data
faa8-fd02 (603): code
fd03-fd0a (  8): data
fd0b-fd2c ( 34): code
fd2d-fd3c ( 16): data
fd3d-fe46 (266): code
fe47-fe4a (  4): unaccessed
fe4b-fe7d ( 51): code
fe7e-fe7f (  2): unaccessed
fe80-fe88 (  9): code
fe89-fe8a (  2): unaccessed
fe8b-fe9e ( 20): code
fe9f-febd ( 31): data
febe-febf (  2): unaccessed
fec0-fee7 ( 40): data (indirectly accessed)
fee8-feed (  6): unaccessed
feee-ffe5 (248): data (indirectly accessed)
ffe6-fff9 ( 20): unaccessed
fffa-fffd (  4): data (note: the NMI and reset vectors)
fffe-ffff (  2): unaccessed (note: the IRQ vector)
```

## Speculation
The Game Genie may have been originally designed to support four codes instead of three:
* At `$fdb0`, `decoded_codes` (`$0090`) is initialized to length 16 instead of 12.
* At `$fe43`, the address of the code that was just decoded is compared to the first three codes, not just two.
* There is space for 32 letters in `entered_letters` (`$066b`).
* Even on an NTSC TV, there would be just enough space vertically for four codes (4 tiles for logo, 8 tiles for virtual keyboard, 16 tiles for the codes).

However:
* There are not enough bits for four codes in `genie_master_control` (`$8000`).
* There are no unused registers immediately after `genie_master_control`.
* A genie usually grants three wishes, not four.

## References
* [NESDev Wiki](http://wiki.nesdev.com) (e.g. [the Game Genie article](http://wiki.nesdev.com/w/index.php/Game_Genie))
* [NES Game Genie Code Format DOC](http://nesdev.com/nesgg.txt)

However, I have *not* used earlier Game Genie disassemblies such as [game-genie-disassembly by Kevin Selwyn](http://github.com/kevinselwyn/game-genie-disassembly) (I discovered it just before publishing my disassembly).

## Programs used
* [FCEUX](http://www.fceux.com) (Debugger, Code/Data Logger, etc.)
* [`nes-sprites.lua`](http://forums.nesdev.com/viewtopic.php?f=2&t=13255) for FCEUX by tokumaru
* a quick&amp;dirty disassembler I wrote myself

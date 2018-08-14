# nes-gg-disassembly

Disassembly of the ROM of the NES cheat cartridge *Game Genie*.
Assembles with Ophis.

## Resources and programs used

* NESDev Wiki (e.g. http://wiki.nesdev.com/w/index.php/Game_Genie)
* FCEUX (Debugger, Code/Data Logger, etc.)
* ``nes-sprites.lua`` by tokumaru (http://forums.nesdev.com/viewtopic.php?f=2&t=13255)
* a quick&dirty disassembler I wrote myself

However, I have *not* used earlier Game Genie disassemblies, e.g. this one by Kevin Selwyn:
http://github.com/kevinselwyn/game-genie-disassembly
(I discovered it just before publishing my disassembly)

## Getting the Game Genie ROM

The Game Genie ROM (``genie.nes``) is required but not included.
The ROM can be downloaded from http://nesdev.com/archive.html or directly from http://nesdev.com/genie.zip
The site claims: *Galoob has ... given permission for free distribution*.
Extract the file and rename it to ``original.nes``.
The size should be 24592 bytes.
The MD5 hash should be ``e354fb5b20e1b9fe4e5ca330f9b3391a``.

## Getting the original PRG-ROM data

First, download the Game Genie ROM as described earlier.
Then, starting from offset 16 (i.e., skipping the first 16 bytes), copy 4096 bytes to a new file named ``original-prg.bin``.
The MD5 hash should be ``8d699c97d164d406c2912aece164cd32``.

## Getting the original CHR-ROM data

First, download the Game Genie ROM as described earlier.
Then, copy the last 256 bytes to a new file named ``original-chr.bin``.
The MD5 hash should be ``0a0b0b2ed4f45699a0d27cd6ddb4d906``.

## Parts of the Game Genie screen

* *flying letter*: a letter that flies from the virtual keyboard to the input area when a letter is entered
* *hand cursor*: looks like a hand, moved with arrow keys, on the input area or on the virtual keyboard
* *input area*: where the entered codes appear; initially filled with dashes (``-``)
* *particles*: small squares that fly when a letter is deleted
* *revolving cursor*: revolves around a letter or dash (``-``) on the input area; looks like a plus sign (``+``)
* *virtual keyboard*: the letters ``AEPOZXLU GKISTVYN``

## Speculation

The Game Genie may have been originally designed to support four codes instead of three:
* at ``$fdb0``, ``decoded_codes`` (``$90``) is initialized to length 16 instead of 12
* at ``$fe43``, the address of the code that was just decoded is compared to the first three codes, not just two
* there is space for 32 letters in ``entered_letters`` (``$66b``)
* even on an NTSC TV, there would be just enough space vertically for four codes (4 tiles for logo, 8 tiles for virtual keyboard, 16 tiles for the codes)

However:
* there are not enough bits for four codes in ``genie_master_control`` (``$8000``)
* there are no unused registers immediately after ``genie_master_control``

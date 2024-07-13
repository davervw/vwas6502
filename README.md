# vwas6502

The overall goal is to produce a command line 6502 assembler running in a bare-bones 6502 system like the assembler command used in common monitor programs running on 8-bit systems.   Only character in, character out facilities are required.

This repository consists of several parts.

1. [Desktop hosted C# assembler](src/dotnet/README.md) The interactive console 6502 assembler was an exercise in assembling text input into 6502 machine code bytes using a modern high level programming language (C#).  Output is only to the screen.  Requires Windows/Linux/Mac or other system capable of running a C# .NET program.

2. [6502 hosted disassembler](src/6502/README.md) On the path of creating an assembler is creating a disassembler.  Originally developed and tested on C64, using only character out so is trivial to port to another 6502 system.

3. Ported this 6502 disassembler to [WozMon for Commodore](https://github.com/davervw/wozmon_cbm/tree/main) and [patches for Apple 1](https://github.com/davervw/wozmon_cbm/blob/main/vwas6502.hex) related systems.  [(blog entry)](https://techwithdave.davevw.com/2024/07/disassembler-for-use-with-wozmon.html)

4. Remimplemented existing command syntax that WozMon uses, added D for disassembler.  It mostly feels like WozMon, but it's remade from scratch (no, doesn't fit in 256 bytes), and it requires C64 for now.

5. Implemented the mini-assembler using syntax A.  (targeting C64)

[Blogged about steps 4 & 5](https://techwithdave.davevw.com/2024/07/mini-assembler-with-disassembler.html)

6. To be continued... 

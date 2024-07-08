# vwas6502

The overall goal is to produce a command line 6502 assembler running in a bare-bones 6502 system like the assembler command used in common monitor programs running on 8-bit systems.   Only character in, character out facilities are required.

This repository consists of several parts.

1. [Desktop hosted C# assembler](src/dotnet/README.md) The interactive console 6502 assembler was an exercise in assembling text input into 6502 machine code bytes using a modern high level programming language (C#).  Output is only to the screen.  Requires Windows/Linux/Mac or other system capable of running a C# .NET program.

2. [6502 hosted disassembler](src/6502/README.md) On the path of creating an assembler is creating a disassembler.  Originally developed and tested on C64, using only character out so is trivial to port to another 6502 system.

3. To be continued...
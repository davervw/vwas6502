# vwas6502

The overall goal is to produce a command line 6502 assembler running in a bare-bones 6502 system like the assembler command used in common monitor programs running on 8-bit systems.   Only character in, character out facilities are required.

## Status: monitor with mini-assembler is working on the C64 and minimum 6502 target

This repository consists of two parts.

1. [Desktop hosted C# assembler](src/dotnet/README.md) The interactive console 6502 assembler was an exercise/prototype in assembling text input into 6502 machine code bytes using a modern high level programming language (C#).  Output is only to the screen.  Requires Windows/Linux/Mac or other system capable of running a C# .NET program.

2. [6502 monitor](src/6502/README.md) is a 6502 hosted monitor supporting Commodore 64 (loaded at $8000), and supporting minimum 6502 + MC6850 system (8K ROM at $E000)

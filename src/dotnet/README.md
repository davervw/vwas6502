# Desktop hosted C# assembler

The interactive console 6502 assembler was an exercise in assembling text input into 6502 machine code bytes using a modern high level programming language (C#).  Output is only to the screen.  Requires Windows/Linux/Mac or other system capable of running a C# .NET program.  Builds and runs with [Visual Studio Code](https://code.visualstudio.com/), [Visual Studio 2022](https://visualstudio.microsoft.com/), or [dotnet](https://learn.microsoft.com/en-us/dotnet/) command line tool.

````
interactive console 6502 assembler
vwas6502 version 1.02
Copyright (c) 2024 by David R. Van Wagner github.com/davervw
MIT LICENSE
? for keywords
? keyword for example(s)

?
ADC AND ASL BCC BCS BEQ BMI BNE BPL BVC BVS BIT BRK CLC CLD CLI CLV 
CMP CPX CPY DEC DEX DEY EOR INC INX INY JMP JSR LDA LDX LDY LSR NOP 
ORA PHA PHP PLA PLP ROL ROR RTI RTS SBC SEC SED SEI STA STX STY TAX 
TAY TSX TXA TXS TYA *= .org ? help mode end quit

? sta
81 IndirectX: STA ($12,X)
85 ZeroPage: STA $12
8D Absolute: STA $1234
91 IndirectY: STA ($12),Y
95 ZeroPageX: STA $12,X

.org 1000
ldy #0
lda #$2a
sta $0400,y
sta $0500,y
sta $0600,y
sta $0700,y
iny
bne $1002
rts

1000 A0 00
1002 A9 2A
1004 99 00 04
1007 99 00 05
100A 99 00 06
100D 99 00 07
1010 C8
1011 D0 EF
1013 60

quit
````

Actually the assembler byte output is interleaved with the input as this is an interactive assembler running from the console.  Shown here in separate sections for instruction.

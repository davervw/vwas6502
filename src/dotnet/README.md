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
ADC AND ASL BCC BCS BEQ BMI BNE BPL BVC BVS BIT BRK CLC CLD CLI CLV CMP CPX CPY DEC DEX DEY EOR INC INX INY JMP JSR LDA LDX LDY LSR NOP ORA PHA PHP PLA PLP ROL ROR RTI RTS SBC SEC SED SEI STA STX STY TAX TAY TSX TXA TXS TYA *= .org ? help mode end quit

? sta
81 IndirectX: STA ($12,X)
85 ZeroPage: STA $12
8D Absolute: STA $1234
91 IndirectY: STA ($12),Y
95 ZeroPageX: STA $12,X
99 AbsoluteY: STA $1234,Y
9D AbsoluteX: STA $1234,X

.org 1000
lda $1234
lda $1234,x
lda $1234,y
asl
lda #$12
lda ($12,x)
lda ($12),y
jmp ($1234)
bne 1000
lda $12
lda $12,x
ldx $12,y
brk

1000 AD 34 12
1003 BD 34 12
1006 B9 34 12
1009 0A
100A A9 12
100C A1 12
100E B1 12
1010 6C 34 12
1013 D0 EB
1015 A5 12
1017 B5 12
1019 B6 12
101B 00

mode

Absolute $1234 ADC AND ASL BIT CMP CPX CPY DEC EOR INC JMP JSR LDA LDX LDY LSR ORA ROL ROR SBC STA STX STY
AbsoluteX $1234,X ADC AND ASL CMP DEC EOR INC LDA LDY LSR ORA ROL ROR SBC STA
AbsoluteY $1234,Y ADC AND CMP EOR LDA LDX ORA SBC STA
Accumulator A ASL LSR ROL ROR
Immediate #$12 ADC AND CMP CPX CPY EOR LDA LDX LDY ORA SBC
Indirect ($1234) JMP
IndirectX ($12,X) ADC AND CMP EOR LDA ORA SBC STA
IndirectY ($12),Y ADC AND CMP EOR LDA ORA SBC STA
None  BRK CLC CLD CLI CLV DEX DEY INX INY NOP PHA PHP PLA PLP RTI RTS SEC SED SEI TAX TAY TSX TXA TXS TYA
Relative $1234 {-128 to +127} BCC BCS BEQ BMI BNE BPL BVC BVS
ZeroPage $12 ADC AND ASL BIT CMP CPX CPY DEC EOR INC LDA LDX LDY LSR ORA ROL ROR SBC STA STX STY
ZeroPageX $12,X ADC AND ASL CMP DEC EOR INC LDA LDY LSR ORA ROL ROR SBC STA STY
ZeroPageY $12,Y LDX STX

quit
````

Actually the assembler byte output is interleaved with the input as this is an interactive assembler running from the console.  Shown here in separate sections for instruction.

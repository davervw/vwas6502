using System.Globalization;

namespace vwas6502;

class VWas6502
{
    // Addressing modes
    // nn       Absolute        $1234
    // nn, X    Absolute X      $1234, X
    // nn, Y    Absolute Y      $1234, Y
    // A        Accumulator     A (or assumed)
    // #n       Immediate       #5A
    // (n, X)   Indirect X      ($5A, X)
    // (n), Y   Indirect Y      ($5A), Y
    // (nn)     Indirect        ($5A)
    // n        Relative        $1234 (must be within +127, -128 bytes of next PC)
    // n        Zero Page       $5A
    // n, X     Zero Page X     $5A, X
    // n, Y     Zero Page Y     $5A, Y
    //          None

    enum AddressingMode
    {
        [Example("$1234")]
        Absolute,
        [Example("$1234,X")]
        AbsoluteX,
        [Example("$1234,Y")]
        AbsoluteY,
        [Example("A")]
        Accumulator,
        [Example("#$12")]
        Immediate,
        [Example("($12,X)")]
        IndirectX,
        [Example("($12),Y")]
        IndirectY,
        [Example("($1234)")]
        Indirect,
        [Example("$1234")]
        Relative,
        [Example("$12")]
        ZeroPage,
        [Example("$12,X")]
        ZeroPageX,
        [Example("$12,Y")]
        ZeroPageY,
        [Example("")]
        None
    }

    enum Operation
    {
        ADC,
        AND,
        ASL,
        BCC,
        BCS,
        BEQ,
        BMI,
        BNE,
        BPL,
        BVC,
        BVS,
        BIT,
        BRK,
        CLC,
        CLD,
        CLI,
        CLV,
        CMP,
        CPX,
        CPY,
        DEC,
        DEX,
        DEY,
        EOR,
        INC,
        INX,
        INY,
        JMP,
        JSR,
        LDA,
        LDX,
        LDY,
        LSR,
        NOP,
        ORA,
        PHA,
        PHP,
        PLA,
        PLP,
        ROL,
        ROR,
        RTI,
        RTS,
        SBC,
        SEC,
        SED,
        SEI,
        STA,
        STX,
        STY,
        TAX,
        TAY,
        TSX,
        TXA,
        TXS,
        TYA,
    }

    class OpCodeInfo
    {
        public byte opcode;
        public Operation op;
        public AddressingMode mode;
    }

    static OpCodeInfo[] opcodes = [
        new OpCodeInfo { opcode=0x00, op=Operation.BRK, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x01, op=Operation.ORA, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0x05, op=Operation.ORA, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x06, op=Operation.ASL, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x08, op=Operation.PHP, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x09, op=Operation.ORA, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0x0A, op=Operation.ASL, mode=AddressingMode.Accumulator },
        new OpCodeInfo { opcode=0x0D, op=Operation.ORA, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x0E, op=Operation.ASL, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0x10, op=Operation.BPL, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0x11, op=Operation.ORA, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0x15, op=Operation.ORA, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x16, op=Operation.ASL, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x18, op=Operation.CLC, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x19, op=Operation.ORA, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0x1D, op=Operation.ORA, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0x1E, op=Operation.ASL, mode=AddressingMode.AbsoluteX },

        new OpCodeInfo { opcode=0x20, op=Operation.JSR, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x21, op=Operation.AND, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0x24, op=Operation.BIT, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x25, op=Operation.AND, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x26, op=Operation.ROL, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x28, op=Operation.PLP, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x29, op=Operation.AND, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0x2A, op=Operation.ROL, mode=AddressingMode.Accumulator },
        new OpCodeInfo { opcode=0x2C, op=Operation.BIT, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x2D, op=Operation.AND, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x2E, op=Operation.ROL, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0x30, op=Operation.BMI, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0x31, op=Operation.AND, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0x35, op=Operation.AND, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x36, op=Operation.ROL, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x38, op=Operation.SEC, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x39, op=Operation.AND, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0x3D, op=Operation.AND, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0x3E, op=Operation.ROL, mode=AddressingMode.AbsoluteX },

        new OpCodeInfo { opcode=0x40, op=Operation.RTI, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x41, op=Operation.EOR, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0x45, op=Operation.EOR, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x46, op=Operation.LSR, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x48, op=Operation.PHA, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x49, op=Operation.EOR, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0x4A, op=Operation.LSR, mode=AddressingMode.Accumulator },
        new OpCodeInfo { opcode=0x4C, op=Operation.JMP, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x4D, op=Operation.EOR, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x4E, op=Operation.LSR, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0x50, op=Operation.BVC, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0x51, op=Operation.EOR, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0x55, op=Operation.EOR, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x56, op=Operation.LSR, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x58, op=Operation.CLI, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x59, op=Operation.EOR, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0x5D, op=Operation.EOR, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0x5E, op=Operation.LSR, mode=AddressingMode.AbsoluteX },

        new OpCodeInfo { opcode=0x60, op=Operation.RTS, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x61, op=Operation.ADC, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0x65, op=Operation.ADC, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x66, op=Operation.ROR, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x68, op=Operation.PLA, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x69, op=Operation.ADC, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0x6A, op=Operation.ROR, mode=AddressingMode.Accumulator },
        new OpCodeInfo { opcode=0x6C, op=Operation.JMP, mode=AddressingMode.Indirect },
        new OpCodeInfo { opcode=0x6D, op=Operation.ADC, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x6E, op=Operation.ROR, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0x70, op=Operation.BVS, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0x71, op=Operation.ADC, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0x75, op=Operation.ADC, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x76, op=Operation.ROR, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x78, op=Operation.SEI, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x79, op=Operation.ADC, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0x7D, op=Operation.ADC, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0x7E, op=Operation.ROR, mode=AddressingMode.AbsoluteX },

        new OpCodeInfo { opcode=0x81, op=Operation.STA, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0x84, op=Operation.STY, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x85, op=Operation.STA, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x86, op=Operation.STX, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0x88, op=Operation.DEY, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x8A, op=Operation.TXA, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x8C, op=Operation.STY, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x8D, op=Operation.STA, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0x8E, op=Operation.STX, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0x90, op=Operation.BCC, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0x91, op=Operation.STA, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0x94, op=Operation.STY, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x95, op=Operation.STA, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0x96, op=Operation.STX, mode=AddressingMode.ZeroPageY },
        new OpCodeInfo { opcode=0x98, op=Operation.TYA, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x99, op=Operation.STA, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0x9A, op=Operation.TXS, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0x9D, op=Operation.STA, mode=AddressingMode.AbsoluteX },

        new OpCodeInfo { opcode=0xA0, op=Operation.LDY, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0xA1, op=Operation.LDA, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0xA2, op=Operation.LDX, mode=AddressingMode.Immediate},
        new OpCodeInfo { opcode=0xA4, op=Operation.LDY, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xA5, op=Operation.LDA, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xA6, op=Operation.LDX, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xA8, op=Operation.TAY, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xA9, op=Operation.LDA, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0xAA, op=Operation.TAX, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xAC, op=Operation.LDY, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0xAD, op=Operation.LDA, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0xAE, op=Operation.LDX, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0xB0, op=Operation.BCS, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0xB1, op=Operation.LDA, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0xB4, op=Operation.LDY, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0xB5, op=Operation.LDA, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0xB6, op=Operation.LDX, mode=AddressingMode.ZeroPageY },
        new OpCodeInfo { opcode=0xB8, op=Operation.CLV, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xB9, op=Operation.LDA, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0xBA, op=Operation.TSX, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xBC, op=Operation.LDY, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0xBD, op=Operation.LDA, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0xBE, op=Operation.LDX, mode=AddressingMode.AbsoluteY },

        new OpCodeInfo { opcode=0xC0, op=Operation.CPY, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0xC1, op=Operation.CMP, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0xC4, op=Operation.CPY, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xC5, op=Operation.CMP, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xC6, op=Operation.DEC, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xC8, op=Operation.INY, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xC9, op=Operation.CMP, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0xCA, op=Operation.DEX, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xCC, op=Operation.CPY, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0xCD, op=Operation.CMP, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0xCE, op=Operation.DEC, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0xD0, op=Operation.BNE, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0xD1, op=Operation.CMP, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0xD5, op=Operation.CMP, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0xD6, op=Operation.DEC, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0xD8, op=Operation.CLD, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xD9, op=Operation.CMP, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0xDD, op=Operation.CMP, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0xDE, op=Operation.DEC, mode=AddressingMode.AbsoluteX },

        new OpCodeInfo { opcode=0xE0, op=Operation.CPX, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0xE1, op=Operation.SBC, mode=AddressingMode.IndirectX },
        new OpCodeInfo { opcode=0xE4, op=Operation.CPX, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xE5, op=Operation.SBC, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xE6, op=Operation.INC, mode=AddressingMode.ZeroPage },
        new OpCodeInfo { opcode=0xE8, op=Operation.INX, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xE9, op=Operation.SBC, mode=AddressingMode.Immediate },
        new OpCodeInfo { opcode=0xEA, op=Operation.NOP, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xEC, op=Operation.CPX, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0xED, op=Operation.SBC, mode=AddressingMode.Absolute },
        new OpCodeInfo { opcode=0xEE, op=Operation.INC, mode=AddressingMode.Absolute },

        new OpCodeInfo { opcode=0xF0, op=Operation.BEQ, mode=AddressingMode.Relative },
        new OpCodeInfo { opcode=0xF1, op=Operation.SBC, mode=AddressingMode.IndirectY },
        new OpCodeInfo { opcode=0xF5, op=Operation.SBC, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0xF6, op=Operation.INC, mode=AddressingMode.ZeroPageX },
        new OpCodeInfo { opcode=0xF8, op=Operation.SED, mode=AddressingMode.None },
        new OpCodeInfo { opcode=0xF9, op=Operation.SBC, mode=AddressingMode.AbsoluteY },
        new OpCodeInfo { opcode=0xFD, op=Operation.SBC, mode=AddressingMode.AbsoluteX },
        new OpCodeInfo { opcode=0xFE, op=Operation.INC, mode=AddressingMode.AbsoluteX },
    ];

    static void Main(string[] args)
    {
        var pc = (ushort?)null;
        var operationNames = Enum.GetNames(typeof(Operation)).ToHashSet();
        while (true)
        {
            string line = Console.ReadLine();
            line = line.Replace(",", " , "); // make sure commas are their own separate words
            line = line.Replace("(", " ( ");
            line = line.Replace(")", " ) ");
            line = line.Replace("#", " # ");
            line = line.Replace("=", " = ");
            line = line.Replace("*", " * ");
            line = line.Replace("?", " ? ");
            line = line.Replace("$", "");
            var words = line.Split(' ', StringSplitOptions.RemoveEmptyEntries).ToList();
            if (words.Count == 0)
                continue;
            if (words.Count == 1 && words[0] == "?")
            {
                foreach (var name in operationNames)
                    Console.Write($"{name} ");
                Console.WriteLine("*= .org");
                continue;
            }
            if (words.Count == 2 && words[0] == "?" && operationNames.Contains(words[1].ToUpper()))
            {
                Operation operation = (Operation)Enum.Parse(typeof(Operation), words[1], true);
                foreach (var opcode in opcodes.Where(x => x.op == operation))
                    Console.WriteLine($"{opcode.opcode:X2} {opcode.mode}: {operation} {GetExample(opcode.mode)}");
                continue;
            }
            if (words.Count == 2 && words[0] == "?" && words[1] == "*"
                || words.Count == 3 && words[0] == "?" && words[1] == "*" && words[2] == "=")
            {
                Console.WriteLine("*=$1234");
                continue;   
            }
            if (words.Count == 2 && words[0] == "?" && words[1] == ".org")
            {
                Console.WriteLine(".org $1234");
                continue;   
            }
            if (words[0] == "*" && words[1] == "=" && parseArgument(words[2], out pc, 2))
                continue;
            if (words[0].ToLower() == ".org" && parseArgument(words[1], out pc, 2))
                continue;
            if (words[0].Length > 3)
            {
                var word0 = words[0];
                words.RemoveAt(0);
                words.Insert(0, word0.Substring(0, 3));
                words.Insert(1, word0.Substring(3));
            }
            if (!operationNames.Contains(words[0].ToUpper()))
            {
                Console.WriteLine($"? {words[0]}");
                continue;
            }
            Operation op = (Operation)Enum.Parse(typeof(Operation), words[0], true);
            AddressingMode? mode = null;
            ushort? arg = null;
            if (words.Count == 1)
                mode = AddressingMode.None;
            else if (words.Count == 2 && words[1].ToUpper() == "A")
                mode = AddressingMode.Accumulator;
            else if (words.Count == 3 && words[1] == "#" && parseArgument(words[2], out arg, 1))
                mode = AddressingMode.Immediate;
            else if (words.Count == 4 && parseArgument(words[1], out arg, 1) && words[2] == "," && words[3].ToUpper() == "X")
                mode = AddressingMode.ZeroPageX;
            else if (words.Count == 4 && parseArgument(words[1], out arg, 1) && words[2] == "," && words[3].ToUpper() == "Y")
                mode = AddressingMode.ZeroPageY;
            else if (words.Count == 4 && parseArgument(words[1], out arg, 2) && words[2] == "," && words[3].ToUpper() == "X")
                mode = AddressingMode.AbsoluteX;
            else if (words.Count == 4 && parseArgument(words[1], out arg, 2) && words[2] == "," && words[3].ToUpper() == "Y")
                mode = AddressingMode.AbsoluteY;
            else if (words.Count == 6 && words[1] == "(" && parseArgument(words[2], out arg, 1) && words[3] == "," && words[4].ToUpper() == "X" && words[5] == ")")
                mode = AddressingMode.IndirectX;
            else if (words.Count == 5 && words[1] == "(" && parseArgument(words[2], out arg, 1) && words[3] == "," && words[4].ToUpper() == "X")
                mode = AddressingMode.IndirectX;
            else if (words.Count == 6 && words[1] == "(" && parseArgument(words[2], out arg, 1) && words[3] == ")" && words[4] == "," && words[5].ToUpper() == "Y")
                mode = AddressingMode.IndirectY;
            else if (words.Count == 5 && words[1] == "(" && parseArgument(words[2], out arg, 1) && words[3] == "," && words[4].ToUpper() == "Y")
                mode = AddressingMode.IndirectY;
            else if (words.Count == 4 && words[1] == "(" && parseArgument(words[2], out arg, 2) && words[3] == ")")
                mode = AddressingMode.Indirect;
            else if (words.Count == 3 && words[1] == "(" && parseArgument(words[2], out arg, 2))
                mode = AddressingMode.Indirect;
            else if (op != Operation.BIT && words[0].ToUpper()[0] == 'B' && words.Count == 2 && parseArgument(words[1], out arg, 2))
                mode = AddressingMode.Relative;
            else if (words.Count == 2 && parseArgument(words[1], out arg, 1))
                mode = AddressingMode.ZeroPage;
            else if (words.Count == 2 && parseArgument(words[1], out arg, 2))
                mode = AddressingMode.Absolute;
            else if (words.Count == 4 && parseArgument(words[1], out arg, 1) && words[2] == "," && words[3].ToUpper() == "X")
                mode = AddressingMode.ZeroPageX;
            else if (words.Count == 4 && parseArgument(words[1], out arg, 1) && words[2] == "," && words[3].ToUpper() == "Y")
                mode = AddressingMode.ZeroPageY;

            // for (int i = 0; i < words.Count; i++)
            //     Console.WriteLine($"[{i}]={words[i]}");

            OpCodeInfo info = opcodes.FirstOrDefault(x => x.op == op && x.mode == mode);
            if (info == null && mode == AddressingMode.None)
            {
                mode = AddressingMode.Accumulator;
                info = opcodes.FirstOrDefault(x => x.op == op && x.mode == mode);
            }
            if (info == null && mode == AddressingMode.ZeroPage)
            {
                mode = AddressingMode.Absolute;
                info = opcodes.FirstOrDefault(x => x.op == op && x.mode == mode);
            }
            if (info == null && mode == AddressingMode.ZeroPageX)
            {
                mode = AddressingMode.AbsoluteX;
                info = opcodes.FirstOrDefault(x => x.op == op && x.mode == mode);
            }
            if (info == null && mode == AddressingMode.ZeroPageY)
            {
                mode = AddressingMode.AbsoluteY;
                info = opcodes.FirstOrDefault(x => x.op == op && x.mode == mode);
            }

            if (info == null)
            {
                Console.WriteLine($"Addressing mode {(mode==null?"Unknown":mode)} is not valid for {op}");
                continue;
            }
            //Console.WriteLine($"{op} {info.mode} {info.opcode:X2}");
            if (pc == null)
                Console.Write("????");
            else
                Console.Write($"{pc:X4}");
            Console.Write($" {info.opcode:X2}");
            if (pc != null)
                ++pc;
            if (mode == AddressingMode.Relative)
            {
                ++pc;
                if (pc == null)
                {
                    Console.Write(" ??");
                }
                else {
                    int offset = arg.Value - pc.Value;
                    if ((sbyte)offset == offset)
                        Console.Write($" {(byte)offset:X2}");
                    else
                        Console.Write(" ??");
                }
            }
            if (mode == AddressingMode.Immediate
                || mode == AddressingMode.ZeroPage
                || mode == AddressingMode.ZeroPageX
                || mode == AddressingMode.ZeroPageY
                || mode == AddressingMode.IndirectX
                || mode == AddressingMode.IndirectY) 
            {
                Console.Write($" {(byte)arg:X2}");
                if (pc != null)
                    ++pc;
            }
            else if (mode == AddressingMode.Absolute
                || mode == AddressingMode.AbsoluteX
                || mode == AddressingMode.AbsoluteY
                || mode == AddressingMode.Indirect)
            {
                Console.Write($" {(byte)arg:X2} {(arg >> 8):X2}");
                if (pc != null)
                    ++pc;
            }
            Console.WriteLine();
        }
    }

    // from: https://stackoverflow.com/questions/11200286/how-do-you-make-an-enum-that-has-data-tied-to-it CustomAttributes
    [AttributeUsage(AttributeTargets.Field)]
    private class ExampleAttribute: System.Attribute
    {
        public string Value { get; private set; }
        public ExampleAttribute(string value)
        {
            Value = value;
        }
    }
    private static string GetExample(AddressingMode mode)
    {
        System.Reflection.FieldInfo fieldInfo = typeof(AddressingMode).GetField(mode.ToString());
        if (!ReferenceEquals(fieldInfo, null))
        {
            object[] attributes = fieldInfo.GetCustomAttributes(typeof(ExampleAttribute), true);
            if (!ReferenceEquals(attributes, null) && attributes.Length > 0)
            {
                return ((ExampleAttribute)attributes[0]).Value;
            }
        }
        //Not valid value or it didn't have the attribute
        return null;
    }

    private static bool parseArgument(string arg, out ushort? value, int maxBytes)
    {
        value = null;
        if (maxBytes < 1 || maxBytes > 2)
            throw new InvalidOperationException($"maxBytes 1 or 2 are only values supported, {maxBytes} is invalid value");
        if (arg.Length > 4)
            return false;
        if (arg.Length > 2 && maxBytes == 1)
            return false;
        if (ushort.TryParse(arg, System.Globalization.NumberStyles.HexNumber, CultureInfo.InvariantCulture, out var n))
            value = n;
        return value != null;
    }
}

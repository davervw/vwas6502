;; vwas6502.asm - interactive console 6502 assembler
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIT License
;;
;; Copyright (c) 2024 David R. Van Wagner
;; davevw.com
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

*=$cc00

; kernal/system calls
charout=$ffd2

; zeropage
ptr1=$fb ; and $fc
ptr2=$fd ; and $fe
tmp=$ff

; test
start:
    lda #<start
    ldx #>start
    sta ptr1
    stx ptr1+1
    lda #<end
    ldx #>end
    sta ptr2
    stx ptr2+1
    ; fall through to disassemble

disassemble:
    rts

disp_opcode: ; .A opcode byte
    ldy #nopcodes
    ldx #nopcodes-1
-   cmp opcodes,x
    beq +
    dex
    dey
    bne -
    rts
+   lda instidx, x
    ; fall through to display instruction

dispinst: ; .A instruction index 0..55
    tax
    cpx #ninst
    bcs +
    lda inst0, x
    jsr charout
    lda inst1, x
    jsr charout
    lda inst2, x
    jsr charout
+   rts

disphexword: ; .A low, .X high, 0000..FFFF
    pha
    txa
    jsr disphexbyte
    pla
    ;fall through to call again

disphexbyte: ; .A 00..FF
    pha
    lsr
    lsr
    lsr
    lsr
    jsr disphexnybble
    pla
    ;fall through to call again

disphexnybble: ; .A 0..F
    and #$0F
    ora #$30
    cmp #$3A
    bcc +
    adc #$06
+   jmp charout

end: brk

; instruction textual mnuemonic first, second, third letters (read down in source)
ninst = 56
inst0 !text "AAABBBBBBBBBBCCCCCCCDDDEIIIJJLLLLNOPPPPRRRRSSSSSSSTTTTTT"
inst1 !text "DNSCCEIMNPRVVLLLLMPPEEEONNNMSDDDSORHHLLOOTTBEEETTTAASXXY"
inst2 !text "CDLCSQTIELKCSCDIVPXYCXYRCXYPRAXYRPAAPAPLRISCCDIAXYXYXASA"

; 6502 addressing modes (number of bytes per instruction shown at end of comment)
modeAcc  = 0 ; 0 Accumulator 1
modeNone = 1 ; 1 None 1
modeImm  = 2 ; 2 Immediate 2
modeIndX = 3 ; 3 IndirectX 2
modeIndY = 4 ; 4 IndirectY 2
modeRel  = 5 ; 5 Relative 2
modeZP   = 6 ; 6 ZeroPage 2
modeZPX  = 7 ; 7 ZeroPageX 2
modeZPY  = 8 ; 8 ZeroPageY 2
modeAbs  = 9 ; 9 Absolute 3
modeAbsX = 10 ; 10 AbsoluteX 3
modeAbsY = 11 ; 11 AbsoluteY 3
modeInd  = 12 ; 12 Indirect 3

; opcode table of byte values (opcodes), instructions, and addressing modes
nopcodes = 151
opcodes !byte $00,$01,$05,$06,$08,$09,$0A,$0D,$0E,$10,$11,$15,$16,$18,$19,$1D,$1E,$20,$21,$24,$25,$26,$28,$29,$2A,$2C,$2D,$2E,$30,$31,$35,$36,$38,$39,$3D,$3E,$40,$41,$45,$46,$48,$49,$4A,$4C,$4D,$4E,$50,$51,$55,$56,$58,$59,$5D,$5E,$60,$61,$65,$66,$68,$69,$6A,$6C,$6D,$6E,$70,$71,$75,$76,$78,$79,$7D,$7E,$81,$84,$85,$86,$88,$8A,$8C,$8D,$8E,$90,$91,$94,$95,$96,$98,$99,$9A,$9D,$A0,$A1,$A2,$A4,$A5,$A6,$A8,$A9,$AA,$AC,$AD,$AE,$B0,$B1,$B4,$B5,$B6,$B8,$B9,$BA,$BC,$BD,$BE,$C0,$C1,$C4,$C5,$C6,$C8,$C9,$CA,$CC,$CD,$CE,$D0,$D1,$D5,$D6,$D8,$D9,$DD,$DE,$E0,$E1,$E4,$E5,$E6,$E8,$E9,$EA,$EC,$ED,$EE,$F0,$F1,$F5,$F6,$F8,$F9,$FD,$FE
instidx !byte $0A,$22,$22,$02,$24,$22,$02,$22,$02,$09,$22,$22,$02,$0D,$22,$22,$02,$1C,$01,$06,$01,$27,$26,$01,$27,$06,$01,$27,$07,$01,$01,$27,$2C,$01,$01,$27,$29,$17,$17,$20,$23,$17,$20,$1B,$17,$20,$0B,$17,$17,$20,$0F,$17,$17,$20,$2A,$00,$00,$28,$25,$00,$28,$1B,$00,$28,$0C,$00,$00,$28,$2E,$00,$00,$28,$2F,$31,$2F,$30,$16,$35,$31,$2F,$30,$03,$2F,$31,$2F,$30,$37,$2F,$36,$2F,$1F,$1D,$1E,$1F,$1D,$1E,$33,$1D,$32,$1F,$1D,$1E,$04,$1D,$1F,$1D,$1E,$10,$1D,$34,$1F,$1D,$1E,$13,$11,$13,$11,$14,$1A,$11,$15,$13,$11,$14,$08,$11,$11,$14,$0E,$11,$11,$14,$12,$2B,$12,$2B,$18,$19,$2B,$21,$12,$2B,$18,$05,$2B,$2B,$18,$2D,$2B,$2B,$18
modeidx !byte $01,$03,$06,$06,$01,$02,$00,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$09,$03,$06,$06,$06,$01,$02,$00,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$01,$03,$06,$06,$01,$02,$00,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$01,$03,$06,$06,$01,$02,$00,$0C,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$03,$06,$06,$06,$01,$01,$09,$09,$09,$05,$04,$07,$07,$08,$01,$0B,$01,$0A,$02,$03,$02,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$08,$01,$0B,$01,$0A,$0A,$0B,$02,$03,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$02,$03,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A
;lengths !byte 
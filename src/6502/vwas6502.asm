;; vwas6502.asm - interactive console 6502 assembler
;;
;; >>> STATUS: display/edit memory + run(JMP) + disassembler + assembler <<<
;; >>>         targeting C64 for now...                                  <<<
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

;; VWAS6502 (C) 2024 DAVID R. VAN WAGNER
;; MIT LICENSE DAVEVW.COM
;; _
;; INTERACTIVE SYNTAX >>> display/edit/run, and assemble/disassemble are working <<<
;; (WOZMON SIMILAR - note if wozmon present, could leverage existing code)
;; 1000 (display memory at $1000)
;; 1000.2000 (display memory range $1000 to $2000)
;; 1000. (display page of memory starting at $1000)
;; 1000 r (JMP $1000)
;; 1000: 01 02 03 (modify memory)
;; (NEW SYNTAX)
;; 1000 d (disassemble starting at address, for screenful)
;; 1000 a (assemble starting at, interactive until empty line) 
;; (FUTURE SYNTAX, not implemented)
;; 1000.2000 "text" ? (search for text in address range inclusive)
;; 1000.2000 A9 FF ? (search for byte sequence in address range inclusive)
;; 1000.2000 3000 m (move bytes $1000-$2000 inclusive to $3000, left/right move as appropriate)
;; 1000.2000: 01 02 03 (fill bytes to inclusive address range)
;; . (display registers, VICE format or custom? screen editor changeable?)
;; .A 00 (change register, replace A with X, Y, SP, PC, SR, N, V, B, D, I, Z, C as appropriate)
;; 1000.2000 "filename" 08 save (save range of bytes from $1000 up to not including $2000, Commodore drive address is optional, can abbreviate to s)
;; 1000 "filename" 08 load (load absolute, address optional, drive address is optional, can abbreviate to l)
;; ? (commands help)
;; ? a (list instructions available)
;; ? adc (assembler addressing modes examples for a specific instruction, replace adc with desired instruction)
;; ? mode (show addressing modes example syntax for 6502)
;;
;; (INTERACTIVE ASSEMBLER)
;; 1000 _
;;      ADC #$12
;; 1000 69 12    ADC #$12
;; 1002 _
;;      JSR $1234
;; 1002 20 34 12 JSR $1234
;; 1005 _
;;      RTS
;; 1005 60       RTS
;; 1006 _
;;      XYZ
;;      XYZ ?
;; 1006 _
;; _
;;
;; line editor version (Commodore) can revise address, and can overwrite input line with results of assembly
;; and can cursor up to revise, 
;;
;; can also assume assembler mode on the fly regardless if line editor or raw terminal if see instruction name after address, so a command is superfluous
;;

; global
inputbuf=$0200

; kernal/system calls
charout=$ffd2
charin=$ffcf ; screen editor
getkey=$ffe4

; zeropage
ptr1=$fb ; and $fc
ptr2=$fd ; and $fe
tmp=$ff
opidx=$22
inidx=$23
mode=$24
size=$25
ptr3=$26 ; and $27
count=$a3
len=$a4
savepos=$a5
tmp2=$a6
flag=$a7

*=$c000
start:
    lda #<copyright
    ldx #>copyright
    jsr strout
-   jsr inputline
    jsr parseline
    jmp -

test: ; all the addressing modes here for testing disassembly
    nop
    lda $1234
    lda $1234,x
    lda $1234,y
    asl
    lda #$12
    lda ($12,x)
    lda ($12),y
    jmp ($1234)
-   bne -
    lda $12
    lda $12,x
    ldx $12,y
    !byte $FF ; unknown

disassemble:
    lda #23
    sta count
-   ldy #0
    lda (ptr1),y
    jsr find_opcode
    jsr disp_current
    lda size
    bpl +
    lda #1
+   clc
    adc ptr1
    sta ptr1
    bcc +
    inc ptr1+1
+   dec count
    bne -
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #<page_disassemble
    ldx #>page_disassemble
    jmp strout

compareptrs:
    lda ptr1+1
    cmp ptr2+1
    bne +
    lda ptr1
    cmp ptr2
+   rts

find_opcode: ; INPUT: .A opcode byte, OUTPUT: C flag set if found, .A instruction index, .X opcode index, .Y mode, otherwise C clear, and .A/.X/.Y all $FF
; and properties updated in ZP globals size,inidx,opidx,mode
    ldy #nopcodes
    ldx #nopcodes-1
-   cmp opcodes,x
    beq +
    dex
    dey
    bne -
    clc
    lda #1
    sta size
    lda #$FF
    tax
    tay
    bcc ++
+   lda instidx, x
    ldy modeidx, x
    jsr getsize
    sec
++  sta inidx
    stx opidx
    sty mode
    rts

getsize: ; y is addressing mode index (0..12), registers untouched except status
    ; result returned in size
    pha
    lda #1
    sta size
    pla
    cpy #2 // Immediate
    bcc +
    inc size
    cpy #9 // Absolute
    bcc +
    inc size
+   rts

disp_opcode: ; .A opcode byte
    jsr find_opcode
    txa
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
    jmp charout
+   lda #'?'
    jsr charout
    jsr charout
    jsr charout
    rts

disp_current:
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #$20
    jsr charout
    ldy #0
    ldx size
-   lda (ptr1),y
    jsr disphexbyte
    lda #$20
    jsr charout
    iny
    dex
    bne -
-   cpy #3
    beq +
    lda #$20
    jsr charout
    jsr charout
    jsr charout
    iny
    bne -
+   lda inidx
    jsr dispinst
    lda #$20
    jsr charout
    jsr disp_mode
    lda #13
    jmp charout

disp_mode
    lda mode
    cmp #13
    bcs +
    asl
    tax
    lda mode_jmptable+1,x
    pha
    lda mode_jmptable,x
    pha
+   rts

dispModeAcc:
    lda #'A'
    jmp charout

dispModeNone:
    rts

dispModeImm:
    lda #'#'
    jsr charout
dispModeZP:
    lda #'$'
    jsr charout
    ldy #1
    lda (ptr1),y
    jmp disphexbyte

dispModeIndX:
    lda #'('
    jsr charout
    lda #'$'
    jsr charout
    ldy #1
    lda (ptr1),y
    jsr disphexbyte
    lda #','
    jsr charout
    lda #'X'
    jsr charout
    lda #')'
    jmp charout

dispModeIndY:
    lda #'('
    jsr charout
    lda #'$'
    jsr charout
    ldy #1
    lda (ptr1),y
    jsr disphexbyte
    lda #')'
    jsr charout
    lda #','
    jsr charout
    lda #'Y'
    jmp charout

dispModeRel:
    lda #'$'
    jsr charout
    clc
    lda ptr1
    adc #2
    sta ptr3
    lda ptr1+1
    adc #0
    sta ptr3+1
    ldy #1
    lda (ptr1),y
    bpl +
    ; I'm not sure how to successfully navigate page boundries adding signed byte to unsigned byte, so I'm subtracting unsigned bytes instead
    eor #$FF ; inverse
    clc
    adc #1 ; complete getting absolute value from two's complement
    sta tmp
    sec
    lda ptr3
    sbc tmp
    sta ptr3
    bcs ++
    dec ptr3+1
    bcc ++
+   clc ; simple case of adding
    adc ptr3
    sta ptr3
    bcc ++
    inc ptr3+1
++  lda ptr3
    ldx ptr3+1
    jmp disphexword

dispModeZPX:
    jsr dispModeZP
    lda #','
    jsr charout
    lda #'X'
    jmp charout

dispModeZPY:
    jsr dispModeZP
    lda #','
    jsr charout
    lda #'Y'
    jmp charout

dispModeAbs:
    lda #'$'
    jsr charout
    ldy #1
    lda (ptr1),y
    pha
    iny
    lda (ptr1),y
    tax
    pla
    jmp disphexword

dispModeAbsX:
    jsr dispModeAbs
    lda #','
    jsr charout
    lda #'X'
    jmp charout

dispModeAbsY:
    jsr dispModeAbs
    lda #','
    jsr charout
    lda #'Y'
    jmp charout

dispModeInd:
    lda #'('
    jsr charout
    jsr dispModeAbs
    lda #')'
    jmp charout

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

inputhexword: ; C set if fails
    tya
    tax ; save buffer pointer in x
    jsr inputhexbyte
    bcs ++ ; failed
+   sta ptr1 ; assume one byte
    lda #0
    sta ptr1+1 ; extend to 16 bits
    jsr inputhexbyte
    bcs + ; failed
    ldx ptr1 ; two bytes so shift the bytes
    stx ptr1+1
    sta ptr1
+   clc
    rts
++  txa
    tay ; restore buffer pointer
    rts

inputhexbyte:
    jsr inputhexnybble
    bcs ++
    sta tmp
    jsr inputhexnybble
    bcc +
    clc ; allow single digit as byte
    lda tmp
    bcc ++
+   asl tmp
    asl tmp
    asl tmp
    asl tmp
    ora tmp
++  rts

inputhexnybble:
    lda $0200,y
    and #$7F
    sec
    sbc #$30
    bcc ++
    cmp #10
    bcc +
    sbc #7
    bcc ++
    cmp #10
    bcc ++
    cmp #16
    bcs ++
+   iny
    rts
++  sec
    rts

strout:
    sta ptr3
    stx ptr3+1
    ldy #0
-   lda (ptr3),y
    beq +
    jsr charout
    iny
    bne -
+   rts

inputline:
    ldy #0
-   jsr charin
    sta inputbuf,y
    iny
    cmp #13
    bne -
+   rts

parseline:
    cpy #1
    bne +
-   jmp newline
+   dey
    sty len
    ; skip whitespace
    ; check for address, put in ptr1
    ; or check for dot, then require address put in ptr2
    ; or check for ?, and optional parameter, execute help
    ; check for whitespace
    ; check for address, put in ptr3, check if is byte sequence, store at start of inputbuf instead
    ; check for string, store at start of inputbuf
    ; check for drive number
    ; check for whitespace
    ; check command ":rda?mls", execute command
    ldy #0
    jsr skipspaces
    cpy len
    beq -
    jsr chkdot
    bne +
    jmp executedot
+   jsr chkhelp
    bne +
    jmp executehelp
+   jsr chkhexaddr1
    bne error
    jmp executeaddr1
error:
    jmp reporterr

executeaddr1:
    cpy len
    bne +
    jmp executedisplay1
+   jsr chkdot
    bne +
    cpy len
    beq ++
    jsr chkhexaddr2
    bne error
    clc
    ror flag
    jmp executeaddr12
+   jsr skipspaces
    jsr chkcolon
    bne +
    jmp executemodify
+   jsr chkaddr1cmd ; rda, will not return here if cmd
    jsr chkfilename
    bne error
    jmp executeloadfilename
++  lda ptr1
    clc
    adc #$b7
    sta ptr2
    lda ptr1+1
    adc #$00
    sta ptr2+1
    bcc +
    lda #$ff
    sta ptr2
    sta ptr2+1
+   sec
    ror flag
    ; fall through to executeaddr12

executeaddr12:
    cpy len
    bne +
    jmp executedisplay12
+   jmp reportnotimplemented

executedisplay1:
    lda ptr1
    sta ptr2
    lda ptr1+1
    sta ptr2+1
    ; fall through executedisplay12

executedisplay12:
    lda #$ff
    sta count
-   inc count
    lda count
    and #$07
    bne +
    lda #13
    jsr charout
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #':'
    jsr charout
    lda #' '
    jsr charout
+   ldy #0
    lda (ptr1),y
    jsr disphexbyte
    lda #' '
    jsr charout
    inc ptr1
    bne +
    inc ptr1+1
    beq ++
+   jsr compareptrs
    bcc -
    beq -
    bit flag
    bpl ++
    jsr newline
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #<page_displaymemory
    ldx #>page_displaymemory
    jmp strout
++  jmp newline

executemodify:
    jsr skipspaces
    cpy len
    beq ++
    jsr chkhexbyteofsequence
    beq +
    jmp error
+   sty tmp
    ldy #0
    sta (ptr1),y
    inc ptr1
    bne +
    inc ptr1+1
+   ldy tmp
    bne executemodify
++  jmp newline

executeloadfilename:
executedot:
executeaddr1cmd:
executehelp:
    jmp reportnotimplemented

executeassemble:
    pla ; remove low byte return address
    pla ; return high byte return address
    lda #20
    jsr charout
    jsr charout
    ; save current pointer
--  lda ptr1
    ldx ptr1+1
    sta ptr3
    stx ptr3+1
    jsr inputline
    cpy #1
    beq ++
    dey
    sty len
    ldy #0
    jsr skipspaces
    cpy len
    beq ++
    ;jsr chkhexaddr1 *** WARNING: interferes with ADC, BCC, DEC because those names are exclusively valid HEX alphabetic characters
    ;jsr skipspaces
    jsr chkinstruction
    beq +
-   jmp error    
+   jsr chkaddressing
    bne -
    jsr find_inst_and_mode
    bne -
    jsr store_assembly
    clc
    lda size
    adc ptr3
    sta ptr1
    lda ptr3+1
    adc #0
    sta ptr1+1
    jsr newline ; TODO disassemble on screen as assemble for validation
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #' '
    jsr charout
    jmp --
++  jmp newline

store_assembly:
    ldx opidx
    lda opcodes, x
    ldy #0
    sta (ptr3), y
    iny
    ldx size
    cpx #1
    beq ++
+   cpx #2
    bne +
    lda tmp2
    sta (ptr3), y
    rts
+   cpx #3
    bne ++
    lda ptr1
    sta (ptr3), y
    iny
    lda ptr1+1
    sta (ptr3), y
++  rts

chkaddressing: ; match input to addressing mode, note caller may need to adjust
    jsr skipspaces
    ldx #0
    stx mode
    jsr chkaccumulator
    beq +
    inc mode
    cpy len ; chknone
    beq +
    inc mode
    jsr chkimmediate
    beq +
    inc mode
    jsr chkindirectx
    beq +
    inc mode
    jsr chkindirecty
    beq +
    inc mode
    jsr chkrelative
    beq +
    inc mode
    jsr chkzeropage
    beq +
    inc mode
    jsr chkzeropagex
    beq +
    inc mode
    jsr chkzeropagey
    beq +
    inc mode
    jsr chkabsolute
    beq +
    inc mode
    jsr chkabsolutex
    beq +
    inc mode
    jsr chkabsolutey
    beq +
    inc mode
    jsr chkindirect
+   php ; save Z
    lda mode
    plp ; restore Z 
    rts

chkaccumulator:
    cpy len
    bne +
    lda inidx
    cmp #2 ; ASL
    beq ++
    cmp #32 ; LSR
    beq ++
    cmp #39 ; ROL
    beq ++
    cmp #40 ; ROR
    jmp ++
+   lda inputbuf, y
    cmp #'A'
    bne ++
    lda inputbuf+1,y
    cmp #13 ; Z set true/false whether parsed exactly
++  rts

chkimmediate:
    sty savepos
    lda inputbuf, y
    cmp #'#'
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexbyte
    bne ++
+   cpy len
    bne ++
    rts
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkindirectx:
    sty savepos
    lda inputbuf, y
    cmp #'('
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexbyte
    bne ++
    jsr skipspaces
    lda inputbuf, y
    cmp #','
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'X'
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #')'
    bne ++
    iny
    cpy len
    bne ++
    rts
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkindirecty:
    sty savepos
    lda inputbuf, y
    cmp #'('
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexbyte
    bne ++
    jsr skipspaces
    lda inputbuf, y
    cmp #')'
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #','
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'Y'
    bne ++
    iny
    cpy len
    bne ++
    rts
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkrelative:
    sty savepos
    ldx inidx
    cpx #6 ; BIT
    beq ++
    lda inst0, x
    cmp #'B'
    bne ++
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexword
    bne ++
    cpy len
    bne ++
    jsr computeoffset
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

computeoffset:
;   compute next address
    lda ptr3+1
    sta ptr2+1
    lda ptr3
    clc
    adc #2
    sta ptr2
    bcc +
    inc ptr2+1
+  ; subtract argument
    sec
    lda ptr1
    sbc ptr2
    sta tmp2
    lda ptr1+1
    sbc ptr2+1
    beq chkoffsetto127 ; offset is byte sized, make sure is positive signed byte
    cmp #$FF
    bne failedoffset ; 0 and FF were only options so fail
    ; chkeck negative offset
    lda tmp2
    bmi successoffset ; branch if signed byte is negative
    bpl failedoffset ; otherwise fail
chkoffsetto127:
    lda tmp2
    bmi failedoffset ; branch if too large an offset 128 bytes or more
successoffset:    
    lda #0 ; Z true (EQ)
    rts
failedoffset:
    lda #1 ; Z false (NE)
    rts

chkzeropage:
    sty savepos
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexbyte
    bne ++
    cpy len
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkzeropagex:
    sty savepos
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexbyte
    bne ++
    jsr skipspaces
    lda inputbuf, y
    cmp #','
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'X'
    bne ++
    iny
    cpy len
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkzeropagey:
    sty savepos
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexbyte
    bne ++
    jsr skipspaces
    lda inputbuf, y
    cmp #','
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'Y'
    bne ++
    iny
    cpy len
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkabsolute:
    sty savepos
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexword
    bne ++
    cpy len
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkabsolutex:
    sty savepos
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexword
    bne ++
    jsr skipspaces
    lda inputbuf, y
    cmp #','
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'X'
    bne ++
    iny
    cpy len
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkabsolutey:
    sty savepos
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexword
    bne ++
    jsr skipspaces
    lda inputbuf, y
    cmp #','
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'Y'
    bne ++
    iny
    cpy len
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkindirect:
    sty savepos
    lda inputbuf, y
    cmp #'('
    bne ++
    iny
    jsr skipspaces
    lda inputbuf, y
    cmp #'$'
    bne +
    iny
+   jsr chkhexword
    bne ++
    jsr skipspaces
    lda inputbuf, y
    cmp #')'
    bne ++
    iny
    cpy len
    bne ++
    rts ; Z true (EQ)
++  ldy savepos
    ldx #1 ; Z false (NE)
    rts

chkinstruction:
    cpy len
    beq ++
    sty tmp
    ldx #(ninst-1)
-   lda inputbuf,y
    cmp inst0,x
    bne +
    iny
    lda inputbuf,y
    cmp inst1,x
    bne +
    iny
    lda inputbuf,y
    cmp inst2,x
    bne +
    iny
    txa
    sta inidx
    ldx #0
    rts
+   ldy tmp
    dex
    bpl -
++  ldx #1 ; Z false (NE)
    rts

find_inst_and_mode: ; INPUT: inidx & mode, OUTPUT: Z true: opidx & size, otherwise false
    ; and allows mode promotion
    jsr find_inst_and_mode2
    beq ++
    lda mode
    cmp #6
    bcc ++
    cmp #10
    bcs +
    adc #3
    sta mode ; promote ZeroPage modes to Absolute modes
    jmp find_inst_and_mode2 ; try again once
+   ldx #1 ; Z false (NE)
++  rts

find_inst_and_mode2: ; INPUT: inidx & mode, OUTPUT: Z true: opidx & size, otherwise false
    ldx #nopcodes-1
-   lda instidx, x
    ldy modeidx, x
    cmp inidx
    bne +
    cpy mode
    bne +
    stx opidx
    jsr getsize
    ldx #0 ; Z true (EQ)
    rts
+   dex
    cpx #$ff
    bne -
    ldx #1 ; Z false (NE)
    rts

executerun:
    pla ; remove low byte return address
    pla ; return high byte return address
    pla ; again, we're really not returning
    pla ; again, we're really not returning
    jsr newline
    sec
    lda ptr1
    sbc #1
    sta ptr1
    bcs +
    dec ptr1+1
+   lda ptr1+1
    pha
    lda ptr1
    pha
    rts

executedisassemble:
    pla ; remove low byte return address
    pla ; return high byte return address
    jsr newline
    jmp disassemble

reportnotimplemented:
    lda #<notimplemented
    ldx #>notimplemented
    jmp strout

reporterr:
    cpy #0
    beq +
    lda #' '
-   jsr charout
    dey
    bne -
+   lda #'?'
    jsr charout
    lda #13
    jsr charout
    rts

skipspaces:
-   lda inputbuf, y
    cmp #$20
    bne +
    iny
    bne -
+   rts

chkdot:
    lda inputbuf, y
    cmp #'.'
    bne +
    iny
    lda #0 ; Z true (EQ)
+   rts

chkhelp:
    lda inputbuf, y
    cmp #'?'
    bne +
    iny
    lda #0 ; Z true (EQ)
+   rts

chkcolon:
    lda inputbuf, y
    cmp #':'
    bne +
    iny
    lda #0 ; Z true (EQ)
+   rts

chkfilename:
    sty tmp
    lda inputbuf, y
    cmp #34
    bne ++
-   iny
    lda inputbuf, y
    cmp #13
    bne +
    ldy tmp
    bne ++
+   cmp #34
    bne -
++  rts

chkhexbyteofsequence:
    jsr inputhexbyte
    jsr +
    bne ++ ; Z false (NE) if failed checks
    cpy len
    beq ++ ; Z true (EQ) if end of input
    sta tmp
    lda inputbuf,y
    cmp #$20
    bne ++ ; Z false (NE) if not space
    lda tmp
    ldx #0 ; Z true (EQ) is space delimeter
++  rts

chkhexword:
chkhexaddr1:
    jsr inputhexword
+   ldx #0 ; Z true (EQ)
    bcc +
    inx ; Z false (NE)
+   rts

chkhexbyte
    tya
    pha ; save y
    jsr inputhexbyte
    bcs +
    sta tmp2
    jsr inputhexnybble
    bcc +
    pla ; throw away saved y
    lda tmp2
    ldx #0 ; Z true (EQ)
    rts
+   pla
    tay ; won't be zero, so Z false (NE)
    rts    

chkhexaddr2:
    lda ptr1
    pha
    lda ptr1+1
    pha
    jsr chkhexaddr1
    beq +
    pla
    pla
    lda #1 ; Z false (NE)
    rts
+   lda ptr1
    sta ptr2
    lda ptr1+1
    sta ptr2+1
    pla
    sta ptr1+1
    pla
    sta ptr1
    lda #0 ; Z true (EQ)
    rts    

chkaddr1cmd:
    lda inputbuf, y
+   cmp #'A'
    bne +
    iny
    jmp executeassemble
+   cmp #'D'
    bne +
    iny
    jmp executedisassemble
+   cmp #'R'
    bne +
    iny
    jmp executerun
+   rts

newline:
    lda #13
    jmp charout

; charout: ; for debugging, wait for scan line to pass over entire screen at least once
;     jsr $ffd2
;     pha
; -   lda $d011
;     bpl -
; -   lda $d011
;     bmi -
; -   lda $d011
;     bpl -
; -   lda $d011
;     bmi -
;     pla
;     rts

end: brk

; instruction textual mnuemonic first, second, third letters (read down in source)
ninst = 56
inst0 !text "AAABBBBBBBBBBCCCCCCCDDDEIIIJJLLLLNOPPPPRRRRSSSSSSSTTTTTT"
inst1 !text "DNSCCEIMNPRVVLLLLMPPEEEONNNMSDDDSORHHLLOOTTBEEETTTAASXXY"
inst2 !text "CDLCSQTIELKCSCDIVPXYCXYRCXYPRAXYRPAAPAPLRISCCDIAXYXYXASA"

; 6502 addressing modes by index number and number of bytes per instruction shown at end of comment
mode_jmptable:
!word dispModeAcc-1; 0 Accumulator 1
!word dispModeNone-1 ; 1 None 1
!word dispModeImm-1 ; 2 Immediate 2
!word dispModeIndX-1 ; 3 IndirectX 2
!word dispModeIndY-1 ; 4 IndirectY 2
!word dispModeRel-1 ; 5 Relative 2
!word dispModeZP-1 ; 6 ZeroPage 2
!word dispModeZPX-1 ; 7 ZeroPageX 2
!word dispModeZPY-1 ; 8 ZeroPageY 2
!word dispModeAbs-1 ; 9 Absolute 3
!word dispModeAbsX-1 ; 10 AbsoluteX 3
!word dispModeAbsY-1 ; 11 AbsoluteY 3
!word dispModeInd-1 ; 12 Indirect 3

mode_examples:
!text "A", 0 ; 0 Accumulator
!text "", 0 ; 1 None
!text "#$12", 0 ; 2 Immediate
!text "($12,X)", 0 ; 3 IndirectX
!text "($12),Y", 0 ; 4 IndirectY
!text "$1234 (-128 to +127)", 0 ; 5 Relative
!text "$12", 0 ; 6 ZeroPage
!text "$12,X", 0 ; 7 ZeroPageX
!text "$12,Y", 0 ; 8 ZeroPageY
!text "$1234", 0 ; 9 Absolute
!text "$1234,X", 0 ; 10 AbsoluteX
!text "$1234,Y", 0 ; 11 AbsoluteY
!text "($1234)", 0 ; 12 Indirect

; opcode table of byte values (opcodes), instructions, and addressing modes
nopcodes = 151
opcodes !byte $00,$01,$05,$06,$08,$09,$0A,$0D,$0E,$10,$11,$15,$16,$18,$19,$1D,$1E,$20,$21,$24,$25,$26,$28,$29,$2A,$2C,$2D,$2E,$30,$31,$35,$36,$38,$39,$3D,$3E,$40,$41,$45,$46,$48,$49,$4A,$4C,$4D,$4E,$50,$51,$55,$56,$58,$59,$5D,$5E,$60,$61,$65,$66,$68,$69,$6A,$6C,$6D,$6E,$70,$71,$75,$76,$78,$79,$7D,$7E,$81,$84,$85,$86,$88,$8A,$8C,$8D,$8E,$90,$91,$94,$95,$96,$98,$99,$9A,$9D,$A0,$A1,$A2,$A4,$A5,$A6,$A8,$A9,$AA,$AC,$AD,$AE,$B0,$B1,$B4,$B5,$B6,$B8,$B9,$BA,$BC,$BD,$BE,$C0,$C1,$C4,$C5,$C6,$C8,$C9,$CA,$CC,$CD,$CE,$D0,$D1,$D5,$D6,$D8,$D9,$DD,$DE,$E0,$E1,$E4,$E5,$E6,$E8,$E9,$EA,$EC,$ED,$EE,$F0,$F1,$F5,$F6,$F8,$F9,$FD,$FE
instidx !byte $0A,$22,$22,$02,$24,$22,$02,$22,$02,$09,$22,$22,$02,$0D,$22,$22,$02,$1C,$01,$06,$01,$27,$26,$01,$27,$06,$01,$27,$07,$01,$01,$27,$2C,$01,$01,$27,$29,$17,$17,$20,$23,$17,$20,$1B,$17,$20,$0B,$17,$17,$20,$0F,$17,$17,$20,$2A,$00,$00,$28,$25,$00,$28,$1B,$00,$28,$0C,$00,$00,$28,$2E,$00,$00,$28,$2F,$31,$2F,$30,$16,$35,$31,$2F,$30,$03,$2F,$31,$2F,$30,$37,$2F,$36,$2F,$1F,$1D,$1E,$1F,$1D,$1E,$33,$1D,$32,$1F,$1D,$1E,$04,$1D,$1F,$1D,$1E,$10,$1D,$34,$1F,$1D,$1E,$13,$11,$13,$11,$14,$1A,$11,$15,$13,$11,$14,$08,$11,$11,$14,$0E,$11,$11,$14,$12,$2B,$12,$2B,$18,$19,$2B,$21,$12,$2B,$18,$05,$2B,$2B,$18,$2D,$2B,$2B,$18
modeidx !byte $01,$03,$06,$06,$01,$02,$00,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$09,$03,$06,$06,$06,$01,$02,$00,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$01,$03,$06,$06,$01,$02,$00,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$01,$03,$06,$06,$01,$02,$00,$0C,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$03,$06,$06,$06,$01,$01,$09,$09,$09,$05,$04,$07,$07,$08,$01,$0B,$01,$0A,$02,$03,$02,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$08,$01,$0B,$01,$0A,$0A,$0B,$02,$03,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$02,$03,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A

copyright !text 13,145,"VWAS6502 (C) 2024 DAVID R. VAN WAGNER", 13, "MIT LICENSE DAVEVW.COM", 157, 13, 0
notimplemented !text "NOT IMPLEMENTED",13,0
page_disassemble !text "D",157,157,157,157,157,0
page_displaymemory !text ".",157,157,157,157,157,0

finish = *

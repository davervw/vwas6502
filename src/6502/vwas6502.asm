;; vwas6502.asm - interactive console 6502 assembler
;;
;; >>> STATUS: display/edit memory + run(JMP) + disassembler + assembler <<<
;; >>>                     *** MULTIPLATFORM ***                         <<<
;; >>>       ****************************************************        <<<
;; >>>       **           TARGETS:                              *        <<<
;; >>>       ** (1)  C64 8000-9FFF screen editor                *        <<<
;; >>>       ** (2)  C64 8000-9FFF terminal edition             *        <<<
;; >>>       ** (3)  6502+MC6850 E000-FFFF minimum system       *        <<<
;; >>>       *   56K(RAM),8K(ROM), 2 bytes IO for MC6850 UART   *        <<<
;; >>>       ****************************************************        <<<
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

;; DEFINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Important! define exactly ONE target (set in build.sh)
;C64SCREEN = 1
;C64TERMINAL = 1
;MINIMUM = 1

; options (set in build.sh)
;NEEDECHO = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; _
;; INTERACTIVE SYNTAX >>> display/edit/run, and assemble/disassemble are working <<<
;; (WOZMON SIMILAR - note if wozmon present, could leverage existing code)
;; 1000 (display memory at $1000)
;; 1000.2000 (display memory range $1000 to $2000)
;; 1000. (display next screenful of memory starting at $1000)
;; . (display next screenful of memory)
;; 1000 r (JMP $1000)
;; 1000: 01 02 03 (modify memory)
;; (NEW SYNTAX)
;; 1000 d (disassemble starting at address, for screenful)
;; d (continue disassembling from last address)
;; 1000 a (assemble starting at, interactive until empty line)
;; x (exit monitor -- C64 only)
;; ? (commands help)
;; ?a (list instructions available)
;; ?adc (assembler addressing modes examples for a specific instruction, replace adc with desired instruction)
;; ?mode (show addressing modes example syntax for 6502)
;; 1000.2000 "filename" 08 s (C64: save range of bytes from $1000 up to not including $2000, Commodore drive address is optional, defaults to 8)
;; .? (display registers in format .1234 10110000 01 02 03 f6)
;; .1234 10110000 01 02 03 f6 (set registers PC FLAGS A X Y S)
;; .A:00 (change register, replace A with X, Y, S, P as appropriate, use space or colon as separator)
;; (FUTURE SYNTAX, not implemented)
;; 1000.2000 "text" ? (search for text in address range inclusive)
;; 1000.2000 A9 FF ? (search for byte sequence in address range inclusive)
;; 1000.2000 3000 m (move bytes $1000-$2000 inclusive to $3000, left/right move as appropriate)
;; 1000.2000: 01 02 03 (fill bytes to inclusive address range)
;; 1000 "filename" 08 load (load absolute, address optional, drive address is optional, can abbreviate to l)
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

!ifdef MINIMUM {
charout=JUART_OUT
getkey=JUART_IN
}

!ifdef C64SCREEN {
setlfs=$ffba
setnam=$ffbd
charin=$ffcf ; screen editor
charout=$ffd2
fsave =$ffd8
getkey=$ffe4
}

!ifdef C64TERMINAL {
setlfs=$ffba
setnam=$ffbd
charout=$ffd2
getkey=$ffe4
fsave =$ffd8
}

; zeropage
!ifdef MINIMUM {
ptr1=$fc ; and $fd
ptr3=$fe ; and $ff
}

!ifdef MINIMUM {
* = $e000
    jmp start
} else { // any C64
* = $8000
    jmp init64
}

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

!ifndef MINIMUM {
    ; any C64
init64:    
    ; check if irq/brk vector installed
    lda $316
    ldx $317
    cpx #>brk64
    beq +
    sta savebrkvector
    stx savebrkvector+1
    lda #<brk64
    ldx #>brk64
    sta $316
    stx $317
+   jsr install_nmi64
    ; fall through to start
}

start:
    cld
    cli
    lda #<copyright
    ldx #>copyright
    jsr strout
    lda #<firsthelp
    ldx #>firsthelp
    jsr strout
    jmp save_registers

input_loop:
    jsr inputline
    jsr parseline
    jmp input_loop

!ifndef MINIMUM {
; C64 only

install_nmi64:
    ; check if nmi vector installed
    lda $318
    ldx $319
    cpx #>nmi64
    beq +
    sta savenmivector
    stx savenmivector+1
    lda #<nmi64
    ldx #>nmi64
    sta $318
    stx $319
+   rts

uninstall_nmi64:
    lda savenmivector
    ldx savenmivector+1
    sta $318
    stx $319
    rts

chkextrac64:
    jsr chkexit
    bne +
    jmp execute_exit
+   clc ; no error
    ldx #1 ; Z false - not consumed
    rts

chkexit:
    lda inputbuf, y
    cmp #'X'
    bne +
    iny
    cpy len ; validate no extra characters
    beq +
    jmp extra_error
+   rts

extra_error:
    ; pop local return address
    pla
    pla
    sec ; error
    rts

execute_exit:
    lda savebrkvector
    ldx savebrkvector+1
    sta $316
    stx $317
    jsr uninstall_nmi64
    ; pop monitor return addresses, so only original caller is left
    pla
    pla
    pla
    pla
    pla
    pla
    rts

display_extra_help:
    lda #<extra_help
    ldx #>extra_help
    jmp strout

!ifdef C64SCREEN {
display_page_disassemble:
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #<page_disassemble
    ldx #>page_disassemble
    jmp strout

display_page_displaymemory:
    jsr newline
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #<page_displaymemory
    ldx #>page_displaymemory
    jmp strout

inputlinec64:
    ldy #0
-   jsr charin
    sta inputbuf,y
    iny
    cmp #13
    bne -
    rts

continueassemblec64:
    lda #20
    jsr charout
    jsr charout
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #' '
    jsr charout
    jsr charout
    jmp charout
}

chkfilename:
    jsr skipspaces
    sty tmp
    lda inputbuf, y
    cmp #34 ; double quote
    bne ++
-   iny
    lda inputbuf, y
    cmp #13
    bne +
    ldy tmp
    bne ++
+   cmp #34 ; double quote
    bne -
    tya ; index of ending double quotes
    pha ; save
    clc ; will subtract one more
    sbc tmp ; subtract index of first quote, have filename length
    ldx tmp
    inx ; low address of filename
    ldy #>inputbuf ; high address of filename
    jsr setnam
    pla
    tay
    iny ; advance past ending double quotes
    lda #0 ; set Z true
++  rts

check_execute_save:
+   jsr chkfilename
    beq +
    jsr reporterr
    ldx #1 ; set Z false
    rts
+   jsr chkoptionaldrive
    jsr chksave
    bne +
    jsr executesave
    ldx #0 ; set Z true
+   rts

chkoptionaldrive:
    lda #8
    sta drive
    jsr skipspaces
    jsr chkhexbyte
    bne +
    sta drive
    jsr skipspaces
    lda #0 ; set Z true
+   rts

chksave:
    jsr skipspaces
    lda inputbuf, y
    cmp #'S'
    rts

executesave:
    jsr newline
    lda $22
    pha
    lda $23
    pha
    lda ptr1
    sta $22
    lda ptr1+1
    sta $23
    lda #$c0 ; KERNAL control and error messages
    sta $9d ; set messages to be displayed
    lda #1
    ldx drive
    ldy #15
    jsr setlfs
    lda #$22
    ldx ptr2
    ldy ptr2+1
    jsr fsave
    pla
    sta $23
    pla
    sta $22
    jmp newline
}

disassemble:
    lda #23
    sta count
-   ldy #0
    jsr lda_at_ptr1_y
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
!ifdef C64SCREEN {
    jmp display_page_disassemble
} else {
    rts
}

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

dispinst: ; .A instruction index 0..55, note modifies A and X
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
-   jsr lda_at_ptr1_y
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
    jsr lda_at_ptr1_y
    jmp disphexbyte

dispModeIndX:
    lda #'('
    jsr charout
    lda #'$'
    jsr charout
    ldy #1
    jsr lda_at_ptr1_y
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
    jsr lda_at_ptr1_y
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
    jsr lda_at_ptr1_y
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
    jsr lda_at_ptr1_y
    pha
    iny
    jsr lda_at_ptr1_y
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

dispbinbyte: ; .A 00..FF
    ldx #8
    sta tmp
-   lda #'0'
    rol tmp
    bcc +
    lda #'1'
+   jsr charout
    dex
    bne -
    rts

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
    lda #0
    sta ptr1
    sta ptr1+1

    lda #4 ; word is at most 4 nybbles
    sta count
--  jsr inputhexnybble
    bcs +

    ; shift nibble up
    asl
    asl
    asl
    asl

    ldx #4 ; 4 bits rotated into word
-   rol
    rol ptr1
    rol ptr1+1
    dex
    bne - ; repeat bits

    dec count
    bne -- ; repeat nybbles

+   lda count
    cmp #4 ; set C if 4, otherwise clear
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
    ora tmp ; necessary to assemble the two nybbles
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
strout2:
    ldy #0
-   jsr lda_at_ptr3_y
    beq +
    jsr charout
    iny
    bne -
+   tya
    sec
    adc ptr3
    sta ptr3
    bcc +
    inc ptr3+1
+   rts

inputline:
!ifdef C64SCREEN {
    jmp inputlinec64
} else {
    ldy #0
--  sty count
-   jsr getkey
    beq -
    ldy count
!ifdef MINIMUM {
    cmp #8 ; backspace
} else {
    cmp #20
}
    bne +
    cpy #0
    beq -
    dey
!if NEEDECHO = 1 {
    jsr charout
}
    jmp --
+   cmp #13
    beq +
    cmp #' '
    bcc -
    cmp #128
    bcs -
+
!if NEEDECHO = 1 {
    jsr charout
}
    sta inputbuf,y
    iny
    cmp #13
    bne --
    rts
}

parseline:
    cpy #1
    bne +
-
!ifdef C64SCREEN {
    jmp newline
} else {
    rts
}
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
    jsr chkcontinuedis
    bne +
    jsr executedisassemble ; note won't return
+   jsr chkcontinueasm
    bne +
    jsr continueassemble ; note won't return
+   jsr chkdot
    bne +
    jmp executedot
+   jsr chkhelp
    bne +
    jmp executehelp
+
!ifndef MINIMUM {
    jsr chkextrac64 ; check syntax only available on C64
    bcs + ; error if C set
    beq ++ ; consumed if Z set, skip next test(s)
}
    jsr chkhexaddr1
    bne error
++  jmp executeaddr1
error:
    jmp reporterr

executeaddr1:
    cpy len
    bne +
    jmp executedisplay1
+   jsr chkdot
    bne +
    cpy len
    beq executepagedisplay
    jsr chkhexaddr2
    bne error
    clc
    ror flag
    jmp executeaddr12
+   jsr skipspaces
    jsr chkcolon
    bne +
    jmp executemodify
+   jsr chkaddr1cmd ; r/d/a, will not return here if cmd
!ifdef MINIMUM {
    jmp reportnotimplemented
} else {
    jsr chkfilename
    bne error
    jmp executeloadfilename
}

executepagedisplay:
    lda ptr1
    clc
!ifdef MINIMUM {
    adc #$5f
} else {
    adc #$b7
}
    sta ptr2
    lda ptr1+1
!ifdef MINIMUM {
    adc #$01
} else {
    adc #$00
}
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
!ifndef MINIMUM { // any C64
+   jsr check_execute_save
    beq ++
}
+   jmp reportnotimplemented
++  rts

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
!ifdef MINIMUM {
    and #$0f
} else {
    and #$07
}
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
    jsr lda_at_ptr1_y
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
!ifdef C64SCREEN {
    jmp display_page_displaymemory
}
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
    jsr sta_at_ptr1_y
    inc ptr1
    bne +
    inc ptr1+1
+   ldy tmp
    bne executemodify
++  jmp newline

executedot:
    cpy len
    bne +
    jmp executepagedisplay
+   jsr chkloadregister
    bne +
+   jsr chkloadregisters

executeloadfilename:
executeaddr1cmd:
+   jmp reportnotimplemented

chkloadregister:
    jsr chkload_a_x_y_s_p
    jsr chkload_pc
    jsr chkload_n_v_b_d_i_z_c
    rts

chkload_a_x_y_s_p:
    sty tmp
    lda inputbuf, y
    ldx #0
    cmp #'A'
    beq +
    inx
    cmp #'X'
    beq +
    inx
    cmp #'Y'
    beq +
    inx
    cmp #'S'
    beq +
    inx
    cmp #'P'
    bne ++
+   iny
    lda inputbuf, y
!ifndef MINIMUM {
    cmp #$A0
    beq +
}
    cmp #' '
    beq +
    cmp #':'
    bne ++
+   iny
    jsr skipspaces
    stx mode
    jsr chkhexbyte
    bne ++
    ldx mode
    sta registerA, x
!ifndef MINIMUM {
    jsr newline
}
    ; pop call stack so return to input_loop
    pla
    pla
    pla
    pla
    jmp +++
++  ldy tmp ; not Z (NE)
+++ rts

chkload_pc:
    lda #1 ; not Z (NE)
    rts

chkload_n_v_b_d_i_z_c:
    lda #1 ; not Z (NE)
    rts

chkloadregisters:
    jsr chkhexword
    bne ++
    lda ptr1
    sta registerPC
    lda ptr1+1
    sta registerPC+1
    jsr chkspace
    bne +
    jsr chkbinbyte
    bne +
    sta registerSR
    jsr chkspace
    bne +
    jsr chkhexbyte
    bne +
    sta registerA
    jsr chkspace
    bne +
    jsr chkhexbyte
    bne +
    sta registerX
    jsr chkspace
    bne +
    jsr chkhexbyte
    bne +
    sta registerY
    jsr chkspace
    bne +
    jsr chkhexbyte
    bne +
    sta registerSP
    ; don't return to executedot
+   pla
    pla
!ifdef C64SCREEN {
    jsr newline
}
    lda #0 ; set Z
++  rts

executehelp:
!ifdef C64SCREEN {
    jsr newline
}
    cpy len
    bne +
    jmp displayhelp
+   jsr skipspaces
    jsr chkhelpinstructions
    bne +
    jmp displayinstructions
+   jsr chkhelpmodes
    bne +
    jmp displaymodes
+   jsr chkinstruction
    bne +
    jmp executehelpinstruction
+   jsr chkhelpregisters
    bne +
    jmp execute_display_registers
+   jmp reportnotimplemented

displayhelp:
    lda #<generalhelp
    ldx #>generalhelp
    jsr strout
    lda #<generalhelp2
    ldx #>generalhelp2
    jsr strout
    lda #<generalhelp3
    ldx #>generalhelp3
    jsr strout
!ifndef MINIMUM { // any C64
    jsr display_extra_help
}
    jmp newline

chkhelpinstructions:
    lda inputbuf, y
    cmp #'A'
    bne +
    lda inputbuf+1, y
    cmp #13
    ; no need to increment y if found, done parsing line
+   rts

chkhelpregisters:
    lda inputbuf, y
    cmp #'.'
    bne +
    lda inputbuf+1, y
    cmp #13
+   rts

chkhelpmodes:
    lda #<modes_keyword
    ldx #>modes_keyword
    ; fall through to chkkeyword

chkkeyword:
    sty count
    sta ptr3
    stx ptr3+1
    ldx count
    ldy #0
-   jsr lda_at_ptr3_y
    cmp inputbuf, x
    bne +
    inx
    iny
    cpx len
    bne -
    jsr lda_at_ptr3_y ; matched if end of string, will set Z
+   php ; save Z
    ldy count
    plp ; restore Z
    rts

displayinstructions:
    ldy #ninst
    ldx #0
-   txa
    pha
    jsr dispinst
    lda #' '
    jsr charout
    pla
    tax
    inx
    dey
    bne -
    jmp newline

displaymodes:
    sec
    lda #0
-   pha
    tax
    lda mode_sorted, x
    jsr dispmode
    pla
    clc
    adc #1
    cmp #nmodes
    bcc -
    clc
    rts

dispmode:
    cmp #nmodes
    bcs +
    sta mode
    jsr disp_modename_and_example
    jmp dispmodeinstructions
+   rts

dispmodeinstructions:
!ifdef C64SCREEN {
    ; 3 IndirectX and 4 IndirectY are the same, so compact them together to fit on screen
    lda mode
    cmp #3
    bne +
    lda #' '
    jmp charout
}
    ; display instructions with this mode
+   ldy #0
--  sty inidx
    ldx #0
-   stx tmp
    lda inidx
    cmp instidx, x
    bne ++
    lda modeidx, x
    cmp mode
    bne ++
    lda #' '
    jsr charout
    lda instidx, x
    jsr dispinst
    ldx tmp
++  inx
    cpx #nopcodes
    bcc -
    iny
    cpy #ninst
    bne --
    jmp newline

disp_modename_and_example:
    asl
    tax
    lda modes, x
    pha
    lda modes+1, x
    tax
!ifdef C64SCREEN {
    lda #18
    jsr charout
}
    pla
    jsr strout
    lda #' '
    jsr charout
    jsr strout2
!ifdef C64SCREEN {
    lda #146
    jsr charout
}
    rts

disp_modename_instruction_example:
    asl
    tax
    lda modes, x
    pha
    lda modes+1, x
    tax
    pla
    jsr strout ; mode name
    lda ptr3
    pha
    lda ptr3+1
    pha
    lda #':'
    jsr charout
    lda #' '
    jsr charout
    lda inidx
    jsr dispinst ; instruction
    lda #' '
    jsr charout
    pla
    sta ptr3+1
    pla
    sta ptr3
    jsr strout2 ; example
    rts


executehelpinstruction:
    ldy #0
-   sty opidx
    lda instidx, y
    cmp inidx
    bne +
    lda modeidx, y
    sta mode
    ldx opidx
    lda opcodes, x
    jsr disphexbyte
    lda #' '
    jsr charout
    lda mode
    jsr disp_modename_instruction_example
    jsr newline
+   ldy opidx
    iny
    cpy #nopcodes
    bcc -
    rts

continueassemble:
!ifdef C64SCREEN {
    jsr continueassemblec64
}
    ; continue...

executeassemble:
    pla ; remove low byte return address
    pla ; return high byte return address
!ifdef C64SCREEN {
    lda #20
    jsr charout
    jsr charout
} else {
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #' '
    jsr charout
}
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
-   lda ptr3
    sta ptr1
    lda ptr3+1
    sta ptr1+1
    jmp error
+   jsr chkaddressing
    bne -
    jsr find_inst_and_mode
    bne -
    jsr store_assembly ; TODO disassemble on screen as assemble for validation
    clc
    lda size
    adc ptr3
    sta ptr1
    lda ptr3+1
    adc #0
    sta ptr1+1
!ifdef C64SCREEN {
    jsr newline
}
    lda ptr1
    ldx ptr1+1
    jsr disphexword
    lda #' '
    jsr charout
    jmp --
++
!ifdef C64SCREEN {
    jmp newline
} else {
    rts
}

store_assembly:
    ldx opidx
    lda opcodes, x
    ldy #0
    jsr sta_at_ptr3_y
    iny
    ldx size
    cpx #1
    beq ++
+   cpx #2
    bne +
    lda tmp2
    jsr sta_at_ptr3_y
    rts
+   cpx #3
    bne ++
    lda ptr1
    jsr sta_at_ptr3_y
    iny
    lda ptr1+1
    jsr sta_at_ptr3_y
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
    jsr find_inst_and_mode2 ; try again once
    bne ++
    lda tmp2
    sta ptr1
    lda #0
    sta ptr1+1
    rts ; Z true (EQ)
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
!ifndef MINIMUM {
; any C64
    jsr install_nmi64
}
+   ; restore registers
    ldx registerSP
    txs
    lda registerSR
    pha
    lda registerA
    ldx registerX
    ldy registerY
    plp
    jmp (ptr1)

chkcontinuedis:
    lda inputbuf,y
    cmp #'D'
    bne +
    lda inputbuf+1,y
    cmp #13
    bne +
    iny
    ldx #0 ; restore Z set
+   rts

chkcontinueasm:
    lda inputbuf,y
    cmp #'A'
    bne +
    lda inputbuf+1,y
    cmp #13
    bne +
    iny
    ldx #0 ; restore Z set
+   rts

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

chkspace:
    lda inputbuf, y
!ifndef MINIMUM {
    ; skip SHIFT-SPACES too on Commodore
    cmp #$A0
    beq +
}
    cmp #$20
    bne ++
+   iny
    jsr skipspaces
    lda #0 ; set Z
++  rts

skipspaces:
-   lda inputbuf, y
!ifndef MINIMUM {
    ; skip SHIFT-SPACES too on Commodore
    cmp #$A0
    beq +
}
    cmp #$20
    bne ++
+   iny
    bne -
++  rts

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

chkhexbyte:
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

chkbinbyte:
    ldx #8
-   jsr chkbindigit
    bne +
    dex
    bne -
    php ; save Z true
    lda tmp2
    plp ; restore Z true
+   rts

chkbindigit:
    lda inputbuf, y
    sec
    sbc #$30
    bcc +
    cmp #$02
    bcs +
    lsr ; rotate bit value into C
    rol tmp2 ; rotate bit value into tmp2
    iny ; advance in buffer
    lda #0 ; set Z
+   rts

chkaddr1cmd:
    lda inputbuf, y
+   cmp #'A'
    bne +
    ldx inputbuf+1, y
    cpx #13
    bne +
    iny
    jmp executeassemble
+   cmp #'D'
    bne +
    ldx inputbuf+1, y
    cpx #13
    bne +
    iny
    jmp executedisassemble
+   cmp #'R'
    bne +
    ldx inputbuf+1, y
    cpx #13
    bne +
    iny
    jmp executerun
+   rts

newline:
    lda #13
    jmp charout

space:
    lda #32
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

save_registers:
    php
    sta registerA
    stx registerX
    sty registerY
    pla
    sta registerSR
    tsx
    stx registerSP
    jmp +

execute_display_registers:
    pla ; remove return address
    pla
+
    ; need some normality
    cli
    cld

    jsr newline
    jsr display_registers
!ifndef MINIMUM {
    ; any C64
    jsr uninstall_nmi64
}
    jmp input_loop

; PC   NV-BDIZC .A .X .Y .S
; 1234 10111011 01 02 03 FF
display_registers:
    lda #<reg_header
    ldx #>reg_header
    jsr strout
    lda registerPC
    ldx registerPC+1
    jsr disphexword
    jsr space
    lda registerSR
    jsr dispbinbyte
    jsr space
    lda registerA
    jsr disphexbyte
    jsr space
    lda registerX
    jsr disphexbyte
    jsr space
    lda registerY
    jsr disphexbyte
    jsr space
    lda registerSP
    jsr disphexbyte
    jmp newline

!ifdef MINIMUM {

lda_at_ptr1_y:
    lda (ptr1),y
    rts

sta_at_ptr1_y:
    sta (ptr1),y
    rts

lda_at_ptr3_y:
    lda (ptr3),y
    rts

sta_at_ptr3_y:
    sta (ptr3),y
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MC6850
UART_DATA=$FFF8
UART_STCR=$FFF9

UART_INIT:
	ldx #0b00000111 ; 11=reset device
	stx UART_STCR
	inx ; #0b00001000 ; 0=rint disabled, 00=rtsn low, tint disabled 010=7e1 00=div 1
	sta UART_STCR
	rts
UART_OUT:
	pha
-	lda UART_STCR
	and #2
	beq - ; branch if TDRE=0, not finished transmitting
	pla
	pha
	and #$7F ; force 7-bit ASCII output, mask out any high bit
	sta UART_DATA
	pla
	rts
UART_IN:
-	lda UART_STCR
	and #1
	beq - ; branch if TDRF=0, not received
	lda UART_DATA
	; software "CAPS LOCK" because wozmon expects only uppercase
	cmp #$1C ; ^\ to act like a BRK, to return to monitor, if reading keys
	beq BREAK
    ; force lowercase alphabet to uppercase
    cmp #'a'
	bcc +
	cmp #'z'+1
	bcs +
	eor #$20
+	;ora #$80 ; Apple Model 1 expects 7-bit with marked parity (8th bit always set)
 	rts
UART_CHK: ; set or clear N flag based on read ready (character waiting)
	pha ; save A
	lda UART_STCR
	lsr ; put rightmost bit in carry
	pla ; restore A affects flags
	ror ; move carry to left bit, right bit to carry
	php ; push processor to save N
	rol ; restore A affects flags
	plp ; pull processor to restore N
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Processor start and interrupts

NMI: ; unused on minimum (no source of interrupt)
    rti

IRQ:
    pha
    php
    pla
    and #$10
    beq ++ ; not break
;BREAK HANDLER
    pla
    sta registerA
    pla
    sta registerSR
    pla
    cld
    sec
    sbc #2
    sta registerPC
    pla
    sbc #0
    sta registerPC+1
    lda #>save_registers
    pha
    lda #<save_registers
    pha
    lda registerSR
    pha
    lda registerA
    pha
++  pla
    rti

BREAK:
    jmp RESET

RESET:
    cld
    ldx #$FF
    txs
    jsr JUART_INIT
    cli
    jmp start
; !ifdef MINIMUM
} else { ; not MINIMUM

ptr1 = sta_at_ptr1_y + 1 ; and + 2

lda_at_ptr1_y:
    lda ptr1
    sta lda_ptr1_op+1
    lda ptr1+1
    sta lda_ptr1_op+2
lda_ptr1_op:
    lda $ffff, y
    rts

sta_at_ptr1_y:
    sta $ffff, y
    rts

ptr3 = sta_at_ptr3_y + 1 ; and + 2

lda_at_ptr3_y:
    lda ptr3
    sta lda_ptr3_op+1
    lda ptr3+1
    sta lda_ptr3_op+2
lda_ptr3_op:
    lda $ffff, y
    rts

sta_at_ptr3_y:
    sta $ffff, y
    rts

nmi64:
    sei
    sta registerA
    pla
    sta registerSR
    pla
    cld
    sta registerPC
    pla
    sta registerPC+1
    lda #>save_registers
    pha
    lda #<save_registers
    pha
    lda registerSR
    pha
    lda registerA
    rti

brk64:
    pla
    tay
    pla
    tax
    pla
    sta registerA
    pla
    sta registerSR
    pla
    cld
    sec
    sbc #2
    sta registerPC
    pla
    sbc #0
    sta registerPC+1
    lda #>save_registers
    pha
    lda #<save_registers
    pha
    lda registerSR
    pha
    lda registerA
    rti
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; data

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

nmodes = 13

mode_sorted:
!byte 9, 10, 11, 0, 2, 12, 3, 4, 1, 5, 6, 7, 8

!ifdef MINIMUM {
mode_0: !text "Accumulator", 0, "A", 0
mode_1: !text "None", 0, 8, 0
mode_2: !text "Immediate", 0, "#$12", 0
mode_3: !text "IndirectX", 0, "($12,X)", 0
mode_4: !text "IndirectY", 0, "($12),Y", 0
mode_5: !text "Relative", 0, "$1234 {-128 to +127}", 0
mode_6: !text "ZeroPage", 0, "$12", 0
mode_7: !text "ZeroPageX", 0, "$12,X", 0
mode_8: !text "ZeroPageY", 0, "$12,Y", 0
mode_9: !text "Absolute", 0, "$1234", 0
mode_10: !text "AbsoluteX", 0, "$1234,X", 0
mode_11: !text "AbsoluteY", 0, "$1234,Y", 0
mode_12: !text "Indirect", 0, "($1234)", 0
} else {
mode_0: !text "ACCUMULATOR", 0, "A", 0
mode_1: !text "NONE", 0, 20, 0
mode_2: !text "IMMEDIATE", 0, "#$12", 0
mode_3: !text "INDIRECTX", 0, "($12,X)", 0
mode_4: !text "INDIRECTY", 0, "($12),Y", 0
mode_5: !text "RELATIVE", 0, "$1234", 146, " [-128 TO +127]", 0
mode_6: !text "ZEROPAGE", 0, "$12", 0
mode_7: !text "ZEROPAGEX", 0, "$12,X", 0
mode_8: !text "ZEROPAGEY", 0, "$12,Y", 0
mode_9: !text "ABSOLUTE", 0, "$1234", 0
mode_10: !text "ABSOLUTEX", 0, "$1234,X", 0
mode_11: !text "ABSOLUTEY", 0, "$1234,Y", 0
mode_12: !text "INDIRECT", 0, "($1234)", 0
}

modes: ; table for easily displaying each mode_example
!word mode_0
!word mode_1
!word mode_2
!word mode_3
!word mode_4
!word mode_5
!word mode_6
!word mode_7
!word mode_8
!word mode_9
!word mode_10
!word mode_11
!word mode_12

; opcode table of byte values (opcodes), instructions, and addressing modes
nopcodes = 151
opcodes !byte $00,$01,$05,$06,$08,$09,$0A,$0D,$0E,$10,$11,$15,$16,$18,$19,$1D,$1E,$20,$21,$24,$25,$26,$28,$29,$2A,$2C,$2D,$2E,$30,$31,$35,$36,$38,$39,$3D,$3E,$40,$41,$45,$46,$48,$49,$4A,$4C,$4D,$4E,$50,$51,$55,$56,$58,$59,$5D,$5E,$60,$61,$65,$66,$68,$69,$6A,$6C,$6D,$6E,$70,$71,$75,$76,$78,$79,$7D,$7E,$81,$84,$85,$86,$88,$8A,$8C,$8D,$8E,$90,$91,$94,$95,$96,$98,$99,$9A,$9D,$A0,$A1,$A2,$A4,$A5,$A6,$A8,$A9,$AA,$AC,$AD,$AE,$B0,$B1,$B4,$B5,$B6,$B8,$B9,$BA,$BC,$BD,$BE,$C0,$C1,$C4,$C5,$C6,$C8,$C9,$CA,$CC,$CD,$CE,$D0,$D1,$D5,$D6,$D8,$D9,$DD,$DE,$E0,$E1,$E4,$E5,$E6,$E8,$E9,$EA,$EC,$ED,$EE,$F0,$F1,$F5,$F6,$F8,$F9,$FD,$FE
instidx !byte $0A,$22,$22,$02,$24,$22,$02,$22,$02,$09,$22,$22,$02,$0D,$22,$22,$02,$1C,$01,$06,$01,$27,$26,$01,$27,$06,$01,$27,$07,$01,$01,$27,$2C,$01,$01,$27,$29,$17,$17,$20,$23,$17,$20,$1B,$17,$20,$0B,$17,$17,$20,$0F,$17,$17,$20,$2A,$00,$00,$28,$25,$00,$28,$1B,$00,$28,$0C,$00,$00,$28,$2E,$00,$00,$28,$2F,$31,$2F,$30,$16,$35,$31,$2F,$30,$03,$2F,$31,$2F,$30,$37,$2F,$36,$2F,$1F,$1D,$1E,$1F,$1D,$1E,$33,$1D,$32,$1F,$1D,$1E,$04,$1D,$1F,$1D,$1E,$10,$1D,$34,$1F,$1D,$1E,$13,$11,$13,$11,$14,$1A,$11,$15,$13,$11,$14,$08,$11,$11,$14,$0E,$11,$11,$14,$12,$2B,$12,$2B,$18,$19,$2B,$21,$12,$2B,$18,$05,$2B,$2B,$18,$2D,$2B,$2B,$18
modeidx !byte $01,$03,$06,$06,$01,$02,$00,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$09,$03,$06,$06,$06,$01,$02,$00,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$01,$03,$06,$06,$01,$02,$00,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$01,$03,$06,$06,$01,$02,$00,$0C,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$03,$06,$06,$06,$01,$01,$09,$09,$09,$05,$04,$07,$07,$08,$01,$0B,$01,$0A,$02,$03,$02,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$08,$01,$0B,$01,$0A,$0A,$0B,$02,$03,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A,$02,$03,$06,$06,$06,$01,$02,$01,$09,$09,$09,$05,$04,$07,$07,$01,$0B,$0A,$0A

copyright
;                  1         2         3         4
;         1234567890123456789012345678901234567890
!text 13,"6502 MONITOR AND MINI-ASSEMBLER"
!ifdef C64TERMINAL {
    !text 13, "(TERMINAL VERSION)"
}
!text 13,"VWAS6502 (C) 2024 DAVID R. VAN WAGNER"
!text 13, "MIT LICENSE DAVEVW.COM"
!text 0

firsthelp
;!text 13, "? KEYWORD FOR EXAMPLE(S)"
!text 13, 13
!text "TYPE ? FOR HELP"
!text 13, 0

!ifndef MINIMUM {
; C64 only
extra_help:
    !text "X           (EXIT MONITOR)", 13
    !text "1000.2000 ", 34, "FILENAME", 34, " 08 S  (SAVE)", 13
    !text 0

!ifdef C64SCREEN {
page_disassemble !text "D",157,157,157,157,157,0
page_displaymemory !text ".",157,157,157,157,157,0
}

}

notimplemented !text "NOT IMPLEMENTED",13,0

generalhelp
!text "1000        (DISPLAY MEMORY CONTENTS)",13
!text "1000.100F   (DISPLAY RANGE CONTENTS)", 13
!text "1000.       (SCREENFULL OF MEMORY)", 13
!text ".           (NEXT SCREENFULL OF MEMORY)", 13
!text "1000: 01 02 (MODIFY MEMORY)", 13
!text "1000 R      (RUN PROGRAM - JMP)", 13
!text "1000 A      (ASSEMBLE AT ADDRESS)", 13
!text 0
generalhelp2
!text "1000 D      (DISASSEMBLE AT ADDRESS)", 13
!text "A           (ASSEMBLE MORE)", 13
!text "D           (DISASSEMBLE MORE)", 13
!text "?A          (LIST 6502 INSTRUCTIONS)", 13
!text "?ADC        (/ADC/ ADDRESSING MODES)", 13
!text "?MODE       (ADDRESSING MODES)", 13
!text 0
generalhelp3
!text "?.          (DISPLAY REGISTERS)", 13
!text ".1234       (MODIFY PC,FLAGS,REGISTERS)", 13
!text ".A:01       (MODIFY REGISTER A,X,Y,S,P)", 13
!text 0

modes_keyword !text "MODE", 0

reg_header !text " PC   NV-BDIZC .A .X .Y .S", 13, '.', 0

!ifdef MINIMUM {
opidx=$dfed
inidx=$dfee
mode=$dfef
size=$dff0
count=$dff1
len=$dff2
savepos=$dff3
tmp2=$dff4
flag=$dff5
ptr2=$dff6;/7
tmp=$dff8
registerA = $dff9
registerX = $dffa
registerY = $dffb
registerSP = $dffc
registerSR = $dffd
registerPC = $dffe;/f
} else {
opidx !byte 0
inidx !byte 0
mode !byte 0
size !byte 0
count !byte 0
len !byte 0
savepos !byte 0
tmp2 !byte 0
flag !byte 0
tmp !byte 0
ptr2 !word 0
savebrkvector !word 0
savenmivector !word 0
drive !byte 0
registerA !byte 0
registerX !byte 0
registerY !byte 0
registerSP !byte 0
registerSR !byte 0
registerPC !word 0
}

!ifdef MINIMUM {
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; JUMP table for some stability
* = $FFEE
JUART_INIT: JMP UART_INIT
JUART_OUT: JMP UART_OUT
JUART_IN: JMP UART_IN
JUART_CHK: JMP UART_CHK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 6502 vectors
* = $fffa
    !word NMI
    !word RESET
    !word IRQ
} else { // C64
    !if * > $a000 {
        !error "code/data overran $a000"
    }
}

finish = *

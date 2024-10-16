#!/bin/sh -x
export ACME=${USERPROFILE}/Downloads/acme0.97win/acme
export VICE=${USERPROFILE}/Downloads/GTK3VICE-3.8-win64/bin
export PROG=vwas6502
${ACME}/acme -DC64SCREEN=1 -DNEEDECHO=0 -f cbm -o ${PROG}.prg -l ${PROG}.lbl -r ${PROG}.lst ${PROG}.asm \
&& ${ACME}/acme -DMINIMUM=1 -DNEEDECHO=1 -o ${PROG}.bin -l ${PROG}-min.lbl -r ${PROG}-min.lst ${PROG}.asm \
&& ${VICE}/c1541 ${PROG}.d64 -attach ${PROG}.d64 8 -delete "${PROG} 32768" -write ${PROG}.prg "${PROG} 32768" \
&& ${VICE}/x64sc -moncommands ${PROG}.lbl -autostart ${PROG}.d64 >/dev/null 2>&1 &
#&& cp ${PROG}.bin c:/src/c-simple-emu6502-cbm/roms/minimum \
#&& (cd C:/src/c-simple-emu6502-cbm ; C:/src/c-simple-emu6502-cbm/src/c-simple-emu6502-cbm/x64/Release/c-simple-emu6502-cbm.exe) &

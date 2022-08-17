; > asmsyntax=tasm

; Virtual file handles for standard output and error.
stdout = 1
stderr = 2

; Newline \r\n
nl equ 10,13

; Path size (max length), corresponding to the MS-DOS max path length.
pathsz = 64

;
; Represents a row/column position.
;
struc Pos
    ; Column.
    col db ?
    ; Row.
    row db ?
ends

;
; Represents a far address, an address of memory in another segment.
;
struc FarAddr
    ; Offset.
    off dw 0
    ; Segment.
    seg dw 0
ends

;
; Bios Data Area.
;
struc Bda
    ; Interrupt Vector Table.
    ivt db 1024 dup(?)
    ; Base I/O address for serial and parallel ports.
    ports dw 8 dup(?)
    ; Equipment Flags.
    eflags dw ?
    ; Interrupt flag - POST.
    iflag db ?
    ; Memory size in KiB.
    memsz dw ?
    ; Reserved.
    dw ?
    ; Keyboard shift flags.
    kbflags dw ?
    ; Keyboard Alt numpad work area.
    kbalt db ?
    ; Keyboard buffer head.
    kbufhd dw ?
    ; Keyboard buffer tail.
    kbuftl dw ?
    ; Keyboard buffer.
    kbuf db 32 dup(?)
    ; Disk drives status registers.
    ddst db 11 dup(?)
    ; Screen mode.
    scrmode db ?
    ; Screen columns.
    scrcols dw ?
    ; Current video page size in bytes.
    scrvpsz dw ?
    ; Current video page offset relative to the start of VRAM.
    scrvpoff dw ?
    ; Cursor position (for video page 0).
    crspos0 Pos <?,?>
    ; Cursor position (for video page 1).
    crspos1 Pos <?,?>
    ; Cursor position (for video page 2).
    crspos2 Pos <?,?>
    ; Cursor position (for video page 3).
    crspos3 Pos <?,?>
    ; Cursor position (for video page 4).
    crspos4 Pos <?,?>
    ; Cursor position (for video page 5).
    crspos5 Pos <?,?>
    ; Cursor position (for video page 6).
    crspos6 Pos <?,?>
    ; Cursor position (for video page 7).
    crspos7 Pos <?,?>
    ; Cursor shape.
    crsshape dw ?
    ; Active video page.
    scrpage db ?
    ; Base port address for CRT controller.
    scrbport dw ?
    ; CGA color palette.
    scrpal db ?
    ; Reserved.
    db 5 dup(?)
    ; Daily timer counter.
    tcount dd ?
    ; Timer overflow flag.
    toflg db ?
    ; Keyboard break interrupt flag.
    brkflg db ?
    ; Soft reset flag.
    srflg db ?
    ; Status of last hard disk drive operation.
    hdopst db ?
    ; Number of hard disk drives.
    hdnum db ?
    ; Reserved.
    db ?
    ; Port offset to current hard disk disk adapter.
    hdaoff db ?
    ; LPT ports timeout.
    lptto db 4 dup(?)
    ; COM ports timeout.
    comto db 4 dup(?)
    ; Keyboard buffer start.
    kbufst dw ?
    ; Keyboard buffer end.
    kbufed dw ?
    ; Screen rows (0-based).
    scrrows db ?
    ; Number of scanlines per character.
    scrslc db ?
    ; Unused.
    db ?
    ; Video mode options (EGA+).
    scropts dw ?
    ; Video display data (VGA+).
    scrvga dw ?
    ; Floppy disk configuration area.
    fdconf db ?
    ; Hard disk drive status.
    hdst db 3 dup(?)
    ; Floppy disk drive information.
    fdinf db 7 dup(?)
    ; Keyboard mode/type and LED flags.
    kbflags2 dw ?
    ; Pointer to user wait complete flag.
    usrwflag FarAddr <?,?>
    ; User wait timeout value in microseconds.
    usrwto dd ?
    ; RTC wait function flag.
    rtcflags db ?
    ; LANA DMA channel flags and status.
    lanadma db 3 dup(?)
    ; Hard disk interrupt vector.
    hdiv FarAddr <?,?>
    ; BIOS video Save/Override pointer table address.
    scrsopt FarAddr <?,?>
    ; Reserved.
    db 8 dup(?)
    ; Keyboard NMI control and break pending flags.
    kbflags3 db 5 dup(?)
    ; Port 60 single byte queue.
    p60bq db ?
    ; Scan code of last key.
    kblksc db ?
    ; NMI head pointer.
    nmihd db ?
    ; NMI tail pointer.
    nmitl db ?
    ; NMI scan code buffer.
    nmibuf db 16 dup(?)
    ; Day counter.
    tday dw ?
    ; Intra-applications communications area.
    iac db 16 dup(?)
ends

;
; VRAM segment areas.
;
struc Vram
    ; Monochrome screen video memory.
    bw db 32768 dup(?)
    ; Color screen video memory.
    color db 32768 dup(?)
ends

;
; Command line tail.
;
struc Clt
    ; Length.
    length db ?
    ; Tail, terminated by a 0d (carriage return).
    data db 127 dup(?)
ends

;
; Program segment prefix.
;
struc Psp
    ; Exit matchine code (INT 20h).
    exit dw ?
    ; Segment of first byte beyond allocated memory.
    memtop dw ?
    ; Reserved.
    db ?
    ; CP/M-like far call entry into DOS.
    cpmfc db 5 dup(?)
    ; Terminate address of previous program.
    prevta FarAddr <?,?>
    ; Break address of previous program.
    prevba FarAddr <?,?>
    ; Critical error address of previous program.
    prevcea FarAddr <?,?>
    ; Parent PSP segment.
    prntpsp dw ?
    ; Job File Table.
    jft db 20 dup(?)
    ; Environment variable segment.
    envseg dw ?
    ; SS:SP on entry to last INT 21h call.
    lastsp FarAddr <?,?>
    ; Job File Table size.
    jftsz dw ?
    ; Job File Table pointer.
    jftptr FarAddr <?,?>
    ; PSP of previous program.
    prevpsp FarAddr <?,?>
    ; Reserved.
    db 4 dup(?)
    ; DOS version to return.
    dosver dw ?
    ; Reserved.
    db 14 dup(?)
    ; DOS function dispatcher (INT 21h + RETF).
    dosfd db 3 dup(?)
    ; Reserved.
    db 9 dup(?)
    ; Unopened standard FCB1.
    fcb1 db 16 dup(?)
    ; Unopened standard FCB2.
    fcb2 db 20 dup(?)
    ; Command-line tail.
    clt Clt ?
ends

;
; Disk transfer address.
;
struc Dta
    ; Reserved.
    db 21 dup(?)
    ; Attributes of matching file.
    attr db ?
    ; File time.
    ftime dw ?
    ; File date.
    fdate dw ?
    ; File size.
    fsize dd ?
    ; ASCIIZ filename.
    fname db 13 dup(?)
ends

;
; Execution parameters.
;
; Used by INT 21,4B (Load and execute program) to set up the PSP of the
; executed program.
;
struc ExecParams
    ; Environment segment (0 if using parent).
    envseg dw 0
    ; Pointer to command line tail.
    clt FarAddr <0,0>
    ; Pointer to FCB passed as first.
    fcb1 FarAddr <0,0>
    ; Pointer to FCB passed as second.
    fcb2 FarAddr <0,0>
ends

;
; Represents an ASCIIZ filesystem path.
;
; In MS-DOS, max path length is 64, but ASCIIZ requires a NUL terminator at
; the end.
;
struc Path
    db '\'
    db pathsz dup(0)
ends

;
; Represents an ASCIIZ filesystem path with drive information.
;
struc FullPath
    ; Drive letter.
    drive db 'C'
    ; Drive designator.
    db ':'
    ; Absolute path.
    path Path {}
ends

; PSP address.
psp = Psp ptr 0000h

; Command line tail address. Can also be accessed via [psp.clt].
clt = Clt ptr 0080h

; Default disk transfer area. Overlaps with command line tail.
dta = Dta ptr 0080h

; Content of the bios segment.
segment bdaseg at 0000h
    bda Bda ?
ends

; Content of the VRAM segment.
; The location counter is expected to overflow here.
nowarn lco
segment vramseg at 0b000h
    vram Vram ?
ends
warn lco


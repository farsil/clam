; > asmsyntax=tasm
ideal
model tiny
jumps
smart
p286

; BIOS/DOS definitions.
include "system.inc"

; Pointer to nothing. In clam address 0 it is reserved for the PSP, so it's
; never going to be a valid address.
null = 0

; Command size (max length).
cmdsz = 8

; Title size (max length).
titlesz = 36

; Deque size (max number of entries).
dequesz = 16

;
; Computes the minimum between two registers.
;
; @@left first register, will hold the minimum value after comparison
; @@right other register
;
macro min @@left:req, @@right:req
local @@skip
    cmp @@left, @@right
    jle @@skip
    mov @@left, @@right
@@skip:
endm

;
; Computes the maximum between two registers.
;
; @@left first register, will hold the maximum value after comparison
; @@right other register
;
macro max @@left:req, @@right:req
local @@skip
    cmp @@left, @@right
    jge @@skip
    mov @@left, @@right
@@skip:
endm

;
; Replaces the enter instruction. Uses more space but it's slightly faster.
;
; @@localsize the amount of bytes to reserve for stack allocation
;
macro enter @@localsize
    push bp
    mov bp, sp
    ifnb <@@localsize>
        if @@localsize
            sub sp, @@localsize
        endif
    endif
endm

;
; Static string data.
;
struc Strings
    ; Empty line.
    eol db nl,'$'
    ; Message to display when the user doesn't provide a path via command line
    ; arguments.
    usage db \
        "Batch files launcher.",nl,nl,\
        "CLAM path",nl,nl,\
        "  path  ",\
        "Path to scan for batch files.",nl,nl,'$'
    ; Message to display at startup.
    welcome db nl,\
        "Welcome to clam!",nl,nl,\
        "  *  List programs",nl,\
        "  .  Exit to DOS",nl,nl,'$'
    ; Message to display when prompting the user for a keypress.
    continue db \
        "Press any key to continue...$"
    ; Message to display after the number of programs is printed by the list
    ; virtual command.
    programs db \
        " program(s)",nl,'$'
    ; Message to display if the supplied drive/path isn't valid.
    badpath db \
        "Illegal path.",nl,'$'
    ; Message to display if no suitable program is matched.
    badprog db \
        "Invalid program.",nl,'$'
    ; Message to display if COMSPEC can't be found.
    badenv db \
        "Bad program environment.",nl,'$'
    ; ASCIIZ string that represents the *.BAT file glob.
    glob db "*.BAT",0
    ; COMSPEC environment variable name that is used for lookup in the
    ; environment segment.
    comspec db "COMSPEC="
ends

;
; Contains the different segments.
;
; VRAM and BDA segments are always at the same location, but it's convenient
; to have them in memory because you can't mov an immediate into a segment
; register.
;
struc Segments
    ; The VRAM segment, always 0B00.
    vram dw 0b000h
    ; The Bios Data Area segment, always 0000.
    bda dw 0000h
    ; The program segment, as found in ds.
    clam dw ?
ends

;
; Textual representation of a program command.
;
struc Cmd
    ; The command string, lacking the NUL terminator.
    db cmdsz dup(0)
    ; The command string actual length.
    length db 0
ends

;
; Textual representation of a program title.
;
struc Title
    ; The title string, lacking the NUL terminator.
    db titlesz dup(0)
    ; The title string length.
    length db 0
ends

;
; A program that can be executed.
;
struc Prog
    ; The command used to launch and identify the program.
    ; For non-virtual programs, it corresponds to the batch file basename.
    cmd Cmd {}
    ; The program title.
    title Title {}
ends

;
; The program database.
;
struc ProgDb
    ; Pointer to the position array. Note that the array elements are pointers
    ; to the entries.
    entries dw null
    ; Number of entries.
    count dw 0
ends

;
; Default programs data.
;
; Offsets are defined with labels because TASM does not allow proper nested
; struc initialization. They have invalid DOS names so that they cannot
; conflict with real files.
;
struc DefaultProgs
    ; The list program, used to list all the registered programs.
    label list Prog
            Cmd<'*', 1>
            Title<"List programs", 13>
    ; The exit program, used to exit to DOS.
    label exit Prog
            Cmd<'.', 1>
            Title<"Exit to DOS", 11>
ends

;
; A double-ended queue node that stores pointers to programs.
;
struc Node
    ; The node value.
    value dw null
    ; The previous node in the queue.
    prev dw null
    ; The next node in the queue.
    next dw null
ends

;
; Double-ended queue.
;
struc Deque
    ; The pointer to the first element of the queue.
    head dw null
    ; The number of elements in the queue.
    count db 0
    ; The elements array.
    nodes Node dequesz dup({})
ends

dataseg
    ; VRAM base offset.
    vrambase dw offset vramseg:vram.color
    ; The static strings.
    strings Strings {}
    ; Default programs.
    defprogs DefaultProgs {}
    ; The program segments.
    segments Segments {}
    ; Current working directory.
    workdir FullPath ?
    ; Directory that contains the batch files.
    batdir FullPath ?
    ; Absolute path to the COMMAND.COM interpreter.
    comspec FullPath ?
    ; Program database.
    progdb ProgDb ?
    ; Stack and initial stack pointer.
    db 512 dup(?)
    initsp = $
    ; First available byte. Memory from this point onwards is available
    ; for allocation. It will be used to store the program database entries
    ; and their position in the sorted array.
    fab = $

codeseg
    startupcode
    mov sp, offset initsp
    mov bp, sp
    mov [segments.clam], ds

    ; Backs up current drive, the DOS service is 0-based (a=0, b=1, etc).
    mov ah, 19h
    int 21h
    add al, 'A'
    mov [workdir.drive], al

    ; Write leading slash and drive designator.
    mov [word ptr workdir + 1], '\:'

    ; Backs up current directory, the DOS service does not add leading slash.
    mov ah, 47h
    xor dl, dl
    lea si, [offset workdir.path + 1]
    int 21h

    ; Support hercules cards.
    mov es, [segments.bda]
    mov ax, [es:bda.eflags]
    mov es, [segments.clam]
    test al, 30h
    jpo @@load
    mov [vrambase], offset vramseg:vram.bw

@@load:
    ; Parses the enviroment looking for the COMSPEC variable.
    push offset comspec
    push [psp.envseg]
    call parseenv

    ; Parses the command line tail looking for the batch path.
    push offset batdir
    call parseclt

    ; Loads the program database.
    push offset fab
    push offset progdb
    push offset batdir
    call loaddb

    ; Resize allocated memory to make room for program execution.
    ; bx is the allocated memory in paragraphs (16 bytes), rounded up.
    mov bx, ax
    add bx, 15
    shr bx, 4
    mov ah, 4ah
    int 21h

    ; Sorts the program database, necessary for the list command and to ease
    ; insertion into the matches priority queue.
    push offset progdb
    call sortdb

    lea dx, [strings.welcome]
    mov ah, 09h
    int 21h

@@loop:
    ; ax = cursor line offset (screen columns * cursor row).
    ; bx = screen columns.
    mov es, [segments.bda]
    mov bx, [es:bda.scrcols]
    mov ax, bx
    mul [es:bda.crspos0.row]
    mov es, [segments.clam]

    ; cx =  VRAM line offset (VRAM base offset + cursor line offset * 2).
    mov cx, ax
    shl cx, 1
    add cx, [vrambase]

    ; Prompt the user for input.
    push ax
    push bx
    push cx
    push offset progdb
    call prompt

    ; Checks if the match is a virtual or real program.
    cmp ax, offset defprogs.exit
    je @@exit
    cmp ax, offset defprogs.list
    je @@list
    or ax, ax
    jnz @@exec

    ; No match.
    mov ah, 09h
    lea dx, [strings.badprog]
    int 21h
    jmp @@loop

@@exec:
    ; Executes a program.
    push ax
    push offset batdir
    push offset comspec
    call exec
    jmp @@loop

@@list:
    ; Prints the program database.
    push offset progdb
    call listdb
    jmp @@loop

@@exit:
    ; Exits this program.
    push offset workdir
    call exit

;
; Parses the environment segment.
;
; @@envseg the environment segment
; @@comspec pointer to the path that will hold the COMSPEC value
;
; Scans in the environment segments for the COMSPEC environment variable
; and saves it to the provided path. 
;
proc parseenv
arg @@envseg:word, @@comspec:word = @@argsize
    enter 
    mov dx, es
    mov es, [@@envseg]
    ; The environment variables start from address 0.
    xor di, di

@@scan:
    ; Check if the current env var is COMSPEC.
    lea si, [strings.comspec]
    mov cx, 4
    repe cmpsw
    jz @@endscan

    ; Scan for the end of the current env var (marked by a 0).
    xor al, al
    ; The COMSPEC length is path size + drive designator and letter
    mov cl, pathsz + 2
    repne scasb

    ; The end of the environment block is marked by a lone 0, if we reached the
    ; end without finding COMSPEC we need to display an error.
    cmp [byte ptr es:di + 1], 0
    je @@badenv
    jmp @@scan

@@endscan:
    ; Copies the environment variable from si to @@comspec.
    mov si, di
    mov di, [@@comspec]
    xor bx, bx

@@move:
    mov al, [es:si + bx]
    mov [di + bx], al
    ; Copies until a 0 is found.
    or al, al
    jz @@return
    inc bx
    jmp @@move

@@return:
    mov es, dx
    leave
    ret @@argsize

@@badenv:
    lea dx, [strings.badenv]
    mov ah, 09h
    int 21h
    exitcode 1
endp

;
; Parses the command line tail.
;
; @@path pointer to the path of the batch files
;
; Parses the command line tail for a path that will be saved in the provided 
; path.
;
proc parseclt
arg @@path:word = @@argsize
    enter
    ; If the command line tail is empty show usage.
    xor ch, ch
    mov cl, [clt.length]
    or cx, cx
    jz @@usage

    ; If all chars are spaces (or if we reach the end of clt) show usage.
    lea di, [clt.data]
    mov al, ' '
    repe scasb
    jz @@usage

    ; First non-space character.
    lea si, [di - 1]

    ; Copy the path from the command line tail to the specified path.
    mov di, [@@path]
    ; We went one further with repe scasb.
    inc cx

    ; Make sure it doesn't move more than drive letter + drive designator ':' + 
    ; max path length.
    min cx, pathsz + 2

@@move:
    lodsb
    cmp al, ' '
    je @@return
    stosb
    dec cx
    jnz @@move

@@return:
    ; Makes sure the path has both the drive designator and the leading slash.
    mov bx, [@@path]
    mov [word ptr bx + 1], '/:'

    ; If the path is too short, NUL needs to be placed after the drive
    ; designator and letter.
    add bx, 3
    max bx, di
    ; The command line tail is not ASCIIZ, but path needs to.
    mov [byte ptr bx], 0
    leave
    ret @@argsize

@@usage:
    lea dx, [strings.usage]
    mov ah, 09h
    int 21h
    exitcode 1
endp

;
; Parses a program command.
;
; @@src pointer to the source where to parse the command from
; @@prog pointer to the program where to save the command to
;
; Copies the basename of the program command from the specified source to the
; specified program.
;
proc parsecmd
arg @@src:word, @@prog:word = @@argsize
    enter
    mov si, [@@src]
    mov di, [@@prog]
    if offset (Prog).cmd
        lea di, [di + Prog.cmd]
    endif
    mov cx, cmdsz

    ; Back up start of prog cmd, it will be needed for length calculation.
    mov bx, di

@@move:
    lodsb
    cmp al, '.'
    je @@return
    stosb
    dec cx
    jnz @@move

@@return:
    sub cx, 8
    neg cx
    mov [bx + Cmd.length], cl
    leave
    ret @@argsize
endp

;
; Parses a program title.
;
; @@src pointer to the source where to parse the title from
; @@length source length
; @@prog pointer to the program where to save the title to
;
; Copies the program title from the specified source to the specified program.
; This procedure strips leading and trailing whitespcae from the title
; definition. A title is detected if the first three bytes of the source
; are :::.
;
proc parsetitle
arg @@src:word, @@length:word, @@prog:word = @@argsize
    enter
    ; If bytes are too few don't bother looking for a title, prevents from
    ; reading invalid memory.
    mov bx, [@@length]
    cmp bx, 4
    jl @@nomove

    ; Check for the leading :::.
    mov di, [@@src]
    cmp [word ptr di], "::"
    jnz @@nomove
    cmp [byte ptr di+2], ':'
    jnz @@nomove

    ; Look for the first space.
    mov al, ' '
    ; The title marker ::: was already parsed.
    lea cx, [bx - 3]
    add di, 3
    repe scasb
    ; If counter is exhausted there are only spaces in the line.
    jz @@nomove

    dec di
    ; si now points to the start of the title.
    mov si, di

    ; If si is a \r we reached the end of the line without any valid title.
    cmp [byte ptr si], 0dh
    je @@nomove

    ; Look for the carriage return \r.
    mov al, 0dh
    inc cx
    repne scasb
    ; Cancels the extra increment when counter is exhausted, which may happen
    ; if the line is too long.
    jnz @@backwards
    dec di

@@backwards:
    ; di points to the first character before carriage return/end of file.
    dec di
    ; If it is not a space it means we don't need to scan for the first
    ; non-space character, we already found it.
    cmp [byte ptr di], ' '
    jne @@count

    ; Scan backwards for the first non-space character, which is the end of
    ; the title.
    std
    mov al, ' '
    mov cx, di
    sub cx, [@@src]
    repe scasb
    cld
    ; Cancel the extra increment when the counter is exhausted.
    jz @@count
    inc di

@@count:
    ; di points to the character before the end of the title.
    lea cx, [di + 1]
    sub cx, si
    ; Don't move more than titlesz bytes.
    min cx, titlesz

@@move:
    mov di, [@@prog]
    if offset (Prog).title
        lea di, [di + Prog.title]
    endif
    mov [di + Title.length], cl
    rep movsb
    leave
    ret @@argsize

@@nomove:
    mov di, [@@prog]
    mov [byte ptr di + Prog.title.length], 0
    leave
    ret @@argsize
endp

;
; Loads the program database.
;
; @@batdir pointer to the path to the batch files
; @@db pointer to the database
; @@freemem address of available memory where entries will be stored
;
; This procedure loads all entries from the program database and stores them
; in the memory area after the provided address. It also stores the position
; array of both the two virtual EXIT and LIST programs and the real programs
; after the program entries.
;
; Returns the pointer to the first available byte after the database is loaded.
;
proc loaddb
arg @@batdir:word, @@db:word, @@freemem:word = @@argsize
local @@buf:byte:80, @@curprog:word, @@count:word = @@localsize
    enter @@localsize
    mov ax, [@@freemem]
    mov [@@curprog], ax
    mov [@@count], 2

    ; Change current drive.
    mov ah, 0eh
    mov bx, [@@batdir]
    mov dl, [bx + FullPath.drive]
    or dl, 20h
    sub dl, 'a'
    int 21h
    jc @@badpath

    ; Change current path.
    mov ah, 3bh
    lea dx, [bx + FullPath.path]
    int 21h
    jc @@badpath

    ; Finds first batch file in current working dir.
    mov ah, 4eh
    lea dx, [strings.glob]
    int 21h

@@load:
    ; If there are no more files, return.
    jc @@return

    push [@@curprog]
    push offset dta.fname
    call parsecmd

    ; Open batch filename.
    mov ax, 3d00h
    lea dx, [dta.fname]
    int 21h

    ; bx holds the opened file handle.
    mov bx, ax

    ; Read first 80 characters into buffer.
    mov ah, 3fh
    lea dx, [@@buf]
    mov cx, 80
    int 21h

    ; ax holds the number of bytes read after the interrupt.
    mov cx, ax

    ; Close the file.
    mov ah, 3eh
    int 21h

    push [@@curprog]
    push cx
    push dx
    call parsetitle

@@next:
    add [@@curprog], size Prog
    inc [@@count]

    ; Finds next batch file.
    mov ah, 4fh
    int 21h
    jmp @@load

@@return:
    ; Write in the program database the program count and the entries pointer.
    mov ax, [@@count]
    mov bx, [@@db]
    mov [bx + ProgDb.count], ax
    mov di, [@@curprog]
    mov [bx + ProgDb.entries], di

    ; Write default programs pointers in the position array first.
    mov [word ptr di], offset defprogs.list
    mov [word ptr di + 2], offset defprogs.exit

    ; ax = first program pointer.
    ; dx = first entry in the position array, which marks the end of the
    ; program area.
    mov ax, [@@freemem]
    mov dx, di
    ; Skip the default program entries.
    add di, 4

@@entries:
    ; Write all the program pointers unordered into the position array.
    stosw
    add ax, size Prog
    cmp ax, dx
    jne @@entries

    ; Returns the pointer to the first available byte.
    mov ax, di
    leave
    ret @@argsize

@@badpath:
    mov ah, 09h
    lea dx, [strings.badpath]
    int 21h
    exitcode 1
endp

;
; Compares two programs.
;
; @@left the first program
; @@right the other program
;
; This procedure is meant to be used inside qsort. It compares two programs by 
; title, case insensitive. Sets the appropriate flags after returning and 
; preserves registers si, di and bx.
;
proc progcmp
arg @@left:word, @@right:word = @@argsize
    enter
    push si
    push di
    push bx

    mov si, [@@left]
    mov di, [@@right]
    ; Return (preserving ZF) if both pointers point to the same program.
    cmp si, di
    je @@return

    mov si, [si]
    mov di, [di]
    mov bx, offset (Prog).title

@@loop:
    mov dl, [si + bx]
    mov dh, [di + bx]
    ; Case insensitive comparison.
    or dx, 2020h
    cmp dl, dh
    jne @@return
    inc bx
    jmp @@loop

@@return:
    pop bx
    pop di
    pop si
    leave
    ret @@argsize
endp

;
; Recursive implementation of quick sort.
;
; @@low pointer to the start of the partition
; @@high pointer to the end of the partition
;
; Comparisons are done by calling progcmp. It always recurses into the
; smaller partition first, guaranteeing O(logn) space occupation on the stack.
;
proc qsort
arg @@low:word, @@high:word = @@argsize
    enter
    ; Only sort the array if low < high.
    mov ax, [@@low]
    mov cx, [@@high]
    cmp ax, cx
    jge @@return

    ; Calculates the pivot as the median between cx and ax.
    mov bx, cx
    sub bx, ax
    shr bx, 1
    ; This and handles odd array sizes.
    and bl, 0feh
    add bx, ax

    ; si = low, di = high.
    mov si, ax
    mov di, cx

@@loop:
    ; Stop looping if si >= di.
    cmp si, di
    jge @@recurse

@@incsi:
    ; Increment si while progcmp(si, bx) >= 0 and si < high.
    cmp si, cx
    jge @@decdi

    push bx
    push si
    call progcmp
    jg @@decdi

    add si, 2
    jmp @@incsi

@@decdi:
    ; Decrement di while progcmp(di, bx) >= 0.
    push bx
    push di
    call progcmp
    jle @@swap

    sub di, 2
    jmp @@decdi

@@swap:
    ; Don't swap and exit the loop if si >= di.
    cmp si, di
    jge @@recurse

    mov dx, [si]
    xchg dx, [di]
    mov [si], dx
    jmp @@loop

@@recurse:
    mov dx, [bx]
    xchg dx, [di]
    mov [bx], dx

    ; si = di - 2 - ax.
    lea dx, [di - 2]
    mov si, dx
    sub si, ax

    ; di = cx - di - 2.
    lea bx, [di + 2]
    mov di, cx
    sub di, bx

    ; Recurse into the shorter partition first to guarantee O(logn)
    ; space occupation.
    cmp si, di
    jg @@sort

    ; Swap partition boundaries.
    xchg cx, dx
    xchg ax, bx

@@sort:
    ; Same as invoking qsort(ax, dx) followed by qsort(bx, cx), but preserves
    ; all registers.
    push cx
    push bx
    push dx
    push ax
    call qsort
    call qsort

@@return:
    leave
    ret @@argsize
endp

;
; Sorts the program database in place.
;
; @@db pointer to the program database
;
proc sortdb
arg @@db:word = @@argsize
    enter
    mov bx, [@@db]
    ; ax = low, cx = high (calculated as low + 2 * count).
    mov ax, [bx + ProgDb.entries]
    mov cx, [bx + ProgDb.count]
    dec cx
    shl cx, 1
    add cx, ax

    push cx
    push ax
    call qsort
    leave
    ret @@argsize
endp

;
; Writes the program description in the provided buffer.
;
; @@buf pointer to the buffer that will contain the program description
; @@limit the point where to stop blanking the buffer
; @@match pointer to the matches deque node
;
; This function writes the program description but also blanks the rest of the
; buffer to avoid printing dirty data. For efficiency purposes no overflow
; checks are being done on the buffer when writing, so its length should be
; at least 40 bytes larger than the limit.
;
proc writedesc
arg @@buf:word, @@limit:word, @@match:word = @@argsize
    enter
    mov bx, [@@match]
    xor cx, cx
    mov dx, [@@buf]
    mov di, dx

    ; Don't write description if current match is null.
    or bx, bx
    jz @@blank
    mov bx, [bx + Node.value]

    ; Append to the line the string "(Program title)".
    lea si, [bx + Prog.title]
    mov cl, [si + Title.length]

    ; Display the command filename if title is empty.
    or cx, cx
    je @@notitle

    ; Leave a blank space after the last character in the query.
    mov [word ptr di], "( "
    add di, 2
    rep movsb
    mov [byte ptr di], ')'
    inc di
    jmp @@blank

@@notitle:
    ; Append to the line the string "(Run PROGNAME.BAT)".
    lea si, [bx + Prog.cmd]
    mov cl, [si + Cmd.length]

    ; Leave two blank spaces after the last character in the query.
    mov [word ptr di], "( "
    mov [word ptr di + 2], "uR"
    mov [word ptr di + 4], " n"
    add di, 6
    rep movsb
    mov [word ptr di], "B."
    mov [word ptr di + 2], "TA"
    mov [byte ptr di + 4], ')'
    add di, 5

@@blank:
    ; Blank out rest of the line after writing the program description.
    xor ax, ax
    ; dx is the pointer to the buffer.
    mov cx, dx
    add cx, [@@limit]
    ; di is the pointer to the last drawn char.
    sub cx, di
    ; We are using stosw, so we need to add 1 and divide by two.
    inc cx
    shr cx, 1
    rep stosw
    leave
    ret @@argsize
endp

;
; Sets the screen cursor position.
;
; @@base base position of the cursor
; @@offset offset of the cursor relative to the base position
;
; The effective hardware position of the cursor is computed as @@base +
; @@offset. In prompt, these two values are passed as:
;
;   @@base = cursor row * screen columns
;   @@offset = cursor column
;
; The position is then set by writing into the hardware ports.
;
proc setcrs
arg @@base:word, @@offset:word = @@argsize
    enter
    mov bx, [@@base]
    add bx, [@@offset]

    mov es, [segments.bda]
    mov dx, [es:bda.scrbport]
    mov es, [segments.clam]

	mov al, 0fh
    mov ah, bl
	out dx, ax

	mov al, 0eh
    mov ah, bh
	out dx, ax

    leave
    ret @@argsize
endp

;
; Writes a line into character VRAM.
;
; @@src pointer to the source buffer
; @@length amount of characters to write
; @@vram destination VRAM offset
;
proc drawln
arg @@src:word, @@length:word, @@vram:word = @@argsize
    enter
    mov es, [segments.vram]

    ; Set character attributes (gray on black).
    mov ah, 07h
    mov si, [@@src]
    mov di, [@@vram]
    mov cx, [@@length]

@@write:
    lodsb
    stosw
    dec cx
    jne @@write

    mov es, [segments.clam]
    leave
    ret @@argsize
endp

;
; Adds a node holding the provided value to the end of the matches deque.
;
; @@deque pointer to the matches deque
; @@value the value to add
;
; Does nothing if the deque is full.
;
proc addlast
arg @@deque:word, @@value:word = @@argsize
    enter
    mov bx, [@@deque]
    mov al, [bx + Deque.count]
    or al, al
    je @@empty
    ; Do not add anything if the deque is full.
    cmp al, dequesz
    jge @@return

    ; si = head, di = tail, dx = deque.
    mov si, [bx + Deque.head]
    mov di, [si + Node.prev]
    mov dx, bx

    inc [bx + Deque.count]

    ; bx = new node (deque pointer + Deque.nodes + size Node * count).
    mov ah, size Node
    mul ah
    add bx, offset (Deque).nodes
    add bx, ax

    ; new.value = value, new.next = head, new.prev = tail.
    mov ax, [@@value]
    mov [bx + Node.value], ax
    mov [bx + Node.next], si
    mov [bx + Node.prev], di

    ; head.prev = new, tail.next = new
    mov [si + Node.prev], bx
    mov [di + Node.next], bx
    jmp @@return

@@empty:
    lea di, [bx + Deque.nodes]
    inc [bx + Deque.count]
    mov [bx + Deque.head], di

    mov [di + Node.prev], di
    mov [di + Node.next], di
    mov ax, [@@value]
    mov [di + Node.value], ax

@@return:
    leave
    ret @@argsize
endp

;
; Adds a node holding the provided value to the beginning of the matches deque.
;
; @@deque pointer to the matches deque
; @@value the value to add
;
; Does nothing if the deque is full.
;
proc addfirst
arg @@deque:word, @@value:word = @@argsize
    enter
    push [@@value]
    push [@@deque]
    call addlast

    mov bx, [@@deque]
    mov si, [bx + Deque.head]
    mov si, [si + Node.prev]
    mov [bx + Deque.head], si
    leave
    ret @@argsize
endp

;
; Finds programs that match the specified query.
;
; @@db pointer to the program database
; @@query pointer to the query to match
; @@length the query length
; @@matches pointer to the resulting matches deque
;
; Searches in the program database for matching programs. The match is 
; performed by looking for exact command matches, or if the query is contained 
; inside a title. Both string comparisons are case insensitive.
;
; The supplied matches deque is cleared of all data before the search begins.
;
proc find
arg @@db:word, @@query:word, @@length:word, @@matches:word = @@argsize
local @@entry:word = @@localsize
    enter @@localsize
    mov bx, [@@matches]
    mov [bx + Deque.count], 0
    mov [bx + Deque.head], null

    ; No matches if the query length is 0.
    cmp [@@length], 0
    jz @@return

    mov bx, [@@db]
    mov cx, [bx + ProgDb.count]
    mov bx, [bx + ProgDb.entries]
    mov [@@entry], bx

@@loop:
    mov si, [@@query]
    ; Remember that the entries in the sorted arrays are pointers to programs.
    mov di, [@@entry]
    mov di, [di]

@@cmd:
    xor bh, bh
    mov bl, [di + Prog.cmd.length]
    ; Don't bother comparing byte per byte if the length is different.
    cmp bx, [@@length]
    jne @@title

@@cmdcmp:
    ; Compare command byte per byte, in reverse order, case insensitive.
    mov ah, [si + bx - 1]
    mov al, [di + bx + offset (Prog).cmd - 1]
    or ax, 2020h
    cmp al, ah
    jne @@title
    dec bx
    jnz @@cmdcmp

    ; If bx = 0 the command is matched, add to the head if there's enough room.
    push di
    push [@@matches]
    call addfirst
    jmp @@next

@@title:
    ; Skip title match if query length is larger than title length.
    mov bx, [@@length]
    xor dh, dh
    mov dl, [di + Prog.title.length]
    sub dx, bx
    js @@next
    ; Substring search iterations are title length - query length + 1.
    inc dx

@@titleshift:
    mov bx, [@@length]

@@titlecmp:
    ; Compare title byte per byte, in reverse order, case insensitive.
    mov ah, [si + bx - 1]
    mov al, [di + bx + offset (Prog).title - 1]
    or ax, 2020h
    cmp al, ah
    jne @@titlenext
    dec bx
    jnz @@titlecmp

    ; If bx = 0 the title is matched, add to the tail if there's enough room.
    mov di, [@@entry]
    push [di]
    push [@@matches]
    call addlast
    jmp @@next

@@titlenext:
    inc di
    dec dx
    jnz @@titleshift

@@next:
    add [@@entry], 2
    dec cx
    jnz @@loop

@@return:
    leave
    ret @@argsize
endp

;
; Prompts the user for the program to execute.
;
; @@db pointer to the program database used to find matches
; @@vram the character VRAM offset to write the prompt line to
; @@scrcols the number of screen columns
; @@crsbase the base offset of the hardware cursor
;
; This function simulates a prompt and attempts to find a program by querying
; what the user types against the database.
;
; When the user presses ENTER, the current match is accepted and returned,
; whether it is a pointer to the matched program or null (0) if no programs
; could be matched.
;
; If the user inputs the CTRL + C combination, a pointer to the exit virtual
; program is returned no matter the query.
;
proc prompt
arg @@db:word, @@vram:word, @@scrcols:word, @@crsbase:word = @@argsize
local @@line:byte:128, @@crscol:word, @@endq:word, @@find:byte, \
      @@matches:Deque, @@curmatch:word = @@localsize
    @@startq = 2
    enter @@localsize

    ; Prepend prompt string to the line buffer.
    mov [word ptr @@line], " @"

    ; Blank the remainder of the line buffer.
    xor ax, ax
    mov cx, [@@scrcols]
    ; Decrement because we already set the prompt string.
    dec cx
    shr cx, 1
    lea di, [@@line + @@startq]
    rep stosw

    mov [@@crscol], @@startq
    mov [@@endq], @@startq
    mov [@@curmatch], null
    mov [@@find], 0

@@loop:
    cmp [@@find], 0
    je @@writedesc

    ; Match from the start of the query for at most endq - startq chars.
    lea ax, [@@matches]
    lea bx, [@@line + @@startq]
    mov cx, [@@endq]
    sub cx, @@startq

    ; Find all the possible results.
    push ax
    push cx
    push bx
    push [@@db]
    call find

    mov bx, [@@matches.head]
    mov [@@curmatch], bx
    mov [@@find], 0

@@writedesc:
    ; Write the description starting from the end of the query + 1 blank space.
    mov si, [@@endq]
    lea bx, [@@line]
    lea bx, [bx + si + 1]

    ; Write at most scrcols - endq characters.
    mov cx, [@@scrcols]
    sub cx, si

    ; Write the match description.
    push [@@curmatch]
    push cx
    push bx
    call writedesc

    ; Draw the line into VRAM.
    push [@@vram]
    push [@@scrcols]
    lea ax, [@@line]
    push ax
    call drawln

    ; Set cursor position.
    push [@@crscol]
    push [@@crsbase]
    call setcrs

    ; Wait for keyboard input.
    xor ah, ah
    int 16h

    cmp ax, 2e03h
    je @@break
    cmp al, 20h
    jge @@printable
    cmp ah, 0eh
    je @@backspace
    cmp ah, 0fh
    je @@tab
    cmp ah, 1ch
    je @@enter
    cmp ah, 47h
    je @@home
    cmp ah, 48h
    je @@arrowu
    cmp ah, 4bh
    je @@arrowl
    cmp ah, 4dh
    je @@arrowr
    cmp ah, 4fh
    je @@end
    cmp ah, 50h
    je @@arrowd
    cmp ah, 53h
    je @@delete

    ; Discard other keystrokes.
    jmp @@loop

@@break:
    ; Same as if the user inputted the "exit" program.
    mov ah, 09h
    lea dx, [strings.eol]
    int 21h
    lea ax, [defprogs.exit]
    leave
    ret @@argsize

@@home:
    ; Move cursor to the beginning of the line.
    mov [@@crscol], @@startq
    jmp @@loop

@@end:
    ; Move cursor to the end of the line.
    mov ax, [@@endq]
    mov [@@crscol], ax
    jmp @@loop

@@backspace:
    ; Delete previous character and shift left.
    mov bx, [@@crscol]
    mov cx, [@@endq]
    ; Do nothing if the cursor is placed at the first character.
    cmp bx, @@startq
    je @@loop

    lea si, [@@line]
    lea di, [si + bx - 1]
    add si, bx
    ; cx is 1 higher so we can shift left a blank.
    sub cx, bx
    add cx, 2
    rep movsb

    dec [@@crscol]
    dec [@@endq]
    mov [@@find], 1
    jmp @@loop

@@delete:
    ; Delete current character and shift left.
    mov bx, [@@crscol]
    mov cx, [@@endq]
    ; Do nothing if the cursor is placed after the last character.
    cmp bx, cx
    je @@loop

    lea di, [@@line]
    lea si, [di + bx + 1]
    add di, bx
    ; cx is 1 higher so we can shift left a blank.
    sub cx, bx
    rep movsb

    dec [@@endq]
    mov [@@find], 1
    jmp @@loop

@@arrowu:
    ; Selects previous match.
    mov bx, [@@curmatch]
    or bx, bx
    jz @@loop
    mov bx, [bx + Node.prev]
    mov [@@curmatch], bx
    jmp @@loop

@@arrowl:
    ; Move cursor left.
    cmp [@@crscol], @@startq
    je @@loop
    dec [@@crscol]
    jmp @@loop

@@arrowr:
    ; Move cursor right.
    mov ax, [@@crscol]
    cmp ax, [@@endq]
    je @@loop
    inc [@@crscol]
    jmp @@loop

@@tab:
@@arrowd:
    ; Selects next match.
    mov bx, [@@curmatch]
    or bx, bx
    jz @@loop
    mov bx, [bx + Node.next]
    mov [@@curmatch], bx
    jmp @@loop

@@printable:
    ; Shift right if we are not at the end of the line and print the character
    ; at current position.
    mov bx, [@@endq]
    mov cx, [@@scrcols]
    dec cx
    mov dx, [@@crscol]
    ; Don't print if we are at the last screen column.
    cmp bx, cx
    je @@loop

    std
    lea di, [@@line]
    lea si, [di + bx - 1]
    add di, bx
    mov cx, bx
    sub cx, dx
    rep movsb
    cld

    mov bx, dx
    lea di, [@@line]
    mov [di + bx], al

    inc [@@crscol]
    inc [@@endq]
    mov [@@find], 1
    jmp @@loop

@@enter:
    ; Prints a newline and returns current match.
    lea dx, [strings.eol]
    mov ah, 09h
    int 21h

    ; Do not return if we don't have a query.
    cmp [@@endq], @@startq
    je @@loop

    mov bx, [@@curmatch]
    or bx, bx
    jnz @@success    

    xor ax, ax
    leave
    ret @@argsize

@@success:
    mov ax, [bx + Node.value]
    leave
    ret @@argsize
endp

;
; Executes the specified program.
;
; @@comspec pointer to the full path to the COMMAND.COM interpreter
; @@batdir pointer to the full path to the batch files directory
; @@prog pointer to the program to execute
;
; The specified program must be a real program present in the batch directory.
; This procedure sets the current working directory and drive to the provided
; path before loading and executing the subprocess.
;
proc exec
arg @@comspec:word, @@batdir:word, @@prog:word = @@argsize
local @@eparams:ExecParams, @@clt:Clt, @@ss:word, @@sp:word = @@localsize
    enter @@localsize
    mov bx, [@@batdir]

    ; Change current drive.
    mov ah, 0eh
    mov dl, [bx + FullPath.drive]
    or dl, 20h
    sub dl, 'a'
    int 21h

    ; Change current path.
    mov ah, 3bh
    lea dx, [bx + FullPath.path]
    int 21h

    mov si, [@@prog]
    lea di, [@@clt]
    xor ch, ch
    mov cl, [si + Prog.cmd.length]

    ; Command line tail length is 4 for the switch + the command length.
    mov [di + Clt.length], cl
    add [byte ptr di], 4
    ; COMMAND.COM execute command switch  " /c ".
    mov [word ptr di + 1], "/ "
    mov [word ptr di + 3], " c"

    lea si, [si + Prog.cmd]
    add di, 5
    rep movsb

    ; Command line tail terminator.
    mov [byte ptr di], 0dh

    lea bx, [@@eparams]
    mov [bx + ExecParams.envseg], 0
    mov [bx + ExecParams.fcb1.off], 0
    mov [bx + ExecParams.fcb1.seg], 0
    mov [bx + ExecParams.fcb2.off], 0
    mov [bx + ExecParams.fcb2.seg], 0

    lea dx, [@@clt]
    mov [bx + ExecParams.clt.off], dx
    mov [bx + ExecParams.clt.seg], ss

    cli
    mov [@@ss], ss
    mov [@@sp], sp
    sti

    mov ax, 4b00h
    mov dx, [@@comspec]
    int 21h

    cli
    mov ss, [@@ss]
    mov sp, [@@sp]
    sti

@@return:
    leave
    ret @@argsize
endp

;
; Prints the database listing.
;
; @@db pointer to the program database
;
; This is the implementation of the LIST virtual program.
;
proc listdb
arg @@db:word = @@argsize
local @@line:byte:80 = @@localsize
    enter @@localsize

    ; Resets foreground and background color.
    mov ax, 0900h
    mov bx, 0007h
    mov es, [segments.bda]
    mov cx, [es:bda.scrcols]
    mov es, [segments.clam]
    int 10h

    @@scrrows = 25
    mov bx, [@@db]
    mov ax, [bx + ProgDb.count]
    mov bx, [bx + ProgDb.entries]
    mov dx, @@scrrows - 1

@@entry:
    mov si, [bx]
    xor ch, ch
    mov cl, [si + Prog.title.length]
    jcxz @@notitle

@@title:
    add si, offset (Prog).title
    lea di, [@@line]
    rep movsb

    mov [word ptr di], "[ "
    add di, 2

    mov si, [bx]
    add si, offset (Prog).cmd
    mov cl, [si + Cmd.length]
    rep movsb

    ; Writes "]\r\n" to the end of the line.
    mov [byte ptr di], "]"
    mov [word ptr di + 1], 0a0dh
    add di, 3

    jmp @@printentry

@@notitle:
    add si, offset (Prog).cmd
    lea di, [@@line]
    mov cl, [si + Cmd.length]
    rep movsb

    ; Writes "\r\n" to the end of the line.
    mov [word ptr di], 0a0dh
    add di, 2

@@printentry:
    push bx
    push ax
    push dx

    ; Prints cx characters to stdout.
    mov ah, 40h
    mov bx, stdout
    lea dx, [@@line]
    mov cx, di
    sub cx, dx
    int 21h

    pop dx
    dec dx
    jnz @@continue

    mov ah, 09h
    lea dx, [strings.continue]
    int 21h

    xor ah, ah
    int 16h

    mov ah, 09h
    lea dx, [strings.eol]
    int 21h

    mov dx, @@scrrows - 1

@@continue:
    pop ax
    pop bx

    add bx, 2
    dec ax
    jnz @@entry

@@count:
    ; Reserving bytes for 2 spaces + 4 digit number.
    lea di, [@@line + 6]
    mov si, [@@db]
    mov ax, [si + ProgDb.count]
    mov dl, 10

@@itoa:
    div dl
    add ah, 30h
    mov [di - 1], ah
    or al, al
    je @@printcount
    dec di
    xor ah, ah
    jmp @@itoa

@@printcount:
    lea dx, [di - 3]
    mov [word ptr di - 3], 0000h

    lea si, [strings.programs]
    lea di, [@@line + 6]
    mov cx, 14
    rep movsb

    ; dx already set from earlier.
    mov ah, 09h
    int 21h

    leave
    ret @@argsize
endp

;
; Terminates execution.
;
; @@workdir the pointer to the working directory
;
; This is the implementation of the EXIT virtual program. Restores path
; before exiting.
;
proc exit
arg @@workdir:word = @@argsize
    enter

    ; Restore original drive.
    mov si, [@@workdir]
    mov ah, 0eh
    mov dl, [si + FullPath.drive]
    or dl, 20h
    sub dl, 'a'
    int 21h

    ; Restore original directory.
    mov ah, 3bh
    lea dx, [si + FullPath.path]
    int 21h

    exitcode 0
endp

end


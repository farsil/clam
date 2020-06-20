'
' A program that can be executed
'
TYPE Program
    ' The program filename without extension
    Basename AS STRING * 8
    ' The program title
    Title AS STRING * 32
END TYPE

' index of the LIST virtual program
CONST LISTIDX = -1

' index of the EXIT virtual program
CONST EXITIDX = -2

' no program is matched
CONST NOTFOUND = -3

' loads the program database
DECLARE FUNCTION LoadDb% (Db() AS Program, Path AS STRING)

' finds a program
DECLARE FUNCTION Find%(Db() AS Program, DbSize AS INTEGER, Query AS STRING)

' prompts the user for input
DECLARE FUNCTION Prompt% (Db() AS Program, DbSize AS INTEGER)

' executes a program
DECLARE SUB Exec (Db() AS Program, DbSize AS INTEGER, Index AS INTEGER, Path AS STRING)

' COMMAND$ contains the path as first (and only) argument
Path$ = COMMAND$ + "\"

' REDIM is necessary because the final size may be different after
' invoking the loading function
REDIM Db(64) AS Program
DbSize% = LoadDb%(Db(), Path$)

DO:
    Index% = Prompt%(Db(), DbSize%)
    Exec Db(), DbSize%, Index%, Path$
LOOP

'
' Loads program database from the batch files in the specified path
' into the provided array.
'
' The array will be REDIMed if it is not large enough to contain all the
' program entries. This function will search in each detected batch file for
' a special comment in the first line, which starts with three colons :::.
' If such comment is found, it is read and it will be the program title.
'
' @param Db Program() the array which will contain the program database
' @param Path STRING the path where to look for batch files
'
' @return INTEGER the number of entries in the program database
'
FUNCTION LoadDb% (Db() AS Program, Path AS STRING)
    DbSize% = UBOUND(Db)
    LineNo% = LBOUND(Db)
    Line$ = ""

    ' Saves the list of batch files to a temporary file
    SHELL "DIR /b " + Path + "*.BAT > CLAM.TMP"

    TmpFile = FREEFILE
    OPEN "CLAM.TMP" FOR INPUT AS #TmpFile
    DO WHILE NOT EOF(TmpFile)
        ' REDIM if database is too small
        IF LineNo% > DbSize% THEN
            DIM Aux(DbSize%) AS Program
            FOR I% = 0 TO DbSize% - 1
                Aux(I%) = Db(I%)
            NEXT
            REDIM Db(LineNo% + 64) AS Program
            FOR I% = 0 TO DbSize% - 1
                Db(I%) = Aux(I%)
            NEXT
            DbSize% = LineNo% + 64
        END IF

        ' reads filename from the shell output read from the temporary file
        LINE INPUT #TmpFile, Line$
        Db(LineNo%).Basename = LEFT$(Line$, LEN(Line$) - 4)

        ' reads description from first line in batch file
        ' line$ still holds the filename + extension when the file is opened
        BatFile = FREEFILE
        OPEN Path + Line$ FOR INPUT AS #BatFile
        IF NOT EOF(BatFile) THEN
            LINE INPUT #BatFile, Line$
            IF INSTR(Line$, ":::") THEN
                Db(LineNo%).Title = LTRIM$(RIGHT$(Line$, LEN(Line$) - 3))
            END IF
        END IF
        CLOSE #BatFile

        LineNo% = LineNo% + 1
    LOOP
    CLOSE #TmpFile
    ' lineno% is the number of files
    LoadDb% = LineNo%
END FUNCTION

'
' Finds a program in the database by attempting to match the specified query.
'
' There are two virtual programs that can be matched by supplying a
' specific query:
'
' * LIST, matched by providing "list" or "l".
' * EXIT, matched by providing "exit" or "x".
'
' This function returns the special indices LISTIDX (-1) and
' EXITIDX (-2) respectively when the virtual programs are matched.
'
' Returns NOTFOUND (-3) when no match could be found.
'
' @param Db Program() the program database
' @param DbSize INTEGER the number of programs in the database
' @param Query STRING the query
'
' @return INTEGER the program index in the database or a special index
'
FUNCTION Find% (Db() AS Program, DbSize AS INTEGER, Query AS STRING)
    Query = UCASE$(Query)
    IF Query = "L" OR INSTR("LIST", Query) THEN
        Find% = LISTIDX
    ELSEIF Query = "X" OR INSTR("EXIT", Query) THEN
        Find% = EXITIDX
    ELSE
        FOR I% = 0 TO DbSize - 1
            ' TODO: it only finds the first right now
            IF INSTR(UCASE$(Db(I%).Title), Query) THEN
                Find% = I%
                EXIT FUNCTION
            ELSEIF INSTR(Db(I%).Basename, Query) THEN
                Find% = I%
                EXIT FUNCTION
            END IF
        NEXT
        Find% = NOTFOUND
    END IF
END FUNCTION

'
' Prompts the user for the program to execute.
'
' This function simulates a prompt and attempts to find a program
' by querying what the user types against the database while typing.
'
' When the user presses ENTER, the query stops, and the index of the matched
' program is returned, or NOTFOUND (-3) if no programs could be matched.
' See the FIND() function documentation for more details on the possible
' return values.
'
' @param Db Program() the program database
' @param DbSize INTEGER the number of programs in the database
'
' @return INTEGER the program index
'
FUNCTION Prompt% (Db() AS Program, DbSize AS INTEGER)
    DIM ProgIndex AS INTEGER
    DIM K AS STRING
    Line$ = ""

    ' we need direct access to the memory address of k
    DEF SEG = VARSEG(K)

    ' -> arrow prompt
    PRINT CHR$(26) + " ";
    LOCATE , , 1

    ' the position where the line starts on the screen
    StartPos% = POS(0)

    ' begin listening for keypresses
    DO
        ' clear the buffer for residual key presses before SLEEPing
        DO: K = INKEY$: LOOP UNTIL K = ""

        ' read from the keyboard buffer
        SLEEP
        K = INKEY$

        ' process the received keypress
        IF LEN(K) = 1 THEN
            ' no modifier keys, read the first byte of k
            SELECT CASE PEEK(SADD(K))
                CASE 8 ' backspace
                    ' only enable backspace simulation if we can delete the character
                    ' without "bumping" into the arrow prompt
                    IF POS(0) > StartPos% THEN
                        ' derive the position of the char to remove from the column
                        Index% = POS(0) - StartPos%
                        Line$ = LEFT$(Line$, Index% - 1) + RIGHT$(Line$, LEN(Line$) - Index%)
                        ' redraw the line, position cursor on the character next to
                        ' the one that was removed
                        LOCATE , StartPos%
                        PRINT Line$ + " ";
                        LOCATE , Index% + StartPos% - 1
                    END IF
                CASE IS > 31 ' printable characters
                    ' derive the position where to insert the char from the column
                    Index% = POS(0) - StartPos%
                    Line$ = LEFT$(Line$, Index%) + K + RIGHT$(Line$, LEN(Line$) - Index%)
                    ' redraw the line, position cursor on the character next to
                    ' the one that was inserted
                    LOCATE , StartPos%
                    PRINT Line$;
                    LOCATE , Index% + StartPos% + 1
            END SELECT
        ELSE
            ' modifier keys, read the second byte of k
            SELECT CASE PEEK(SADD(K) + 1)
                CASE 71 ' home
                    LOCATE , StartPos%
                CASE 75 ' left arrow
                    IF POS(0) > StartPos% THEN
                        LOCATE , POS(0) - 1
                    END IF
                CASE 77 ' right arrow
                    IF POS(0) < StartPos% + LEN(Line$) THEN
                        LOCATE , POS(0) + 1
                    END IF
                CASE 79 ' end
                    LOCATE , StartPos% + LEN(Line$)
                CASE 83 ' delete
                    ' derive the position of the char to remove from the column
                    Index% = POS(0) - StartPos% + 1
                    Line$ = LEFT$(Line$, Index% - 1) + RIGHT$(Line$, LEN(Line$) - Index%)
                    ' redraw the line, position cursor on the character next to
                    ' the one that was removed
                    LOCATE , StartPos%
                    PRINT Line$ + " ";
                    LOCATE , Index% + StartPos% - 1
            END SELECT
        END IF

        ' update the program index with the current line
        ProgIndex = Find%(Db(), DbSize, RTRIM$(LTRIM$(Line$)))
    LOOP UNTIL K = CHR$(13)

    ' the user expects a newline when they press enter
    PRINT

    ' restore the default segment, we're done with k
    DEF SEG

    IF ProgIndex = NOTFOUND AND LEN(Line$) THEN
        PRINT "Illegal program: " + RTRIM$(LTRIM$(Line$)) + "."
    END IF
    Prompt% = ProgIndex
END FUNCTION

'
' Executes the program in the database at the specified index.
'
' There are two virtual programs that are always possible to execute:
'
' * LIST, which lists the programs in the database.
' * EXIT, which exits the program to DOS.
'
' They are invoked by suppliyng the special program indices LISTIDX (-1)
' and EXITIDX (-2).
'
' Nothing is executed if NOTFOUND (-3) is passed as an index.
'
' This function does not attempt to calculate the database size using
' UBOUND, but relies on the supplied program count.
'
' @param Db Program() the program database
' @param DbSize INTEGER the number of programs in the database
' @param Index INTEGER the index of the program to execute
'
SUB Exec (Db() AS Program, DbSize AS INTEGER, Index AS INTEGER, Path AS STRING)
    SELECT CASE Index
        CASE LISTIDX
            FOR I% = 0 TO DbSize - 1
                IF ASC(Db(I%).Title) <> 0 THEN
                    PRINT RTRIM$(Db(I%).Title) + " [";
                END IF
                PRINT RTRIM$(Db(I%).Basename);
                IF ASC(Db(I%).Title) <> 0 THEN
                    PRINT "]";
                END IF
                PRINT
            NEXT
            PRINT "   " + STR$(DbSize) + " program(s)"
        CASE EXITIDX
            END
        CASE IS <> NOTFOUND
            ' COMMAND /c solves loading issues with certain games
            SHELL "COMMAND /c " + Path + Db(Index).Basename + ".BAT"
    END SELECT
END SUB

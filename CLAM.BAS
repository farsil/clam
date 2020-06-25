'
' A program that can be executed
'
TYPE Program
    ' The program filename without extension
    Basename AS STRING * 8
    ' The program title
    Title AS STRING * 32
END TYPE

' 80x25 text mode, but QuickBasic doesn't normally use the last row,
' so the effective rows are 24
CONST SCREENROWS = 24, SCREENCOLS = 80

' booleans
CONST FALSE = 0, TRUE = 1

' index, command and description of the LIST virtual program
CONST LISTIDX = -1, LISTCMD = "L", LISTDESC = "List programs"

' index, command and description of the EXIT virtual program
CONST EXITIDX = -2, EXITCMD = "X", EXITDESC = "Exit to DOS"

' no program is matched
CONST NOTFOUND = -3

' loads the program database
DECLARE FUNCTION LoadDb% (Db() AS Program, Path AS STRING)

' finds a program
DECLARE FUNCTION Find% (Db() AS Program, DbSize AS INTEGER, Query AS STRING)

' prompts the user for input
DECLARE FUNCTION Prompt% (Db() AS Program, DbSize AS INTEGER)

' executes a program
DECLARE SUB Exec (Db() AS Program, DbSize AS INTEGER, _
                  Index AS INTEGER, Path AS STRING)

' COMMAND$ contains the path as first (and only) argument
Path$ = COMMAND$ + "\"

' REDIM is necessary because the final size may be different after
' invoking the loading function
REDIM Db(63) AS Program
DbSize% = LoadDb%(Db(), Path$)

PRINT
DO:
    Index% = Prompt%(Db(), DbSize%)
    Exec Db(), DbSize%, Index%, Path$
LOOP

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
    ' don't attempt to find anything on an empty string
    IF LEN(Query) THEN
        Query = UCASE$(Query)
        IF Query = LISTCMD OR INSTR(UCASE$(LISTDESC), Query) THEN
            Find% = LISTIDX
            EXIT FUNCTION
        ELSEIF Query = EXITCMD OR INSTR(UCASE$(EXITDESC), Query) THEN
            Find% = EXITIDX
            EXIT FUNCTION
        ELSE
            FOR I% = 0 TO DbSize - 1
                ' TODO: it only finds the first right now
                IF INSTR(UCASE$(Db(I%).Title), Query) THEN
                    Find% = I%
                    EXIT FUNCTION
                ELSEIF RTRIM$(Db(I%).Basename) = Query THEN
                    Find% = I%
                    EXIT FUNCTION
                END IF
            NEXT
        END IF
    END IF
    Find% = NOTFOUND
END FUNCTION

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
    DbSize% = UBOUND(Db) - LBOUND(Db) + 1
    Index% = LBOUND(Db)
    Line$ = ""

    ' saves the list of batch files to a temporary file
    SHELL "DIR /b " + Path + "*.BAT > CLAM.TMP"

    TmpFile = FREEFILE
    OPEN "CLAM.TMP" FOR INPUT AS #TmpFile
    DO WHILE NOT EOF(TmpFile)
        ' REDIM if database is too small
        IF Index% = DbSize% THEN
            DIM Aux(DbSize% - 1) AS Program
            FOR I% = 0 TO DbSize% - 1
                Aux(I%) = Db(I%)
            NEXT
            REDIM Db(DbSize% + 64) AS Program
            FOR I% = 0 TO DbSize% - 1
                Db(I%) = Aux(I%)
            NEXT
            DbSize% = DbSize% + 64
        END IF

        ' reads filename from the shell output read from the temporary file
        LINE INPUT #TmpFile, Line$
        Db(Index%).Basename = LEFT$(Line$, LEN(Line$) - 4)

        ' reads description from first line in batch file
        ' line$ still holds the filename + extension when the file is opened
        BatFile = FREEFILE
        OPEN Path + Line$ FOR INPUT AS #BatFile
        IF NOT EOF(BatFile) THEN
            LINE INPUT #BatFile, Line$
            IF INSTR(Line$, ":::") THEN
                Db(Index%).Title = LTRIM$(RIGHT$(Line$, LEN(Line$) - 3))
            END IF
        END IF
        CLOSE #BatFile

        Index% = Index% + 1
    LOOP
    CLOSE #TmpFile
    ' Index% is the number of files
    LoadDb% = Index%
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
    DIM Keystroke AS STRING
    Redraw% = FALSE
    Query$ = ""

    ' -> arrow prompt
    Arrow$ = CHR$(26) + " "
    PRINT Arrow$;
    LOCATE , , 1

    PromptRow% = CSRLIN
    PromptLen% = LEN(Arrow$)

    ' the index of the cursor relative to the query, used to determine where
    ' to place the cursor on the screen row and where to insert or remove chars
    CurIndex% = 0

    ' begin listening for keypresses
    DO
        ' clear the buffer for residual key presses before SLEEPing
        DO
            Keystroke = INKEY$
        LOOP UNTIL Keystroke = ""

        ' read from the keyboard buffer
        SLEEP
        Keystroke = INKEY$

        ' process the received keypress
        DEF SEG = VARSEG(Keystroke)
        IF LEN(Keystroke) = 1 THEN
            ' no modifier keys, read the first byte
            SELECT CASE PEEK(SADD(Keystroke))
                CASE 8 ' backspace
                    ' delete the previous character and move the cursor back
                    IF CurIndex% > 0 THEN
                        Query$ = LEFT$(Query$, CurIndex% - 1) + _
                                RIGHT$(Query$, LEN(Query$) - CurIndex%)
                        CurIndex% = CurIndex% - 1
                        Redraw% = TRUE
                    END IF
                CASE IS > 31 ' printable characters
                    ' insert a character and move the cursor forward
                    Query$ = LEFT$(Query$, CurIndex%) + Keystroke + _
                            RIGHT$(Query$, LEN(Query$) - CurIndex%)
                    CurIndex% = CurIndex% + 1
                    Redraw% = TRUE
            END SELECT
        ELSE
            ' modifier keys, read the second byte
            SELECT CASE PEEK(SADD(Keystroke) + 1)
                CASE 71 ' home
                    ' move the cursor to the beginning
                    CurIndex% = 0
                CASE 75 ' left arrow
                    ' move the cursor backwards
                    IF CurIndex% > 0 THEN
                        CurIndex% = CurIndex% - 1
                    END IF
                CASE 77 ' right arrow
                    ' move the cursor forward
                    IF CurIndex% < LEN(Query$) THEN
                        CurIndex% = CurIndex% + 1
                    END IF
                CASE 79 ' end
                    ' move the cursor to the end
                    CurIndex% = LEN(Query$)
                CASE 83 ' delete
                    ' delete a charcater
                    IF CurIndex% < LEN(Query$) THEN
                        Query$ = LEFT$(Query$, CurIndex%) + _
                                RIGHT$(Query$, LEN(Query$) - CurIndex% - 1)
                        Redraw% = TRUE
                    END IF
            END SELECT
        END IF
        DEF SEG

        ' update the program index with the current query
        ProgIndex = Find%(Db(), DbSize, RTRIM$(LTRIM$(Query$)))

        ' redraw the line if it changed
        IF Redraw% THEN
            Line$ = Arrow$ + Query$
            SELECT CASE ProgIndex
                CASE LISTIDX
                    Line$ = Line$ + "  (" + LISTDESC + ")"
                CASE EXITIDX
                    Line$ = Line$ + "  (" + EXITDESC + ")"
                CASE IS <> NOTFOUND
                    ' the title is empty if it filled with \0
                    ' in this case, checking the first byte is enough
                    IF ASC(Db(ProgIndex).Title) <> 0 THEN
                        Line$ = Line$ + "  (" + RTRIM$(Db(ProgIndex).Title) + ")"
                    ELSE
                        Line$ = Line$ + "  (Run " + RTRIM$(Db(ProgIndex).Basename) + ".BAT)"
                    END IF
            END SELECT

            ' reset the cursor and print the line, adding enough right
            ' padding to clear the row
            LOCATE PromptRow%, 1
            PRINT Line$; SPACE$(SCREENCOLS - (POS(0) - 1));

            ' update the prompt row if scrolling happened
            RowsPrinted% = INT(LEN(Line$) / SCREENCOLS)
            IF PromptRow% + RowsPrinted% > SCREENROWS THEN
                PromptRow% = PromptRow% - 1
            END IF
            Redraw% = FALSE
        END IF

        ' calculate the cursor position based on the cursor index and relocate
        CurRow% = PromptRow% + INT((PromptLen% + CurIndex%) / SCREENCOLS)
        CurCol% = 1 + (PromptLen% + CurIndex%) MOD SCREENCOLS
        LOCATE CurRow%, CurCol%
    LOOP UNTIL Keystroke = CHR$(13)

    ' the user expects a newline when they press enter
    PRINT

    IF ProgIndex = NOTFOUND AND LEN(Query$) THEN
        PRINT "Illegal program: " + RTRIM$(LTRIM$(Query$)) + "."
    END IF
    Prompt% = ProgIndex
END FUNCTION


'
' A program that can be executed.
'
TYPE Program
    ' The program filename without extension
    Basename AS STRING * 8
    ' The program title
    Title AS STRING * 32
END TYPE

'
' A node to be used with the priority queue.
'
TYPE QueueNode
    ' The queue value
    Value AS INTEGER
    ' The priority of the program
    Priority AS INTEGER
    ' The previous node in the queue
    Predecessor AS INTEGER
    ' The next node in the queue
    Successor AS INTEGER
END TYPE

'
' A descriptor for queues.
'
' Must be paired with the array that holds the queue data.
'
TYPE QueueDescriptor
    ' The first element of the queue in the array
    First AS INTEGER
    ' The total number of elements in the queue
    Count AS INTEGER
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
DECLARE SUB Find (Db() AS Program, DbSize AS INTEGER, Matches() AS QueueNode, _
                  MatchesDesc As QueueDescriptor, Query AS STRING)

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
SUB Find (Db() AS Program, DbSize AS INTEGER, Matches() AS QueueNode, _
                MatchesDesc AS QueueDescriptor, Query AS STRING)
    ' empty the queue
    MatchesDesc.Count = 0

    ' don't attempt to find anything on an empty string
    IF LEN(Query) = 0 THEN
        EXIT SUB
    END IF
    Query = UCASE$(Query)

    Priority% = INSTR(UCASE$(LISTDESC), Query)
    IF Query = LISTCMD THEN
        Push Matches(), MatchesDesc, LISTIDX, 0
    ELSEIF Priority% <> 0 THEN
        Push Matches(), MatchesDesc, LISTIDX, Priority%
    END IF

    Priority% = INSTR(UCASE$(EXITDESC), Query)
    IF Query = EXITCMD THEN
        Push Matches(), MatchesDesc, EXITIDX, 0
    ELSEIF Priority% <> 0 THEN
        Push Matches(), MatchesDesc, EXITIDX, Priority%
    END IF

    FOR I% = 0 TO DbSize - 1
        Priority% = INSTR(UCASE$(Db(I%).Title), Query)
        IF RTRIM$(Db(I%).Basename) = Query THEN
            Push Matches(), MatchesDesc, I%, 0
        ELSEIF Priority% <> 0 THEN
            Push Matches(), MatchesDesc, I%, Priority%
        END IF
    NEXT
END SUB

SUB Push (Queue() AS QueueNode, QDesc AS QueueDescriptor, Value AS INTEGER, _
          Priority AS INTEGER)
    ' I am assuming MatchCount < UBOUND(Matches) always
    IF QDesc.Count = 0 THEN
        Queue(0).Value = Value
        Queue(0).Priority = Priority
        Queue(0).Predecessor = 0
        Queue(0).Successor = 0
        QDesc.First = 0
        QDesc.Count = 1
    ELSEIF Priority <= Queue(QDesc.First).Priority THEN
        ' if there are Desc.Count elements in the queue, it is safe to
        ' overwrite the Desc.Count node, because the array is 0-based
        Last% = Queue(QDesc.First).Predecessor
        Queue(QDesc.Count).Value = Value
        Queue(QDesc.Count).Priority = Priority
        Queue(QDesc.Count).Successor = QDesc.First
        Queue(QDesc.Count).Predecessor = Last%
        Queue(Last%).Successor = QDesc.Count
        Queue(QDesc.First).Predecessor = QDesc.Count
        QDesc.First = QDesc.Count
        QDesc.Count = QDesc.Count + 1
    ELSE
        Successor% = Queue(QDesc.First).Successor
        DO WHILE Successor% <> QDesc.First
            IF Priority <= Queue(Successor%).Priority THEN
                EXIT DO
            END IF
            Successor% = Queue(Successor%).Successor
        LOOP
        Current% = Queue(Successor%).Predecessor
        Queue(QDesc.Count).Value = Value
        Queue(QDesc.Count).Priority = Priority
        Queue(QDesc.Count).Successor = Successor%
        Queue(QDesc.Count).Predecessor = Current%
        Queue(Successor%).Predecessor = QDesc.Count
        Queue(Current%).Successor = QDesc.Count
        QDesc.Count = QDesc.Count + 1
    END IF
END SUB

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
    DIM Matches(DbSize) AS QueueNode
    DIM MatchesDesc AS QueueDescriptor
    DIM MatchesIndex AS INTEGER
    DIM Keystroke AS STRING

    MatchesDesc.Count = 0
    Redraw% = FALSE
    Query$ = ""
    QueryChanged% = FALSE

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
        WHILE INKEY$ <> "": WEND

        ' read from the keyboard buffer
        SLEEP: Keystroke = INKEY$

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
                        QueryChanged% = TRUE
                        Redraw% = TRUE
                    END IF
                CASE 9 ' tab
                    IF MatchesDesc.Count > 0 THEN
                        MatchesIndex = Matches(MatchesIndex).Successor
                        Redraw% = TRUE
                    END IF
                CASE IS > 31 ' printable characters
                    ' insert a character and move the cursor forward
                    Query$ = LEFT$(Query$, CurIndex%) + Keystroke + _
                            RIGHT$(Query$, LEN(Query$) - CurIndex%)
                    CurIndex% = CurIndex% + 1
                    QueryChanged% = TRUE
                    Redraw% = TRUE
            END SELECT
        ELSE
            ' modifier keys, read the second byte
            SELECT CASE PEEK(SADD(Keystroke) + 1)
                CASE 71 ' home
                    ' move the cursor to the beginning
                    CurIndex% = 0
                CASE 72 ' up arrow
                    IF MatchesDesc.Count > 0 THEN
                        MatchesIndex = Matches(MatchesIndex).Predecessor
                        Redraw% = TRUE
                    END IF
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
                CASE 80 'down arrow
                    IF MatchesDesc.Count > 0 THEN
                        MatchesIndex = Matches(MatchesIndex).Successor
                        Redraw% = TRUE
                    END IF
                CASE 83 ' delete
                    ' delete a charcater
                    IF CurIndex% < LEN(Query$) THEN
                        Query$ = LEFT$(Query$, CurIndex%) + _
                                RIGHT$(Query$, LEN(Query$) - CurIndex% - 1)
                        QueryChanged% = TRUE
                        Redraw% = TRUE
                    END IF
            END SELECT
        END IF
        DEF SEG

        IF QueryChanged% THEN
            ' update the matches queue with the current query
            Find Db(), DbSize, Matches(), MatchesDesc, RTRIM$(LTRIM$(Query$))

            ' reset the current matches index, it may be an invalid value, so
            ' it must be guarded with a MatchesDesc.Count check
            MatchesIndex = MatchesDesc.First
        END IF

        IF Redraw% THEN
            Line$ = Arrow$ + Query$
            IF MatchesDesc.Count > 0 THEN
                DbIndex% = Matches(MatchesIndex).Value
                SELECT CASE DbIndex%
                    CASE LISTIDX
                        Line$ = Line$ + "  (" + LISTDESC + ")"
                    CASE EXITIDX
                        Line$ = Line$ + "  (" + EXITDESC + ")"
                    CASE IS <> NOTFOUND
                        ' the title is empty if it filled with \0
                        ' in this case, checking the first byte is enough
                        IF ASC(Db(DbIndex%).Title) <> 0 THEN
                            Line$ = Line$ + "  (" + _
                                    RTRIM$(Db(DbIndex%).Title) + ")"
                        ELSE
                            Line$ = Line$ + "  (Run " + _
                                    RTRIM$(Db(DbIndex%).Basename) + ".BAT)"
                        END IF
                END SELECT
            END IF

            ' reset the cursor and print the line, adding enough right
            ' padding to clear the row
            LOCATE PromptRow%, 1
            PRINT Line$; SPACE$(SCREENCOLS - (POS(0) - 1));

            ' update the prompt row if scrolling happened
            RowsPrinted% = INT(LEN(Line$) / SCREENCOLS)
            IF PromptRow% + RowsPrinted% > SCREENROWS THEN
                PromptRow% = PromptRow% - 1
            END IF
            QueryChanged% = FALSE
        END IF

        ' calculate the cursor position based on the cursor index and relocate
        CurRow% = PromptRow% + INT((PromptLen% + CurIndex%) / SCREENCOLS)
        CurCol% = 1 + (PromptLen% + CurIndex%) MOD SCREENCOLS
        LOCATE CurRow%, CurCol%
    LOOP UNTIL Keystroke = CHR$(13)

    ' the user expects a newline when they press enter
    PRINT

    IF MatchesDesc.Count <> 0 THEN
        Prompt% = Matches(MatchesIndex).Value
    ELSE
        IF LEN(Query$) THEN
            PRINT "Illegal program: " + RTRIM$(LTRIM$(Query$)) + "."
        END IF
        Prompt% = NOTFOUND
    END IF
END FUNCTION


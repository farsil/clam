'
' A program that can be executed
'
TYPE PROGRAM
    ' The program filename without extension
    basename AS STRING * 8
    ' The program title
    title AS STRING * 32
END TYPE

CONST LISTIDX = -1
CONST EXITIDX = -2
CONST NOTFOUND = -3

' COMMAND$ contains the path as first (and only) argument
path$ = COMMAND$ + "\"

' load program database
REDIM db(64) AS PROGRAM
dbsize% = DBLOAD(db(), path$)

DO:
    ' PROMPT$ displays a prompt and returns the command
    idx% = PROMPT(db(), dbsize%)
    CALL EXEC(db(), dbsize%, idx%)
LOOP

'
' Loads program database from the batch files in the specified path
' into the provided array.
'
' The array may be REDIMed if it is not large enough to contain all the
' program entries. This function will search in each detected batch file for
' a special comment in the first line, which starts with three colons :::.
' If such comment is found, it is read and it will be the program title.
'
' @param db PROGRAM() the array which will contain the program database
' @param path STRING the path where to look for batch files
'
' @return the number of entries in the program database
'
FUNCTION DBLOAD (db() AS PROGRAM, path AS STRING)
    dbsize% = UBOUND(db)
    lineno% = LBOUND(db)
    line$ = ""

    ' Saves the list of batch files to a temporary file
    SHELL "DIR /b " + path + "*.BAT > CLAM.TMP"

    OPEN "CLAM.TMP" FOR INPUT AS 1
    DO WHILE NOT EOF(1)
        ' REDIM if database is too small
        IF lineno% > dbsize% THEN
            DIM aux(dbsize%) AS PROGRAM
            FOR i% = 0 TO dbsize%
                aux(i%) = db(i%)
            NEXT
            REDIM db(lineno% + 64)
            FOR i% = 0 TO dbsize%
                db(i%) = aux(i%)
            NEXT
            dbsize% = lineno% + 64
        END IF

        ' reads filename from the shell output read from the temporary file
        LINE INPUT #1, line$
        db(lineno%).basename = LEFT$(line$, LEN(line$) - 4)

        ' reads description from first line in batch file
        ' line$ still holds the filename + extension when the file is opened
        OPEN path + line$ FOR INPUT AS 2
        IF NOT EOF(2) THEN
            LINE INPUT #2, line$
            idx% = INSTR(line$, ":::")
            IF idx% <> 0 THEN
                db(lineno%).title = LTRIM$(RIGHT$(line$, LEN(line$) - 3))
            END IF
        END IF
        CLOSE #2

        lineno% = lineno% + 1
    LOOP
    CLOSE #1
    ' lineno% is the number of files
    DBLOAD = lineno%
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
' @param db PROGRAM() the program database
' @param dbsize INTEGER the number of programs in the database
' @param text STRING the query
'
' @return the program index in the database or a special index
'
FUNCTION FIND (db() AS PROGRAM, dbsize AS INTEGER, query AS STRING)
    query = LCASE$(query)
    IF query = "l" OR query = "list" THEN
        FIND = LISTIDX
    ELSEIF query = "x" OR query = "exit" THEN
        FIND = EXITIDX
    ELSE
        ' TODO: querying the db not yet implemented
        FIND = NOTFOUND
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
' @param db PROGRAM() the program database
' @param dbsize INTEGER the number of programs in the database
'
' @return the program index
'
FUNCTION PROMPT (db() AS PROGRAM, dbsize AS INTEGER)
    DIM dbidx AS INTEGER
    DIM k AS STRING
    line$ = ""

    ' we need direct access to the memory address of k
    DEF SEG = VARSEG(k)

    PRINT " ";
    LOCATE , , 1

    ' the position where the line starts on the screen
    startpos% = POS(0)

    ' begin listening for keypresses
    DO
        ' clear the buffer for residual key presses before SLEEPing
        DO: k = INKEY$: LOOP UNTIL k = ""

        ' read from the keyboard buffer
        SLEEP
        k = INKEY$

        ' process the received keypress
        IF LEN(k) = 1 THEN
            ' no modifier keys, read the first byte of k
            SELECT CASE PEEK(SADD(k))
                CASE 8 ' backspace
                    ' only enable backspace simulation if we can delete the character
                    ' without "bumping" into the arrow prompt
                    IF POS(0) > startpos% THEN
                        ' derive the position of the char to remove from the column
                        idx% = POS(0) - startpos%
                        line$ = LEFT$(line$, idx% - 1) + RIGHT$(line$, LEN(line$) - idx%)
                        ' redraw the line, position cursor on the character next to
                        ' the one that was removed
                        LOCATE , startpos%
                        PRINT line$ + " ";
                        LOCATE , idx% + startpos% - 1
                    END IF
                CASE IS > 31 ' printable characters
                    ' derive the position where to insert the char from the column
                    idx% = POS(0) - startpos%
                    line$ = LEFT$(line$, idx%) + k + RIGHT$(line$, LEN(line$) - idx%)
                    ' redraw the line, position cursor on the character next to
                    ' the one that was inserted
                    LOCATE , startpos%
                    PRINT line$;
                    LOCATE , idx% + startpos% + 1
            END SELECT
        ELSE
            ' modifier keys, read the second byte of k
            SELECT CASE PEEK(SADD(k) + 1)
                CASE 71 ' home
                    LOCATE , startpos%
                CASE 75 ' left arrow
                    IF POS(0) > startpos% THEN
                        LOCATE , POS(0) - 1
                    END IF
                CASE 77 ' right arrow
                    IF POS(0) < startpos% + LEN(line$) THEN
                        LOCATE , POS(0) + 1
                    END IF
                CASE 79 ' end
                    LOCATE , startpos% + LEN(line$)
                CASE 83 ' delete
                    ' derive the position of the char to remove from the column
                    idx% = POS(0) - startpos% + 1
                    line$ = LEFT$(line$, idx% - 1) + RIGHT$(line$, LEN(line$) - idx%)
                    ' redraw the line, position cursor on the character next to
                    ' the one that was removed
                    LOCATE , startpos%
                    PRINT line$ + " ";
                    LOCATE , idx% + startpos% - 1
            END SELECT
        END IF

        ' update the database index with the current line
        dbidx = FIND(db(), dbsize, RTRIM$(LTRIM$(line$)))
    LOOP UNTIL k = CHR$(13)

    ' the user expects a newline when they press enter
    PRINT

    ' restore the default segment, we're done with k
    DEF SEG

    IF dbidx = NOTFOUND THEN
        PRINT "Illegal query: " + RTRIM$(LTRIM$(line$)) + "."
    END IF
    PROMPT = dbidx
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
' @param db PROGRAM() the program database
' @param dbsize INTEGER the number of programs in the database
' @param idx INTEGER the index of the program to execute
'
SUB EXEC (db() AS PROGRAM, dbsize AS INTEGER, idx AS INTEGER)
    SELECT CASE idx
        CASE LISTIDX
            FOR i% = 0 TO dbsize - 1
                IF ASC(db(i%).title) <> 0 THEN
                    PRINT RTRIM$(db(i%).title) + " [";
                END IF
                PRINT RTRIM$(db(i%).basename);
                IF ASC(db(i%).title) <> 0 THEN
                    PRINT "]";
                END IF
                PRINT
            NEXT
            PRINT "   " + STR$(dbsize) + " batch file(s)"
        CASE EXITIDX
            END
        CASE IS <> NOTFOUND
            ' TODO: run the program
    END SELECT
END SUB

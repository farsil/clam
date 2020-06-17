'
' A program that can be executed
'
TYPE PROGRAM
    ' The program filename without extension
    basename AS STRING * 8
    ' The program title
    title AS STRING * 32
END TYPE

' COMMAND$ contains the path as first (and only) argument
path$ = COMMAND$ + "\"

' load program database
REDIM db(64) AS PROGRAM
dbsize% = DBLOAD(db(), path$)

DO:
    ' PROMPT$ displays a prompt and returns the command
    CALL EXEC(db(), dbsize%, PROMPT$)
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
' Displays a prompt and returns what the user types.
'
' @return the user input
'
FUNCTION PROMPT$
    DIM k AS STRING
    DEF SEG = VARSEG(k)
    line$ = ""
    PRINT " ";
    ' The position where the line starts on the screen
    startpos% = POS(0)
    LOCATE , , 1
    DO
        ' clear the buffer for residual key presses before SLEEPing
        DO: k = INKEY$: LOOP UNTIL k = ""
        SLEEP
        k = INKEY$
        IF LEN(k) = 1 THEN
            ' no modifier keys, read the first byte
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
            ' modifier keys, read the second byte
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
    LOOP UNTIL k = CHR$(13)
    DEF SEG
    PRINT
    PROMPT = RTRIM$(LTRIM$(LCASE$(line$)))
END FUNCTION



'
' Executes a command.
'
' There are two special commands that are always available:
'
' * exit (or x), which exits the program to DOS
' * list (or l), which lists the programs in the database
'
' This function does not attempt to calculate the database size using
' UBOUND, but relies on the supplied program count.
'
' @param db PROGRAM() the program database
' @param dbsize INTEGER the number of programs in the database
' @param cmd STRING the text query
'
SUB EXEC (db() AS PROGRAM, dbsize AS INTEGER, cmd AS STRING)
    IF cmd = "l" OR cmd = "list" THEN
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
    ELSEIF cmd = "x" OR cmd = "exit" THEN
        END
    ELSEIF LEN(cmd) > 0 THEN
        PRINT "Illegal command: " + cmd + "."
    END IF
END SUB


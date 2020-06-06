'
' A program that can be executed
'
TYPE PROGRAM
    ' The program filename and extension
    filename AS STRING * 12
    ' The program description
    desc AS STRING * 64
END TYPE

' load entries, COMMAND$ contains the path as first (and only) argument
REDIM progs(64) AS PROGRAM
DIM progcount AS INTEGER
progcount = LOADPROGS(COMMAND$, progs())

DO:
    ' check query for recognized sequences
    ' PROMPT$ displays a prompt and returns the command
    CALL QUERY(PROMPT$, progs(), progcount)
LOOP

'
' Loads program entries from the batch files in the specified path
' into the provided array of PROGRAMs.
'
' The array may be REDIMed if it is not large enough to contain all the
' program entries. This function will search in each detected batch file for
' a special comment in the first line, which starts with three colons :::.
' If such comment is found, it is read and it will be the program full name.
'
' @param path STRING the path where to look for batch files
' @param progs PROGRAM() the array which will contain the program entries
'
' @return the number of batch files
'
FUNCTION LOADPROGS (path AS STRING, progs() AS PROGRAM)
    length% = UBOUND(progs)
    lineno% = LBOUND(progs)

    ' Saves the list of batch files to a temporary file
    SHELL "DIR /b " + path + "*.BAT > CLAM.TMP"

    OPEN "CLAM.TMP" FOR INPUT AS 1
    DO WHILE NOT EOF(1)
        ' REDIM if array is too small
        IF lineno% > length% THEN
            DIM aux(length%) AS PROGRAM
            FOR i% = 0 TO length%
                aux(i%) = progs(i%)
            NEXT
            REDIM progs(lineno% + 64)
            FOR i% = 0 TO length%
                progs(i%) = aux(i%)
            NEXT
            length% = lineno% + 64
        END IF

        ' reads filename from the shell output read from the temporary file
        LINE INPUT #1, progs(lineno%).filename

        ' reads description from first line in batch file
        firstline$ = ""
        OPEN path + progs(lineno%).filename FOR INPUT AS 2
        IF NOT EOF(2) THEN
            LINE INPUT #2, firstline$
            idx% = INSTR(firstline$, ":::")
            IF idx% <> 0 THEN
                progs(lineno%).desc = LTRIM$(RIGHT$(firstline$, LEN(firstline$) - 3))
            END IF
        END IF
        CLOSE #2

        lineno% = lineno% + 1
    LOOP
    CLOSE #1
    ' lineno% is the number of files
    LOADPROGS = lineno%
END FUNCTION

'
' Displays a prompt and returns what the user types.
'
' @return the user input
'
FUNCTION PROMPT$
    DIM k AS STRING
    line$ = ""
    PRINT " ";
    LOCATE , , 1
    DO:
        SLEEP
        k = INKEY$
        ' this length check avoids processing of modifier keys like Shift
        IF LEN(k) = 1 THEN
            kcode% = ASC(k)
            IF kcode% = 8 THEN
                ' simulate "natural" behavior for backspace
                IF LEN(line$) > 0 THEN
                    ' only enable backspace if the line is not empty
                    col% = POS(0)
                    LOCATE , col% - 1
                    PRINT " ";
                    LOCATE , col% - 1
                    line$ = LEFT$(line$, LEN(line$) - 1)
                END IF
            ELSEIF kcode% > 31 THEN
                ' exclude other control characters
                line$ = line$ + k
                PRINT k;
            END IF
        END IF
    LOOP UNTIL k = CHR$(13)
    PRINT
    PROMPT = line$
END FUNCTION

'
' Performs a query against the program database.
'
' There are two special queries that are always available:
'
' * exit (or x), which exits the program to DOS
' * list, which lists the programs in the database
'
' This function does not attempt to calculate the database size using
' UBOUND, but relies on the supplied program count.
'
' @param text STRING the text query
' @param progs PROGRAM() the program database
' @param progcount INTEGER the number of programs in the database
'
SUB QUERY (text AS STRING, progs() AS PROGRAM, progcount AS INTEGER)
    text = LCASE$(text)
    IF text = "l" OR text = "list" THEN
        FOR i% = 0 TO progcount - 1
            IF ASC(progs(i%).desc) <> 0 THEN
                PRINT RTRIM$(progs(i%).desc) + " [";
            END IF
            PRINT RTRIM$(progs(i%).filename);
            IF ASC(progs(i%).desc) <> 0 THEN
                PRINT "]";
            END IF
            PRINT
        NEXT
        PRINT SPACE$(3) + STR$(progcount) + " batch file(s)"
    ELSEIF text = "x" OR text = "exit" THEN
        END
    END IF
END SUB


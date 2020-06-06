TYPE PROGRAM
    filename AS STRING * 12
    desc AS STRING * 64
END TYPE

CONST TMPFILE = "CLAM.TMP"
CONST ARROW = ""

' load entries, COMMAND$ contains the path as first (and only) argument
REDIM progs(64) AS PROGRAM
DIM progcount AS INTEGER
progcount = LOADPROGS(COMMAND$, progs())

END

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
FUNCTION LOADPROGS (path AS STRING, progs() AS PROGRAM)
    length% = UBOUND(progs)
    lineno% = LBOUND(progs)

    ' Saves the list of batch files to TMPFILE
    SHELL "DIR /b " + path + "*.BAT > " + TMPFILE

    OPEN TMPFILE FOR INPUT AS 1
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

        ' reads program filename from the shell output saved in TMPFILE
        LINE INPUT #1, progs(lineno%).filename

        ' reads program description from first line in batch file
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


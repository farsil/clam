# `clam`
`clam` is a modern program launcher for MS-DOS written in QuickBasic 4.5. It
has a light memory footprint, and it is easy to use.

# Setup
**Compilation note**: it is not recommended to compile `clam` in standalone
mode, as it has a much larger memory footprint.

`clam` works by detecting all batch files in a specified folder. It's not
necessaryto do anything in particular to make batch files in that folder
discoverable, but it's possible to specify a "title" in order to make them
easier to identify. You can add a title to a batch file by writing in the
_first line_ the special comment `:::`, for example:
```PX3.BAT
::: Planet X3

@ECHO OFF

C:\
CD \GAMES\PX3
CALL PX3_CGA.COM
```
The folder is specified by providing its path as a command line argument:
```
> CLAM C:\BATCH
```

# Usage
`clam` behaves like a shell, and queries the program list each time the user
inputs a keypress. `clam` searches for matches in both the program basename and
title, if present, and if it finds some results, the first one is displayed
next to the query:
```
→ pl_ (Planet X3)
```
where `_` marks the cursor location. Basename matches take priority over title
matches, so if you remember the batch filename, you are guaranteed that the 
first result will be the corresponding program.

## Navigation

You can edit the query as you'd expect from a shell:

- `←` and `→` move the cursor left and right
- `HOME` moves the cursor at the beginning of the line
- `END` moves the cursor at the end of the line
- `DELETE` deletes the character under the cursor
- `BACKSPACE` deletes the character before the cursor

If the query matches multiple results, you can also navigate among the result
set:

- `↓` or `TAB` move to the next result
- `↑` moves to the previous result

At last, `ENTER` executes the displayed program.

## Virtual programs

Among the real programs, `clam` adds two virtual programs:

- `EXIT` returns to DOS
- `LIST` shows a list of the registered programs, including itself and `EXIT`

They can be queried as normal:
```
→ ex_ (Exit to DOS)
```
or you can use the special characters `?` and `.` to invoke `LIST` and `EXIT`
respectively.

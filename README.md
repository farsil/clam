# `clam`
`clam` is a modern program launcher for MS-DOS written in x86 assembly.
Originally written in QuickBasic 4.5 it has a light memory footprint, 
and it is easy to use.

Its only requirement is an IBM AT or later class machine (essentially anything 
that has a 286 processor or better).

# Build

`clam` was written in the Turbo Assembler dialect, and can be built by
invoking the following commands, assuming the Turbo Assembler binaries are in
your `PATH`:

```
tasm /ml /m3 clam.asm
tlink /c /t clam.obj
```

The source files make the distinction between lowercase and uppercase symbols,
so it's important to compile and link using the appropriate switches.

# Setup

`clam` works by detecting all batch files in a specified folder. It's not
necessary to do anything in particular to make batch files in that folder
discoverable, but it's possible to specify a "title" in order to make them
easier to identify. You can add a title to a batch file by writing in the
_first line_ the special comment `:::`, for example:

```
::: Planet X3
@echo off

c:\
cd \games\px3
call px3_cga.com
```

The folder is specified by providing its path as a command line argument:

```
clam c:\batch
```

# Usage

`clam` behaves like a shell, and queries the program list each time the user
inputs a keypress. `clam` searches for matches in both the program basename and
title, if present, and if it finds some results, the first one is displayed
next to the query:

```
@ pl_ (Planet X3)
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

You can also immediately quit clam by sending a `CTRL + C` combination.

If the query matches multiple results, you can also navigate the result set:

- `↓` or `TAB` move to the next result
- `↑` moves to the previous result

At last, `ENTER` executes the displayed program.

## Virtual programs

Among the real programs, `clam` adds two virtual programs:

- `EXIT` returns to DOS
- `LIST` shows a list of the registered programs, including itself and `EXIT`

They can be queried as normal:
```
@ ex_ (Exit to DOS)
```
or you can use the special characters `*` and `.` to invoke `LIST` and `EXIT`
respectively.

@echo off
CLS
ECHO Compiling QuArK.exe without debug information
ECHO ---------------------------------------------
ECHO.
REM B = recompile ALL units, H = show hints, W = show warnings, Q = don't list all the unit file names
REM $D- = no debug info $L- = no local debug symbols
DCC32.EXE QuArK.dpr -B -H -W -Q -$D- -$L-
REM If the Delphi compiler didn't exit with an error level of 0, exit the batch file.
IF %ERRORLEVEL% == 1 GOTO Finished
ECHO.
ECHO.
ECHO Trying UPX compression
ECHO ----------------------
ECHO.
REM Make Windows look for UPX in the PATH system variable, but don't display the output.
UPX.EXE > NUL 2> NUL
REM The ERRORLEVEL for 'file not found' is 9009
IF %ERRORLEVEL% == 9009 GOTO NoUPX
REM Compress it and show output
UPX.EXE ..\runtime\QuArK.exe
GOTO Finished

:NoUPX
ECHO UPX.EXE is not in your PATH variable!
ECHO If you don't have a copy check out:
ECHO http://upx.sourceforge.net/

:Finished
ECHO.
PAUSE


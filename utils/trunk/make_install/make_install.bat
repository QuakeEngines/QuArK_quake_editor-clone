@echo off

rem ###################### CHANGE THESE TO YOUR SYSTEM SETTINGS ######################
set QK_RUNTIMEPATH=c:\quark\runtime
set QK_SOURCEPATH=c:\quark\source
set QK_INFOBASEPATH=c:\quark\infobase
set QK_UTILSPATH=c:\quark\utils
set QK_MYPATH=%QK_UTILSPATH%\make_install
set QK_UPXPATH=%QK_MYPATH%\upx\upx.exe
set QK_ZIPPERPATH=%QK_MYPATH%\zipper\zipper.exe
set QK_ADDMAKERPATH=%QK_MYPATH%\addmaker\addmaker.exe
set QK_SFXSTUBPATH=%QK_MYPATH%\sfx\sfx.exe
set QK_PERLEXE=c:\perl\perl\bin\perl.exe
rem ##################################################################################

rem ################################# clear vars #####################################
set QK_D3D=0
set QK_OUTPUTDIR=
set QK_DEBUG=0
set QK_VERSION=QuArK
set QK_ONLYHELP=xy
set QK_NOHELP=xx
set QK_NOVERSION=0
set QK_COMPRESSEXE=1
set QK_COMPILE=1
set QK_SFX=1
set QK_RUNTIME=1

rem ################################## setup vars ####################################
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="ALLOWD3D" set QK_D3D=1
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="DEBUG" set QK_DEBUG=1
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="ONLYHELP" set QK_ONLYHELP=1
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="NOHELP" set QK_NOHELP=1
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="NOCOMPRESSEXE" set QK_COMPRESSEXE=0
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="NOCOMPILE" set QK_COMPILE=0
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="NOSFX" set QK_SFX=0
for %%z in (%3 %4 %5 %6 %7 %8 %9 %10) do if "%%z%"=="NORUNTIME" set QK_RUNTIME=0

rem ##################################################################################
rem for more command line options, copy a 'for' line
rem and change it to what you want
rem ie:
rem   for %%z in (%3 %4 %5 %6 %7 %8 %9 %10 %11) do if "%%z%"=="EXTRA" set QK_EXTRA=0 
rem                                          /\
rem                     you need to add another possible commandline switch
rem                     to each for statement.
rem ##################################################################################

if "%1" == "" goto usage
if "%2" == "" goto usage

if "%1" == "HELP" goto usage

rem ################################ check for errors ################################

if "%QK_NOHELP%" == "%QK_ONLYHELP%" goto usage
if "%2" == "NOVERSION" set QK_NOVERSION=1
if not "%2" == "NOVERSION" set QK_VERSION=%2
set QK_OUTPUTDIR=%1

rem ##################################### START ######################################
goto setup-dirs
:post-setup-dirs
goto version
:post-version
goto runtime
:post-runtime
goto build
:post-build
goto remove-d3d
:post-remove-d3d
goto zip-up
:post-zip-up
goto help
:post-help
goto sfx
:post-sfx
goto end
rem ##################################### FINISH #####################################

rem #################################### copy files ##################################
:setup-dirs
echo Setting up destination directory
if exist %QK_OUTPUTDIR% deltree %QK_OUTPUTDIR%
md %QK_OUTPUTDIR% >nul
if "%QK_NOHELP%" == "xx" md %QK_OUTPUTDIR%\help >nul
goto post-setup-dirs

rem #################################### copy files ##################################
:runtime
if "%QK_ONLYHELP%" == "1" goto help
if "%QK_RUNTIME%"=="0" goto post-runtime
echo Copying runtime files...
copy %QK_SOURCEPATH%\AUTHORS.TXT %QK_OUTPUTDIR% >nul
copy %QK_SOURCEPATH%\COPYING.TXT %QK_OUTPUTDIR% >nul
xcopy %QK_RUNTIMEPATH%\*.* %QK_OUTPUTDIR% /E /EXCLUDE:%QK_MYPATH%\exclude.txt >nul
goto post-runtime

rem #################################### set version #################################
:version
if "%QK_NOVERSION%" == "1" goto end-version
cd %QK_UTILSPATH%
%QK_PERLEXE% setqversion.pl %QK_VERSION%
cd %QK_MYPATH%
echo Changed version to %QK_VERSION%
:end-version
goto post-version

rem ############################## create + copy + zip infobase ######################
:help
if "%QK_NOHELP%" == "1" goto post-help
echo Creating Help Files...
cd %QK_INFOBASEPATH%
build.py >nul
cd %QK_MYPATH%
echo Copying Help Files...
xcopy %QK_INFOBASEPATH%\output\*.* %QK_OUTPUTDIR%\help >nul
dir /B /S %QK_OUTPUTDIR%\help\*.* >filelist.txt
if exist "%QK_OUTPUTDIR%\%QK_VERSION%-Help.zip" del %QK_OUTPUTDIR%\%QK_VERSION%-Help.zip >nul
echo Zipping Help Files...
%QK_ZIPPERPATH% "-r%QK_OUTPUTDIR%" %QK_OUTPUTDIR%\%QK_VERSION%-Help.zip @filelist.txt >nul
del filelist.txt >nul
goto post-help

rem ################################## remove d3d files ##############################
:remove-d3d
if "%QK_RUNTIME%"=="0" goto post-remove-d3d
if "%QK_D3D%" == "1" goto post-remove-d3d
echo Removing D3D files...
del %QK_OUTPUTDIR%\dlls\d3dxas.dll >nul
del %QK_OUTPUTDIR%\dlls\readme.txt >nul
goto post-remove-d3d

rem ############################## build debug or non-debug exe ######################
:build
if "%QK_COMPILE%"=="0" goto post-build
echo Compiling new executable...
cd %QK_SOURCEPATH%
copy quark.cfg dcc32.cfg >nul
if "%QK_DEBUG%" == "1" goto debug
dcc32 -B -E%QK_OUTPUTDIR% QuArK.dpr >%QK_SOURCEPATH%\compile_errors.txt
goto post-dcc
:debug
echo DEBUG = ON
dcc32 -B -DDEBUG -E%QK_OUTPUTDIR% QuArK.dpr >%QK_SOURCEPATH%\compile_errors.txt
:post-dcc
if not exist "%QK_OUTPUTDIR%\QuArK.exe" echo There was an error during compile - see "%QK_SOURCEPATH%\compile_errors.txt"!!
if exist "%QK_OUTPUTDIR%\QuArK.exe" del %QK_SOURCEPATH%\compile_errors.txt
del dcc32.cfg >nul
cd %QK_MYPATH%
if not exist "%QK_OUTPUTDIR%\QuArK.exe" goto build-end
if "%QK_COMPRESSEXE%"=="0" goto build-end
echo Compressing executable...
%QK_UPXPATH% "%QK_OUTPUTDIR%\QuArK.exe" >nul
:build-end
goto post-build

rem ##################################### zip-up #####################################
:zip-up
echo Creating zip file...
dir /B /S %QK_OUTPUTDIR%\*.* >filelist.txt
if exist "%QK_OUTPUTDIR%\%QK_VERSION%.zip" del %QK_OUTPUTDIR%\%QK_VERSION%.zip >nul
%QK_ZIPPERPATH% "-r%QK_OUTPUTDIR%" %QK_OUTPUTDIR%\%QK_VERSION%.zip @filelist.txt >nul
del filelist.txt >nul
goto post-zip-up

rem ###################################### SFX #######################################
:sfx
if "%QK_SFX%"=="0" goto post-sfx
echo Creating sfx files...
if not exist %QK_OUTPUTDIR%\%QK_VERSION%.zip goto sfx-help
%QK_ADDMAKERPATH% -caption "%QK_VERSION% Installer" -message "This will install %QK_VERSION%. Do you want to continue?" -messagestyle 2 -defaultdir "c:\%QK_VERSION%" -output %QK_VERSION%.add -flags 12 -pyflags 5 -pyver 150 >nul
copy /b %QK_SFXSTUBPATH% + %QK_VERSION%.add + %QK_OUTPUTDIR%\%QK_VERSION%.zip %QK_OUTPUTDIR%\%QK_VERSION%-Setup.exe >nul
del %QK_VERSION%.add >nul
:sfx-help
if not exist %QK_OUTPUTDIR%\%QK_VERSION%-help.zip goto sfx-end
%QK_ADDMAKERPATH% -caption "%QK_VERSION% Help Installer" -message "This will help files for %QK_VERSION%. Do you want to continue?" -messagestyle 2 -defaultdir "c:\%QK_VERSION%" -output %QK_VERSION%-help.add -flags 12 -pyflags 0 -pyver 0 >nul
copy /b %QK_SFXSTUBPATH% + %QK_VERSION%-help.add + %QK_OUTPUTDIR%\%QK_VERSION%-help.zip %QK_OUTPUTDIR%\%QK_VERSION%-Setup-help.exe >nul
del %QK_VERSION%-help.add >nul
:sfx-end
goto post-sfx

rem ###################################### usage #####################################
:usage
echo Usage:
echo.
echo   make_install.bat dir version [option1] [option2] ....
echo.
echo   where:
echo     dir      = dir to move files to
echo                (must be fully qualified ie "c:\quark\install")
echo     version  = version of quark to build use "NOVERSION" if
echo                perl is not installed or no version change
echo                necessary.
echo                must be in quotes with no spaces,
echo                ie "QuArK_6.2" instead of "QuArK 6.2".
echo.
echo   options:
echo     DEBUG         = make debug exe [optional]
echo     ALLOWD3D      = doesn't remove quark d3d files [optional]
echo     NOHELP        = no help files created 
echo                     [optional - can't be used with ONLYHELP] 
echo     ONLYHELP      = only creates help files
echo                     [optional - can't be used with NOHELP] 
echo     NOCOMPILE     = doesn't compile a new .exe [optional]
echo     NOCOMPRESSEXE = doesn't compress .exe with "upx" [optional]
echo     NOSFX         = doesn't create sfx archives.
echo     NORUNTIME     = doesn't copy runtime files
goto end

:end
pause

Andys QuArK Batch File
======================================================================================

Before you use this batch file, you should set up a path to perl, if you wish
to use the 'version' setting, the variable is called "QK_PERLEXE" and is
near the top of the file. If you put a "Readme.txt" and/or "Patch.txt" in the same
directory as this batch file, then they will be included in the main zip file.
if you use NOVERSION as the version then it will the files produced will be called
"QuArK.zip", & "QuArK-Help.zip", if you specifiy a version then the files produced
will be called "(version).zip" and "(version)-Help.zip". this batch file must
be placed in the same directory as the "Source","Runtime", "Infobase", and "Utils"
directories.

Usage:

   make_install.bat dir version [option1] [option2] .....
   
   where:
     dir      = dir to move files to
                (must be fully qualified ie "c:\quark\install")
     version  = version of quark to build use "NOVERSION" if
                perl is not installed or no version change
                necessary.
                must be in quotes with no spaces,
                ie "QuArK_6.2" instead of "QuArK 6.2".
   otpions:
     DEBUG         = make debug exe [optional]
     ALLOWD3D      = doesn't remove quark d3d files [optional]
     NOHELP        = no help files created
                     [optional - can't be used with ONLYHELP] 
     ONLYHELP      = only creates help files
                     [optional - can't be used with NOHELP] 
     NOCOMPILE     = doesn't compile a new .exe [optional]
     NOCOMPRESSEXE = doesn't compress .exe with "upx" [optional] 
     NOSFX         = doesn't create sfx archives.
     NORUNTIME     = doesn't copy runtime files

======================================================================================

(c) 2001 Andy Vincent.

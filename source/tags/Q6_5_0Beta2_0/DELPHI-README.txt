This is the source code of QuArK. It should work with Delphi 2, 3, 4, 5.

*** INSTALLATION ***

Important ! Follow the steps below !

1) Checkout all the "source" module with your cvs program (I guess you
   already did if you are reading this).

2) Run Delphi but DON'T OPEN THE QUARK.DPR PROJECT IMMEDIATELY

3) Install the required components (found in the "components" subdirectory):

    For Delphi 4 and 5, you must install the package "vclar40"; doing so will
    install the Delphi components I use in QuArK. See Delphi's Component menu,
    Install Packages...

    For Delphi 2 and 3, directly install the components (Component, Install).
    All the .pas files in the "components" subdirectory contain components,
    expected "ShellObj.pas" and "BrowseForFolder.pas".

    Important : with Delphi 2 and 3, if the source code of any component is
    modified, you must do Component, Rebuild library. Be careful about it as
    Wincvs will probably not explicitely warn you that the component source code
    was modified. With later versions of Delphi I guess (but I'm not sure) that
    recompiling occurs automatically.

    The components in TB97 are by Jordan Russell; marsCaption is by Chen Ken.
    These ones are not covered by the GNU General Public Licence. The other
    components are by myself (Armin Rigo) and covered by the GNU GPL.

4) Only then you can open the "QuArK.dpr" project. Be careful that the source on
   cvs does not include (on purpose) a file "QuArK.cfg" which would give the
   compiling options of the project, because this file contains absolute paths.
   You have to open the Project, Options dialog box and configure a few things:

    First, in Conditional defines, add DEBUG. This will make QuArK compile as
    beta version with a lot of internal tests. Note: after you added or removed
    this setting, you must completely rebuild QuArK (Compile menu, Build) or you
    will get compilation errors.

    Second, set output directories. I would recommend that all .dcu and .exe files
    are written into a completely separated directory reserved for this purpose,
    like c:\temp\delphiexe.

    Third, if necessary, set the directories where Delphi should look for files.
    What you mainly need to add is the "components" subdirectory.

5) Compile and enjoy!


*** THE INFOBASE ***

Don't forget the link to the InfoBase:

  http://www.planetquake.com/quark/infobase/index.html

You are welcome to make changes to the cvs-stored InfoBase. After you did so,
visit this URL (warning, this URL might change later):

  http://imaux.unil.ch:8000/infobase/

Doing so will update the InfoBase at PlanetQuake.


*** RUNNING NOTES ***

QuArK expects to find a lot of things (the .qrk files, the quarkpy and plugins
directories, etc.) where the quark.exe file is. With a recent version of Delphi
you can configure it to write the compiled units (.dcu) into c:\temp\delphiexe
and only the quark.exe into another directory. Another solution is to use an
environment variable: if QUARKPATH is set, QuArK will use this path instead of
the path where the .exe is stored. To set environment variables on Win95/98, edit
your file "autoexec.bat" and add a line:

  SET QUARKPATH=c:\some\path

On WinNT, go to Control Panel, System. An alternate solution could be added if
you think it might be useful, e.g. having QuArK recognize a run-time parameter
like "--quark-path=...". Delphi lets you specify run-time parameters and they are
saved when you leave Delphi.


Armin Rigo
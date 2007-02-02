# Microsoft Developer Studio Project File - Name="VTFLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=VTFLib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "VTFLib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "VTFLib.mak" CFG="VTFLib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "VTFLib - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "VTFLib - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "VTFLib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I ".\NVDXT\\" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /D "_MBCS" /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I ".\NVDXT\\" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /D "_MBCS" /GZ /c
# ADD BASE MTL /nologo /win32
# ADD MTL /nologo /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /implib:"$(OutDir)/VTFLib.lib" /pdbtype:sept /libpath:".\NVDXT\\"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /implib:"$(OutDir)/VTFLib.lib" /pdbtype:sept /libpath:".\NVDXT\\"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
OutDir=.\Debug
SOURCE="$(InputPath)"
PostBuild_Desc=Coping binaries...
PostBuild_Cmds=copy "$(OutDir)\VTFLib.dll" "..\bin"	copy "$(OutDir)\VTFLib.lib" "..\lib"
# End Special Build Tool

!ELSEIF  "$(CFG)" == "VTFLib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Ot /Og /Oi /I ".\NVDXT\\" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /D "_MBCS" /c
# ADD CPP /nologo /MT /W3 /GX /Zi /Ot /Og /Oi /I ".\NVDXT\\" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /D "_MBCS" /c
# ADD BASE MTL /nologo /win32
# ADD MTL /nologo /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /implib:"$(OutDir)/VTFLib.lib" /pdbtype:sept /libpath:".\NVDXT\\" /opt:ref /opt:icf
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /implib:"$(OutDir)/VTFLib.lib" /pdbtype:sept /libpath:".\NVDXT\\" /opt:ref /opt:icf
# Begin Special Build Tool
OutDir=.\Release
SOURCE="$(InputPath)"
PostBuild_Desc=Coping binaries...
PostBuild_Cmds=copy "$(OutDir)\VTFLib.dll" "..\bin"	copy "$(OutDir)\VTFLib.lib" "..\lib"
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "VTFLib - Win32 Debug"
# Name "VTFLib - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;def;odl;idl;hpj;bat;asm;asmx"
# Begin Group "Library"

# PROP Default_Filter ""
# Begin Group "Nodes"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\VMTGroupNode.cpp
# End Source File
# Begin Source File

SOURCE=.\VMTIntegerNode.cpp
# End Source File
# Begin Source File

SOURCE=.\VMTNode.cpp
# End Source File
# Begin Source File

SOURCE=.\VMTSingleNode.cpp
# End Source File
# Begin Source File

SOURCE=.\VMTStringNode.cpp
# End Source File
# Begin Source File

SOURCE=.\VMTValueNode.cpp
# End Source File
# End Group
# Begin Group "IO"

# PROP Default_Filter ""
# Begin Group "Readers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\FileReader.cpp
# End Source File
# Begin Source File

SOURCE=.\MemoryReader.cpp
# End Source File
# Begin Source File

SOURCE=.\ProcReader.cpp
# End Source File
# End Group
# Begin Group "Writers"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\FileWriter.cpp
# End Source File
# Begin Source File

SOURCE=.\MemoryWriter.cpp
# End Source File
# Begin Source File

SOURCE=.\ProcWriter.cpp
# End Source File
# End Group
# End Group
# Begin Group "Diagnostics"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Error.cpp
# End Source File
# End Group
# Begin Group "Miscellaneous"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Float16.cpp
# End Source File
# End Group
# Begin Source File

SOURCE=.\Proc.cpp
# End Source File
# Begin Source File

SOURCE=.\VMTFile.cpp
# End Source File
# Begin Source File

SOURCE=.\VMTWrapper.cpp
# End Source File
# Begin Source File

SOURCE=.\VTFFile.cpp
# End Source File
# Begin Source File

SOURCE=.\VTFLib.cpp
# End Source File
# Begin Source File

SOURCE=.\VTFMathlib.cpp
# End Source File
# Begin Source File

SOURCE=.\VTFWrapper.cpp
# End Source File
# End Group
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;inc;xsd"
# Begin Group "Library No. 1"

# PROP Default_Filter ""
# Begin Group "Nodes No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\VMTGroupNode.h
# End Source File
# Begin Source File

SOURCE=.\VMTIntegerNode.h
# End Source File
# Begin Source File

SOURCE=.\VMTNode.h
# End Source File
# Begin Source File

SOURCE=.\VMTNodes.h
# End Source File
# Begin Source File

SOURCE=.\VMTSingleNode.h
# End Source File
# Begin Source File

SOURCE=.\VMTStringNode.h
# End Source File
# Begin Source File

SOURCE=.\VMTValueNode.h
# End Source File
# End Group
# Begin Group "IO No. 1"

# PROP Default_Filter ""
# Begin Group "Readers No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\FileReader.h
# End Source File
# Begin Source File

SOURCE=.\MemoryReader.h
# End Source File
# Begin Source File

SOURCE=.\ProcReader.h
# End Source File
# Begin Source File

SOURCE=.\Reader.h
# End Source File
# Begin Source File

SOURCE=.\Readers.h
# End Source File
# End Group
# Begin Group "Writers No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\FileWriter.h
# End Source File
# Begin Source File

SOURCE=.\MemoryWriter.h
# End Source File
# Begin Source File

SOURCE=.\ProcWriter.h
# End Source File
# Begin Source File

SOURCE=.\Writer.h
# End Source File
# Begin Source File

SOURCE=.\Writers.h
# End Source File
# End Group
# End Group
# Begin Group "Diagnostics No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Error.h
# End Source File
# End Group
# Begin Group "Miscellaneous No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Float16.h
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\stdafx.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\Proc.h
# End Source File
# Begin Source File

SOURCE=.\VMTFile.h
# End Source File
# Begin Source File

SOURCE=.\VMTWrapper.h
# End Source File
# Begin Source File

SOURCE=.\VTFDXTn.h
# End Source File
# Begin Source File

SOURCE=.\VTFFile.h
# End Source File
# Begin Source File

SOURCE=.\VTFFormat.h
# End Source File
# Begin Source File

SOURCE=.\VTFLib.h
# End Source File
# Begin Source File

SOURCE=.\VTFMathlib.h
# End Source File
# Begin Source File

SOURCE=.\VTFWrapper.h
# End Source File
# End Group
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "rc;ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe;resx"
# Begin Source File

SOURCE=.\Resource.rc
# End Source File
# End Group
# End Target
# End Project

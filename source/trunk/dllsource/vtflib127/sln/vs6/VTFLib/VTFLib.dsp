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
!MESSAGE "VTFLib - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "VTFLib - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "VTFLib - Win32 Release"

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
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "VTFLib - Win32 Debug"

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
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /YX /FD /GZ  /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "VTFLIB_EXPORTS" /YX /FD /GZ  /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "VTFLib - Win32 Release"
# Name "VTFLib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "Diagnostics Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\Error.cpp
# End Source File
# End Group
# Begin Group "Node Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTGroupNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTIntegerNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTSingleNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTStringNode.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTValueNode.cpp
# End Source File
# End Group
# Begin Group "IO Source Files"

# PROP Default_Filter ""
# Begin Group "Reader Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\FileReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\MemoryReader.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\ProcReader.cpp
# End Source File
# End Group
# Begin Group "Writer Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\FileWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\MemoryWriter.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\ProcWriter.cpp
# End Source File
# End Group
# Begin Source File

SOURCE=..\..\..\VTFLib\Proc.cpp
# End Source File
# End Group
# Begin Group "Miscellaneous Source Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\Float16.cpp
# End Source File
# End Group
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTFile.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTWrapper.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFFile.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFLib.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFMathlib.cpp
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFWrapper.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Group "Diagnostics Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\Error.h
# End Source File
# End Group
# Begin Group "Node Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTGroupNode.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTIntegerNode.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTNode.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTNodes.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTSingleNode.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTStringNode.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTValueNode.h
# End Source File
# End Group
# Begin Group "IO Header Files"

# PROP Default_Filter ""
# Begin Group "Reader Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\FileReader.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\MemoryReader.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\ProcReader.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\Reader.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\Readers.h
# End Source File
# End Group
# Begin Group "Writer Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\FileWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\MemoryWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\ProcWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\Writer.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\Writers.h
# End Source File
# End Group
# Begin Source File

SOURCE=..\..\..\VTFLib\Proc.h
# End Source File
# End Group
# Begin Group "Miscellaneous Header Files"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\..\..\VTFLib\Float16.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\resource.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\stdafx.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFDXTn.h
# End Source File
# End Group
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTFile.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VMTWrapper.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFFile.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFFormat.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFLib.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFMathlib.h
# End Source File
# Begin Source File

SOURCE=..\..\..\VTFLib\VTFWrapper.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\..\..\VTFLib\Resource.rc
# End Source File
# End Group
# End Target
# End Project

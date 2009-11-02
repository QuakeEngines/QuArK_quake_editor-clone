# Microsoft Developer Studio Project File - Name="HLLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=HLLib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "HLLib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "HLLib.mak" CFG="HLLib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "HLLib - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "HLLib - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "HLLib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "HLLIB_EXPORTS" /D "_MBCS" /GZ PRECOMP_VC7_TOBEREMOVED /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "HLLIB_EXPORTS" /D "_MBCS" /GZ /c
# ADD BASE MTL /nologo /win32
# ADD MTL /nologo /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /implib:"$(OutDir)/HLLib.lib" /pdbtype:sept
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /out:"../bin/debug/HLLib.dll" /implib:"../lib/debug/HLLib.lib" /pdbtype:sept
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "HLLib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "HLLIB_EXPORTS" /D "_MBCS" PRECOMP_VC7_TOBEREMOVED /c
# ADD CPP /nologo /MT /W3 /GX /Zi /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "HLLIB_EXPORTS" /D "_MBCS" /c
# ADD BASE MTL /nologo /win32
# ADD MTL /nologo /win32
# ADD BASE RSC /l 0x409
# ADD RSC /l 0x409
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /implib:"$(OutDir)/HLLib.lib" /pdbtype:sept /opt:ref /opt:icf
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:IX86 /out:"../bin/HLLib.dll" /implib:"../lib/HLLib.lib" /pdbtype:sept /opt:ref /opt:icf
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "HLLib - Win32 Debug"
# Name "HLLib - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;def;odl;idl;hpj;bat;asm;asmx"
# Begin Group "DirectoryItems"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\DirectoryFile.cpp
DEP_CPP_DIREC=\
	".\DirectoryFile.h"\
	".\DirectoryItem.h"\
	".\HLLib.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\DirectoryFolder.cpp
DEP_CPP_DIRECT=\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\HLLib.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\DirectoryItem.cpp
DEP_CPP_DIRECTO=\
	".\DirectoryItem.h"\
	".\HLLib.h"\
	".\stdafx.h"\
	
# End Source File
# End Group
# Begin Group "Packages"

# PROP Default_Filter ""
# Begin Group "Mapping"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\FileMapping.cpp
# End Source File
# Begin Source File

SOURCE=.\Mapping.cpp
# End Source File
# Begin Source File

SOURCE=.\MemoryMapping.cpp
# End Source File
# End Group
# Begin Source File

SOURCE=.\BSPFile.cpp
DEP_CPP_BSPFI=\
	".\BSPFile.h"\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\HLLib.h"\
	".\MappedPackage.h"\
	".\Package.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\GCFFile.cpp
DEP_CPP_GCFFI=\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\GCFFile.h"\
	".\HLLib.h"\
	".\MappedPackage.h"\
	".\Package.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\MappedPackage.cpp
DEP_CPP_MAPPE=\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\HLLib.h"\
	".\MappedPackage.h"\
	".\Package.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\Package.cpp
DEP_CPP_PACKA=\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\HLLib.h"\
	".\Package.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\PAKFile.cpp
DEP_CPP_PAKFI=\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\HLLib.h"\
	".\MappedPackage.h"\
	".\Package.h"\
	".\PAKFile.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\WADFile.cpp
DEP_CPP_WADFI=\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\HLLib.h"\
	".\MappedPackage.h"\
	".\Package.h"\
	".\stdafx.h"\
	".\WADFile.h"\
	
# End Source File
# End Group
# Begin Group "Miscellaneous"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Error.cpp
# End Source File
# Begin Source File

SOURCE=.\HLLib.cpp
DEP_CPP_HLLIB=\
	".\HLLib.h"\
	".\stdafx.h"\
	
# End Source File
# Begin Source File

SOURCE=.\stdafx.cpp
DEP_CPP_STDAF=\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "HLLib - Win32 Debug"

# ADD CPP /nologo /GX /Yc"stdafx.h" /GZ

!ELSEIF  "$(CFG)" == "HLLib - Win32 Release"

# ADD CPP /nologo /GX /Yc"stdafx.h"

!ENDIF 

# End Source File
# End Group
# Begin Group "Utilities"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\PackageFactory.cpp
DEP_CPP_PACKAG=\
	".\BSPFile.h"\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\GCFFile.h"\
	".\HLLib.h"\
	".\MappedPackage.h"\
	".\Package.h"\
	".\PackageFactory.h"\
	".\PAKFile.h"\
	".\stdafx.h"\
	".\WADFile.h"\
	
# End Source File
# Begin Source File

SOURCE=.\PackageUtility.cpp
DEP_CPP_PACKAGE=\
	".\DirectoryFile.h"\
	".\DirectoryFolder.h"\
	".\DirectoryItem.h"\
	".\DirectoryItems.h"\
	".\HLLib.h"\
	".\Package.h"\
	".\PackageUtility.h"\
	".\stdafx.h"\
	
# End Source File
# End Group
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;inc;xsd"
# Begin Group "DirectoryItems No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\DirectoryFile.h
# End Source File
# Begin Source File

SOURCE=.\DirectoryFolder.h
# End Source File
# Begin Source File

SOURCE=.\DirectoryItem.h
# End Source File
# Begin Source File

SOURCE=.\DirectoryItems.h
# End Source File
# End Group
# Begin Group "Packages No. 1"

# PROP Default_Filter ""
# Begin Group "Mapping No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\FileMapping.h
# End Source File
# Begin Source File

SOURCE=.\Mapping.h
# End Source File
# Begin Source File

SOURCE=.\Mappings.h
# End Source File
# Begin Source File

SOURCE=.\MemoryMapping.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\BSPFile.h
# End Source File
# Begin Source File

SOURCE=.\GCFFile.h
# End Source File
# Begin Source File

SOURCE=.\MappedPackage.h
# End Source File
# Begin Source File

SOURCE=.\Package.h
# End Source File
# Begin Source File

SOURCE=.\PAKFile.h
# End Source File
# Begin Source File

SOURCE=.\WADFile.h
# End Source File
# End Group
# Begin Group "Miscellaneous No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Error.h
# End Source File
# Begin Source File

SOURCE=.\HLLib.h
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\stdafx.h
# End Source File
# End Group
# Begin Group "Utilities No. 1"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\PackageFactory.h
# End Source File
# Begin Source File

SOURCE=.\PackageUtility.h
# End Source File
# End Group
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "rc;ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe;resx"
# Begin Source File

SOURCE=.\HLLib.rc
# End Source File
# End Group
# End Target
# End Project

#
# NSIS installer script for QuArK 6.4+
# Copyright (C) 2003  Peter Brett <peter@peter-b.co.uk>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA
#
# Builds an NSIS installer for QuArK
#
# $Id$
#

#---------------#
# QuArK Version #
#---------------#

# These defines are used to produce the output filename and installer
# name, and a couple of registry entries (i.e. they're important):
#   e.g. quark-win32-6.4.0alpha1.exe
#   e.g. Quake Army Knife 6.4.1 rel 5 Setup

!define QRK_MAJOR_VER 6.4
!define QRK_MINOR_VER 0
!define QRK_STATE "beta"
!define QRK_RELEASE 1

#-----------------------------#
# Setup the installer GUI etc #
#-----------------------------#

!include "MUI.nsh" # Uses NSIS Modern UI

!define QRK_VERSION "${QRK_MAJOR_VER}.${QRK_MINOR_VER} ${QRK_STATE} ${QRK_RELEASE}"
!define QRK_NAME "QuArK ${QRK_VERSION}"
!define QRK_OUTFILE "quark-win32-${QRK_MAJOR_VER}.${QRK_MINOR_VER}${QRK_STATE}${QRK_RELEASE}.exe"

Var STARTMENU_FOLDER

Name "${QRK_NAME}"
OutFile ${QRK_OUTFILE}
InstallDir "$PROGRAMFILES\QuArK"
InstallDirRegKey HKLM "SOFTWARE\${MUI_PRODUCT}" "INSTDIR"

!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "install_header.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP "install_splash.bmp"
!define MUI_WELCOMEPAGE_TEXT "This wizard will guide you through the installation of the Quake Army Knife (QuArK), an editor for games with a Quake-like engine.\r\n\r\n"
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "SOFTWARE\QuArK"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "STARTMENUDIR"
!define MUI_STARTMENUPAGE_DEFAULTFOLDER "QuArK"
!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_FINISHPAGE_RUN "$INSTDIR\QuArK.exe"
!define MUI_FINISHPAGE_RUN_TEXT "Run QuArK"
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\readme.txt"
!define MUI_FINISHPAGE_LINK "Visit the QuArK website"
!define MUI_FINISHPAGE_LINK_LOCATION http://www.planetquake.com/quark

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "license.rtf"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_STARTMENU "QuArK" $STARTMENU_FOLDER
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

Function .onInit
  SetShellVarContext all
FunctionEnd

#---------------#
# Install QuArK #
#---------------#

Section "QuArK" SectionQuArK
  SectionIn RO
  SetOutPath "$INSTDIR"

  # Create Shortcuts  
  !insertmacro MUI_STARTMENU_WRITE_BEGIN "QuArK"

    Push $R0
    StrCpy $R0 $STARTMENU_FOLDER
    CreateShortCut "$R0\QuArK.lnk" "$INSTDIR\QuArK.exe" "" "" "" "" "" "Quake Army Knife"
    CreateShortCut "$R0\QuArK Readme.lnk" "$INSTDIR\readme.txt"
    CreateShortCut "$R0\Uninstall QuArK.lnk" "$INSTDIR\uninstall.exe"
    Pop $R0

  !insertmacro MUI_STARTMENU_WRITE_END

  WriteUninstaller "uninstall.exe"

  # Write info for Add/Remove Programs
  WriteRegStr "HKLM" "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\QuArK" "DisplayName" \
	"Quake Army Knife ${QRK_VERSION} (remove only)"
  WriteRegStr "HKLM" "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\QuArK" "UninstallString" "$INSTDIR\uninstall.exe"
  WriteRegStr "HKLM" "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\QuArK" "InstallLocation" "$INSTDIR"
  WriteRegDWORD "HKLM" "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\QuArK" "NoModify" 1
  WriteRegDWORD "HKLM" "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\QuArK" "NoRepair" 1
  
  # Write other registry info
  WriteRegStr HKLM "SOFTWARE\QuArK" "INSTDIR" $INSTDIR

  # Install files
  SetOutPath $INSTDIR
  File /r "quark\help"
  File /r "quark\plugins"
  File /r "quark\quarkpy"
  File /r "quark\dlls"
  File /r "quark\images"
  File /r "quark\lgicons"
  File /r "quark\addons"
  File "quark\README.txt"
  File "quark\COPYING.txt"
  File "quark\AUTHORS.txt"
  File "quark\NEWS.txt"
  File "quark\QuArK.exe"
SectionEnd

#-------------#
# Uninstaller #
#-------------#

Section "Uninstall"
  SetShellVarContext all

  # Delete Shortcuts
  Push $R0
  !insertmacro MUI_STARTMENU_GETFOLDER "QuArK" $R0
  StrCpy $R0 "$SMPROGRAMS\$R0"
  Delete "$R0\QuArK.lnk"
  Delete "$R0\QuArK Readme.lnk"
  Delete "$R0\Uninstall QuArK.lnk"
  RMDir $R0
  Pop $R0

  Delete "$INSTDIR\QuArK.exe"
  Delete "$INSTDIR\readme.txt"
  Delete "$INSTDIR\COPYING.txt"
  Delete "$INSTDIR\uninstall.exe"
  Delete "$INSTDIR\QUARK.LOG"
  Delete "$INSTDIR\Setup.qrk"
  RMDIR /r "$INSTDIR\help"
  RMDIR /r "$INSTDIR\plugins"
  RMDIR /r "$INSTDIR\quarkpy"
  RMDIR /r "$INSTDIR\dlls"
  RMDIR /r "$INSTDIR\lib"
  RMDIR /r "$INSTDIR\images"
  RMDIR /r "$INSTDIR\lgicons"
  RMDIR /r "$INSTDIR\addons"

  DeleteRegKey "HKLM" "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\QuArK"
  DeleteRegKey "HKLM" "SOFTWARE\QuArK"

  RMDIR $INSTDIR
SectionEnd
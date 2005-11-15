#$Header: /cvsroot/quark/source/QuArk.nsi,v 1.3 2005/11/15 07:44:38 cdunde Exp $


; QuArK installer x.x.x
; HomePage: http://dynamic.gamespy.com/~quark/
; Version:  NSIS 2.08
; Arthour:  Fredrick Vamstad
; Date:     18 Aug. 2005
; nullsoft NSIS installer program available at:
;   http://nsis.sourceforge.net
;
; Last update 1 Nov. 2005 - cdunde
;
; Setup and Use to create QuArK NSIS installer:
; ============================================
; 1) Change " PRODUCT_VERSION " (line 24) below.
; 2) Change " OutFile " name (line 77) below.
; 3) Create folder named " QuArK_installer_files " in C:\ directory.
; 4) Place QuArK .exe and all runtime files in the above folder.
; 5) Create folder named " QuArK_installer_splash_image " in C:\ directory.
; 6) Copy splash.bmp file from source\icones folder to the above folder.
; 7) Click on NSIS.exe to start program, select "MakeNSISW (compiler interface)".
; 8) Drag this file, QuArK.nsi, into the compiler window.
; 9) The finished QuArK installer will be place in the same location as this file.

!define PRODUCT_NAME "QuArK"
!define PRODUCT_VERSION "6.5.0 Alpha 3"
!define PRODUCT_WEB_SITE "http://dynamic.gamespy.com/~quark/"
!define PRODUCT_WEB_Forum "http://quark.ironfoot.co.uk/"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\QuArK.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

; MUI 1.67 compatible ------
!include "MUI.nsh"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install-blue.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall-blue.ico"
; Loads the splash window
!define MUI_WELCOMEFINISHPAGE_BITMAP "C:\QuArK_installer_splash_image\splash.bmp"

; Language Selection Dialog Settings
!define MUI_LANGDLL_REGISTRY_ROOT "${PRODUCT_UNINST_ROOT_KEY}"
!define MUI_LANGDLL_REGISTRY_KEY "${PRODUCT_UNINST_KEY}"
!define MUI_LANGDLL_REGISTRY_VALUENAME "NSIS:Language"

; Welcome page
!insertmacro MUI_PAGE_WELCOME
; License page
!define MUI_LICENSEPAGE_CHECKBOX
!insertmacro MUI_PAGE_LICENSE "C:\QuArK_installer_files\COPYING.txt"
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\QuArK.exe"
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\README.txt"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "Arabic"
!insertmacro MUI_LANGUAGE "Dutch"
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "Finnish"
!insertmacro MUI_LANGUAGE "French"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Greek"
!insertmacro MUI_LANGUAGE "Norwegian"
!insertmacro MUI_LANGUAGE "Russian"
!insertmacro MUI_LANGUAGE "TradChinese"
; MUI end ------

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "quark-win32-6.5.0alpha3.exe"
InstallDir "$PROGRAMFILES\QuArK"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show

Function .onInit
  !insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

Section "MainSection" SEC01
  CreateDirectory "$SMPROGRAMS\QuArK"
  CreateShortCut "$SMPROGRAMS\QuArK\QuArK.lnk" "$INSTDIR\QuArK.exe"
  CreateShortCut "$DESKTOP\QuArK.lnk" "$INSTDIR\QuArK.exe"
  SetOutPath "$INSTDIR\addons\6DX"
  File "C:\QuArK_installer_files\addons\6DX\*.*"
  SetOutPath "$INSTDIR\addons\Crystal_Space"
  File "C:\QuArK_installer_files\addons\Crystal_Space\*.*"
  SetOutPath "$INSTDIR\addons\Doom_3"
  File "C:\QuArK_installer_files\addons\Doom_3\*.*"
  SetOutPath "$INSTDIR\addons\Genesis3D"
  File "C:\QuArK_installer_files\addons\Genesis3D\*.*"
  SetOutPath "$INSTDIR\addons\Half-Life"
  File "C:\QuArK_installer_files\addons\Half-Life\*.*"
  SetOutPath "$INSTDIR\addons\Half-Life2"
  File "C:\QuArK_installer_files\addons\Half-Life2\*.*"
  SetOutPath "$INSTDIR\addons\Heretic_II"
  File "C:\QuArK_installer_files\addons\Heretic_II\*.*"
  SetOutPath "$INSTDIR\addons\Hexen_II"
  File "C:\QuArK_installer_files\addons\Hexen_II\*.*"
  SetOutPath "$INSTDIR\addons\JA"
  File "C:\QuArK_installer_files\addons\JA\*.*"
  SetOutPath "$INSTDIR\addons\JK2"
  File "C:\QuArK_installer_files\addons\JK2\*.*"
  SetOutPath "$INSTDIR\addons\KingPin"
  File "C:\QuArK_installer_files\addons\KingPin\*.*"
  SetOutPath "$INSTDIR\addons\MOHAA"
  File "C:\QuArK_installer_files\addons\MOHAA\*.*"
  SetOutPath "$INSTDIR\addons\Quake_1"
  File "C:\QuArK_installer_files\addons\Quake_1\*.*"
  SetOutPath "$INSTDIR\addons\Quake_2"
  File "C:\QuArK_installer_files\addons\Quake_2\*.*"
  SetOutPath "$INSTDIR\addons\Quake_3"
  File "C:\QuArK_installer_files\addons\Quake_3\*.*"
  SetOutPath "$INSTDIR\addons\RTCW"
  File "C:\QuArK_installer_files\addons\RTCW\*.*"
  SetOutPath "$INSTDIR\addons\Sin"
  File "C:\QuArK_installer_files\addons\Sin\*.*"
  SetOutPath "$INSTDIR\addons\SOF"
  File "C:\QuArK_installer_files\addons\SOF\*.*"
  SetOutPath "$INSTDIR\addons\SoF2"
  File "C:\QuArK_installer_files\addons\SoF2\*.*"
  SetOutPath "$INSTDIR\addons\STVEF"
  File "C:\QuArK_installer_files\addons\STVEF\*.*"
  SetOutPath "$INSTDIR\addons\Sylphis"
  File "C:\QuArK_installer_files\addons\Sylphis\*.*"
  SetOutPath "$INSTDIR\addons\Torque"
  File "C:\QuArK_installer_files\addons\Torque\*.*"
  SetOutPath "$INSTDIR\addons\WildWest"
  File "C:\QuArK_installer_files\addons\WildWest\*.*"
  SetOutPath "$INSTDIR\addons"
  File "C:\QuArK_installer_files\addons\*.*"
  SetOutPath "$INSTDIR\dlls"
  File "C:\QuArK_installer_files\dlls\*.*"
  SetOutPath "$INSTDIR\help"
  File "C:\QuArK_installer_files\help\*.*"
  SetOutPath "$INSTDIR\images"
  File "C:\QuArK_installer_files\images\*.*"
  SetOutPath "$INSTDIR\lgicons"
  File "C:\QuArK_installer_files\lgicons\*.*"
  SetOutPath "$INSTDIR\plugins"
  File "C:\QuArK_installer_files\plugins\*.*"
  SetOutPath "$INSTDIR\quarkpy"
  File "C:\QuArK_installer_files\quarkpy\*.*"
  SetOutPath "$INSTDIR"
  File "C:\QuArK_installer_files\*.*"
SectionEnd

Section -AdditionalIcons
  WriteIniStr "$INSTDIR\${PRODUCT_NAME}.url" "InternetShortcut" "URL" "${PRODUCT_WEB_SITE}"
  CreateShortCut "$SMPROGRAMS\QuArK\Website.lnk" "$INSTDIR\${PRODUCT_NAME}.url"
  CreateShortCut "$SMPROGRAMS\QuArK\Uninstall.lnk" "$INSTDIR\uninst.exe"
SectionEnd

; PuG's forum
Section -AdditionalIcons
  CreateShortCut "$SMPROGRAMS\QuArK\Forum.lnk" "${PRODUCT_WEB_Forum}"
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\QuArK.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\QuArK.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_Forum}"
SectionEnd


Function un.onInit
!insertmacro MUI_UNGETLANGUAGE
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nRemove any files you wish to save before clicking 'Yes' to continue this uninstall." IDYES +2
  Abort
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Are you sure you want to completely remove $(^Name) and all of its components now?" IDYES +2
  Abort
FunctionEnd

Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(^Name) was successfully removed from your computer."
FunctionEnd


Section Uninstall
  Delete "$INSTDIR\${PRODUCT_NAME}.url"
  Delete "$INSTDIR\addons\WildWest\*.*"
  Delete "$INSTDIR\addons\Torque\*.*"
  Delete "$INSTDIR\addons\Sylphis\*.*"
  Delete "$INSTDIR\addons\STVEF\*.*"
  Delete "$INSTDIR\addons\SoF2\*.*"
  Delete "$INSTDIR\addons\SOF\*.*"
  Delete "$INSTDIR\addons\Sin\*.*"
  Delete "$INSTDIR\addons\RTCW\*.*"
  Delete "$INSTDIR\addons\Quake_3\*.*"
  Delete "$INSTDIR\addons\Quake_2\*.*"
  Delete "$INSTDIR\addons\Quake_1\*.*"
  Delete "$INSTDIR\addons\MOHAA\*.*"
  Delete "$INSTDIR\addons\KingPin\*.*"
  Delete "$INSTDIR\addons\JK2\*.*"
  Delete "$INSTDIR\addons\JA\*.*"
  Delete "$INSTDIR\addons\Hexen_II\*.*"
  Delete "$INSTDIR\addons\Heretic_II\*.*"
  Delete "$INSTDIR\addons\Half-Life2\*.*"
  Delete "$INSTDIR\addons\Half-Life\*.*"
  Delete "$INSTDIR\addons\Genesis3D\*.*"
  Delete "$INSTDIR\addons\Doom_3\*.*"
  Delete "$INSTDIR\addons\Crystal_Space\*.*"
  Delete "$INSTDIR\addons\6DX\*.*"
  Delete "$INSTDIR\addons\*.*"
  Delete "$INSTDIR\dlls\*.*"
  Delete "$INSTDIR\help\*.*"
  Delete "$INSTDIR\images\*.*"
  Delete "$INSTDIR\lgicons\*.*"
  Delete "$INSTDIR\plugins\*.*"
  Delete "$INSTDIR\quarkpy\*.*"
  Delete "$INSTDIR\*.*"


  Delete "$SMPROGRAMS\QuArK\*.*"
  Delete "$DESKTOP\QuArK.lnk"


  RMDir "$INSTDIR\addons\WildWest"
  RMDir "$INSTDIR\addons\Torque"
  RMDir "$INSTDIR\addons\Sylphis"
  RMDir "$INSTDIR\addons\STVEF"
  RMDir "$INSTDIR\addons\SoF2"
  RMDir "$INSTDIR\addons\SOF"
  RMDir "$INSTDIR\addons\Sin"
  RMDir "$INSTDIR\addons\RTCW"
  RMDir "$INSTDIR\addons\Quake_3"
  RMDir "$INSTDIR\addons\Quake_2"
  RMDir "$INSTDIR\addons\Quake_1"
  RMDir "$INSTDIR\addons\MOHAA"
  RMDir "$INSTDIR\addons\KingPin"
  RMDir "$INSTDIR\addons\JK2"
  RMDir "$INSTDIR\addons\JA"
  RMDir "$INSTDIR\addons\Hexen_II"
  RMDir "$INSTDIR\addons\Heretic_II"
  RMDir "$INSTDIR\addons\Half-Life2"
  RMDir "$INSTDIR\addons\Half-Life"
  RMDir "$INSTDIR\addons\Genesis3D"
  RMDir "$INSTDIR\addons\Doom_3"
  RMDir "$INSTDIR\addons\Crystal_Space"
  RMDir "$INSTDIR\addons\6DX"
  RMDir "$INSTDIR\addons"
  RMDir "$INSTDIR\dlls"
  RMDir "$INSTDIR\help"
  RMDir "$INSTDIR\images"
  RMDir "$INSTDIR\lgicons"
  RMDir "$INSTDIR\plugins"
  RMDir "$INSTDIR\quarkpy"
  RMDir "$INSTDIR"
  RMDir "$SMPROGRAMS\QuArK"


  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd


# ----------- REVISION HISTORY ------------
#
# $Log: QuArK.nsi,v $
#
# Revision 1.3  2005/11/15 07:44:38  cdunde
# To setup Header and history Log for
# QuArK  NSIS installer script
#
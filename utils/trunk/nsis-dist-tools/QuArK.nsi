
; QuArK installer x.x.x
; HomePage: http://dynamic.gamespy.com/~quark/
; Version:  NSIS 2.22
; Author:  Fredrick Vamstad & DanielPharos
; Date:     18 Aug. 2005 & 5 January 2007
; nullsoft NSIS installer program available at:
;   http://nsis.sourceforge.net
;
; Last update 5 January. 2007 - DanielPharos
;
; Setup and Use to create QuArK NSIS installer:
; ============================================
; 1) Change " BUILDDIR " (line 25) below to the directory containing the runtime files to use (including the executable!).
; 2) Change " INSTALLEREXENAME " name (line 26) below to the name of the installer executable file.
; 3) Change " PRODUCT_VERSION " (line 28) below to match the new version number.
; 4) Click on NSIS.exe to start program, select "MakeNSISW (compiler interface)".
; 5) Drag this file, QuArK.nsi, into the compiler window, or use the "File > Load Script" method to open this file.
; 6) The finished QuArK installer will be place in the same location as this file, ready for distrubution!

!define BUILDDIR "C:\QuArK_installer_files"
!define INSTALLEREXENAME "quark-win32-6.5.0Beta1.exe"
!define PRODUCT_NAME "QuArK"
!define PRODUCT_VERSION "6.5.0 Beta 1"
!define PRODUCT_WEB_SITE "http://quark.planetquake.gamespy.com/"
!define PRODUCT_WEB_Forum "http://www.dark-forge.com/"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\QuArK.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "${INSTALLEREXENAME}"
InstallDir "$PROGRAMFILES\QuArK"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show
SetCompressor LZMA   ; We will use LZMA for best compression

; MUI 1.76 compatible ------
!include "MUI.nsh"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_ABORTWARNING_CANCEL_DEFAULT
!define MUI_UNABORTWARNING
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install-blue.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall-blue.ico"
; Loads the splash window
!define MUI_WELCOMEFINISHPAGE_BITMAP "install_splash.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "install_splash.bmp"
; Loads the header picture
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "install_header.bmp"
!define MUI_HEADERIMAGE_UNBITMAP "install_header.bmp"

; Language Selection Dialog Settings
!define MUI_LANGDLL_REGISTRY_ROOT "${PRODUCT_UNINST_ROOT_KEY}"
!define MUI_LANGDLL_REGISTRY_KEY "${PRODUCT_UNINST_KEY}"
!define MUI_LANGDLL_REGISTRY_VALUENAME "NSIS:Language"

; Welcome page
!insertmacro MUI_PAGE_WELCOME
; License page
!define MUI_LICENSEPAGE_CHECKBOX
!insertmacro MUI_PAGE_LICENSE "${BUILDDIR}\COPYING.txt"
; Component page
!define MUI_COMPONENTSPAGE_SMALLDESC
!insertmacro MUI_PAGE_COMPONENTS
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!define MUI_FINISHPAGE_RUN "$INSTDIR\QuArK.exe"
!define MUI_FINISHPAGE_SHOWREADME "$INSTDIR\README.txt"
!define MUI_FINISHPAGE_LINK "Click here to go to the QuArK website"
!define MUI_FINISHPAGE_LINK_LOCATION "${PRODUCT_WEB_SITE}"
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

Function .onInit
  !insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

Section "Main Program Files" SEC01
  CreateDirectory "$SMPROGRAMS\QuArK"
  CreateShortCut "$SMPROGRAMS\QuArK\QuArK.lnk" "$INSTDIR\QuArK.exe"
  CreateShortCut "$DESKTOP\QuArK.lnk" "$INSTDIR\QuArK.exe"
  SetOutPath "$INSTDIR\addons\6DX"
  File "${BUILDDIR}\addons\6DX\*.*"
  SetOutPath "$INSTDIR\addons\Crystal_Space"
  File "${BUILDDIR}\addons\Crystal_Space\*.*"
  SetOutPath "$INSTDIR\addons\Doom_3"
  File "${BUILDDIR}\addons\Doom_3\*.*"
  SetOutPath "$INSTDIR\addons\Genesis3D"
  File "${BUILDDIR}\addons\Genesis3D\*.*"
  SetOutPath "$INSTDIR\addons\Half-Life"
  File "${BUILDDIR}\addons\Half-Life\*.*"
  SetOutPath "$INSTDIR\addons\Half-Life2"
  File "${BUILDDIR}\addons\Half-Life2\*.*"
  SetOutPath "$INSTDIR\addons\Heretic_II"
  File "${BUILDDIR}\addons\Heretic_II\*.*"
  SetOutPath "$INSTDIR\addons\Hexen_II"
  File "${BUILDDIR}\addons\Hexen_II\*.*"
  SetOutPath "$INSTDIR\addons\JA"
  File "${BUILDDIR}\addons\JA\*.*"
  SetOutPath "$INSTDIR\addons\JK2"
  File "${BUILDDIR}\addons\JK2\*.*"
  SetOutPath "$INSTDIR\addons\KingPin"
  File "${BUILDDIR}\addons\KingPin\*.*"
  SetOutPath "$INSTDIR\addons\MOHAA"
  File "${BUILDDIR}\addons\MOHAA\*.*"
  SetOutPath "$INSTDIR\addons\Quake_1"
  File "${BUILDDIR}\addons\Quake_1\*.*"
  SetOutPath "$INSTDIR\addons\Quake_2"
  File "${BUILDDIR}\addons\Quake_2\*.*"
  SetOutPath "$INSTDIR\addons\Quake_3"
  File "${BUILDDIR}\addons\Quake_3\*.*"
  SetOutPath "$INSTDIR\addons\Quake_4"
  File "${BUILDDIR}\addons\Quake_4\*.*"
  SetOutPath "$INSTDIR\addons\RTCW"
  File "${BUILDDIR}\addons\RTCW\*.*"
  SetOutPath "$INSTDIR\addons\RTCW\QuArK files"
  SetOutPath "$INSTDIR\addons\RTCW\QuArK files\bspc"
  File "${BUILDDIR}\addons\RTCW\QuArK files\bspc\*.*"
  SetOutPath "$INSTDIR\addons\RTCW-ET"
  File "${BUILDDIR}\addons\RTCW-ET\*.*"
  SetOutPath "$INSTDIR\addons\Sin"
  File "${BUILDDIR}\addons\Sin\*.*"
  SetOutPath "$INSTDIR\addons\SOF"
  File "${BUILDDIR}\addons\SOF\*.*"
  SetOutPath "$INSTDIR\addons\SoF2"
  File "${BUILDDIR}\addons\SoF2\*.*"
  SetOutPath "$INSTDIR\addons\STVEF"
  File "${BUILDDIR}\addons\STVEF\*.*"
  SetOutPath "$INSTDIR\addons\Sylphis"
  File "${BUILDDIR}\addons\Sylphis\*.*"
  SetOutPath "$INSTDIR\addons\Torque"
  File "${BUILDDIR}\addons\Torque\*.*"
  SetOutPath "$INSTDIR\addons\WildWest"
  File "${BUILDDIR}\addons\WildWest\*.*"
  SetOutPath "$INSTDIR\addons"
  File "${BUILDDIR}\addons\*.*"
  SetOutPath "$INSTDIR\dlls"
  File "${BUILDDIR}\dlls\*.*"
  SetOutPath "$INSTDIR\help"
  File "${BUILDDIR}\help\*.*"
  SetOutPath "$INSTDIR\images"
  File "${BUILDDIR}\images\*.*"
  SetOutPath "$INSTDIR\lgicons"
  File "${BUILDDIR}\lgicons\*.*"
  SetOutPath "$INSTDIR\plugins"
  File "${BUILDDIR}\plugins\*.*"
  SetOutPath "$INSTDIR\quarkpy"
  File "${BUILDDIR}\quarkpy\*.*"
  SetOutPath "$INSTDIR"
  File "${BUILDDIR}\*.*"
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
  Delete "$INSTDIR\addons\RTCW-ET\*.*"
  Delete "$INSTDIR\addons\RTCW\QuArK files\bspc\*.*"
  Delete "$INSTDIR\addons\RTCW\*.*"
  Delete "$INSTDIR\addons\Quake_4\*.*"
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
  RMDir "$INSTDIR\addons\RTCW-ET"
  RMDir "$INSTDIR\addons\RTCW\QuArK files\bspc"
  RMDir "$INSTDIR\addons\RTCW\QuArK files"
  RMDir "$INSTDIR\addons\RTCW"
  RMDir "$INSTDIR\addons\Quake_4"
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

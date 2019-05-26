; QuArK installer x.x.x
; HomePage: http://quark.sourceforge.net/
; Author:  Fredrick Vamstad, DanielPharos & cdunde
; Date:     18 Aug. 2005 & 5 January 2007
; nullsoft NSIS installer program available at:
;   http://nsis.sourceforge.net

; Setup and Use to create QuArK NSIS installer:
; ============================================
; 1) Change "INSTALLER_EXENAME" (line 31) below to the name of the installer executable file.
; 2) Change "PRODUCT_VERSION" (line 33) below to match the new version number.
; 3) Change "InstallDir" (line 44) below to the new Program Files location.
; 4) Create a folder named " QuArK_installer_files " in your C:\ directory.
; 5) Place the QuArK.exe, all runtime files and the help folder in the above folder.
; 6) Create a folder named " QuArK_installer_splash_image " in your C:\ directory.
; 7) Copy the install_header.bmp and install_splash.bmp files from utils\nsis-dist-tools folder to the above folder.
; 8) Click on NSIS.exe to start program, select "MakeNSISW (compiler interface)".
; 9) Drag this file, QuArK.nsi, into the compiler window, or use the "File > Load Script" method to open this file.
; 10) The finished QuArK installer will be place in the same location as this file, ready for distrubution!

; Modern UI 2 ------
!include "MUI2.nsh"
SetCompressor /SOLID lzma   ; We will use LZMA for best compression

!define BUILDDIR "C:\QuArK_installer_files"
!define SPLASHDIR "C:\QuArK_installer_splash_image"
!define INSTALLER_EXENAME "quark-win32-6.6.0Beta7nightly21oct2017.exe"
!define PRODUCT_NAME "QuArK"
!define PRODUCT_NAME_FULL "Quake Army Knife"
!define PRODUCT_VERSION "6.6.0 Beta 7"
!define PRODUCT_WEB_SITE "http://quark.sourceforge.net/"
!define PRODUCT_WEB_FORUM "http://quark.sourceforge.net/forums/"
!define PRODUCT_INFOBASE "http://quark.sourceforge.net/infobase/"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\QuArK.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"
!define PRODUCT_PUBLISHER "QuArK Development Team"

Name "${PRODUCT_NAME} ${PRODUCT_VERSION}"
OutFile "${INSTALLER_EXENAME}"
InstallDir "$PROGRAMFILES\QuArK 6.6"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_ABORTWARNING_CANCEL_DEFAULT
!define MUI_UNABORTWARNING
!define MUI_ICON "${NSISDIR}\Contrib\Graphics\Icons\modern-install-blue.ico"
!define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\modern-uninstall-blue.ico"
; Loads the splash window
!define MUI_WELCOMEFINISHPAGE_BITMAP "${SPLASHDIR}\install_splash.bmp"
!define MUI_UNWELCOMEFINISHPAGE_BITMAP "${SPLASHDIR}\install_splash.bmp"
; Loads the header picture
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "${SPLASHDIR}\install_header.bmp"
!define MUI_HEADERIMAGE_UNBITMAP "${SPLASHDIR}\install_header.bmp"

; Language Selection Dialog Settings
!define MUI_LANGDLL_ALWAYSSHOW
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
!define MUI_FINISHPAGE_LINK "QuArK website"
!define MUI_FINISHPAGE_LINK_LOCATION "${PRODUCT_WEB_SITE}"
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "French"
!insertmacro MUI_LANGUAGE "German"
!insertmacro MUI_LANGUAGE "Dutch"
!insertmacro MUI_LANGUAGE "Finnish"
!insertmacro MUI_LANGUAGE "Greek"
!insertmacro MUI_LANGUAGE "Norwegian"
!insertmacro MUI_LANGUAGE "Russian"
!insertmacro MUI_LANGUAGE "Arabic"
!insertmacro MUI_LANGUAGE "TradChinese"

; Language strings
LangString TEXT_SEC01_TITLE ${LANG_ENGLISH} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_FRENCH} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_GERMAN} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_DUTCH} "Programma Bestanden"
LangString TEXT_SEC01_TITLE ${LANG_FINNISH} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_GREEK} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_NORWEGIAN} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_RUSSIAN} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_ARABIC} "Main Program Files"
LangString TEXT_SEC01_TITLE ${LANG_TRADCHINESE} "Main Program Files"

LangString TEXT_SEC02_TITLE ${LANG_ENGLISH} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_FRENCH} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_GERMAN} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_DUTCH} "Help Bestanden"
LangString TEXT_SEC02_TITLE ${LANG_FINNISH} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_GREEK} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_NORWEGIAN} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_RUSSIAN} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_ARABIC} "Help Files"
LangString TEXT_SEC02_TITLE ${LANG_TRADCHINESE} "Help Files"

LangString TEXT_SEC01_DESC ${LANG_ENGLISH} "All the main files needed to run QuArK."
LangString TEXT_SEC01_DESC ${LANG_FRENCH} "Main files."
LangString TEXT_SEC01_DESC ${LANG_GERMAN} "Main files."
LangString TEXT_SEC01_DESC ${LANG_DUTCH} "Alle programma bestanden die nodig zijn om QuArK uit te voeren."
LangString TEXT_SEC01_DESC ${LANG_FINNISH} "Main files."
LangString TEXT_SEC01_DESC ${LANG_GREEK} "Main files."
LangString TEXT_SEC01_DESC ${LANG_NORWEGIAN} "Main files."
LangString TEXT_SEC01_DESC ${LANG_RUSSIAN} "Main files."
LangString TEXT_SEC01_DESC ${LANG_ARABIC} "Main files."
LangString TEXT_SEC01_DESC ${LANG_TRADCHINESE} "Main files."

LangString TEXT_SEC02_DESC ${LANG_ENGLISH} "The help files."
LangString TEXT_SEC02_DESC ${LANG_FRENCH} "Help files."
LangString TEXT_SEC02_DESC ${LANG_GERMAN} "Help files."
LangString TEXT_SEC02_DESC ${LANG_DUTCH} "De help bestanden."
LangString TEXT_SEC02_DESC ${LANG_FINNISH} "Help files."
LangString TEXT_SEC02_DESC ${LANG_GREEK} "Help files."
LangString TEXT_SEC02_DESC ${LANG_NORWEGIAN} "Help files."
LangString TEXT_SEC02_DESC ${LANG_RUSSIAN} "Help files."
LangString TEXT_SEC02_DESC ${LANG_ARABIC} "Help files."
LangString TEXT_SEC02_DESC ${LANG_TRADCHINESE} "Help files."

LangString TEXT_UNINSTALL1 ${LANG_ENGLISH} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_FRENCH} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_GERMAN} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_DUTCH} "Dit zal ALLE bestanden in de QuArK map en sub-mappen verwijderen, inclusief alle zelfgemaakte bestanden.$\n$\nVerplaats alle bestanden die U wilt bewaren voordat U op 'Ja' drukt om de deïnstallatie voort te zetten."
LangString TEXT_UNINSTALL1 ${LANG_FINNISH} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_GREEK} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_NORWEGIAN} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_RUSSIAN} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_ARABIC} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."
LangString TEXT_UNINSTALL1 ${LANG_TRADCHINESE} "This will remove ALL files in the QuArK folder and sub-folders including any custom files.$\n$\nMove any files you wish to save before clicking 'Yes' to continue this uninstall."

LangString TEXT_UNINSTALL2 ${LANG_ENGLISH} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_FRENCH} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_GERMAN} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_DUTCH} "Weet U het zeker dat U $(^Name) en al zijn componenten wilt verwijderen?"
LangString TEXT_UNINSTALL2 ${LANG_FINNISH} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_GREEK} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_NORWEGIAN} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_RUSSIAN} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_ARABIC} "Are you sure you want to completely remove $(^Name) and all of its components now?"
LangString TEXT_UNINSTALL2 ${LANG_TRADCHINESE} "Are you sure you want to completely remove $(^Name) and all of its components now?"

LangString TEXT_UNINSTALL3 ${LANG_ENGLISH} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_FRENCH} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_GERMAN} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_DUTCH} "$(^Name) werd succesvol verwijderd van Uw computer."
LangString TEXT_UNINSTALL3 ${LANG_FINNISH} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_GREEK} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_NORWEGIAN} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_RUSSIAN} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_ARABIC} "$(^Name) was successfully removed from your computer."
LangString TEXT_UNINSTALL3 ${LANG_TRADCHINESE} "$(^Name) was successfully removed from your computer."
; MUI end ------

Section "$(TEXT_SEC01_TITLE)" SEC01
  SetOutPath "$INSTDIR\addons\6DX"
  File "${BUILDDIR}\addons\6DX\*.*"
  SetOutPath "$INSTDIR\addons\Alice"
  File "${BUILDDIR}\addons\Alice\*.*"
  SetOutPath "$INSTDIR\addons\CoD1"
  File "${BUILDDIR}\addons\CoD1\*.*"
  SetOutPath "$INSTDIR\addons\CoD2"
  File "${BUILDDIR}\addons\CoD2\*.*"
  SetOutPath "$INSTDIR\addons\Crystal_Space"
  File "${BUILDDIR}\addons\Crystal_Space\*.*"
  SetOutPath "$INSTDIR\addons\Doom_3"
  File "${BUILDDIR}\addons\Doom_3\*.*"
  SetOutPath "$INSTDIR\addons\EF2"
  File "${BUILDDIR}\addons\EF2\*.*"
  SetOutPath "$INSTDIR\addons\FAKK2"
  File "${BUILDDIR}\addons\FAKK2\*.*"
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
  SetOutPath "$INSTDIR\addons\NEXUIZ"
  File "${BUILDDIR}\addons\NEXUIZ\*.*"
  SetOutPath "$INSTDIR\addons\Prey"
  File "${BUILDDIR}\addons\Prey\*.*"
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
  SetOutPath "$INSTDIR\addons\Warsow"
  File "${BUILDDIR}\addons\Warsow\*.*"
  SetOutPath "$INSTDIR\addons\WildWest"
  File "${BUILDDIR}\addons\WildWest\*.*"
  SetOutPath "$INSTDIR\addons"
  File "${BUILDDIR}\addons\*.*"
  SetOutPath "$INSTDIR\dlls"
  File "${BUILDDIR}\dlls\*.*"
  SetOutPath "$INSTDIR\images"
  File "${BUILDDIR}\images\*.*"
  SetOutPath "$INSTDIR\lgicons"
  File "${BUILDDIR}\lgicons\*.*"
  SetOutPath "$INSTDIR\Lib"
  File "${BUILDDIR}\Lib\*.*"
  SetOutPath "$INSTDIR\plugins"
  File "${BUILDDIR}\plugins\*.*"
  SetOutPath "$INSTDIR\quarkpy"
  File "${BUILDDIR}\quarkpy\*.*"
  SetOutPath "$INSTDIR"
  File "${BUILDDIR}\*.*"
  WriteIniStr "$INSTDIR\${PRODUCT_NAME}.url" "InternetShortcut" "URL" "${PRODUCT_WEB_SITE}"
SectionEnd

Section "$(TEXT_SEC02_TITLE)" SEC02
  SetOutPath "$INSTDIR\help\pics"
  File "${BUILDDIR}\help\pics\*.*"
  SetOutPath "$INSTDIR\help"
  File "${BUILDDIR}\help\*.*"
SectionEnd

Section -DesktopIcon
  CreateShortCut "$DESKTOP\QuArK.lnk" "$INSTDIR\QuArK.exe"
SectionEnd

Section -StartMenuIcons
  CreateDirectory "$SMPROGRAMS\QuArK"
  CreateShortCut "$SMPROGRAMS\QuArK\QuArK.lnk" "$INSTDIR\QuArK.exe"
  CreateShortCut "$SMPROGRAMS\QuArK\Website.lnk" "$INSTDIR\${PRODUCT_NAME}.url"
  CreateShortCut "$SMPROGRAMS\QuArK\Forum.lnk" "${PRODUCT_WEB_FORUM}"
  CreateShortCut "$SMPROGRAMS\QuArK\Online Infobase.lnk" "${PRODUCT_INFOBASE}"
  CreateShortCut "$SMPROGRAMS\QuArK\Readme.lnk" "$INSTDIR\README.txt"
  CreateShortCut "$SMPROGRAMS\QuArK\Uninstall.lnk" "$INSTDIR\uninst.exe"
SectionEnd

Section -Post
  WriteUninstaller "$INSTDIR\uninst.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\QuArK.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "${PRODUCT_NAME_FULL} (${PRODUCT_NAME})"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninst.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\QuArK.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
SectionEnd

Section Uninstall
  Delete "$INSTDIR\${PRODUCT_NAME}.url"
  Delete "$INSTDIR\addons\WildWest\*.*"
  Delete "$INSTDIR\addons\Warsow\*.*"
  Delete "$INSTDIR\addons\Torque\*.*"
  Delete "$INSTDIR\addons\Sylphis\*.*"
  Delete "$INSTDIR\addons\STVEF\*.*"
  Delete "$INSTDIR\addons\SoF2\*.*"
  Delete "$INSTDIR\addons\SOF\*.*"
  Delete "$INSTDIR\addons\Sin\*.*"
  Delete "$INSTDIR\addons\RTCW-ET\*.*"
  Delete "$INSTDIR\addons\RTCW\*.*"
  Delete "$INSTDIR\addons\Quake_4\*.*"
  Delete "$INSTDIR\addons\Quake_3\*.*"
  Delete "$INSTDIR\addons\Quake_2\*.*"
  Delete "$INSTDIR\addons\Quake_1\*.*"
  Delete "$INSTDIR\addons\Prey\*.*"
  Delete "$INSTDIR\addons\NEXUIZ\*.*"
  Delete "$INSTDIR\addons\MOHAA\*.*"
  Delete "$INSTDIR\addons\KingPin\*.*"
  Delete "$INSTDIR\addons\JK2\*.*"
  Delete "$INSTDIR\addons\JA\*.*"
  Delete "$INSTDIR\addons\Hexen_II\*.*"
  Delete "$INSTDIR\addons\Heretic_II\*.*"
  Delete "$INSTDIR\addons\Half-Life2\*.*"
  Delete "$INSTDIR\addons\Half-Life\*.*"
  Delete "$INSTDIR\addons\Genesis3D\*.*"
  Delete "$INSTDIR\addons\FAKK2\*.*"
  Delete "$INSTDIR\addons\EF2\*.*"
  Delete "$INSTDIR\addons\Doom_3\*.*"
  Delete "$INSTDIR\addons\Crystal_Space\*.*"
  Delete "$INSTDIR\addons\CoD2\*.*"
  Delete "$INSTDIR\addons\CoD1\*.*"
  Delete "$INSTDIR\addons\Alice\*.*"
  Delete "$INSTDIR\addons\6DX\*.*"
  Delete "$INSTDIR\addons\*.*"
  Delete "$INSTDIR\dlls\*.*"
  Delete "$INSTDIR\help\pics\*.*"
  Delete "$INSTDIR\help\*.*"
  Delete "$INSTDIR\images\*.*"
  Delete "$INSTDIR\lgicons\*.*"
  Delete "$INSTDIR\Lib\*.*"
  Delete "$INSTDIR\plugins\*.*"
  Delete "$INSTDIR\quarkpy\*.*"
  Delete "$INSTDIR\*.*"


  Delete "$SMPROGRAMS\QuArK\*.*"
  Delete "$DESKTOP\QuArK.lnk"


  RMDir "$INSTDIR\addons\WildWest"
  RMDir "$INSTDIR\addons\Warsow"
  RMDir "$INSTDIR\addons\Torque"
  RMDir "$INSTDIR\addons\Sylphis"
  RMDir "$INSTDIR\addons\STVEF"
  RMDir "$INSTDIR\addons\SoF2"
  RMDir "$INSTDIR\addons\SOF"
  RMDir "$INSTDIR\addons\Sin"
  RMDir "$INSTDIR\addons\RTCW-ET"
  RMDir "$INSTDIR\addons\RTCW"
  RMDir "$INSTDIR\addons\Quake_4"
  RMDir "$INSTDIR\addons\Quake_3"
  RMDir "$INSTDIR\addons\Quake_2"
  RMDir "$INSTDIR\addons\Quake_1"
  RMDir "$INSTDIR\addons\Prey"
  RMDir "$INSTDIR\addons\NEXUIZ"
  RMDir "$INSTDIR\addons\MOHAA"
  RMDir "$INSTDIR\addons\KingPin"
  RMDir "$INSTDIR\addons\JK2"
  RMDir "$INSTDIR\addons\JA"
  RMDir "$INSTDIR\addons\Hexen_II"
  RMDir "$INSTDIR\addons\Heretic_II"
  RMDir "$INSTDIR\addons\Half-Life2"
  RMDir "$INSTDIR\addons\Half-Life"
  RMDir "$INSTDIR\addons\Genesis3D"
  RMDir "$INSTDIR\addons\FAKK2"
  RMDir "$INSTDIR\addons\EF2"
  RMDir "$INSTDIR\addons\Doom_3"
  RMDir "$INSTDIR\addons\Crystal_Space"
  RMDir "$INSTDIR\addons\CoD2"
  RMDir "$INSTDIR\addons\CoD1"
  RMDir "$INSTDIR\addons\Alice"
  RMDir "$INSTDIR\addons\6DX"
  RMDir "$INSTDIR\addons"
  RMDir "$INSTDIR\dlls"
  RMDir "$INSTDIR\help\pics"
  RMDir "$INSTDIR\help"
  RMDir "$INSTDIR\images"
  RMDir "$INSTDIR\lgicons"
  RMDir "$INSTDIR\Lib"
  RMDir "$INSTDIR\plugins"
  RMDir "$INSTDIR\quarkpy"
  RMDir "$INSTDIR"
  RMDir "$SMPROGRAMS\QuArK"


  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SEC01} "$(TEXT_SEC01_DESC)"
  !insertmacro MUI_DESCRIPTION_TEXT ${SEC02} "$(TEXT_SEC02_DESC)"
!insertmacro MUI_FUNCTION_DESCRIPTION_END

Function .onInit
  !insertmacro MUI_LANGDLL_DISPLAY
FunctionEnd

Function un.onInit
!insertmacro MUI_UNGETLANGUAGE
  MessageBox MB_ICONEXCLAMATION|MB_YESNO|MB_DEFBUTTON2 "$(TEXT_UNINSTALL1)" IDYES +2
  Abort
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "$(TEXT_UNINSTALL2)" IDYES +2
  Abort
FunctionEnd

Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(TEXT_UNINSTALL3)"
FunctionEnd

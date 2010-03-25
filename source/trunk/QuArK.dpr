(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) QuArK Development Team

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

http://quark.sourceforge.net/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.66  2009/07/15 10:38:10  danielpharos
Updated website link.

Revision 1.65  2009/05/04 18:34:10  danielpharos
Added switch to change memory manager easily.

Revision 1.64  2009/04/28 20:54:03  cdunde
Model Editor Bone Rebuild merge to HEAD.
Complete change of bone system.

Revision 1.63  2009/03/16 08:47:21  danielpharos
Updated to DevIL 1.7.8, added IWI loading, and added many new image loading/saving options.

Revision 1.62.2.2  2009/04/14 22:15:24  danielpharos
Create new :sd type for storing generic data.

Revision 1.62  2009/02/21 17:09:36  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.61  2008/12/12 12:47:52  danielpharos
Moved GlobalWarning to QkExceptions, and added QkTextBoxForm.

Revision 1.60  2008/12/04 12:14:00  danielpharos
Fixed a redraw-clipping problem, removed a redundant file and cleaned-up the constructor of the EdSceneObjects.

Revision 1.59  2008/11/20 23:45:50  danielpharos
Big update to renderers: mostly cleanup, and stabilized Direct3D a bit more.

Revision 1.58  2008/10/12 11:31:32  danielpharos
Moved 6DX map format to separate file, and re-factored QkMap and QkQuakeMap.

Revision 1.57  2008/10/09 11:31:51  danielpharos
Added decent .col Sylphis archive support.

Revision 1.56  2008/09/16 12:12:50  danielpharos
Added support for CoD2 iwd files.

Revision 1.55  2008/09/06 15:57:38  danielpharos
Moved exception code into separate file.

Revision 1.54  2008/08/12 15:46:08  danielpharos
Revert prev rev

Revision 1.53  2008/08/11 23:13:49  danielpharos
Enabled FastMM for normal use

Revision 1.52  2008/07/17 14:47:59  danielpharos
Big (experimental) change to model bones, tags and boundframes

Revision 1.51  2008/05/16 20:55:39  danielpharos
Changed comment characters to avoid compiler confusion

Revision 1.50  2008/05/01 12:57:40  danielpharos
Moved BrowseForFolder to a better place.

Revision 1.49  2008/02/03 13:12:46  danielpharos
Update for the AutoUpdater. Beginning of the install-window.

Revision 1.48  2008/01/31 16:07:18  danielpharos
Added FTX file loading and saving support (Heavy Metal: F.A.K.K. 2 textures).

Revision 1.47  2007/09/23 21:04:31  danielpharos
Add Desktop Window Manager calls to disable Desktop Composition on Vista. This should fix/workaround corrupted OpenGL and DirectX viewports.

Revision 1.46  2007/09/12 16:21:42  danielpharos
Added MD5 hash capabilities! This is now used to check if QuArKSAS is up-to-date.

Revision 1.45  2007/09/12 15:35:40  danielpharos
Moved update settings to seperate config section and added beginnings of online update check.

Revision 1.44  2007/08/14 16:33:00  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.43  2007/07/05 10:22:36  danielpharos
Moved the Quake .map format code to a separate file.

Revision 1.42  2007/06/13 11:56:48  danielpharos
Added FreeImage as an alternative for DevIL. PNG and JPEG file handling now also uses these two libraries. Set-up a new section in the Configuration for all of this.

Revision 1.41  2007/05/02 22:34:50  danielpharos
Added DDS file support. Fixed wrong (but unused then) DevIL DDL interface. DDS file saving not supported at the moment.

Revision 1.40  2007/04/30 21:52:55  danielpharos
Added basic interface to DevIL.

Revision 1.39  2007/03/15 22:19:13  danielpharos
Re-did the entire VMT file loading! It's using the VTFLib now. Saving VMT files not supported yet.

Revision 1.38  2007/02/20 14:56:37  danielpharos
Added a compiler directive to go beyond 2GB with Delphi 7 or larger. Disabled for the moment.

Revision 1.37  2007/02/19 13:32:11  danielpharos
Moved VTFLib dll interface to a separate file, and build the SaveFile for VTF's using it. SaveFile has not been fully tested yet!

Revision 1.36  2007/02/07 16:51:48  danielpharos
Fixed a typo.

Revision 1.35  2007/01/02 21:01:05  danielpharos
To put back the original memory manager. The leaks are killing QuArK!
Added FastMM 4.74 for debugging purposes
MemCheck 2.74 for debugging purposes

Revision 1.34  2006/12/31 21:58:16  danielpharos
Upgraded to FastMM 4.74.This replaces the old MemTester file. FastMM should be faster, cleaner and better for debugging.

Revision 1.33  2006/12/26 22:49:06  danielpharos
Splitted the Ed3DFX file into two separate renderers: Software and Glide

Revision 1.32  2006/11/30 00:41:31  cdunde
To merge all source files that had changes from DanielPharos branch
to HEAD for QuArK 6.5.0 Beta 1.

Revision 1.31.2.9  2006/11/23 20:09:52  danielpharos
Removed now obsolete Ed3DEditors file

Revision 1.31.2.8  2006/11/01 22:22:29  danielpharos
BackUp 1 November 2006
Mainly reduce OpenGL memory leak

Revision 1.31  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.29  2005/07/30 23:04:45  alexander
introduced dispface class for faces with displacement mapping
vmf loader sets some displacement info

Revision 1.28  2005/06/22 01:19:40  alexander
added hl2 material source

Revision 1.27  2005/06/22 01:12:42  alexander
added hl2 material source

Revision 1.26  2005/01/27 00:16:13  alexander
added vmf file loading (brushes only)

Revision 1.25  2005/01/02 15:19:54  alexander
access files via steam service - first

Revision 1.24  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.23  2004/12/19 10:04:14  alexander
added a form for gcf browsing

Revision 1.22  2004/11/25 00:42:19  alexander
first gcf access attempt

Revision 1.21  2003/11/10 19:32:59  silverpaladin
Fixed a problem with menus not showing up on the Toolbox menu.

Revision 1.20  2003/08/13 04:26:21  silverpaladin
final changes to gk1's TG_Form vs TForm1.  Added a couple jpg modules and a reference to default.qrk for easy access

Revision 1.19  2002/05/15 21:19:09  tiglari
add prog/maperror to project

Revision 1.18  2002/02/24 13:48:46  decker_dk
Added QkPNG.PAS and QkTribes2.PAS

Revision 1.17  2001/06/12 18:31:34  decker_dk
Forgot a global-prefix to 'g_Form1'
Added revision-log.
}

program QuArK;
{%File '..\Runtime\addons\Defaults.qrk'}

{$INCLUDE MemManager.inc}

uses
{$IFDEF MemTester}
  MemTester in 'prog\MemTester.pas',
{$ENDIF}
{$IFDEF FastMM}
  FastMM4 in 'prog\FastMM4.pas',
{$ENDIF}
{$IFDEF MemCheck}
  MemCheck in 'prog\MemCheck.pas',
{$ENDIF}
  Forms,
  DWM in '3dfx\DWM.pas',
  DX9 in '3dfx\DX9.pas',
  EdDirect3D in '3dfx\EdDirect3D.pas',
  EdGlide in '3dfx\EdGlide.pas',
  EdOpenGL in '3dfx\EdOpenGL.pas',
  EdSceneObject in '3dfx\EdSceneObject.pas',
  EdSoftware in '3dfx\EdSoftware.pas',
  QkDummyWindow in '3dfx\QkDummyWindow.pas',
  FullScr1 in '3dfx\FullScr1.pas' {TwoMonitorsDlg},
  FullScreenWnd in '3dfx\FullScreenWnd.pas' {FullScrDlg},
  GL1 in '3dfx\GL1.pas',
  Glide in '3dfx\Glide.pas',
  form_Model in 'prog\model\form_Model.pas' {FQMdl},
  Qk3ds in 'prog\model\Qk3ds.pas',
  QkBoneGroup in 'prog\model\QkBoneGroup.pas',
  QkBoundFrame in 'prog\model\QkBoundFrame.pas',
  QkComponent in 'prog\model\QkComponent.pas',
  QkFrame in 'prog\model\QkFrame.pas',
  QkFrameGroup in 'prog\model\QkFrameGroup.pas',
  QkMd2 in 'prog\model\QkMd2.pas',
  QkMd3 in 'prog\model\QkMd3.pas',
  QkMdl in 'prog\model\QkMdl.pas',
  QkMdlObject in 'prog\model\QkMdlObject.pas',
  QkMiscGroup in 'prog\model\QkMiscGroup.pas',
  QkModel in 'prog\model\QkModel.pas',
  QkModelBone in 'prog\model\QkModelBone.pas',
  QkModelFile in 'prog\model\QkModelFile.pas',
  QkModelRoot in 'prog\model\QkModelRoot.pas',
  QkModelTag in 'prog\model\QkModelTag.pas',
  QkQkl in 'prog\model\QkQkl.pas',
  qkskindrawobject in 'prog\model\qkskindrawobject.pas',
  QkSkinGroup in 'prog\model\QkSkinGroup.pas',
  QkSkinSubGroup in 'prog\model\QkSkinSubGroup.pas',
  QkTagFrame in 'prog\model\QkTagFrame.pas',
  About in 'prog\About.pas' {AboutBox},
  AutoUpdater in 'prog\AutoUpdater.pas' {AutoUpdater},
  AutoUpdateInstaller in 'prog\AutoUpdateInstaller.pas' {AutoUpdateInstaller},
  Bezier in 'prog\Bezier.pas',
  BrowseForFolder in 'prog\BrowseForFolder.pas',
  CCode in 'prog\CCode.pas',
  Config in 'prog\Config.pas' {ConfigDlg},
  Console in 'prog\Console.pas' {ConsoleForm},
  crc32 in 'prog\crc32.pas',
  DispFace in 'prog\DispFace.pas',
  Duplicator in 'prog\Duplicator.pas',
  ExtraFunctionality in 'prog\ExtraFunctionality.pas',
  FormCfg in 'prog\FormCfg.pas',
  Game in 'prog\Game.pas' {GameCfgDlg},
  Game2 in 'prog\Game2.pas' {AddOnsAddDlg},
  HelpPopup1 in 'prog\HelpPopup1.pas' {HelpPopup},
  HTTP in 'prog\HTTP.pas',
  Keys in 'prog\Keys.pas' {KeyDlg},
  KeySel in 'prog\KeySel.pas' {KeySelDlg},
  Logging in 'prog\Logging.pas',
  MapError in 'prog\MapError.pas',
  MD5Hash in 'prog\MD5Hash.pas',
  NewFolder in 'prog\NewFolder.pas' {NewFolderDlg},
  ObjProp in 'prog\ObjProp.pas' {FormObjProp},
  OsFolder in 'prog\OsFolder.pas',
  Output1 in 'prog\Output1.pas' {OutputDirDlg},
  PakFiles in 'prog\PakFiles.pas',
  Qk1 in 'prog\Qk1.pas' {Form1},
  Qk3D in 'prog\Qk3D.pas',
  Qk6DX in 'prog\Qk6DX.pas',
  QkApplPaths in 'prog\QkApplPaths.pas',
  QkBmp in 'prog\QkBmp.pas',
  QkBsp in 'prog\QkBsp.pas' {FQBsp},
  QkBspHulls in 'prog\QkBspHulls.pas',
  QkCin in 'prog\QkCin.pas',
  QkCoD2 in 'prog\QkCoD2.pas',
  QkD3 in 'prog\QkD3.pas',
  QkDevIL in 'prog\QkDevIL.pas',
  QkDDS in 'prog\QkDDS.pas',
  QkExceptions in 'prog\QkExceptions.pas',
  QkExplorer in 'prog\QkExplorer.pas',
  QkFileExplorer in 'prog\QkFileExplorer.pas',
  QkFileObjects in 'prog\QkFileObjects.pas' {QForm1},
  QkFtx in 'prog\QkFtx.pas',
  QkForm in 'prog\QkForm.pas',
  QkFormCfg in 'prog\QkFormCfg.pas',
  QkFormVw in 'prog\QkFormVw.pas' {FQFormVw},
  QkFreeImage in 'prog\QkFreeImage.pas',
  QkGCF in 'prog\QkGCF.pas',
  QkGroup in 'prog\QkGroup.pas' {FQGroup},
  QkHL in 'prog\QkHL.pas',
  QkHr2 in 'prog\QkHr2.pas',
  QkImages in 'prog\QkImages.pas' {FQImages},
  QkInclude in 'prog\QkInclude.pas',
  QkIwi in 'prog\QkIwi.pas',
  QkJpg in 'prog\QkJpg.pas',
  QkListView in 'prog\QkListView.pas' {QForm2},
  QkMacro in 'prog\QkMacro.pas',
  QkMap in 'prog\QkMap.pas' {FQMap},
  QkMapObjects in 'prog\QkMapObjects.pas',
  QkMapPoly in 'prog\QkMapPoly.pas',
  QkObjectClassList in 'prog\QkObjectClassList.pas',
  QkObjects in 'prog\QkObjects.pas',
  QkOwnExplorer in 'prog\QkOwnExplorer.pas' {QFormExplorer},
  QkPak in 'prog\QkPak.pas' {FQPak},
  QkPcx in 'prog\QkPcx.pas',
  QkPixelSet in 'prog\QkPixelSet.pas',
  QkPng in 'prog\QkPng.pas',
  QkQ1 in 'prog\QkQ1.pas',
  QkQ2 in 'prog\QkQ2.pas',
  QkQ3 in 'prog\QkQ3.pas',
  QkQme in 'prog\QkQme.pas' {FQQme},
  QkQuakeC in 'prog\QkQuakeC.pas' {FQQuakeC},
  QkQuakeCtx in 'prog\QkQuakeCtx.pas',
  QkQuakeMap in 'prog\QkQuakeMap.pas',
  QkRawFile in 'prog\QkRawFile.pas' {FQRawFile},
  QkSin in 'prog\QkSin.pas',
  QkSoF in 'prog\QkSoF.pas',
  QkSpr in 'prog\QkSpr.pas',
  QkSteamFS in 'prog\QkSteamFS.pas',
  QkSylphis in 'prog\QkSylphis.pas',
  QkSysData in 'prog\QkSysData.pas',
  QkText in 'prog\QkText.pas',
  QkTextBoxForm in 'prog\QkTextBoxForm.pas' {FQTextBoxForm},
  QkTextures in 'prog\QkTextures.pas' {FQTexture},
  QkTga in 'prog\QkTga.pas',
  QkTreeView in 'prog\QkTreeView.pas',
  QkTribes2 in 'prog\QkTribes2.pas',
  QkUnknown in 'prog\QkUnknown.pas' {FQUnknown},
  QkVMF in 'prog\QkVMF.pas',
  QkVMT in 'prog\QkVMT.pas',
  QkVTF in 'prog\QkVTF.pas',
  QkVTFLib in 'prog\QkVTFLib.pas',
  QkWad in 'prog\QkWad.pas' {FQWad},
  QkWav in 'prog\QkWav.pas',
  QkZip2 in 'prog\QkZip2.pas',
  qmath in 'prog\qmath.pas',
  qmatrices in 'prog\qmatrices.pas',
  QPAcc in 'prog\QPAcc.pas',
  QuickWal in 'prog\QuickWal.pas' {QuickWalParser},
  RedLines in 'prog\RedLines.pas',
  Reg2 in 'prog\Reg2.pas',
  Running in 'prog\Running.pas' {RunForm},
  SearchHoles in 'prog\SearchHoles.pas',
  Setup in 'prog\Setup.pas',
  Sprite in 'prog\Sprite.pas',
  SystemDetails in 'prog\SystemDetails.pas',
  TbPalette in 'prog\TbPalette.pas',
  TbTexture in 'prog\TbTexture.pas',
  TbUndoMenu in 'prog\TbUndoMenu.pas' {UndoDlg},
  TexHints in 'prog\TexHints.pas',
  Toolbar1 in 'prog\Toolbar1.pas',
  ToolBox1 in 'prog\ToolBox1.pas' {ToolBoxForm},
  ToolBoxGroup in 'prog\ToolBoxGroup.pas' {FQToolBoxGroup},
  Travail in 'prog\Travail.pas' {FormTravail},
  Undo in 'prog\Undo.pas',
  UNZIP in 'prog\UNZIP.pas',
  ZIP in 'prog\ZIP.pas',
  PyCanvas in 'python\PyCanvas.pas',
  PyControls in 'python\PyControls.pas',
  PyExplorer in 'python\PyExplorer.pas',
  PyFloating in 'python\PyFloating.pas',
  PyFormCfg in 'python\PyFormCfg.pas',
  PyForms in 'python\PyForms.pas' {PyForm},
  PyImages in 'python\PyImages.pas',
  PyMacros in 'python\PyMacros.pas',
  PyMapView in 'python\PyMapView.pas',
  PyMath in 'python\PyMath.pas',
  PyMath3D in 'python\PyMath3D.pas',
  PyMenus in 'python\PyMenus.pas',
  PyObjects in 'python\PyObjects.pas',
  PyPanels in 'python\PyPanels.pas',
  PyProcess in 'python\PyProcess.pas',
  Python in 'python\Python.pas',
  PyToolbars in 'python\PyToolbars.pas',
  PyTravail in 'python\PyTravail.pas',
  PyUndo in 'python\PyUndo.pas',
  Quarkx in 'Python\Quarkx.pas';


(*DanielPharos: Set the support-for-larger-than-2GB-flag, so we can use up to 4 GB!*)
(*NOT SUPPORTED THOUGH*)
{.$IFDEF Delphi7orNewerCompiler}
  {.$SetPEFlags $20}
{.$ENDIF}

{$R *.RES}

begin
{$IFDEF MemCheck}
  MemChk;
{$ENDIF}
  Application.Initialize;
  Application.Title:='Quake Army Knife';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.

(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
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

uses
  MemTester in 'Prog\MemTester.pas',
  {FastMM4 in 'Prog\FastMM4.pas',}     {Enable for FastMM, copy the debug DLL to the runtime directory}
  {MemCheck in 'Prog\MemCheck.pas',}     {Enable for MemCheck, also see below}
  Forms,
  QkObjects in 'Prog\QkObjects.pas',
  Qk1 in 'Prog\Qk1.pas' {Form1},
  QkExplorer in 'Prog\QkExplorer.pas',
  QkGroup in 'Prog\QkGroup.pas' {FQGroup},
  QkUnknown in 'Prog\QkUnknown.pas' {FQUnknown},
  QkFileObjects in 'Prog\QkFileObjects.pas' {QForm1},
  Undo in 'Prog\Undo.pas',
  QkQuakeC in 'Prog\QkQuakeC.pas' {FQQuakeC},
  Travail in 'Prog\Travail.pas' {FormTravail},
  QkMap in 'Prog\QkMap.pas' {FQMap},
  QkMapObjects in 'Prog\QkMapObjects.pas',
  Setup in 'Prog\Setup.pas',
  QkMapPoly in 'Prog\QkMapPoly.pas',
  qmath in 'Prog\qmath.pas',
  FormCfg in 'Prog\FormCfg.pas',
  Config in 'Prog\Config.pas' {ConfigDlg},
  QkInclude in 'Prog\QkInclude.pas',
  qmatrices in 'Prog\qmatrices.pas',
  ToolBox1 in 'Prog\ToolBox1.pas' {ToolBoxForm},
  QkWad in 'Prog\QkWad.pas' {FQWad},
  QkFileExplorer in 'Prog\QkFileExplorer.pas',
  QkTextures in 'Prog\QkTextures.pas' {FQTexture},
  QkOwnExplorer in 'Prog\QkOwnExplorer.pas' {QFormExplorer},
  QkPak in 'Prog\QkPak.pas' {FQPak},
  QkListView in 'Prog\QkListView.pas' {QForm2},
  Game in 'Prog\Game.pas' {GameCfgDlg},
  QkBsp in 'Prog\QkBsp.pas' {FQBsp},
  QkImages in 'Prog\QkImages.pas' {FQImages},
  QkPcx in 'Prog\QkPcx.pas',
  QkBmp in 'Prog\QkBmp.pas',
  TbPalette in 'Prog\TbPalette.pas',
  QkForm in 'Prog\QkForm.pas',
  QkQme in 'Prog\QkQme.pas' {FQQme},
  ObjProp in 'Prog\ObjProp.pas' {FormObjProp},
  Game2 in 'Prog\Game2.pas' {AddOnsAddDlg},
  Toolbar1 in 'Prog\Toolbar1.pas',
  TbUndoMenu in 'Prog\TbUndoMenu.pas' {UndoDlg},
  Qk3D in 'Prog\Qk3D.pas',
  QkMacro in 'Prog\QkMacro.pas',
  QkFormVw in 'Prog\QkFormVw.pas' {FQFormVw},
  Running in 'Prog\Running.pas' {RunForm},
  NewFolder in 'Prog\NewFolder.pas' {NewFolderDlg},
  Glide in '3DFX\Glide.pas',
  GL1 in '3DFX\GL1.pas',
  DX9 in '3DFX\DX9.pas',
  EdSoftware in '3DFX\EdSoftware.pas',
  EdGlide in '3DFX\EdGlide.pas',
  EdOpenGL in '3dfx\EdOpenGL.pas',
  EdDirect3D in '3dfx\EdDirect3D.pas',
  EdSceneObject in '3dfx\EdSceneObject.pas',
  QkRawFile in 'Prog\QkRawFile.pas' {FQRawFile},
  QkWav in 'Prog\QkWav.pas',
  QkCin in 'Prog\QkCin.pas',
  QkQuakeCtx in 'Prog\QkQuakeCtx.pas',
  About in 'Prog\About.pas' {AboutBox},
  Reg2 in 'Prog\Reg2.pas',
  Output1 in 'Prog\Output1.pas' {OutputDirDlg},
  QkBspHulls in 'Prog\QkBspHulls.pas',
  QkTreeView in 'Prog\QkTreeView.pas',
  CCode in 'Prog\CCode.pas',
  Python in 'Python\Python.pas',
  Quarkx in 'Python\Quarkx.pas',
  PyControls in 'Python\PyControls.pas',
  PyExplorer in 'Python\PyExplorer.pas',
  PyImages in 'Python\PyImages.pas',
  PyMenus in 'Python\PyMenus.pas',
  PyObjects in 'Python\PyObjects.pas',
  PyPanels in 'Python\PyPanels.pas',
  PyToolbars in 'Python\PyToolbars.pas',
  PyForms in 'Python\PyForms.pas' {PyForm},
  PyFormCfg in 'Python\PyFormCfg.pas',
  PyMacros in 'Python\PyMacros.pas',
  PyFloating in 'Python\PyFloating.pas',
  PyMapView in 'Python\PyMapView.pas',
  PyMath in 'Python\PyMath.pas',
  PyCanvas in 'Python\PyCanvas.pas',
  PyUndo in 'Python\PyUndo.pas',
  PyMath3D in 'Python\PyMath3D.pas',
  Duplicator in 'Prog\Duplicator.pas',
  PyTravail in 'Python\PyTravail.pas',
  PyProcess in 'Python\PyProcess.pas',
  Console in 'Prog\Console.pas' {ConsoleForm},
  FullScreenWnd in '3DFX\FullScreenWnd.pas' {FullScrDlg},
  FullScr1 in '3DFX\FullScr1.pas' {TwoMonitorsDlg},
  TbTexture in 'Prog\TbTexture.pas',
  RedLines in 'Prog\RedLines.pas',
  SearchHoles in 'Prog\SearchHoles.pas',
  QPAcc in 'Prog\QPAcc.pas',
  KeySel in 'Prog\KeySel.pas' {KeySelDlg},
  Keys in 'Prog\Keys.pas' {KeyDlg},
  TexHints in 'Prog\TexHints.pas',
  QuickWal in 'Prog\QuickWal.pas' {QuickWalParser},
  QkHr2 in 'Prog\QkHr2.pas',
  QkHL in 'Prog\QkHL.pas',
  QkSin in 'Prog\QkSin.pas',
  ToolBoxGroup in 'Prog\ToolBoxGroup.pas' {FQToolBoxGroup},
  HelpPopup1 in 'Prog\HelpPopup1.pas' {HelpPopup},
  QkTga in 'Prog\QkTga.pas',
  QkPixelSet in 'prog\QkPixelSet.pas',
  Bezier in 'prog\Bezier.pas',
  UNZIP in 'prog\UNZIP.pas',
  ZIP in 'prog\ZIP.pas',
  QkZip2 in 'prog\QkZip2.pas',
  QkJpg in 'prog\QkJpg.pas',
  QkText in 'prog\QkText.pas',
  QkSpr in 'prog\QkSpr.pas',
  QkSoF in 'prog\QkSoF.pas',
  form_Model in 'prog\model\form_Model.pas' {FQMdl},
  QkBoneGroup in 'prog\model\QkBoneGroup.pas',
  QkComponent in 'prog\model\QkComponent.pas',
  QkFrame in 'prog\model\QkFrame.pas',
  QkFrameGroup in 'prog\model\QkFrameGroup.pas',
  QkMd2 in 'prog\model\QkMd2.pas',
  QkMdl in 'prog\model\QkMdl.pas',
  QkMdlObject in 'prog\model\QkMdlObject.pas',
  QkMiscGroup in 'prog\model\QkMiscGroup.pas',
  QkModel in 'prog\model\QkModel.pas',
  QkModelBone in 'prog\model\QkModelBone.pas',
  QkModelFile in 'prog\model\QkModelFile.pas',
  QkModelRoot in 'prog\model\QkModelRoot.pas',
  QkModelTag in 'prog\model\QkModelTag.pas',
  QkQkl in 'prog\model\QkQkl.pas',
  QkSkinGroup in 'prog\model\QkSkinGroup.pas',
  QkMd3 in 'prog\model\QkMd3.pas',
  QkQ1 in 'prog\QkQ1.pas',
  QkQ2 in 'prog\QkQ2.pas',
  qkskindrawobject in 'prog\model\qkskindrawobject.pas',
  Qk3ds in 'prog\model\Qk3ds.pas',
  Logging in 'prog\Logging.pas',
  EdTListP2 in '3dfx\EdTListP2.pas',
  OsFolder in 'prog\OsFolder.pas',
  QkObjectClassList in 'prog\QkObjectClassList.pas',
  Sprite in 'prog\Sprite.pas',
  crc32 in 'prog\crc32.pas',
  QkApplPaths in 'prog\QkApplPaths.pas',
  ExtraFunctionality in 'prog\ExtraFunctionality.pas',
  SystemDetails in 'prog\SystemDetails.pas',
  QkFormCfg in 'prog\QkFormCfg.pas',
  QkPng in 'prog\QkPng.pas',
  MapError in 'prog\MapError.pas',
  QkTribes2 in 'prog\QkTribes2.pas',
  JPEGLib in 'components\JPEG\jpeglib.pas',
  JmoreCfg in 'components\jpeg\JMORECFG.PAS',
  QkD3 in 'prog\QkD3.pas',
  QkVMF in 'prog\QkVMF.pas',
  QkVTF in 'prog\QkVTF.pas',
  QkVTFLib in 'prog\QkVTFLib.pas',
  QkGCF in 'prog\QkGCF.pas',
  QkSteamFS in 'prog\QkSteamFS.pas',
  QkQ3 in 'prog\QkQ3.pas',
  QkHL2mat in 'prog\QkHL2mat.pas',
  DispFace in 'prog\DispFace.pas';

{DanielPharos: Set the support-for-larger-than-2GB-flag, so we can use up to 4 GB!}
{.$IFDEF Delphi7orNewerCompiler}
  {.$SetPEFlags $20}
{.$ENDIF}
{$R *.RES}

begin
  {MemChk;}     {Enable for MemCheck}
  Application.Initialize;
  Application.Title:='Quake Army Knife';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

(* In case of compile error  "Missing $ENDIF",
    add {$ENDIF} at the end of line 27, after "'Prog\MemTester.pas'," *)

end.

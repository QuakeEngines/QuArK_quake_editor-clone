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
  FastMove in 'prog\FastMove.pas',
  FastCode in 'prog\FastCode\FastCode.pas',
  Forms,

  //Init the logging module FIRST, otherwise we'll miss log-calls from other init's!
  Logging in 'prog\Logging.pas',

  DWM in '3dfx\DWM.pas',
  DX9 in '3dfx\DX9.pas',
  EdDirect3D in '3dfx\EdDirect3D.pas',
  EdGlide in '3dfx\EdGlide.pas',
  EdOpenGL in '3dfx\EdOpenGL.pas',
  EdSceneObject in '3dfx\EdSceneObject.pas',
  EdSoftware in '3dfx\EdSoftware.pas',
  QkDummyWindow in '3dfx\QkDummyWindow.pas',
  QkFullScreenWindow in '3dfx\QkFullScreenWindow.pas',
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
  QkBBoxGroup in 'prog\model\QkBBoxGroup.pas',
  QkModel in 'prog\model\QkModel.pas',
  QkModelBone in 'prog\model\QkModelBone.pas',
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
  MapError in 'prog\MapError.pas',
  MD5Hash in 'prog\MD5Hash.pas',
  NewFolder in 'prog\NewFolder.pas' {NewFolderDlg},
  ObjProp in 'prog\ObjProp.pas' {FormObjProp},
  OsFolder in 'prog\OsFolder.pas',
  Output1 in 'prog\Output1.pas' {OutputDirDlg},
  PakFiles in 'prog\PakFiles.pas',
  PixelSetSizeCache in 'prog\PixelSetSizeCache.pas',
  Qk1 in 'prog\Qk1.pas' {Form1},
  Qk3D in 'prog\Qk3D.pas',
  Qk6DX in 'prog\Qk6DX.pas',
  QkApplPaths in 'prog\QkApplPaths.pas',
  QkBmp in 'prog\QkBmp.pas',
  QkBsp in 'prog\QkBsp.pas' {FQBsp},
  QkBspHulls in 'prog\QkBspHulls.pas',
  QkCin in 'prog\QkCin.pas',
  QkCoD2 in 'prog\QkCoD2.pas',
  QkConsts in 'prog\QkConsts.pas',
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
  QkHLLib in 'prog\QkHLLib.pas',
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
  QkNCF in 'prog\QkNCF.pas',
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
  QkVPK in 'prog\QkVPK.pas',
  QkVTF in 'prog\QkVTF.pas',
  QkVTFLib in 'prog\QkVTFLib.pas',
  QkWad in 'prog\QkWad.pas' {FQWad},
  QkWav in 'prog\QkWav.pas',
  QkWorldCraft in 'prog\QkWorldCraft.pas',
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
  WorkaroundStringCompare in 'prog\WorkaroundStringCompare.pas',
  ZIP in 'prog\ZIP.pas',
  PyCanvas in 'python\PyCanvas.pas',
  PyControls in 'python\PyControls.pas',
  PyExplorer in 'python\PyExplorer.pas',
  PyFloating in 'python\PyFloating.pas',
  PyFormCfg in 'python\PyFormCfg.pas',
  PyForms in 'python\PyForms.pas' {PyForm},
  PyFullScreen in 'python\PyFullScreen.pas',
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

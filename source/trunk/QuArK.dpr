(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

program QuArK;

uses
  MemTester in 'Prog\MemTester.pas',
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
  Ed3DFX in '3DFX\Ed3DFX.pas',
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
  GL1 in '3DFX\GL1.pas',
  EdOpenGL in '3DFX\EdOpenGL.pas',
  QkTga in 'Prog\QkTga.pas',
  QkPixelSet in 'prog\QkPixelSet.pas',
  Bezier in 'prog\Bezier.pas',
  UNZIP in 'prog\UNZIP.pas',
  ZIP in 'prog\ZIP.pas',
  QkZip2 in 'prog\QkZip2.pas',
  QkJpg in 'prog\QkJpg.pas',
  crc32 in 'prog\crc32.pas',
  QkQ3 in 'prog\QkQ3.pas',
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
  PyLogging in 'python\PyLogging.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title:='Quake Army Knife';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

(* In case of compile error  "Missing $ENDIF",
    add {$ENDIF} at the end of line 27, after "'Prog\MemTester.pas'," *)

end.

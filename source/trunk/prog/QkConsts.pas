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
Revision 1.1  2010/04/16 18:44:59  danielpharos
Reduced missing init-logging entries to a single problematic line. Also, logging now uses const strings (faster).
}

unit QkConsts;

interface

uses SysUtils;

const
  QuArKVersion            = 'QuArK 6.6';
  QuArKMinorVersion       = 'Beta 3';
  QuArKCopyright          = 'Copyright (C) 1996-2009 Armin Rigo and others';
  QuArKUsedCompiler       = 'Delphi 7.0';
  QuArKCompileDate        = 40118;   //This is the compiled date
  { Amount of days that have passed after 30 Dec 1899 (Delphi 2+).
    You can use EncodeDate(Year, Month, Day) to compute it, but this value
    really needs to be a constant, so put the resulting value in here.
    The result can be checked in the About form. }
  QuArKDaysOld            = 270;     //About a 9 month difference...
  { This is the amount of days after which a certain build is considered
    old by the update-check. }
  QuArKWebsite            = 'http://quark.sourceforge.net/';
  QuArKRepository         = 'http://sourceforge.net/projects/quark/';
  QuArKForum              = 'http://quark.sourceforge.net/forums/';
  QuArKInfobase           = 'http://quark.sourceforge.net/infobase/';
  QuArKDefaultHelpPage    = 'index.html'; 
  QuArKUpdateSite         = 'quark.sourceforge.net';
  QuArKUpdateFile         = '/update/index.dat';


  iiUnknownFile           = 0;
  iiExplorerGroup         = 1;
  iiQuakeC                = 2;
  iiBsp                   = 3;
  iiToolBox               = 4;
  iiUnknown               = 5;
  iiPakFolder             = 6;
  iiLinkOverlay           = 7;
  iiModel                 = 8;
  iiMap                   = 9;
  iiPak                   = 10;
  iiEntity                = 11;
  iiBrush                 = 12;
  iiGroup                 = 13;
  iiDuplicator            = 14;
  iiPolyhedron            = 15;
  iiFace                  = 16;
  iiCfgFolder             = 17;
  iiCfg                   = 18;
  iiInvalidPolyhedron     = 19;
  iiInvalidFace           = 20;
  iiNewFolder             = 21;
  iiWad                   = 22;
  iiTexture               = 23;
  iiTextureLnk            = 24;
  iiPcx                   = 25;
  iiQuArK                 = 26;
  iiTexParams             = 27;
  iiQme                   = 28;
  iiToolbar               = 29;
  iiToolbarButton         = 30;
{  iiFrameGroup            = 31;
  iiSkinGroup             = 32;
  iiSkin                  = 33;}
  iiGroupXed              = 31;
  iiGroupHidden           = 32;
  iiGroupHiddenXed        = 33;
  iiFrame                 = 34;
  iiComponent             = 35;
  iiModelGroup            = 36;
  iiQCtx                  = 37;
  iiWav                   = 38;
  iiCin                   = 39;
  iiText                  = 40;
  iiCfgFile               = 41;
  iiImport                = 42;
  iiPython                = 43;
  iiBezier                = 44;
  iiSpriteFile            = 45;
  iiModelTag              = 46;
  iiModelBone             = 47;
  iiFormElement           = 48;
  iiForm                  = 49;
  iiFormContext           = 50;

  InternalImagesCount     = 51;

  fmOpenReadOnly_ShareDenyWrite  = fmOpenRead      or fmShareDenyWrite;
  fmOpenReadWrite_ShareDenyWrite = fmOpenReadWrite or fmShareDenyWrite;

  ofTreeViewSubElement      = $01;   { all objects in a tree-view except top-level (don't change value, must be 1) }
  ofTreeViewInvisible       = $02;   { present but invisible in the tree-view }
  ofTreeViewAlreadyExpanded = $04;   { node has been expanded once }
  ofTreeViewExpanded        = $08;   { node is expanded }
  ofNotLoadedToMemory       = $10;
  ofFileLink                = $20;
  ofWarnBeforeChange        = $40;
  ofModified                = $80;
  ofCloneFlags              = $FF{-ofTvNode};

 {of2TvNodeSel  = $01;
  ofCloneFlags2 = $FF;}

  smNonSel      = $00;
  smSel         = $01;
  smSousSelVide = $02;
  smSpecial     = $04;

  cmObjFirst = $6800;
  cmObjLast  = $68FE;

 {MaxFloatAccept = 1E7-1;
  MinFloatAccept = 1E-8;}

  chrFloatSpec = 128;

  DefaultForm = 'Default';

implementation

end.

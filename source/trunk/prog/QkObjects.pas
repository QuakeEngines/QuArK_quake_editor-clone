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
Revision 1.123  2009/07/12 12:43:32  danielpharos
Updated links to website for new website address.

Revision 1.122  2009/04/28 20:54:03  cdunde
Model Editor Bone Rebuild merge to HEAD.
Complete change of bone system.

Revision 1.121.2.1  2009/04/21 20:27:19  danielpharos
Hide QSysData from treeview, fix access violations in QModelBone if specifics not set, and allow bones-in-bones.

Revision 1.121  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.120  2009/02/05 21:36:54  danielpharos
Add colorboxes in treeview for Model Editor bones to display start_color and end_color.

Revision 1.119  2009/02/05 21:34:31  danielpharos
Fixed an exception raising method and set a warning to its proper log level.

Revision 1.118  2009/01/29 23:52:28  danielpharos
Updated for QuArK 6.6 Beta 2 release.

Revision 1.117  2009/01/08 18:51:51  danielpharos
Resolve all filenames going through FileaccessQ.

Revision 1.116  2008/11/30 20:47:05  danielpharos
TList --> TQList, and fix possible memory leak in a debug routine.

Revision 1.115  2008/11/19 06:14:00  cdunde
Bones system moved to outside of components for Model Editor completed.

Revision 1.114  2008/11/06 21:11:50  danielpharos
Made type Specifics soft-coded: Will lated be changed into a new, yet-to-be-defined type.

Revision 1.113  2008/11/06 20:18:22  danielpharos
Removed old stuff in preparation for new specifics code.

Revision 1.112  2008/10/29 00:31:07  danielpharos
The printout of QuArK internal objects is now much more useful.

Revision 1.111  2008/10/20 20:42:41  danielpharos
Take out the lists-part. Too many problems!

Revision 1.110  2008/10/17 08:07:34  danielpharos
Oops, missed a spot.

Revision 1.109  2008/10/14 00:07:16  danielpharos
Add an integer list as a specific type.

Revision 1.108  2008/09/23 08:27:29  danielpharos
Moved InternalE to QkExceptions.

Revision 1.107  2008/09/14 12:52:30  danielpharos
Changes to Help system: All forms now have a customizable help-link. Also, added an fallback option to the online infobase docs.

Revision 1.106  2008/09/06 15:57:01  danielpharos
Moved exception code into separate file.

Revision 1.105  2008/08/09 19:40:22  danielpharos
Translated a function call

Revision 1.104  2008/08/07 15:23:06  danielpharos
Re-enable turned off error catchers

Revision 1.103  2008/08/07 15:22:16  danielpharos
Fix update file location

Revision 1.102  2008/07/10 22:32:40  danielpharos
Added additional comments and fixed some magic numbers.

Revision 1.101  2008/05/27 07:42:31  cdunde
To allow Save File to work in the Model Editor and stop crashes,
 also to allow Used Skins in the Texture Browser and for them to be used.

Revision 1.100  2008/05/05 05:18:37  cdunde
Update for new release version.

Revision 1.99  2008/05/05 01:26:16  cdunde
Update for new release version.

Revision 1.98  2008/05/04 16:17:54  cdunde
Update for new release.

Revision 1.97  2008/05/01 12:12:49  danielpharos
Revert previous update and do a correct fix.

Revision 1.96  2008/04/12 20:18:34  danielpharos
Possibly fixed the dictitems memory leak and fixed the miscounting of gamefiles-memory.

Revision 1.95  2008/03/29 15:15:47  danielpharos
Moved all the CompareMem stuff to ExtraFunctionality, where it belongs.

Revision 1.94  2007/12/22 18:02:01  cdunde
Update

Revision 1.93  2007/09/12 16:24:29  danielpharos
Moved update settings to seperate config section and added beginnings of online update check.

Revision 1.92  2007/09/12 15:28:16  danielpharos
Replaced redundant property.

Revision 1.91  2007/09/10 10:24:19  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.90  2007/08/29 20:23:04  cdunde
To update date and version name for new release.

Revision 1.89  2007/08/10 12:16:08  danielpharos
Updated the update-check. You can disable it in the Config, and it now asks if you want to go to the website.

Revision 1.88  2007/08/04 14:53:07  danielpharos
Fixed a small mistake in comment from prev rev.

Revision 1.87  2007/08/04 14:47:23  danielpharos
Added a very basic update check when starting up QuArK.

Revision 1.86  2007/03/17 15:43:12  danielpharos
Made another few dictionnary changes. Also fixed a double entry. And a small change in unloading the dll-files of VTFLib.

Revision 1.85  2007/03/05 01:00:43  danielpharos
Found another place where NoShare was used. Commented out, and added a reference in QkObjects.

Revision 1.84  2007/03/01 22:16:03  danielpharos
Big fix for the enormous slowdown.

Revision 1.83  2007/02/07 18:48:34  danielpharos
Fixes for memory leaks

Revision 1.82  2007/02/07 14:09:22  danielpharos
Fix a few Range Check errors

Revision 1.81  2007/01/11 17:46:05  danielpharos
Added comments to a block of ASM code

Revision 1.80  2006/11/30 00:44:32  cdunde
To merge all source files that had changes from DanielPharos branch
to HEAD for QuArK 6.5.0 Beta 1.

Revision 1.79.2.1  2006/11/29 05:00:28  cdunde
For version change to 6.5 Beta.

Revision 1.79  2006/05/05 06:04:44  cdunde
To reverse Texture Memory changes. Cases problems with Quake 3 QkQ3.pas
handling of textures in the Texture Browser, hour glass icon jitters and memeor usage
increases causing prog crash, can not use scrole bar in TB.

Revision 1.78  2006/04/06 19:28:03  nerdiii
Texture memory wasn't freed because texture links had additional references to them.

Revision 1.77  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.75  2005/08/15 21:15:48  cdunde
update version to QuArK 6.5.0 alpha 1

Revision 1.74  2004/12/27 10:57:44  alexander
added a userdata pointer. This is needed to access context data from on_access callbacks

Revision 1.73  2004/01/05 22:16:18  silverpaladin
Clearing is done from end down now with a assigned check added an assigned check to find name

Revision 1.72  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.71  2003/08/12 15:53:11  silverpaladin
Fixed an index out of bounds error in find sub elements by insuring index is < count

Revision 1.70  2002/05/16 09:07:40  tiglari
Update version to 6.4 alpha (no diff from 6,3 yet)

Revision 1.69  2002/05/13 11:31:17  tiglari
update version

Revision 1.68  2002/04/28 21:26:53  tiglari
update version

Revision 1.67  2002/04/03 00:27:16  tiglari
ShareSpecMem now defined iff NoShare Conditional Define is not set

Revision 1.66  2002/02/26 23:20:56  tiglari
oops restore sharespecmem

Revision 1.65  2002/02/26 23:19:46  tiglari
update version

Revision 1.64  2002/02/24 13:46:53  decker_dk
Update version to "QuArK 6.3snap 2002feb24"

Revision 1.63  2002/01/06 10:36:48  decker_dk
update version to "QuArK 6.3snap 2002jan06"

Revision 1.62  2001/12/30 09:59:57  tiglari
alter TQStream release code to prevent an error, depends on 0 not being
a valid FHandle value.  I don't really know what's going on with the
TemporaryClose stuff.

Revision 1.61  2001/10/14 10:07:21  tiglari
Live Pointer Hunt: rollback of finalizings, & free QFileList

Revision 1.57  2001/08/06 00:18:24  tiglari
update version

Revision 1.56  2001/07/09 09:53:56  tiglari
update version #

Revision 1.55  2001/06/18 02:37:08  tiglari
update version

Revision 1.54  2001/06/05 18:41:26  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.53  2001/05/21 12:04:46  tiglari
update version

Revision 1.52  2001/05/07 08:57:22  tiglari
update version

Revision 1.51  2001/04/28 02:42:21  tiglari
update version

Revision 1.50  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.49  2001/03/20 21:45:22  decker_dk
Updated copyright-header

Revision 1.48  2001/03/10 01:14:24  tiglari
version fix

Revision 1.47  2001/03/09 09:30:57  tiglari
update version

Revision 1.46  2001/02/25 11:19:12  tiglari
reset snapshot #

Revision 1.45  2001/02/23 19:25:35  decker_dk
Proper indenting, and other small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.44  2001/02/14 23:35:57  alexander
set name

Revision 1.43  2001/02/12 03:48:25  tiglari
reset snapshot #

Revision 1.42  2001/02/05 11:32:16  tiglari
dynamic folder writing fix

Revision 1.41  2001/02/02 08:17:57  tiglari
updated version #

Revision 1.40  2001/01/29 19:24:58  tiglari
removed priorityadd, fixed bug in locatesubelement

Revision 1.39  2001/01/28 03:30:41  tiglari
LocateSubElement method added

Revision 1.38  2001/01/23 08:05:39  tiglari
Infrastructure for OsFolders (read their contents from folders on disk,
 don't write them when writing the .qrk).

Revision 1.37  2001/01/21 15:49:30  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.36  2001/01/21 07:09:07  tiglari
Fixed OsFolders goofs (wrong version...)

Revision 1.35  2001/01/21 06:18:36  tiglari
support for doing stuff to QObjects before expanding them in treeviews

Revision 1.34  2001/01/07 13:21:25  decker_dk
Remember to put argument-list on procedure/function-definitions, and not only on declarations.
Set Versionname.

Revision 1.33  2000/11/26 19:08:32  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.32  2000/11/25 20:51:32  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.31  2000/11/19 15:31:49  decker_dk
- Added 'ImageListTextureDimension' and 'ImageListLoadNoOfTexAtEachCall' to
Defaults.QRK, for manipulating the TextureBrowser-TextureLists.
- Modified TFQWad.PopulateListView, so it reads the above settings.
- Changed two 'goto bail' statements to 'break' statements, in QkObjects.
- Found the problem in the .MAP exporting entity-numbering, and corrected it.
- Changed the '|' delimiting character in QObject.Ancestry to '->', as I think
it will be more readable in the .MAP file.
- Replaced the function-names:
  = SauverTexte         -> SaveAsText
  = SauverTextePolyedre -> SaveAsTextPolygon
  = SauverTexteBezier   -> SaveAsTextBezier
  = SauverSpec          -> SaveAsTextSpecArgs

Revision 1.30  2000/11/04 04:18:22  tiglari
Put in proper declaration (protected) for QObject.Pedigree

Revision 1.29  2000/10/27 10:13:41  tiglari
reformatted Ancestry value in accord with suggestions by decker

Revision 1.28  2000/10/17 20:29:41  tiglari
Added Ancestry function to QObject

Revision 1.27  2000/10/15 16:01:27  alexander
set name

Revision 1.26  2000/09/24 23:57:50  alexander
set name

Revision 1.25  2000/09/14 18:00:22  decker_dk
Moved QTexture1 and QTexture2 into QkQ1.PAS and QkQ2.PAS

Revision 1.24  2000/09/10 13:00:01  alexander
set name

Revision 1.23  2000/09/09 14:30:32  alexander
fixed GetSousElements bug when acessing a subobject of an object whose file is not yet loaded

Revision 1.22  2000/09/01 00:50:22  alexander
set name

Revision 1.21  2000/08/20 10:48:11  aiv
iiMD3Bone -> iiModelBone
iiMD3Tag -> iiModelTag

Revision 1.20  2000/07/28 15:11:34  alexander
set snapshot name

Revision 1.19  2000/07/25 16:04:32  alexander
set snapshot name

Revision 1.18  2000/07/18 19:38:00  decker_dk
Englishification - Big One This Time...

Revision 1.17  2000/07/18 13:46:10  alexander
set snapshot name

Revision 1.16  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.15  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.14  2000/07/03 23:17:15  alexander
set snapshot version

Revision 1.13  2000/06/09 23:30:36  aiv
Added Image Constants (iiMD3Tag & iiMD3Bone)

Revision 1.12  2000/05/21 13:11:50  decker_dk
Find new shaders and misc.
}

unit QkObjects;

interface

{$I DelphiVer.inc}

uses Windows, SysUtils, Messages, Classes, Clipbrd,
     Controls, Graphics, Forms, qmath, Menus,
     CommCtrl, Python;

const
  QuArKVersion            = 'QuArK 6.6 Beta';
  QuArKMinorVersion       = 'Beta 2';
  QuArKCopyright          = 'Copyright (C) 1996-2009 Armin Rigo and others';
  QuArKUsedCompiler       = 'Delphi 7.0';
  QuArKCompileDate        = 39843;   //This is the compiled date
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

type
  TIsExplorerItem = set of  (ieDisplay
                            ,ieCanDrop
                            ,ieListView
                            ,ieInvisible
                            ,ieNoAutoDrop
                            );

  TAjScene =  (asAucun
              ,asAjoute
              ,asRetire
              ,asRetireEnfant
              ,asDeplace1
              ,asDeplace2
              ,asModifie
              ,asModifieFrere
              ,asModifieParent
              );

const
  ieResult : array[Boolean] of TIsExplorerItem = ([], [ieDisplay, ieCanDrop]);

  eoDescription = $01;
  eoParentSel   = $80;  { internal use of TMyTreeView }

type
  TSpecificsList = TStringList;
  TQList = class;
  TQStream = class;
  QObject = class;
  QObjectClass = class of QObject;

  TEtatObjet =  record
                  IndexImage: Integer;
                  MarsColor: TColorRef;
                end;

  TDisplayDetails = record
                      Icon: PyObject;  { actually PyImage1 }
                      Flags: Integer;   { eoXXX }
                    end;

  TColorBoxList = class
                  private
                    nSpecName: array of String;
                    nColorType: array of String;
                    function GetSpecName(I: Integer) : String;
                    procedure SetSpecName(I: Integer; const SpecName: String);
                    function GetColorType(I: Integer) : String;
                    procedure SetColorType(I: Integer; const ColorType: String);
                  public
                    procedure Add(const SpecName, ColorType: String);
                    function Count : Integer;
                    property SpecName[I: Integer] : String read GetSpecName write SetSpecName; default;
                    property ColorType[I: Integer] : String read GetColorType write SetColorType;
                  end;

  PQStreamRef = ^TQStreamRef;
  TQStreamRef = record
                  Self: TQStream;
                  Position: Integer;
                  StreamSize: Integer;
 {AiV}            OnAccess: Function (Ref: PQStreamRef; var S: TStream) : Integer;
                  PUserData: Pointer;
                end;

  TInfoEnreg1 = class
  public
    Format: Integer;
    F: TStream;
    TransfertSource: Boolean;
    TempObject: QObject;
    procedure WriteSibling(const Path: String; Obj: QObject); virtual;
    destructor Destroy; override;
  end;

  PPythonObj = ^TPythonObj;
  TPythonObj = object(TyObject)
               end;

  QObject = class
  private
    FSpecifics: TSpecificsList;
    FSubElements: TQList;
    function GetTvParent : QObject;
    { Tv for `tree-view; Nil if obj is root in a tree
      view, such was worldspawn, whose parent is
      the map-object it is in, but whose TvParent is Nil. }
    procedure SetTvParent(nParent: QObject);
    function GetIntSpec(const Name: String) : Integer;
    procedure SetIntSpec(const Name: String; Value: Integer);
    function GetSelUnique : Boolean;
    { true if object is selected but not any grand* parent }
    procedure SetSelUnique(Value: Boolean);
    { selects object; unselects grand*parents & their other children }
    function GetVectSpec(const Name: String) : TVect;
    procedure SetVectSpec(const Name: String; const Value: TVect);
    function Copying(F: TStream; TransfertSource: Boolean) : Boolean;
    { for direct copying objects that haven't been loaded
      yet, see QkObjects.txt:Copying }
  protected
    FFlags: Byte; { ofXXX flags above }
    FSelMult: Byte; { smXXX flags above }
    FLoading: Boolean; { true while object is being loaded }
    FPyNoParent: Boolean;  { used by polyhedrons }
    procedure SaveFile(Info: TInfoEnreg1); virtual;
    { core function for writing to file, normally overridden,
      nature of Info varies per file format unit (e.g. QkSin) }
    procedure LoadFile(F: TStream; FSize: Integer); virtual;
    { core function for reading from file, normally overridden }
    procedure FixupReference; virtual;
    { normally does nothing, sometimes packs things up into a
      more efficient format, e.g. TTreeMapEntity origin (QkMapObject.pas)}
    procedure ReadUnformatted(F: TStream; Size: Integer);
    procedure SaveUnformatted(F: TStream);
    { for reading/writing raw data to/from a Data specific,
      used in overrides for Load/SaveFile }
    procedure FileCrashRecoverHack;
  public
    { propriétés commune aux QObjects }
    PythonObj: TPythonObj;
    { structure with Python Object fields; to make a real
      Python object, take a pointer to this }
    FNode: PQStreamRef;
    { info about where on disk the
      objects info is, see QkObjects.txt:delay_loading }
    FParent: QObject;
    Name: String;
    constructor Create(const nName: String; nParent: QObject);
    { prep for expansion in a treeview }
    procedure LoadAll;
    procedure AccesRec;
    procedure Open(F: TQStream; Taille: Integer);
    procedure SaveFile1(Info: TInfoEnreg1);
    destructor Destroy; override;
    procedure FixupAllReferences;
    property Specifics: TSpecificsList read FSpecifics write FSpecifics;
    property SubElements: TQList read FSubElements;
    function Ancestry: String;
    procedure AddRef(Delta: Integer);
    { incr/decr Py reference count, frees if 0 }
    procedure Acces;
    { does actual full read-in }
    procedure LoadInternal(F: TStream; FSize: Integer);
    function GetObjectSize(Loaded: TQStream; LoadNow: Boolean) : Integer;
    procedure ObjectState(var E: TEtatObjet); virtual;
    procedure DisplayDetails(SelIcon: Boolean; var D: TDisplayDetails); virtual;
    property Flags: Byte read FFlags write FFlags;
    { properties concerning display in a tree-view }
    property SelMult: Byte read FSelMult write FSelMult;
    property SelUnique: Boolean read GetSelUnique write SetSelUnique;
    class function ShowInListSel : Boolean; virtual;
   {function AjouterElement(Items: TTreeNodes; nParent, nInsert: TTreeNode) : TTreeNode;}
   {procedure SetNode(nNode, ParentNode: TTreeNode);}
    property TvParent: QObject read GetTvParent write SetTvParent;
    procedure SetSelMult;
    procedure ToggleSelMult;
    function NextInGroup: QObject;
    function Clone(nParent: QObject; CopySel: Boolean) : QObject;
    procedure CopyAllData(Source: QObject; CopySel: Boolean);
    function IsExplorerItem(Q: QObject) : TIsExplorerItem; virtual;
    property IntSpec[const Name: String] : Integer read GetIntSpec write SetIntSpec;
    property VectSpec[const Name: String] : TVect read GetVectSpec write SetVectSpec;
(*    function GetIntsSpec(const Name: String; var I: array of Integer) : Boolean;*)
    function GetFloatSpec(const Name: String; const Default: Single) : Single;
    procedure SetFloatSpec(const Name: String; const Value: Single);
    function GetFloatsSpecPartial(const Name: String; var F: array of Single) : Integer;
    function GetFloatsSpec(const Name: String; var F: array of Single) : Boolean;
    procedure SetFloatsSpec(const Name: String; const F: array of Single);
    function GetSpecArg(const Name: String) : String;
    { Returns "<specific-name>=<args>" }
    function GetArg(const Name: String) : String;
    { Returns "<args>" }
   {procedure SetTextsSpec(const Name: String; L: TStrings);}
    function FindSubObject(const nName: String; WantClass, BrowseClass: QObjectClass) : QObject;
    function LocateSubElement(const LocName: String; var Index: Integer) : QObject; overload;
    procedure FindAllSubObjects(const nName: String; {nName='' for all} WantClass, BrowseClass: QObjectClass; L: TQList);
    procedure Modified;
    { sets ofModified flag (FFlags) for object and its transitive FParents }
    procedure CopyExtraData(var HasText: Boolean); dynamic;
    { for copying non-Quark representations of i.e. images to
      the clipboard.  If no non-Quark rep, does nothing }
    function TopLevel : Boolean;
    procedure LoadedFileLink(const nName: String; ErrorMsg: Integer);
    procedure OperationInScene(Aj: TAjScene; PosRel: Integer); virtual;
    function GetObjectMenu(Control: TControl) : TPopupMenu; dynamic;
   {property NodeLoadInfo: PQStreamRef read FNode;}
    class function TypeInfo: String; virtual; abstract;
    { returns `extension' of object, see QkObjects.txt:TypeInfo }
    function GetFullName: String;
    procedure ClearAllSelection;
    function PyGetAttr(attr: PChar) : PyObject; virtual;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; virtual;
    procedure PySetParent(nParent: QObject);
    function IsClosed: Boolean;
    property PyNoParent: Boolean write FPyNoParent;
    function DirectDataAccess(var S: TStream; var Size: Integer) : Boolean;
    { stuff that should be done when an object has been read in from text rep. }
    procedure FinalizeFromText; virtual;
    function WriteSubelements : Boolean; virtual;
    function ClassList : TStringList;
    function IsAllowedParent(Parent: QObject) : Boolean; virtual;
    function TreeViewColorBoxes : TColorBoxList; virtual;
  end;

  TQList = class(TList)
  private
    function GetItem1(I: Integer) : QObject;
    procedure SetItem1(I: Integer; Q: QObject);
  public
    function Add(Q: QObject) : Integer;
    procedure Delete(I: Integer);
    {$IFDEF CompiledWithDelphi2}
      {$DEFINE NOVIRTUALCLEAR}
    {$ENDIF}
    {$IFDEF CompiledWithDelphi3}
      {$DEFINE NOVIRTUALCLEAR}
    {$ENDIF}
    {$IFDEF NOVIRTUALCLEAR}
    destructor Destroy; override;
    procedure Clear;
    {$ELSE}
    procedure Clear; override;
    {$ENDIF}
    procedure Insert(I: Integer; Q: QObject);
    function Remove(Q: QObject) : Integer;
    property Items1[I: Integer] : QObject read GetItem1 write SetItem1; default;
    function FindName(const nName: String) : QObject;
    function FindShortName(const nName: String) : QObject;
    function FindLastShortName(const nName: String) : QObject;
  end;

  TQStream = class(TStream)
  protected
    FHandle: Integer;   { we cannot use TFileStream because this is private :-(  }
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Handle: Integer read FHandle;
  public
    { --- start of the QuArK-specific part --- }
    RefCount1: Integer;
    Temporary: Boolean;
    DisableDelayLoading: Boolean;
   {Root: QObject;  { actually a QFileObject }
   {destructor Destroy; override;}
    procedure AddRef;
    procedure Release;
    function AddRefNode(a_StreamSize: Integer): PQStreamRef;
    procedure TemporaryClose;
    function ReopenAs(const FileName: String) : Boolean;
  end;

  TRawDataStream = class(TCustomMemoryStream)
  public
    constructor Create(Source: PChar; Size: Longint);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

type
  TModeAcces = set of (maNoOpen, maUnique);
  TClipboardHandler = function (PasteNow: QObject) : Boolean;

function EndOfClipboardChain(PasteNow: QObject) : Boolean;

var
  g_CF_QObjects: Integer;
  g_ClipboardChain: TClipboardHandler = EndOfClipboardChain;
  g_PopupMenuObject: QObject;
(*CodeConstruction: Char;*)

function FileAccessQ(const theFilename: String; Mode: TModeAcces) : TQStream;
procedure LoadedItem(Format: Integer; F: TStream; Q: QObject; Size: Integer);
(*function IntSpecNameOf(const Name: String) : String;*)
function FloatSpecNameOf(const Name: String) : String;
procedure CheckValidSpec(var Spec: String);
function StringListConcatWithSeparator(theStringList: TStrings; theStringSeparator: Byte) : String;
{procedure FreeOldObjects;}
{AiV}function QStreamAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
{procedure QStreamRelease(Ref: TTreeNode);}

function CharToPas(C: array of Byte) : String;
procedure PasToChar(var C: array of Byte; const S: String);
function IntToPackedStr(Value: Integer) : String;
function PackedStrToInt(const S: String) : Integer;

procedure ReleaseStream(S: TStream);

{$IFDEF Debug}
var g_MemQObject: TQList;
procedure DebugCheck;
{function DebugError: Exception;}
procedure DataDump;
{$ENDIF}

 {------------------------}

implementation

uses
  {$IFDEF Debug} MemTester, {$ENDIF}
  QkObjectClassList, QkFileObjects, QkExplorer, Travail, Game,
  PyObjects, PyImages, Quarkx, QkExceptions, Qk1, Logging{, ExtraFunctionality};

var
  QFileList: TStringList;

 {------------------------}

function EndOfClipboardChain(PasteNow: QObject) : Boolean;
begin
  Result:=False;
end;

function FileAccessQ(const theFilename: String; Mode: TModeAcces) : TQStream;
var
  I: Integer;
 {S1, S,} FullName: String;
  Z: array[0..MAX_PATH] of Char;
begin
  FullName:=ExpandFileName(QuickResolveFilename(theFilename));
(*S1:=ExtractFilePath(FullName);
  SetLength(S, 511);
  if GetShortPathName(PChar(S1), PChar(S), 512) = 0 then
    Raise EErrorFmt(5188, [S1]);
  SetLength(S, StrLen(PChar(S)));
  FullName:=S + ExtractFileName(FullName);*)

  if GetShortPathName(PChar(FullName), Z, SizeOf(Z)) = 0 then
  begin
    if maNoOpen in Mode then
    begin
      Result:=Nil;
      Exit;
    end
    else
      Raise EErrorFmt(5188, [FullName]);
  end;
  SetString(FullName, Z, StrLen(Z));

  if QFileList.Find(FullName, I) then
  begin
    Result:=TQStream(QFileList.Objects[I]);

    if maUnique in Mode then
      Raise EErrorFmt(5189, [theFilename]);

    if maNoOpen in Mode then
      Exit;

    Result.Position:=0;
  end
  else
  begin
   {if Ecraser then
      Result:=TQStream.Create(FullName, fmCreate)
    else}
    if maNoOpen in Mode then
    begin
      Result:=Nil;
      Exit;
    end;

    if maUnique in Mode then
      Result:=TQStream.Create(FullName, fmOpenReadWrite_ShareDenyWrite)
    else
      Result:=TQStream.Create(FullName, fmOpenReadOnly_ShareDenyWrite);

    QFileList.AddObject(FullName, Result);
  end;
 {Result.AddRef;}
end;

 {------------------------}

function CharToPas(C: array of Byte) : String;
var
  I: Integer;
begin
  I:=0;
  while (I<=High(C)) and (C[I]<>0) do
    Inc(I);
  SetLength(Result, I);
  Move(C, PChar(Result)^, I);
end;

procedure PasToChar(var C: array of Byte; const S: String);
begin
  if Length(S) <= High(C) then
  begin
    Move(PChar(S)^, C, Length(S));
    FillChar(C[Length(S)], High(C)+1-Length(S), 0);
  end
  else
    Move(PChar(S)^, C, High(C)+1);
end;

 {------------------------}

function TQList.GetItem1(I: Integer) : QObject;
begin
  if (I<0) or (I>=Count) then
    Raise InternalE('GetItem1');

  Result:=QObject(List^[I]);
end;

procedure TQList.SetItem1(I: Integer; Q: QObject);
begin
  if (I<0) or (I>=Count) then
    Raise InternalE('SetItem1');

  QObject(List^[I]).AddRef(-1);
  QObject(List^[I]):=Q;
  Q.AddRef(+1);
end;

function TQList.Add(Q: QObject) : Integer;
begin
  Result:=inherited Add(Q);
  Q.AddRef(+1);
end;

procedure TQList.Clear;
var
  I: Integer;
  QO: QObject;
begin
  // SilverPaladin - 12/01/03 - Clearing is done from end down now with a assigned check
  for I:=Count-1 downto 0
  do begin
    QO := QObject(List^[I]);
    if Assigned(QO) then QO.AddRef(-1);
  end;
  inherited Clear;
end;

{$IFDEF NOVIRTUALCLEAR}
destructor TQList.Destroy;
var
  I: Integer;
begin
  for I:=Count-1 downto 0 do
    QObject(List^[I]).AddRef(-1);
  inherited Destroy;
end;
{$ENDIF}

procedure TQList.Delete(I: Integer);
var
  Q: QObject;
begin
  Q:=QObject(Items[I]);
  inherited Delete(I);
  Q.AddRef(-1);
end;

procedure TQList.Insert(I: Integer; Q: QObject);
begin
  inherited Insert(I, Q);
  Q.AddRef(+1);
end;

function TQList.Remove(Q: QObject) : Integer;
begin
  Result:=IndexOf(Q);
  if Result<>-1 then
    Delete(Result);
end;

function TQList.FindName(const nName: String) : QObject;
var
 I: Integer;
begin
  for I:=0 to Count-1 do
  begin
    Result:=QObject(List^[I]);
    // SilverPaladin - 12/01/03 - Added an assigned check to bullet proof against
    // access violations.
    if not(Assigned(Result))
    then Break; // Quite the loop go on to Result := Nil;
    if CompareText(Result.GetFullName, nName) = 0
    then Exit; // Exit out at current selection, it is the one we want.
  end;
  Result:=Nil;
end;

function TQList.FindShortName(const nName: String) : QObject;
var
  I: Integer;
begin
  for I:=0 to Count-1 do
  begin
    Result:=QObject(List^[I]);
    if CompareText(Result.Name, nName) = 0 then
      Exit;
  end;
  Result:=Nil;
end;

function TQList.FindLastShortName(const nName: String) : QObject;
var
  I: Integer;
begin
  for I:=Count-1 downto 0 do
  begin
    Result:=QObject(List^[I]);
    if CompareText(Result.Name, nName) = 0 then
      Exit;
  end;
  Result:=Nil;
end;

 {------------------------}

constructor TQStream.Create(const FileName: string; Mode: Word);
begin
  if Mode=fmCreate then
  begin
    FHandle:=FileCreate(FileName);
    if FHandle<0 then
      raise EErrorFmt(384, [FileName]);
  end
  else
  begin
    FHandle:=FileOpen(FileName, Mode);
    if FHandle<0 then
      raise EErrorFmt(385, [FileName]);
  end;
end;

destructor TQStream.Destroy;
begin
  { I'm assuming here that valid filehandles will be greater
    than zero, since TemporaryClose sets fhandle to 0 }
  if FHandle>0 then
    FileClose(FHandle);
end;

function TQStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=FileRead(FHandle, Buffer, Count);
  if Result=-1 then
    Result:=0;
end;

function TQStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:=FileWrite(FHandle, Buffer, Count);
  if Result=-1 then
    Result:=0;
end;

function TQStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:=FileSeek(FHandle, Offset, Origin);
end;

procedure TQStream.AddRef;
begin
  Inc(RefCount1);
end;

{AiV}
Function DefaultAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
begin
  Ref^.Self.Position:=Ref^.Position;
  Ref^.Self.AddRef;
  S:=Ref^.Self;
  Result:=Ref^.StreamSize;
end;

function TQStream.AddRefNode(a_StreamSize: Integer): PQStreamRef;
begin
  Inc(RefCount1);
  New(Result);
  Result^.Self:=Self;
  Result^.Position:=Seek(a_StreamSize, soFromCurrent) - a_StreamSize;
  Result^.StreamSize:=a_StreamSize;
  Result^.OnAccess:=DefaultAddRef;
end;

procedure TQStream.Release;
var
  I: Integer;
begin
  Dec(RefCount1);
  if RefCount1<=0 then
  begin
    {$IFDEF Debug}
    if RefCount1<0 then
      Raise InternalE('QStream.RefCount1');
    {$ENDIF}

    I:=QFileList.IndexOfObject(Self);
    if I<0 then
      {$IFDEF Debug}
      Raise InternalE('QStream.Release')
      {$ENDIF}
    else
    begin
      if Temporary then
      begin
        if FHandle>0 then
          TemporaryClose;
        DeleteFile(QFileList[I]);
      end;
      QFileList.Delete(I);
    end;
   {if Root<>Nil then
    begin}
   (*Root.AddRef(-1);
     Root:=Nil;*)
   {end;}
    Free;
  end;
end;

procedure TQStream.TemporaryClose;
begin
  FileClose(FHandle);
  FHandle:=0;
end;

function TQStream.ReopenAs(const FileName: String) : Boolean;
var
  I: Integer;
begin
  I:=QFileList.IndexOfObject(Self);
  if I<0 then
  begin
    {$IFDEF Debug}
    Raise InternalE('ReopenAs');
    {$ENDIF}

    Result:=False;
    Exit;
  end;

  QFileList.Delete(I);

  { rename stream }
  QFileList.AddObject(FileName, Self);
  FHandle:=FileOpen(FileName, fmOpenReadOnly_ShareDenyWrite);
  Result:=FHandle>=0;
end;

function QStreamAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
begin
  Result:=Ref^.OnAccess(Ref, S); {AiV}
end;

procedure QStreamRelease(var Ref: PQStreamRef);
begin
  if Ref<>Nil then
  begin
    Ref^.Self.Release;
    Dispose(Ref);
    Ref:=Nil;
  end;
end;

{function QStreamCanClone(Q: QObject; Ref: TTreeNode) : Boolean;
begin
 Result:=(Ref<>Nil) and (PQStreamRef(Ref)^.Self.Root<>Q);
end;}

function QStreamClone(Ref: PQStreamRef) : PQStreamRef;
begin
  New(Result);
  Result^:=Ref^;
  Result^.Self.AddRef;
end;

{destructor TQStream.Destroy;
var
 aTemp: String;
begin
 aTemp:=Temporary;
 inherited;
 if aTemp<>'' then
  DeleteFile(PChar(aTemp));
end;}

(*procedure QStreamCopying(var Ref: TTreeNode; F: TStream);
var
 P: PQStreamRef;
 OldStream: TQStream;
begin
 if (F=Nil) or not (F is TQStream) then
  Exit;  { keeps the same reference to read the same file again later }

  { if F is a TQStream, we change the reference
    so that it points to the new file }
 P:=PQStreamRef(Ref);
 if P=Nil then
  OldStream:=Nil
 else
  Oldstream:=P^.Self;
 Ref:=Nil;
 try
  if P=Nil then  { builds a new read-again reference }
   New(P);
  TQStream(F).AddRef;
  P^.Self:=TQStream(F);
  P^.Position:=TQStream(F).Position;
  P^.StreamSize:=-1;
  { P^.StreamSize is unknown yet ;
    it will be fixed in CloseCopying if this method is called }
  Ref:=TTreeNode(P);
 finally
  if OldStream<>Nil then
   OldStream.Release;
 end;
end;*)

procedure ReleaseStream(S: TStream);
begin
  if S<>Nil then
  begin
    if S is TQStream then
      TQStream(S).Release
    else
      S.Free;
  end;
end;

 {------------------------}

constructor TRawDataStream.Create(Source: PChar; Size: Longint);
begin
  inherited Create;
  SetPointer(Source, Size);
end;

function TRawDataStream.Write(const Buffer; Count: Longint): Longint;
begin
  Raise InternalE('TRawDataStream.Write');
end;

 {------------------------}

constructor QObject.Create(const nName: String; nParent: QObject);
begin
  {$IFDEF Debug}
(*if QObjectClassList.IndexOfObject(TObject(ClassType))<0 then
    Raise InternalE('Unregistered QObjectClass');*)
  g_MemQObject.Add(Self);
  {$ENDIF}

  Name:=nName;
  FParent:=nParent;

{$IFDEF Debug}
  PythonObj.ob_refcnt := 0;
{$ENDIF}

(*if Self is QFileObject then
    PythonObj.ob_type:=@TyFileObject_Type
  else*)
    PythonObj.ob_type:=@TyObject_Type;

  FSpecifics:=TSpecificsList.Create;
  FSubElements:=TQList.Create;

  if not IsAllowedParent(nParent) then
    Log(LOG_WARNING, 'Object '+nName+' is being created in a non-allowed parent! This might produce errors!');
end;

destructor QObject.Destroy;
var
  I: Integer;
begin
  {$IFDEF Debug}
  I:=g_MemQObject.IndexOf(Self);
  if I<0 then
    Raise InternalE('QObject.Destroy');
  g_MemQObject.Delete(I);
  {$ENDIF}

 {if FFlags and ofTvNode = 0 then}
    QStreamRelease(FNode);

  for I:=0 to FSubElements.Count-1 do
  begin
    if FSubElements[I].FParent=Self then
      FSubElements[I].FParent:=Nil;  // Put break point here with Break When Condition: FSubElements[I].PythonObj.ob_refcnt > 1
  end;

  FSubElements.Free;
  FSpecifics.Free;

{$IFDEF Debug}
(*if FFlags and ofFileLink <> 0 then
  for I:=0 to QFileList.Count-1 do
   if TQStream(QFileList.Objects[I]).Root=Self then
    Raise InternalE('QStream.Root=(Freed)'+Name);*)
{$ENDIF}
end;

procedure QObject.AddRef(Delta: Integer);
begin
  if Self=Nil then
    Exit;

  Inc(PythonObj.ob_refcnt, Delta);
  if PythonObj.ob_refcnt<=0 then
  begin
    {$IFDEF Debug}
    if PythonObj.ob_refcnt<0 then
      Raise InternalE('QObject.AddRef');
    {$ENDIF}

    Free;
  end;
end;

procedure QObject.Modified();
var
 Q: QObject;
begin
  Q:=Self;
  while Q<>Nil do
  begin
    Q.FFlags:=Q.FFlags or ofModified;
    Q:=Q.FParent;
  end;
end;

procedure QObject.CopyExtraData(var HasText: Boolean);
begin
end;

(*procedure FreeOldObjects;
var
 OldList: TQList;
begin
 OldList:=Deleted; try
 Deleted:=Nil;
 OldList.Clear;  { free objects now }
 finally Deleted:=OldList; end;
end;*)

procedure QObject.AccesRec();
var
  S: TQStream;
  ddl: Boolean;
begin
  if (FFlags and ofNotLoadedToMemory <> 0) and not FLoading then
  begin
    { optimization only : tags the source stream as "DisableDelayLoading",
      which means that subobjects will be immediately loaded in LoadedItem
      instead of by the recursive LoadAll call below. }
    S:=FNode^.Self;
    S.AddRef;
    ddl:=S.DisableDelayLoading;
    try
      S.DisableDelayLoading:=True;
      Acces;
    finally
      S.DisableDelayLoading:=ddl;
      S.Release;
    end;
  end;
end;

procedure QObject.LoadAll();
var
  I: Integer;
begin
  AccesRec;
  for I:=0 to SubElements.Count-1 do
    SubElements[I].LoadAll;
end;

{procedure QObject.LireEnteteFichier;
begin
 Raise EErrorFmt(5191, [TypeInfo]);
end;}

procedure QObject.Acces();
var
  Source: TStream;
  SourceSize: Integer;
begin
  if (FFlags and ofNotLoadedToMemory = 0) or FLoading then
    Exit;

  if FNode=Nil then
    Raise InternalE('Acces:FNode=Nil');

  SourceSize:=QStreamAddRef(FNode, Source);
  try
    LoadInternal(Source, SourceSize);
    FFlags:=FFlags and not ofNotLoadedToMemory;
    QStreamRelease(FNode);
  finally {AiV}
    if Source is TQStream then
      TQStream(Source).Release
    else
      Source.Free;
  end;
end;

function QObject.GetFullName: String;
begin
  Result:=Name+TypeInfo();
end;

function QObject.GetObjectSize(Loaded: TQStream; LoadNow: Boolean) : Integer;
var
  I: Integer;
 {F: TSearchRec;}
begin
  Result:=SizeOf(QObject)+QObject.InstanceSize;

  { adds the size of the loaded data }
  for I:=0 to Specifics.Count-1 do
    Inc(Result, SizeOf(String)+Length(Specifics[I]));

  for I:=0 to SubElements.Count-1 do
    Inc(Result, SubElements[I].GetObjectSize(Loaded, LoadNow));

  { adds the size of the data not yet loaded }
  if (FFlags and ofNotLoadedToMemory) <> 0 then
  begin
    if (Loaded<>Nil) and (FNode<>Nil) and (PQStreamRef(FNode)^.Self=Loaded) then
    begin
      if LoadNow then
        Acces   { load data }
      else
        Inc(Result, PQStreamRef(FNode)^.StreamSize);  { size of not yet loaded data }
    end;
  end;

(*if (FFlags and ofNotLoadedToMemory <> 0) and Loaded then
  if FNode=Nil then  { add the size of the not already loaded data }
   begin     { from a not yet opened file }
    if FFlags and ofFileLink = 0 then Exit;  { error }
    if FindFirst((Self as QFileObject).Filename, faAnyFile, F) = 0 then
     Inc(Result, F.Size);
    FindClose(F);
   end
  else      { from an already opened file }
   Inc(Result, PQStreamRef(FNode)^.StreamSize);*)
end;

(*procedure QObject.AccesCopying(F: TStream);
var
 S: String;
 Source: TQStream;
 SourceTaille: Integer;
begin
 Result:=False;  { no automated copy }
 if FFlags and ofNotLoadedToMemory = 0 then
  begin
   if (FFlags and ofTvNode = 0) and (FNode<>Nil) then
    QStreamRelease(FNode);
   Exit;
  end;
 if FFlags and ofTvNode <> 0 then
  Raise InternalE('Acces');
 Source:=Nil;
 try
  if FFlags and ofLienFichier <> 0 then
   begin
    QStreamRelease(FNode);
    S:=(Self as QFileObject).Filename;
    Source:=FileAccessQ(S, False);
    SourceTaille:=Source.Size;
    LireEnteteFichier(Source, S, SourceTaille);
   end
  else
   SourceTaille:=QStreamAddRef(FNode, Source);
  LoadFile(Source, SourceTaille);
  FFlags:=FFlags and not ofNotLoadedToMemory;
  QStreamCopying(FNode, F);
 finally
  if Source<>Nil then
   Source.Release;
 end;
 BuildReferences;
 ProgressIndicatorStart(5442, SubElements.Count);
end;

procedure QObject.CloseCopying;
begin
 ProgressIndicatorStop;
 if FFlags and ofNotLoadedToMemory <> 0 then
  Raise InternalE('CloseReadOnly');
 if (FFlags and ofTvNode <> 0) or (FNode = Nil) then
  Exit;  { object is too much loaded to be unloadable }
 with PQStreamRef(FNode)^ do
  if StreamSize=-1 then
   StreamSize:=Self.Position - Position;  { update the size info }
 FFlags:=FFlags or ofNotLoadedToMemory;
 SubElements.Clear;
 Specifics.Clear;
end;*)

function QObject.Copying(F: TStream; TransfertSource: Boolean) : Boolean;
var
  Source: TStream;
  SourceTaille: Integer;
begin  { if possible, copy directly from the original file into the new one }
  Result:=False;
  if FFlags and ofNotLoadedToMemory = 0 then
    Exit;  { already called "Acces", original source no more available }

  if FNode=Nil then
    Raise InternalE('Copying:FNode=Nil');

  SourceTaille:=QStreamAddRef(FNode, Source);
  try
    if SourceTaille>0 then
      F.CopyFrom(Source, SourceTaille);   { copy data directly }

    if TransfertSource and (F is TQStream) then
    begin  { if the target is a TQStream, it can be used to replace the original source }
      QStreamRelease(FNode);
      F.Seek(-SourceTaille, soFromCurrent);
      FNode:=TQStream(F).AddRefNode(SourceTaille);
    end;
  finally {AiV}
    if Source is TQStream then
      TQStream(Source).Release
    else
      Source.Free;
  end;

(*Source:=Nil;
 try
  if FNode<>Nil then
   SourceTaille:=QStreamAddRef(FNode, Source)
  else
   begin
    if FFlags and ofFileLink = 0 then
     Raise InternalE('Acces without FNode');
    S:=(Self as QFileObject).Filename;
    Source:=FileAccessQ(S, []);
    SourceTaille:=Source.Size;
   end;
  if SourceTaille>0 then
   F.CopyFrom(Source, SourceTaille);   { copy data directly }
  if F is TQStream then
   begin  { the target is a TQStream, so it can be used to replace the original source }
    QStreamRelease(FNode);
    F.Seek(-SourceTaille, soFromCurrent);
    FNode:=TQStream(F).AddRefNode(SourceTaille);
   end;
 finally
  if Source<>Nil then
   Source.Release;
 end;*)

 Result:=True;
end;

procedure QObject.ObjectState;
begin  { note: QFileObject.EtatObject doesn't call "inherited" }
  E.IndexImage:=iiUnknown;
  E.MarsColor:=clNavy;
end;

procedure QObject.DisplayDetails;
var
  E: TEtatObjet;
  args: PyObject;
begin
  ObjectState(E);

  if SelIcon then
    D.Icon:=Nil
  else
    D.Icon:=InternalImages[E.IndexImage, 1];

  if D.Icon=Nil then
    D.Icon:=InternalImages[E.IndexImage, 0];

  if D.Icon<>Nil then
  begin
    if D.Icon^.ob_type = @TyImage1_Type then
      Py_INCREF(D.Icon)
    else
    begin
      args:=Py_BuildValueX('(O)', [@PythonObj]);
      if args<>Nil then
      begin
        try
          D.Icon:=PyEval_CallObject(D.Icon, args);
        finally
          Py_DECREF(args);
        end;
        if (D.Icon<>Nil) and (D.Icon^.ob_type <> @TyImage1_Type) then
        begin
          Py_DECREF(D.Icon);  { Image1 expected }
          D.Icon:=Nil;
        end;
      end;
      PythonCodeEnd;
    end;
  end;

  D.Flags:=0;
end;

procedure QObject.FixupReference;
begin
end;

(*procedure QObject.DoRemoveReference;
begin
 if RemoveReference then  { call this }
  Raise InternalE('DoRemoveReference');  { but it should not come here }
end;*)

procedure BrowseFixupRef(Parent: QObject);
var
  I: Integer;
  Q: QObject;
begin
  { no call to Acces }
  for I:=Parent.SubElements.Count-1 downto 0
  do begin
    Q:=Parent.SubElements[I];
    if (Assigned(Q) and (Q.Name <> ''))
    then begin
      Q.FixupReference;
      BrowseFixupRef(Q);
    end;
  end;
end;

procedure QObject.FixupAllReferences;
begin
  try
    FixupReference;
    BrowseFixupRef(Self);
  except
  end;
end;

{procedure QObject.BuildReferences;
begin
end;}

{function QObject.LoadFormat : Integer;
begin
 LoadFormat:=lf_Native;
end;}

 {------------------------}

const
  qsShortSizeMax      = $6F;  { maximum size stored in "Code" }
  qsLongSize          = $70;  { longer sizes are stored elsewhere }
  qsLongSizeMask      = $07;  { byte count for the long size }
  qsSizeMask          = $7F;  { mask for the previous flags }

  qsBitSubObject      = $80;  { file item is a sub-object }
  qsCodeFileLink      = $F8;  { file item is a file link }

type
  PFileItemInfo = ^TFileItemInfo;
  TFileItemInfo = packed record
                    Code: Byte;
                    NameSize: Byte;
                  end;

procedure QObject.LoadInternal(F: TStream; FSize: Integer);
begin
  FLoading:=True;
  try
    LoadFile(F, FSize);
  finally
    FLoading:=False;
  end;
end;

procedure QObject.FileCrashRecoverHack;
var
  S: String;
begin
  S:=FmtLoadStr1(5190, [Name]);
  if MessageBox(0, PChar(S), 'QuArK', MB_YESNO or MB_DEFBUTTON2 or MB_SYSTEMMODAL) <> IDYES then
    Abort;
end;

procedure QObject.LoadFile(F: TStream; FSize: Integer);
var
  Name, Names, ExtraSizes: String;
  DeltaPos, I, J, FileItemCount, Size, ExtraSize: Integer;
  NamePtr: PChar;
  Info, FileItemInfo: PFileItemInfo;
  Q: QObject;
  Hack: Boolean;
begin
 {if Format<>lf_Native then
    Raise EErrorFmt(5191, [TypeInfo, Format]);}

  Hack:=False;
  FileItemCount:=0;
  try
    F.ReadBuffer(FileItemCount, 1);
  except
    FileCrashRecoverHack;
    Exit;
  end;

  DeltaPos:=1;
  if (FileItemCount and qsSizeMask) > qsShortSizeMax then
  begin
    { file item count is stored in more than one byte }
    Size:=0;
    if FileItemCount and qsLongSizeMask > SizeOf(Size) then
      Raise EErrorFmt(5509, [51]);

    F.ReadBuffer(Size, FileItemCount and qsLongSizeMask);
    Inc(DeltaPos, FileItemCount and qsLongSizeMask);
    FileItemCount:=Size;
  end;

  Size:=FileItemCount * SizeOf(TFileItemInfo);
  Inc(DeltaPos, Size);
  if DeltaPos > FSize then   { Raise EErrorFmt(5509, [52]); }
  begin
    FileCrashRecoverHack;
    Exit;
  end;

  if FileItemCount=0 then
    Exit;  { no data }

  GetMem(FileItemInfo, Size);
  try
    { read file item infos }
    F.ReadBuffer(FileItemInfo^, Size);

    Info:=FileItemInfo;
    Size:=0;
    ExtraSize:=0;

    for I:=1 to FileItemCount do
    begin
      { scan the file item infos }
      Inc(Size, Info^.NameSize);

      if (Info^.Code and qsSizeMask) > qsShortSizeMax then
        Inc(ExtraSize, Info^.Code and qsLongSizeMask);

      Inc(Info);
    end;

    Inc(DeltaPos, Size);

    if DeltaPos > FSize then
      Raise EErrorFmt(5509, [53]);

    SetLength(Names, Size);
    F.ReadBuffer(Names[1], Size);  { read the names }

    if ExtraSize=0 then
      ExtraSizes:=''
    else
    begin
      { read the extra sizes }
      Dec(FSize, ExtraSize);
      I:=FSize-DeltaPos;  { stored at the end of the data block }

      if I<0 then
        Raise EErrorFmt(5509, [54]);

      F.Seek(I, soFromCurrent);
      SetLength(ExtraSizes, ExtraSize);
      J:=F.Read(ExtraSizes[1], ExtraSize);

      if J<ExtraSize then
      begin
        FileCrashRecoverHack;
        Hack:=True;
        FillChar(ExtraSizes[J+1], ExtraSize-J, $FF);
        Dec(I, ExtraSize-J);
      end;

      F.Seek(-ExtraSize-I, soFromCurrent);  { come back }
    end;

    Info:=FileItemInfo;
    NamePtr:=PChar(Names);
    ExtraSize:=1;

    { read actual data }
    for I:=1 to FileItemCount do
    begin
      SetString(Name, NamePtr, Info^.NameSize);  { name }
      Inc(NamePtr, Info^.NameSize);
      Size:=Info^.Code and qsSizeMask;

      if Size > qsShortSizeMax then
      begin
        { long sizes are stored in ExtraSizes }
        Size:=0;
        J:=Info^.Code and qsLongSizeMask;

        if J > SizeOf(Size) then
          Raise EErrorFmt(5509, [55]);

        Move(ExtraSizes[ExtraSize], Size, J);
        Inc(ExtraSize, J);
      end;

      Inc(DeltaPos, Size);
      if DeltaPos > FSize then    {Raise EErrorFmt(5509, [56])}
      begin
        if not Hack then
        begin
          FileCrashRecoverHack;
          Hack:=True;
        end;

        Size:=FSize-(DeltaPos-Size);
        DeltaPos:=FSize;
      end;

      if Info^.Code and qsBitSubObject = 0 then
      begin
        { a Specific/Arg pair }
        SetLength(Name, Info^.NameSize+1+Size);
        Name[Info^.NameSize+1]:='=';
        F.ReadBuffer(Name[Info^.NameSize+2], Size);
        Specifics.Add(Name);
      end
      else
      begin
        if Info^.Code <> qsCodeFileLink then
        begin
          { a sub-objects }
          Q:=ConstructQObject(Name, Self);
          FSubElements.Add(Q);
          LoadedItem(rf_Private, F, Q, Size);
        end
        else
          LoadedFileLink(Name, 0);
      end;

      Inc(Info);
    end;
  finally
    FreeMem(FileItemInfo);
  end;
end;

function QObject.DirectDataAccess(var S: TStream; var Size: Integer) : Boolean;
var
  F: TStream;
  ReadFormat, Taille, FileItemCount, ExtraSize, DeltaPos, J: Integer;
  Info: TFileItemInfo;
  Info1: TFileObjectClassInfo;
  Name: String;
begin
  {$IFDEF Debug}
  if Flags and ofNotLoadedToMemory = 0 then
    Raise InternalE('DirectDataAccess');
  {$ENDIF}

  Result:=False;
  Taille:=QStreamAddRef(FNode, F);
  try
    if Self is QFileObject then
      ReadFormat:=QFileObject(Self).ReadFormat
    else
      ReadFormat:=rf_Private;

    if ReadFormat>=rf_Default then
    begin
      { direct raw access allowed only for objects of type Unknown }
      QFileObject(Self).FileObjectClassInfo(Info1);

      if not Info1.Unformatted then
        Exit;

      Size:=Taille;
    end
    else
    begin
      { special code : try to access the content of the Data= specific
        directly from the stream }
      if ReadFormat<>rf_Private then
        Exit;

      FileItemCount:=0;
      try
        F.ReadBuffer(FileItemCount, 1);
      except
        Exit;
      end;

      if FileItemCount <> 1 then
        Exit;

      DeltaPos:=1+SizeOf(TFileItemInfo);
      if DeltaPos > Taille then
        Exit;

      F.ReadBuffer(Info, SizeOf(TFileItemInfo));  { read single file item info }
      if Info.Code and qsBitSubObject <> 0 then
        Exit;

      {Size:=Info.NameSize;}
      ExtraSize:=0;
      if (Info.Code and qsSizeMask) > qsShortSizeMax then
        ExtraSize:=Info.Code and qsLongSizeMask;

      Inc(DeltaPos, {Size}Info.NameSize);
      if DeltaPos > Taille then
        Exit;

      SetLength(Name, {Size}Info.NameSize);
      F.ReadBuffer(Name[1], {Size}Info.NameSize);  { read the single name }
      if Name<>'Data' then
        Exit;

      if ExtraSize=0 then
        Size:=Info.Code and qsSizeMask
      else
      begin
        { read the extra size }
        Dec(Taille, ExtraSize);
        J:=Taille-DeltaPos;  { stored at the end of the data block }
        if J<0 then
          Exit;

        F.Seek(J, soFromCurrent);
        Size:=0;
        if ExtraSize > SizeOf(Size) then
          Exit;

        if F.Read(Size, ExtraSize) < ExtraSize then
          Exit;

        F.Seek(-ExtraSize-J, soFromCurrent);  { come back }
      end;

      Inc(DeltaPos, Size);
      if DeltaPos > Taille then
        Exit;
    end;

    { everything went right; the data is now at the current position in F. }
    S:=F;
    F:=Nil;
    Result:=True;
  finally
    ReleaseStream(F);
  end;
end;

procedure QObject.LoadedFileLink(const nName: String; ErrorMsg: Integer);
var
  Q: QFileObject;
begin
  try
    Q:=BindFileQObject(nName, Self, False);
    Q.AddRef(+1);
    try
      if Q.FParent<>Self then
      begin
        GlobalWarning(FmtLoadStr1(5225, [nName]));
        Modified;
      end
      else
      begin
        Q.Flags:=Q.Flags or ofWarnBeforeChange;
        SubElements.Add(Q);
      end;
    finally
      Q.AddRef(-1);
    end;
  except
    on EQObjectFileNotFound do  { file link target not found }
    begin
      if ErrorMsg=0 then
        ErrorMsg:=5222;
      GlobalWarning(FmtLoadStr1(ErrorMsg, [nName]));
      Modified;
    end;
  end;
end;

procedure LoadedItem(Format: Integer; F: TStream; Q: QObject; Size: Integer{; Delayed: Boolean});
var
  nEnd: Integer;
begin
  if Q is QFileObject then
    QFileObject(Q).ReadFormat:=Format
  else
  begin
    if Format<>rf_Private then
      Raise InternalE('LoadedItem '+Q.GetFullName+' '+IntToStr(Format));
  end;

  nEnd:=F.Position+Size;

  if (F is TQStream) and not TQStream(F).DisableDelayLoading then
    Q.Open(TQStream(F), Size)
  else
  begin
    Q.FLoading:=True;
    try
      Q.LoadFile(F, Size);
    finally
      Q.FLoading:=False;
    end;
  end;

  Q.FixupReference;
  F.Position:=nEnd;
end;

procedure QObject.Open(F: TQStream; Taille: Integer);
begin
  FNode:=F.AddRefNode(Taille);
  FFlags:=FFlags or ofNotLoadedToMemory;
end;

function RequiredBytesToContainValue(T: Integer) : Integer;
begin
  if T<0 then
    Result:=SizeOf(T) { $FFFFFFFF - $80000000 }
  else
  if T<$100 then
    Result:=1  { $00 - $FF }
  else
  if T<$10000 then
    Result:=2  { $0000 - $FFFF }
  else
  if T<$1000000 then
    Result:=3  { $000000 - $FFFFFF }
  else
    Result:=4; { $00000000 - $7FFFFFFF }
end;

function PackedStrToInt(const S: String) : Integer;
var
  ByteSize: Integer;
begin
  ByteSize:=Length(S);
  if ByteSize>SizeOf(Result) then
    ByteSize:=SizeOf(Result);
  Result:=0;
  if Length(S)>0 then
    Move(S[1], Result, ByteSize);
end;

function IntToPackedStr(Value: Integer) : String;
var
  ByteSize: Integer;
begin
  ByteSize:=RequiredBytesToContainValue(Value);
  SetLength(Result, ByteSize);
  Move(Value, Result[1], ByteSize);
end;


procedure QObject.SaveFile1(Info: TInfoEnreg1);
var
  CheckFormat: Integer;
begin
  if Info.Format <> rf_Siblings then
  begin
    if Self is QFileObject then
      CheckFormat:=QFileObject(Self).ReadFormat
    else
      CheckFormat:=rf_Private;

    if (CheckFormat=Info.Format) and Copying(Info.F, Info.TransfertSource) then
      Exit;  { directly copied }

    Acces;
  end;

  SaveFile(Info);  { otherwise, save normally }
end;

procedure QObject.FinalizeFromText;
begin
end;

function QObject.WriteSubElements;
begin
  Result:=true;
end;

function QObject.IsAllowedParent(Parent: QObject) : Boolean;
begin
  Result:=true;
end;

function QObject.TreeViewColorBoxes : TColorBoxList;
begin
  Result:=nil;
end;

procedure QObject.SaveFile(Info: TInfoEnreg1);
var
  Origin, I, J, FileItemCount, Size, Size2: Integer;
  FileItemInfo, ItemInfo: PFileItemInfo;
  S, S1, Names, ExtraSizes: String;
  Q: QObject;
begin
  with Info do
  begin
    if Format<>rf_Private then
    begin
      if Format=rf_Siblings then
        Exit;

      Raise InternalE('SaveFile '+GetFullName+' '+IntToStr(Format));
    end;

   {BuildReferences;}
    ProgressIndicatorStart(5442, SubElements.Count+1);
    try
      if WriteSubElements then
        FileItemCount:=Specifics.Count + SubElements.Count
      else
        FileItemCount:=Specifics.Count;

      if FileItemCount > qsShortSizeMax then
      begin
        Size:=RequiredBytesToContainValue(FileItemCount);
        J:=qsLongSize or Size;
        F.WriteBuffer(J, 1);
        F.WriteBuffer(FileItemCount, Size);   { long size }
      end
      else
      begin
        F.WriteBuffer(FileItemCount, 1);   { short size }
        if FileItemCount=0 then
          Exit;      { quit if no data }
      end;

      Size:=FileItemCount * SizeOf(TFileItemInfo);
      GetMem(FileItemInfo, Size);
      try
        { the FileItemInfo list is built later }
        FileItemInfo^.Code:=$FF;  { invalid }
        Origin:=F.Position;
        F.WriteBuffer(FileItemInfo^, Size);

        Names:='';
        ItemInfo:=FileItemInfo;
        for I:=0 to Specifics.Count-1 do  { write the Names }
        begin
          S:=Specifics[I];
          J:=Pos('=',S)-1;
          if J<0 then
            Raise InternalE('Specifics='+Copy(S,1,25));
          ItemInfo^.NameSize:=J;
          Names:=Names + Copy(S, 1, J);
          Inc(ItemInfo);
        end;
        if WriteSubElements then
        begin
          for I:=0 to SubElements.Count-1 do
          begin
            Q:=SubElements[I];
            S:=Q.GetFullName;
            J:=Length(S);
            ItemInfo^.NameSize:=J;
            Names:=Names + S;
            Inc(ItemInfo);
          end;
        end;

        if Length(Names)>0 then
          F.WriteBuffer(Names[1], Length(Names));

        ExtraSizes:='';
        ItemInfo:=FileItemInfo;
        for I:=0 to Specifics.Count-1 do  { write the actual data }
        begin
          S:=Specifics[I];
          J:=ItemInfo^.NameSize+2;
          Size:=Length(S)-J+1;
          F.WriteBuffer(S[J], Size);
          if Size > qsShortSizeMax then
          begin
            Size2:=RequiredBytesToContainValue(Size);
            ItemInfo^.Code:=qsLongSize or Size2;
            SetString(S1, PChar(@Size), Size2);
            ExtraSizes:=ExtraSizes+S1;
          end
          else
            ItemInfo^.Code:=Size;

          Inc(ItemInfo);
        end;

        ProgressIndicatorIncrement;
        if WriteSubElements then
        begin
          for I:=0 to SubElements.Count-1 do
          begin
            Q:=SubElements[I];
            if Q.Flags and ofFileLink <> 0 then
            begin
              if Q.Flags and ofModified <> 0 then
                (Q as QFileObject).TrySavingNow;

              ItemInfo^.Code:=qsCodeFileLink;
            end
            else
            begin
              Size:=F.Position;
              Q.SaveFile1(Info);
              Size:=F.Position - Size;
              if Size > qsShortSizeMax then
              begin
                Size2:=RequiredBytesToContainValue(Size);
                ItemInfo^.Code:=(qsBitSubObject or qsLongSize) or Size2;
                SetString(S1, PChar(@Size), Size2);
                ExtraSizes:=ExtraSizes+S1;
              end
              else
                ItemInfo^.Code:=qsBitSubObject or Size;
            end;
            Inc(ItemInfo);
            ProgressIndicatorIncrement;
          end;
        end;

        if Length(ExtraSizes)>0 then
          F.WriteBuffer(ExtraSizes[1], Length(ExtraSizes));

        Size:=F.Position;
        F.Position:=Origin;
        F.WriteBuffer(FileItemInfo^, FileItemCount * SizeOf(TFileItemInfo));
        F.Position:=Size;
      finally
        FreeMem(FileItemInfo);
      end;
    finally
      ProgressIndicatorStop;
    end;
  end;
end;

procedure QObject.ReadUnformatted(F: TStream; Size: Integer);
const
  cSpec1 = 'Data=';
var
  S: String;
begin
  { read as unformatted data }
  S:=cSpec1;
  SetLength(S, Length(cSpec1)+Size);
  F.ReadBuffer(S[Length(cSpec1)+1], Size);
  Specifics.Add(S);
end;

procedure QObject.SaveUnformatted(F: TStream);
const
  cStart = Length('Data=X');
var
  S: String;
begin
  { write as unformatted data }
  S:=GetSpecArg('Data');
  F.WriteBuffer(S[cStart], Length(S)-cStart+1);
end;

procedure TInfoEnreg1.WriteSibling(const Path: String; Obj: QObject);
begin
  Raise InternalE('cannot store the sibling file '+Path);
end;

destructor TInfoEnreg1.Destroy;
begin
  TempObject.AddRef(-1);
  inherited;
end;

(*procedure QObject.SetTextsSpec(const Name: String; L: TStrings);
var
 S, S1: String;
 I, Taille: Integer;
 P: PChar;
begin
 Taille:=0;
 for I:=0 to L.Count-1 do
  Inc(Taille, Length(L[I])+1);
 if Taille=0 then
  S:=''
 else
  begin
   SetLength(S, Taille-1);
   P:=PChar(S);
   for I:=0 to L.Count-1 do
    begin
     S1:=L[I];
     if S1<>'' then
      begin
       Move(S1[1], P^, Length(S1));
       Inc(P, Length(S1));
      end;
     P^:=#$0D;
     Inc(P);
    end;
   Dec(P);
   P^:=#0;
  end;
 Specifics.Values[Name]:=S;
end;*)

 {------------------------}

(*function QObject.AjouterElement(Items: TTreeNodes; nParent, nInsert: TTreeNode) : TTreeNode;
begin
 if FFlags and ofTvNode <> 0 then
  begin
   Result:=Nil;
   Exit;
  end;
 if nInsert=Nil then
  Result:=Items.AddChildObject(nParent, Name, Self)
 else
  Result:=Items.InsertObject(nInsert, Name, Self);
 SetNode(Result, nParent);
end;

procedure QObject.SetNode(nNode, ParentNode: TTreeNode);
begin
 {$IFDEF Debug}
 if FFlags and ofNotLoadedToMemory <> 0 then Raise InternalE('SetNode');
 {$ENDIF}
 FNode:=nNode;
 FFlags:=(FFlags or (ofTreeViewSubElement or ofTvNode)) xor Ord(ParentNode=Nil);
 if FFlags and (ofFileLink or ofTreeViewSubElement) = ofFileLink or ofTreeViewSubElement then
  FNode.OverlayIndex:=0;
end;

procedure QObject.NodeDeletion;
begin
 FNode:=Nil;
 Flags:=Flags and not ofTvNode;
end;*)

function QObject.GetTvParent : QObject;
begin
 {if FFlags and (ofTreeViewSubElement or ofTvNode) = ofTvNode then}
 {if not Odd(FFlags) then}
  if not ((FFlags and ofTreeViewSubElement) <> 0) then {Decker - better clarify what the hell we're testing FFlags for!}
    Result:=Nil    { because it is a root in an Explorer }
  else
    Result:=FParent;
end;

procedure QObject.SetTvParent(nParent: QObject);
begin
  if nParent=Nil then
    FFlags:=FFlags and not ofTreeViewSubElement
  else
    FFlags:=FFlags or ofTreeViewSubElement;

  FParent:=nParent;
end;

function QObject.Ancestry(): String;
{ (Comment by Decker 2001-02-23)
 Recursive function which builds a formatted string containing the ancenstry
 of this object. Primarily used for exporting .MAP files.
}
var
  Parent:QObject;
  I: Integer;
  Idx: String;
  Sue: TQlist;
begin
  Parent:=GetTvParent();
  if Parent<>Nil then
  begin
    Sue:=Parent.SubElements;
    for I:=0 to Sue.Count-1 do
    begin
      if Sue[I]=self then
      begin
        Idx:=IntToStr(I+1);
        break;
      end;
    end;

    if Parent.GetTvParent()<>Nil then {don't bother with worldspawn }
      Result:=Parent.Ancestry()+' -> '+Name+TypeInfo()+'['+Idx+']'
    else
      Result:=Name+TypeInfo()+'['+Idx+']';
  end
  else
    Result:=Name;
end;

function QObject.ClassList : TStringList;
var
  Cls: QObjectClass;
begin
  Result:=TStringList.Create;
  Cls:=QObjectClass(ClassType);
  while Cls<>QObject do
  begin
    Result.Append(Cls.ClassName);
    Cls:=QObjectClass(Cls.ClassParent);
  end;
end;

procedure QObject.SetSelMult();
  procedure OpenGroups(Groupe: QObject);
  begin
    if Groupe<>Nil then
    begin
      OpenGroups(Groupe.TvParent);
     {if Groupe.Flags and ofTvNode <> 0 then
        Groupe.GetNode.Expand(False);}
      Groupe.SelMult:=Groupe.SelMult and not (smSousSelVide or smSpecial);
    end;
  end;
begin
  OpenGroups(TvParent);
  SelMult:=SelMult or smSel;
end;

procedure QObject.ToggleSelMult();
begin
 {if Odd(SelMult) then}
  if ((SelMult and smSel) <> 0) then {Decker - better clarify what the hell we're testing SelMult for!}
    SelMult:=smNonSel
  else
    SetSelMult;
end;

function QObject.GetSelUnique() : Boolean;
var
  Q: QObject;
begin
 {if Odd(SelMult) then}
  if ((SelMult and smSel) <> 0) then {Decker - better clarify what the hell we're testing SelMult for!}
  begin
    Q:=TvParent;
    while (Q<>Nil) and not ({Odd(Q.SelMult)}(Q.SelMult and smSel) <> 0) do {Decker - better clarify what the hell we're testing Q.SelMult for!}
      Q:=Q.TvParent;
    Result:=Q=Nil;
  end
  else
    Result:=False;
end;

procedure QObject.SetSelUnique(Value: Boolean);
  procedure DeselectAllParents(aParent: QObject);
  begin
    if aParent<>Nil then
    begin
      DeselectAllParents(aParent.TvParent);
     {if aParent.Flags and ofTvNode <> 0 then
        aParent.GetNode.Expand(False);}
      aParent.SelMult:=smNonSel;
    end;
  end;
begin
  if Value then
  begin
    DeselectAllParents(TvParent);
    SelMult:=SelMult or smSel;
  end
  else
    SelMult:=smNonSel;
end;

class function QObject.ShowInListSel: Boolean;
begin
  Result:=True;
end;

function QObject.NextInGroup(): QObject;
{ (Comment by Decker 2001-02-23)
 Returns the next sibling in the same group as 'self'.
}
var
  I: Integer;
begin
  I:=FParent.SubElements.IndexOf(Self);
  if (I >= 0) and (I+1 < FParent.SubElements.Count) then
    Result:=FParent.SubElements.Items[I+1]
  else
    Result:=Nil;
end;

procedure QObject.CopyAllData(Source: QObject; CopySel: Boolean);
var
  I: Integer;
begin
  Specifics.Clear;
  SubElements.Clear;

  for I:=0 to Source.Specifics.Count-1 do
    Specifics.Add(Source.Specifics[I]);

  SubElements.Capacity:=Source.SubElements.Count;
  for I:=0 to Source.SubElements.Count-1 do
    SubElements.Add(Source.SubElements[I].Clone(Self, CopySel));
end;

function QObject.Clone(nParent: QObject; CopySel: Boolean) : QObject;
begin
  Result:=QObjectClass(ClassType).Create(Name, nParent);
  try
   {if (FFlags and ofNotLoadedToMemory <> 0) and not QStreamCanClone(Self, FNode) then
      Acces;   { cannot use QStreamClone }
    Result.Flags:=(FFlags and ofCloneFlags) or ofModified;
    if CopySel then
      Result.SelMult:=FSelMult;
   {Result.Flags2:=FFlags2 and ofCloneFlags2;}
    if Result is QFileObject then
      QFileObject(Result).ReadFormat:=QFileObject(Self).ReadFormat;
    if FFlags and ofNotLoadedToMemory = 0 then
      Result.CopyAllData(Self, CopySel)  { data already loaded }
    else
    begin
     {$IFDEF Debug}
     {if FFlags and ofTvNode <> 0 then
        Raise InternalE('Clone');}
     {$ENDIF}
      Result.FNode:=QStreamClone(FNode);
    end;
    Result.FixupReference;
  except
    Result.Free;
    Raise;
  end;
end;

function QObject.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
  Result:=ieResult[False];  { by default }
end;

function QObject.GetIntSpec(const Name: String) : Integer;
begin
  Result:=PackedStrToInt(Specifics.Values[Name]);
end;

procedure QObject.SetIntSpec(const Name: String; Value: Integer);
begin
  Specifics.Values[Name]:=IntToPackedStr(Value);
end;

function QObject.GetVectSpec(const Name: String) : TVect;
{ (Comment by Decker 2001-02-23)
 Retrives a specific's arg-data of 3 Singles, and convert them into
 a vector.
}
var
  V: array[0..2] of Single;
begin
  if GetFloatsSpec(Name, V) then
  begin
    Result.X:=V[0];
    Result.Y:=V[1];
    Result.Z:=V[2];
  end
  else
    Result:=OriginVectorZero;
end;

procedure QObject.SetVectSpec(const Name: String; const Value: TVect);
{ (Comment by Decker 2001-02-23)
 Takes a specific-name and a vector (3 values), converts them into 'Single'
 values, and then stores the specific and 3 Singles.
}
var
  V: array[0..2] of Single;
begin
  V[0]:=Value.X;
  V[1]:=Value.Y;
  V[2]:=Value.Z;
  SetFloatsSpec(Name, V);
end;

(*function IntSpecNameOf(const Name: String) : String;
{ (Comment by Decker 2001-02-23)
 Creates a specific-name that identifies it to contain arg(s)-data of raw
 binary Integer-values!
 (Decker - Personally I find this way, of telling what type of data there is
  in the arg(s), very dangerous. It should be the data itself which told what
  type it is.)
}
begin
  Result:=Name;
  if Length(Result) > 1 then
    Result[2]:=Chr(Ord(Result[2]) or chrFloatSpec);
end;*)

function FloatSpecNameOf(const Name: String) : String;
{ (Comment by Decker 2001-02-23)
 Creates a specific-name that identifies it to contain arg(s)-data of raw
 binary Single-values!
 (Decker - Personally I find this way, of telling what type of data there is
  in the arg(s), very dangerous. It should be the data itself which told what
  type it is.)
}
begin
  Result:=Name;
  Result[1]:=Chr(Ord(Result[1]) or chrFloatSpec);
end;

procedure CheckValidSpec(var Spec: String);
{ (Comment by Decker 2001-02-23)
 This procedure checks to see that the specific-name conforms to the
 character restrictions within QuArK. For instance, any firstsymbol character-
 value from #128 to #255 are considered a 'FloatSpec', meaning the arg(s)-data
 contain raw binary Single-values!
}
const
  ValidSymbols = [' '..#127] - ['=', '{', '}'];   { NEW : ' ' is authorized inside the Specifics }
  FirstSymbol  = ValidSymbols - ['"', '$', '''', '|', '@'];
var
  I: Integer;
begin
  Spec:=Trim(Spec);

  if Spec='' then
    Raise EError(5563);

  if not (Spec[1] in FirstSymbol) then
    Raise EErrorFmt(5564, [Spec[1]]);

  for I:=2 to Length(Spec) do
  begin
    if not (Spec[I] in ValidSymbols) then
      Raise EErrorFmt(5565, [Spec[I]])
    else
    if (Spec[I-1]='/') and (Spec[I]='/') then
      Raise EErrorFmt(5565, ['//']);
  end;
end;

function StringListConcatWithSeparator(theStringList: TStrings; theStringSeparator: Byte) : String;
{ (Comment by Decker 2001-02-23)
 Takes 'theStringList' and concatenates it into one huge string, where each
 item from 'StringList' is separated by a 'theStringSeparator'.
 Example: "item1;item2;item3;item4" (separator=';')
}
var
  S: String;
  I, ResultingStringLength: Integer;
  P: PChar;
begin
  ResultingStringLength:=0;

  { compute the maximum length of returning string }
  for I:=0 to theStringList.Count-1 do
    Inc(ResultingStringLength, Length(theStringList[I])+1);

  { if zero (or only a lonely theStringSeparator), return empty string }
  if ResultingStringLength<=1 then
    Result:=''
  else
  begin
    SetLength(Result, ResultingStringLength-1); { do not store the last StringSeparator }
    P:=PChar(Result);
    { concatenate strings from stringlist into returning string,
      and put a seperator between each. }
    for I:=0 to theStringList.Count-1 do
    begin
      S:=theStringList[I];

      if S<>'' then
      begin
        Move(S[1], P^, Length(S));
        Inc(P, Length(S));
      end;

      P^:=Chr(theStringSeparator);
      Inc(P);
    end;

    Dec(P); { do not store the last StringSeparator }
    P^:=#0;
  end;
end;

function QObject.GetFloatSpec(const Name: String; const Default: Single) : Single;
{ (Comment by Decker 2001-02-23)
 Reads a lonely 'Single' data from the specific in question. If the data isn't
 of length 4, then it is not a 'Single' and therefore the 'Default' value will
 be returned instead.
}
var
  S: String;
begin
  S:=Specifics.Values[FloatSpecNameOf(Name)];
  if Length(S) = 4 then   { SizeOf(Single) }
    Move(S[1], Result, 4)   { SizeOf(Single) }
  else
    Result:=Default;
end;

procedure QObject.SetFloatSpec(const Name: String; const Value: Single);
{ (Comment by Decker 2001-02-23)
 Writes a lonely 'Single' value to a specific, and marks that specific's
 name as containing float/single data.
}
var
  S: String;
begin
  SetLength(S, 4);   { SizeOf(Single) }
  Move(Value, S[1], 4);   { SizeOf(Single) }
  Specifics.Values[FloatSpecNameOf(Name)]:=S;
end;

(*function QObject.GetIntsSpec(const Name: String; var I: array of Integer) : Boolean;
{ (Comment by Decker 2001-02-23)
 If the specific in question, contains the correct number of 'Integer' values
 as the 'I' array have space for, they will be copied into the 'I' array and
 True will be returned. If not, nothing is copied and False is returned.
}
var
  S: String;
begin
  S:=Specifics.Values[IntSpecNameOf(Name)];
  Result:=Length(S) = Succ(High(I))*4;   { SizeOf(Integer) }
  if Result then
    Move(S[1], I[0], Length(S));
end;*)

function QObject.GetFloatsSpec(const Name: String; var F: array of Single) : Boolean;
{ (Comment by Decker 2001-02-23)
 If the specific in question, contains the correct number of 'Single' values
 as the 'F' array have space for, they will be copied into the 'F' array and
 True will be returned. If not, nothing is copied and False is returned.
}
var
  S: String;
begin
  S:=Specifics.Values[FloatSpecNameOf(Name)];
  Result:=Length(S) = Succ(High(F))*4;   { SizeOf(Single) }
  if Result then
    Move(S[1], F[0], Length(S));
end;

function QObject.GetFloatsSpecPartial(const Name: String; var F: array of Single) : Integer;
{ (Comment by Decker 2001-02-23)
 The specific in question, will have its data read, and the 'length div 4' of
 these data tells how many 'single' values it contains. If the 'F' array of
 singles do not have space for all the data-values, only those that fit into the
 array are copied. The number of 'Single' values copied are returned, or 0 if
 no values are copied.
}
var
  S: String;
begin
  S:=Specifics.Values[FloatSpecNameOf(Name)];
  Result:=Length(S) div 4;   { SizeOf(Single) }
  if Result>0 then
  begin
    if Result>Succ(High(F)) then
      Result:=Succ(High(F));
    (*DECKER
    Move(S[1], F[0], Length(S)); {FIXME - shouldn't 'Length(S)' be 'Result*4' so we don't write beyond the end of the array}
    *)
    Move(S[1], F[0], Result * 4); { SizeOf(Single) }
    (*/DECKER*)
  end;
end;

procedure QObject.SetFloatsSpec(const Name: String; const F: array of Single);
{ (Comment by Decker 2001-02-23)
 Writes an array of 'Single' values to a specific, and marks that specific's
 name as containing float/single data.
}
var
  S: String;
begin
  SetLength(S, Succ(High(F))*4);   { SizeOf(Single) }
  Move(F[0], S[1], Length(S));
  Specifics.Values[FloatSpecNameOf(Name)]:=S;
end;

function QObject.GetSpecArg(const Name: String) : String;
{ (Comment by Decker 2001-02-23)
 Returns the concatenated string; "specific=args", for the specific-name in
 question. If the specific-name can not be found, the string "specific=" is
 returned.
}
var
  I: Integer;
begin
  I:=Specifics.IndexOfName(Name);

  if I<0 then
    Result:=Name+'='
  else
    Result:=Specifics[I];
end;

function QObject.GetArg(const Name: String) : String;
{ (Comment by Decker 2001-02-23)
 Returns the string; "args", for the specific-name in question. If the
 specific-name can not be found, an empty string is returned.
}
var
  I: Integer;
  S: String;
begin
  S:=GetSpecArg(Name);
  I:=Pos('=', S);
  Result:=Copy(S, I+1, MaxInt);
end;

function QObject.FindSubObject(const nName: String; WantClass, BrowseClass: QObjectClass) : QObject;
var
  I: Integer;
  Browse: Boolean;
begin
  Acces;

  for I:=0 to SubElements.Count-1 do
  begin
    Result:=QObject(SubElements[I]);

    if (Result is WantClass) and (CompareText(Result.Name, nName) = 0) then
      Exit;  { found it }

    if BrowseClass=Nil then
      Browse:=ieDisplay in IsExplorerItem(Result)
    else
      Browse:=Result is BrowseClass;

    if Browse then
    begin
      Result:=Result.FindSubObject(nName, WantClass, BrowseClass);
      if Result<>Nil then
        Exit;
    end;
  end;

 Result:=Nil;
end;

function QObject.LocateSubElement(const LocName : String; var Index : Integer) : QObject;
{ find subelement by name if present, setting Index to lexical
  order insertion position (CompareText), otherwise return Nil,
  with Index set to appropriate lexical insertion position }
var
 I, C: Integer;
begin
  Result:=Nil;

  if Self=Nil then
  begin
    Index:=0;
    Exit;
  end;

  Acces;
  { in case of alph. order fault }
  if (Index >=SubElements.Count) or (SubElements.Count>0) and (CompareText(LocName, SubElements[Index].Name)<0) then
    Index:=0;

  for I:=Index to SubElements.Count-1 do
  begin
    C:=CompareText(LocName, SubElements[I].Name);
    if C=0 then
    begin
      Result:=SubElements[I];
      Index:=I;
      Exit;
    end;

    if C<0 then { item would be inserted before here}
    begin
      Index:=I;
      Exit;
    end;
  end;

  Index:=SubElements.Count; { item would go onto end }
end;

procedure QObject.FindAllSubObjects(const nName: String; WantClass, BrowseClass: QObjectClass; L: TQList);
var
  I: Integer;
  Browse: Boolean;
  Result: QObject;
begin
  Acces;
  for I:=0 to SubElements.Count-1 do
  begin
    Result:=QObject(SubElements[I]);

    if (Result is WantClass) and ((nName='') or (CompareText(Result.Name, nName) = 0)) then
      L.Add(Result);  { found it }

    if BrowseClass=Nil then
      Browse:=ieDisplay in IsExplorerItem(Result)
    else
      Browse:=Result is BrowseClass;

    if Browse then
      Result.FindAllSubObjects(nName, WantClass, BrowseClass, L);
  end;
end;

function QObject.TopLevel : Boolean;
begin
 {TopLevel:=FFlags and (ofTreeViewSubElement or ofTvNode) = ofTvNode;}
  TopLevel:=not Odd(FFlags); { check for 'not ofTreeViewSubElement' }
end;

procedure QObject.OperationInScene(Aj: TAjScene; PosRel: Integer);
var
  I: Integer;
  T{, T2}: QObject;
 {nInsert: TTreeNode;}
begin
  case Aj of
  asRetire:
    for I:=0 to SubElements.Count-1 do
      SubElements[I].OperationInScene(Aj, PosRel+1);
  end;

  if PosRel=0 then
  begin
    case Aj of
    asDeplace1,
    asRetire:
      if g_WorkingExplorer<>Nil then
        g_WorkingExplorer.ContentsChanged(True);
    end;
  end;

  case Aj of
  asAjoute,
  asDeplace2:
    for I:=0 to SubElements.Count-1 do
      SubElements[I].OperationInScene(Aj, PosRel+1);

  asModifieParent:
    if g_WorkingExplorer<>Nil then
    begin
      for I:=0 to SubElements.Count-1 do
        SubElements[I].OperationInScene(asModifieFrere, MaxInt);
      if PosRel = -1 then
        g_WorkingExplorer.ControlerEtatNoeud(Self);
    end;

  asRetireEnfant:
    if g_WorkingExplorer<>Nil then
      g_WorkingExplorer.ControlerEtatNoeud(Self);
  end;

  if (PosRel=0) and (g_WorkingExplorer<>Nil) then
  begin
    case Aj of
    asModifie:
      begin
        if TopLevel then  { the Root changed }
          g_WorkingExplorer.RootChanging(Self);
       {if Flags and ofTvNode <> 0 then
          GetNode.Text:=Name;}
        g_WorkingExplorer.ContentsChanged(True);
      end;

    asAjoute,
    asDeplace2:
      if g_WorkingExplorer<>Nil then
      begin
        if Flags and ofTreeViewSubElement = 0 then
        begin  { a root was deleted and a new version was reinserted }
          g_WorkingExplorer.AjouterElement(Self{, Nil, Nil});
         {if Flags and ofTvNode<>0 then
            g_WorkingExplorer.SetItemBold(GetNode);}
        end
        else
        begin
          T:=TvParent;
          if (T<>Nil) {and (T.Flags and ofTvNode<>0)}
          and (ieDisplay in T.IsExplorerItem(Self)) then
            g_WorkingExplorer.AjouterElement(Self{, T.GetNode, nInsert});
        end;
      end;
    end;
  end;
end;

function QObject.GetObjectMenu(Control: TControl) : TPopupMenu;
begin
  Result:=g_Form1.GetObjMenu(Control, False);
end;

procedure QObject.ClearAllSelection;
var
  I: Integer;
begin
  for I:=0 to SubElements.Count-1 do
    SubElements[I].ClearAllSelection;
  FSelMult:=smSousSelVide;
end;

function QObject.IsClosed: Boolean;
begin
  Result:=Flags and ofTreeViewExpanded = 0;
  if Result and (SubElements.Count = 0) then
  begin
    Flags:=Flags or ofTreeViewExpanded;
    Result:=False;
  end;
end;

(*procedure QObject.CallMenuCmd(Cmd: Integer);
begin
end;

procedure QObject.CallPopupMenu(const ScreenPoint: TPoint);
var
 Menu: HMenu;
begin
 Menu:=CreatePopupMenu; try
 if PopulateMenu(Menu) then
  begin
   g_PopupMenuObject:=Self;
   TrackPopupMenu(Menu, 0, ScreenPoint.X, ScreenPoint.Y, 0, g_Form1.Handle, Nil)
  end
 else
  MessageBeep(0);
 finally DestroyMenu(Menu); end;
end;*)

function QObject.PyGetAttr(attr: PChar) : PyObject;
var
  I, J, N: Integer;
  o: PyObject;
  S: String;
  PF: ^Single;
(*  PI: ^Integer;*)
  L: TStringList;
begin
  for I:=Low(PyObjMethodTable) to High(PyObjMethodTable) do
  begin
    if StrComp(attr, PyObjMethodTable[I].ml_name) = 0 then
    begin
      Result:=PyCFunction_New(PyObjMethodTable[I], @PythonObj);
      Exit;
    end;
  end;

  case attr[0] of
  'c':
    if StrComp(attr, 'classes') = 0 then
    begin
      L:=ClassList;
      try
        Result:=PyTuple_New(L.Count);
        for I:=0 to L.Count-1 do
        begin
          PyTuple_SetItem(Result, I, PyString_FromString(PChar(L[I])));
        end;
      finally
        L.Free;
      end;
      Exit;
    end;

  'd':
    if StrComp(attr, 'dictitems') = 0 then
    begin
      Acces;
      N:=SubElements.Count;
      Result:=PyDict_New;
      for I:=N-1 downto 0 do
      begin
        o:=@SubElements[I].PythonObj;
        PyDict_SetItemString(Result, PChar(SubElements[I].GetFullName), o);
      end;
      Exit;
    end
    else
    if StrComp(attr, 'dictspec') = 0 then
    begin
      Acces;
      Result:=PyDict_New;
      for I:=0 to Specifics.Count-1 do
      begin
        S:=Specifics[I];
        J:=Pos('=', S);
        if J>1 then
        begin
          S[J]:=#0;
          if Ord(S[1]) and chrFloatSpec = 0 then
          begin
(*            if (Length(S) > 1) and not (Ord(S[2]) and chrFloatSpec = 0) then
            begin
              N:=(Length(S)-J) div 4;    { SizeOf(Integer) }
              PChar(PI):=PChar(S)+J;
              o:=PyTuple_New(N);
              for J:=0 to N-1 do
              begin
                PyTuple_SetItem(o, J, PyInt_FromLong(PI^));
                Inc(PI);
              end;
              S[2]:=Chr(Ord(S[2]) and not chrFloatSpec);
            end
            else*)
              o:=PyString_FromStringAndSize(PChar(S)+J, Length(S)-J);
          end
          else
          begin
            N:=(Length(S)-J) div 4;    { SizeOf(Single) }
            PChar(PF):=PChar(S)+J;
            o:=PyTuple_New(N);
            for J:=0 to N-1 do
            begin
              PyTuple_SetItem(o, J, PyFloat_FromDouble(PF^));
              Inc(PF);
            end;
            S[1]:=Chr(Ord(S[1]) and not chrFloatSpec);
          end;
          PyDict_SetItemString(Result, PChar(S), o);
          Py_DECREF(o);
        end;
      end;
      Exit;
    end;

  'f':
    if StrComp(attr, 'flags') = 0 then
    begin
      IsClosed;
      Result:=PyInt_FromLong(Flags);
      Exit;
    end;

  'i':
    if StrComp(attr, 'itemcount') = 0 then
    begin
      Acces;
      Result:=PyInt_FromLong(SubElements.Count);
      Exit;
    end;

  'n':
    if StrComp(attr, 'name') = 0 then
    begin
      Result:=PyString_FromString(PChar(GetFullName));
      Exit;
    end;

  'p':
    if StrComp(attr, 'parent') = 0 then
    begin
      Result:=GetPyObj(FParent);
      Exit;
    end;

  's':
    if StrComp(attr, 'selected') = 0 then
    begin
      Result:=PyInt_FromLong(Ord(Odd(SelMult)));
      Exit;
    end
    else
    if StrComp(attr, 'shortname') = 0 then
    begin
      Result:=PyString_FromString(PChar(Name));
      Exit;
    end
    else
    if StrComp(attr, 'subitems') = 0 then
    begin
      Acces;
      N:=SubElements.Count;
      Result:=PyList_New(N);
      for I:=0 to N-1 do
      begin
        o:=@SubElements[I].PythonObj;
        Py_INCREF(o);
        PyList_SetItem(Result, I, o);
      end;
      Exit;
    end;

  't':
    if StrComp(attr, 'type') = 0 then
    begin
      Result:=PyString_FromString(PChar(TypeInfo));
      Exit;
    end
    else
    if StrComp(attr, 'treeparent') = 0 then
    begin
      Result:=GetPyObj(TvParent);
      Exit;
    end;
  end;

  Result:=Nil;
end;

function QObject.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  P: PChar;
begin
  Result:=False;
  case attr[0] of
  'f':
    if StrComp(attr, 'flags') = 0 then
    begin
      Flags:=PyInt_AsLong(value);
      Result:=True;
      Exit;
    end;

  's':
    if StrComp(attr, 'selected') = 0 then
    begin
      if PyObject_IsTrue(value) then
        SetSelMult
      else
      begin
        if Odd(SelMult) then
          SelMult:=smNonSel;
      end;
      Result:=True;
      Exit;
    end
    else
    if StrComp(attr, 'shortname') = 0 then
    begin
      P:=PyString_AsString(value);
      if P=Nil then
        Exit;
      Name:=P;
      Result:=True;
      Exit;
    end;
  end;
end;

procedure QObject.PySetParent(nParent: QObject);
begin
  if FPyNoParent then
    FPyNoParent:=False
  else
  begin
    if (FParent<>Nil) and (nParent<>Nil) then
      Raise EError(4422);
  end;

  FParent:=nParent;
end;

 {------------------------}

function TColorBoxList.GetSpecName(I: Integer) : String;
begin
  if (I<0) or (I>=Count) then
   Exit;

  Result:=nSpecName[I];
end;

procedure TColorBoxList.SetSpecName(I: Integer; const SpecName: String);
begin
  if (I<0) or (I>=Count) then
   Exit;

  nSpecName[I]:=SpecName;
end;

function TColorBoxList.GetColorType(I: Integer) : String;
begin
  if (I<0) or (I>=Count) then
   Exit;

  Result:=nColorType[I];
end;

procedure TColorBoxList.SetColorType(I: Integer; const ColorType: String);
begin
  if (I<0) or (I>=Count) then
   Exit;

  nColorType[I]:=ColorType;
end;

procedure TColorBoxList.Add(const SpecName, ColorType: String);
var
  nCount: Integer;
begin
  nCount:=Count+1;
  SetLength(nSpecName, nCount);
  nSpecName[nCount-1]:=SpecName;
  SetLength(nColorType, nCount);
  nColorType[nCount-1]:=ColorType;
end;

function TColorBoxList.Count : Integer;
begin
  Result:=Length(nSpecName);
end;

 {------------------------}

{$IFDEF Debug}
procedure DebugCheck;
begin
end;

{function DebugError: Exception;
begin
 DebugError:=Exception.Create('Raised DebugError !');
end;}

procedure DataDump;
const
  DataDumpFile = 'DataDump.txt';
var
  Text: TStringList;
  I: Integer;
  SomeQStream: TQStream;
  SomeQObject: QObject;
begin
  Text:=TStringList.Create;
  try
    Text.Add(QuArKVersion);
    Text.Add(HeavyMemDump);

    Text.Add('-----');

    Text.Add(Format('%5.5s  %s', ['RefCnt', 'Object']));
    for I:=0 to QFileList.Count-1 do
    begin
      SomeQStream := TQStream(QFileList.Objects[I]);
      Text.Add(Format('%5d  %s', [SomeQStream.RefCount1, QFileList[I]]));
    end;

    Text.Add('-----');

    Text.Add(Format('%5.5s  %2.2s  %s', ['RefCnt', 'Flags', 'Object']));
    for I:=0 to g_MemQObject.Count-1 do
    begin
      SomeQObject := g_MemQObject[I];
      if SomeQObject.PythonObj.ob_refcnt<>1 then
        Text.Add(Format('%5d  %2x  %s', [SomeQObject.PythonObj.ob_refcnt, SomeQObject.Flags, SomeQObject.GetFullName]))
      else
        Text.Add(Format('%5s  %2x  %s', [                             '', SomeQObject.Flags, SomeQObject.GetFullName]));
    end;

    Text.SaveToFile(ExtractFilePath(ParamStr(0))+DataDumpFile);
  finally
    Text.Free;
  end;
end;

procedure TestDataDump;
begin
  if (QFileList.Count>0) or (g_MemQObject.Count>0) then
    if MessageBox(0, 'Some objects were not correctly freed. This is a bug. Do you want to write a data report (DATADUMP.TXT) ?', 'DEBUGGING - BETA VERSION', mb_YesNo) = idYes then
      DataDump;
end;
{$ENDIF}

 {------------------------}

initialization
  QFileList:=TStringList.Create;
  QFileList.Sorted:=True;
  g_CF_QObjects:=RegisterClipboardFormat('QuArK Object');
  {$IFDEF Debug}
  g_DataDumpProc:=@TestDataDump;
  g_MemQObject:=TQList.Create;
  {$ENDIF}

finalization
  QFileList.Free;
  {$IFDEF Debug}
  g_MemQObject.Free;
  {$ENDIF}

end.

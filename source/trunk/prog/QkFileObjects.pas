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
Revision 1.58  2008/10/07 21:16:25  danielpharos
Massive update to get Steam finally working better.

Revision 1.57  2008/09/20 20:44:56  danielpharos
Don't go SearchPath when the file has already been found.

Revision 1.56  2008/09/06 15:57:29  danielpharos
Moved exception code into separate file.

Revision 1.55  2008/08/26 15:11:49  danielpharos
Added filename of broken qrk source file to error-message.

Revision 1.54  2008/08/09 19:40:23  danielpharos
Translated a function call

Revision 1.53  2008/08/09 19:33:54  danielpharos
Fix a possible reference counter mistake.

Revision 1.52  2008/08/07 15:23:07  danielpharos
Re-enable turned off error catchers

Revision 1.51  2008/08/07 15:15:07  danielpharos
Fixed wiNeverOpen not working when trying to open in a standalone window.

Revision 1.50  2008/07/25 19:36:06  danielpharos
Changed confusing parameter-name

Revision 1.49  2008/07/25 19:31:53  danielpharos
Added setting to disable AddToRecent in SaveObject

Revision 1.48  2008/07/24 15:02:38  danielpharos
Cleaned up texture name checking code and interface.

Revision 1.47  2008/05/27 07:42:31  cdunde
To allow Save File to work in the Model Editor and stop crashes,
 also to allow Used Skins in the Texture Browser and for them to be used.

Revision 1.46  2008/05/24 19:02:22  danielpharos
Moved a string to the dictionary

Revision 1.45  2008/05/24 18:52:46  danielpharos
Fix #10#10 being counted as only one line. This should fix the line-numbering being off.

Revision 1.44  2008/02/23 19:25:21  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.43  2007/12/06 22:58:00  danielpharos
Workaround for a weird memory leak.

Revision 1.42  2007/09/24 00:15:55  danielpharos
Made MaxRecentFiles a configurable option.

Revision 1.41  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.39  2005/07/04 18:53:20  alexander
changed steam acces to be a protocol steamaccess://

Revision 1.38  2004/01/05 21:57:39  silverpaladin
TrySavingNow was changed to display a warning if it fails rather than raising an error message.  Then if something like an MD3 is referenced in a qrk file, the rest of the qrk file can still be saved without erroring out.

Revision 1.37  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.36  2003/08/12 15:49:53  silverpaladin
Added ExtraFunctionality to the uses so that platform independant routines are available for pre-Delphi 6 versions.

Revision 1.35  2003/07/21 04:50:02  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.34  2002/04/04 11:46:10  tiglari
comment out the handle close because it might cause an error
 (decker on quark-python, april 4)

Revision 1.33  2002/04/02 21:05:20  tiglari
close event handle

Revision 1.32  2001/10/14 10:13:32  tiglari
Live Pointer Hunt: rollback to version of 2001-10-02

Revision 1.29  2001/06/14 18:54:46  decker_dk
Added a 'ChoiceList' parsing to ConstructObjsFromText() - See function comments for reason.

Revision 1.28  2001/06/05 18:39:10  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.27  2001/03/20 21:49:18  decker_dk
Updated copyright-header

Revision 1.26  2001/03/18 14:03:02  aiv
remove ! fix

Revision 1.25  2001/03/16 00:04:16  aiv
fixed saving of include commands (t_model = !)

Revision 1.24  2001/03/13 01:42:47  aiv
new fgd->qrk converter inbuilt

Revision 1.23  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.22  2001/01/30 19:11:34  decker_dk
Changed to GetApplicationPath().
Changed LienFichierQObject() so it now uses an QApplPaths-object, to ask for sub-directories to search in.

Revision 1.21  2001/01/23 08:06:54  tiglari
Infrastructure for OsFolders (read their contents from folders on disk,
 don't write them when writing the .qrk).

Revision 1.20  2001/01/21 15:48:25  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.19  2001/01/15 19:19:42  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.18  2000/11/25 20:51:33  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.17  2000/11/23 19:06:44  decker_dk
Removed one FindFirst in the ListFiles procedure.

Revision 1.16  2000/11/19 15:31:50  decker_dk
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

Revision 1.15  2000/09/01 00:11:18  alexander
merged in tiglaris .map extension fix from rel6_1 branch

Revision 1.14  2000/08/25 17:59:18  decker_dk
Ready for C/C++ escape-chars, though uncommented for the moment. This does cause slower loading of text-QRK files.

Revision 1.13  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.12  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.11  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.10  2000/05/21 13:11:51  decker_dk
Find new shaders and misc.
}

unit QkFileObjects;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, QkObjects, TB97, EnterEditCtrl, QkForm, ComCtrls, CommCtrl, Python;

const
 {MaxFileExt = 3;}

  fm_Save           = 1;
  fm_SaveAsFile     = 2;
  fm_SaveIfModif    = 3;
  fm_SaveTagOnly    = 4;

  rf_Siblings = -3;
  rf_AsText   = -2;
  rf_Private  = -1;  { QuArK's own private, "native" format }
  rf_NotSaved = 0;
  rf_Default  = 1;

  TagToDelete  = '~Qk';
  TagToDelete2 = 'SQk';
  TagAutoSave  = 'AQk';

type
  TFileObjectWndState = (cmNone, cmWindow, cmOwnExplorer);
  TFileObjectWndInfo  = set of (
                         wiOwnExplorer,   { when opening a file, adds its own Explorer }
                         wiWindow,        { can show in a stand-alone window }
                         wiMaximize,      { opens a window in maximized state by default }
                         wiForm1,         { when opening a file, opens it in g_Form1 }
                         wiNeverOpen,     { contains no interesting data (e.g. groups) }
                         wiSameExplorer); { opens in its parent's explorer (e.g. PAK folders) }

const
  StandAloneWndState = [cmWindow, cmOwnExplorer];

type
  QFileObject = class;
  TQForm1 = class(TQkForm)
    topdock: TDock97;
    leftdock: TDock97;
    rightdock: TDock97;
    bottomdock: TDock97;
    MenuToolbar: TToolbar97;
    procedure Close1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FWndState: TFileObjectWndState;
    FInitialized, FDisabledAlign: Boolean;
   {FModified: Boolean;}
    FAttachPanel: TWinControl;
    DefPosition: TRect;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
    procedure wmSysCommand(var Msg: TWMSysCommand); message wm_SysCommand;
    procedure cmSysColorChange(var Msg: TWMSysCommand); message cm_SysColorChange;
  protected
    FFileObject: QFileObject;
    function GetSmallPosition: TRect;
    function GetViewForm: TCustomForm;
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; virtual;
    function GetConfigStr: String; dynamic;
    procedure ReadSetupInformation(Level: Integer); dynamic;
    procedure SaveToolbars(Remove: Boolean);
    procedure LoadToolbars;
    procedure ForcedAssignObject(Q: QFileObject; State: TFileObjectWndState);
    function EditMenuCommand(Cmd: Integer) : Integer;
  public
    property FileObject: QFileObject read FFileObject;
    property AttachPanel: TWinControl read FAttachPanel;
    property WndState: TFileObjectWndState read FWndState;
   {property Modified: Boolean read FModified;}
    procedure CloseNow;
    procedure Save(AskName: Integer);
    procedure Detach(New: QFileObject);
    procedure Attach(nViewPanel: TWinControl);
    procedure CheckUniqueWindow;
   {function GetSetupObject : QObject;}
    function TmpSwapFileObject(New: QFileObject) : QFileObject;
    function GetSpecTbExtra(const Specific: String) : String;
  end;

  PFileObjectClassInfo = ^TFileObjectClassInfo;
  TFileObjectClassInfo = record
                          WndInfo: TFileObjectWndInfo;
                          FileObjectDescriptionText: String;
                          FileExt: Integer;
                          DefaultExt: String;
                          QuArKFileObject: Boolean;
                          LocalPython: Boolean;
                          Unformatted: Boolean;
                          PythonMacro: String;
                         {OpenFilter: Integer;
                          FileExtCount: Integer;
                          FileExt: array[0..MaxFileExt-1] of Integer;
                          DefaultExt: array[0..MaxFileExt-1] of String;}
                         end;
  QFileObjectClass = class of QFileObject;
  QFileObject = class(QObject)
                private
                  procedure LoadObjsFromText(F: TStream; Taille: Integer);
                  function GetObjectGameCode: Char;
                  procedure SetObjectGameCode(nCode: Char);
                protected
                  function OpenWindow(nOwner: TComponent) : TQForm1; dynamic;
                  function ObtainWindow(nOwner: TComponent; State: TFileObjectWndState) : TQForm1;
                  function ReadStandardFileHeading(Source: TStream; var SourceTaille: Integer) : Integer;
                  procedure WriteStandardFileHeading(TotalSize: Integer; F: TStream);
                  procedure LoadFile(F: TStream; FSize: Integer); override;
                  procedure SaveFile(Info: TInfoEnreg1); override;
                 {function ChargerQuArK(F: TStream; Taille: Integer) : Boolean;
                  function SaveQuArK(Format: Integer; F: TStream) : Boolean;}
                  function LoadName: String;
                  procedure WriteSiblingsTo(Info1: TInfoEnreg1);
                public
                  Filename: String;
                  Protocol: String;
                  ReadFormat: Integer;
                  destructor Destroy; override;
                  class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); virtual;
                  procedure ObjectState(var E: TEtatObjet); override;
                  function OpenInWindow(nParent: TWinControl) : TQForm1;
                  procedure OpenStandAloneWindow(ParentPanel: TWinControl; FullParentPanel: Boolean);
                  function EnumObjectWindow(var F: TQForm1) : Boolean;
                  procedure TrySavingNow;
                  procedure TryRenamingNow(const nName: String);
                  function RecommendFormat: Integer;
                  function TestConversionType(I: Integer) : QFileObjectClass; dynamic;
                  function ConversionFrom(Source: QFileObject) : Boolean; dynamic;
                  procedure CopyToClipboard;
                 {function FindSubObject(const nName: String; WantClass: QObjectClass) : QObject;}
                  function CreateOwnExplorer(nOwner: TComponent) : TWinControl; dynamic;
                  procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
                  function LoadSibling(const nName: String) : QFileObject;
                  function FindFile(const nName: String) : QFileObject; dynamic;
                  procedure CloseUndo;
                  procedure ChangeToObjectGameMode;
                  property ObjectGameCode: Char read GetObjectGameCode write SetObjectGameCode;
                  function NeedObjectGameCode: Char;
                  procedure SaveInFile(Format: Integer; AlternateFile: String);
                  procedure LoadFromStream(F: TStream);
                  function PyGetAttr(attr: PChar) : PyObject; override;
                  function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
                  procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); dynamic;
                end;
 {QQuArKFileObject = class(QFileObject)
                     protected
                       procedure LoadFile(F: TStream; FSize: Integer); override;
                       procedure SaveFile(Format: Integer; F: TStream); override;
                     end;}
  EQObjectFileNotFound = class(Exception);

 {------------------------}

function BindFileQObject(const theFilename: String; nParent: QObject; CheckParent: Boolean) : QFileObject;
function ExactFileLink(const theFilename: String; nParent: QObject; CheckParent: Boolean) : QFileObject;
function BuildFileRoot(const theFilename: String; nParent: QObject) : QFileObject;
procedure CopyToolbar(Source, Dest: TToolbar97);
function GlobalDoAccept{(Sender: TObject)} : Boolean;
function LocalDoAccept(Ac: TControl) : Boolean;
function GlobalDoCancel{(Sender: TObject)} : Boolean;
procedure GlobalWarning(const Texte: String);
procedure GlobalDisplayWarnings;
function OpenFileObjectData(F: TStream; const FullName: String; var Size: LongInt; nParent: QObject) : QFileObject;
procedure DeleteTempFiles;
function SaveObject(FFileObject: QFileObject; AskName, FileType: Integer; ParentForm: TCustomForm; AddToRecentsList: Boolean = true) : QFileObject;
function GetFileRoot(Q: QObject) : QFileObject;
procedure AddToRecentFiles(const FileName: String);
procedure ResizeRecentFiles;
procedure RestoreAutoSaved(const Ext: String);

function ByName(Item1, Item2: Pointer) : Integer;
function ByFileName(Item1, Item2: Pointer) : Integer;
function SortedFindName(Q: TQList; const nName: String) : QObject;
function SortedFindFileName(Q: TQList; const nFileName: String) : QFileObject;

procedure ConstructObjsFromText(Self: QObject; P: PChar; PSize: Integer);
procedure ConvertObjsToText(Self: QObject; L: TStringList; Comment: Boolean);
function CheckFileSignature(var P: PChar) : Boolean;
function MakeTempFileName(const Tag: String) : String;

 {------------------------}

implementation

uses Qk1, Undo, QkExplorer, Setup, qmath, QkGroup, Travail, QkOwnExplorer,
  QkFileExplorer, QkUnknown, Toolbar1, Quarkx, QkExceptions, QkInclude, PyObjects,
  PyForms, QkTreeView, Game, QkObjectClassList, QkApplPaths, ExtraFunctionality;

{$R *.DFM}

 {------------------------}

const
 c_FileSignatureQQRK   = $4B525151;  {QQRK}
 c_FileVersionBinary   = $314E4942;  {BIN1}
 c_FileVersionText     = $31435253;  {SRC1}
 c_FileVersionText2    = $32435253;  {SRC2}

 c_FileSignatureSize = 2*SizeOf(LongInt);  { Signature + Version }

type
 TFileHeaderBinary = record
                    Signature: LongInt;
                    Version: LongInt;
                    TotalFileLength: LongInt;
                    InfoType: array[0..19] of Byte;
                   end;

function ExactFileLink(const theFilename: String; nParent: QObject; CheckParent: Boolean) : QFileObject;
var
 Source: TQStream;
begin
 Source:=FileAccessQ(theFilename, []);
 Source.AddRef;
 try
 {if Source.Root=Nil then
   begin  { the file was not previously opened, create a new object }
    Result:=BuildFileRoot(theFilename, nParent);
    Result.Open(Source, Source.Size);
 (* Source.Root:=Result;
   {Result.AddRef(+1);}
   end
  else  { reuse the same object for the file }
   begin
    Result:=QFileObject(Source.Root);
    if CheckParent and (Result.FParent <> nParent) then
     Raise EErrorFmt(5224, [theFilename]);
   end;*)
 finally
  Source.Release;
 end;
end;

function BindFileQObject(const theFilename: String; nParent: QObject; CheckParent: Boolean) : QFileObject;
var
 pathAndFileName, {IncludePath,} CurDir: String;
 Q: QFileObject;
 NoPath: Boolean;
 searchPaths: QApplPaths;
 somePath: String;
begin
  pathAndFileName:=theFilename;
  NoPath:=ExtractFilePath(theFilename) = '';

  if NoPath and (nParent<>Nil) then
  begin  { try to complete the file path }
    Q:=GetFileRoot(nParent);
    if (Q<>Nil) and (Q.Filename<>'') then
    begin
      CurDir:=ExtractFilePath(Q.Filename);
      if FileExists(CurDir+theFilename) then
        pathAndFileName:=CurDir+theFilename;
    end;
  end;

  if not FileExists(pathAndFileName) then
  begin
    { search through the defined QuArK path-structure, until either
      the file is found, or there are no further defined QuArK paths to search. }
    searchPaths:=QApplPaths.Create;
    try
      repeat
        if (searchPaths.GetNextPath(somePath) = False) then
        begin
          GetDir(0, CurDir);
          Raise EQObjectFileNotFound.Create(FmtLoadStr1(5203, [theFilename, CurDir]));
        end;

        pathAndFileName := somePath + theFilename;
      until FileExists(pathAndFileName);
    finally
      searchPaths.Free;
    end;
  end;

  Result:=ExactFileLink(ExpandFileName(pathAndFileName), nParent, CheckParent);
end;

function BuildFileRoot(const theFilename: String; nParent: QObject) : QFileObject;
begin
 Result:=ConstructQObject(ExtractFileName(theFilename), nParent) as QFileObject;
 Result.Filename:=theFilename;
(*  { set ReadFormat according to the file extension }
 Result.ReadFormat:=Ord(CodeConstruction)-(Ord('A')-rf_Default); *)
 Result.ReadFormat:=rf_Default;
 Result.Flags:=Result.Flags or ofFileLink;
end;

function OpenFileObjectData(F: TStream; const FullName: String; var Size: LongInt; nParent: QObject) : QFileObject;
var
 Q: QObject;
begin
 Q:=ConstructQObject(FullName, nParent);
 if not (Q is QFileObject) then
  begin   { error }
   {$IFDEF Debug} Raise InternalE('OpenFileObjectData'); {$ENDIF}
   Q.Free;
   Q:=QUnknown.Create(FullName, nParent);
  end;
 Result:=QFileObject(Q);
end;
(*function OpenFileObjectData(F: TStream; const FullName: String; var Size: LongInt; nParent: QObject) : QFileObject;
var
 Q: QObject;
 Origin, Size1: Integer;
begin
 Origin:=F.Position;
 Q:=ConstructQObject(FullName, nParent);
 try
  Size1:=Size;
  with Q as QFileObject do
   LireEnteteFichier(F, Name, Size1);
  Size:=Size1;
  Result:=QFileObject(Q);
 except  { an error... let's turn the file into unknown type }
  Q.Free;
  F.Position:=Origin;
  Result:=QUnknown.Create(FullName, nParent);
  Size1:=Size;
  Result.LireEnteteFichier(F, FullName, Size1);  { this one should not crash }
  Size:=Size1;
 end;
end;*)

 {------------------------}

function ByName(Item1, Item2: Pointer) : Integer;
begin
 Result:=CompareText(QObject(Item1).Name, QObject(Item2).Name);
end;

function ByFileName(Item1, Item2: Pointer) : Integer;
begin
 Result:=CompareText(QFileObject(Item1).Filename, QFileObject(Item2).Filename);
end;

function SortedFindName(Q: TQList; const nName: String) : QObject;
var
 Min, Max, Test, Diff: Integer;
begin
 Min:=0; Max:=Q.Count;
 while Max>Min do
  begin
   Test:=(Min+Max) div 2;
   Result:=Q[Test];
   Diff:=CompareText(nName, Result.Name);
   if Diff=0 then
    Exit;  { found it }
   if Diff>0 then
    Max:=Test
   else
    Min:=Test+1;
  end;
 Result:=Nil;  { didn't find it }
end;

function SortedFindFileName(Q: TQList; const nFileName: String) : QFileObject;
var
 Min, Max, Test, Diff: Integer;
begin
 Min:=0; Max:=Q.Count;
 while Max>Min do
  begin
   Test:=(Min+Max) div 2;
   Result:=QFileObject(Q[Test]);
   Diff:=CompareText(nFileName, Result.Filename);
   if Diff=0 then
    Exit;  { found it }
   if Diff<0 then
    Max:=Test
   else
    Min:=Test+1;
  end;
 Result:=Nil;  { didn't find it }
end;

 {------------------------}

var
 GlobalWarnings: TStringList;

procedure GlobalWarning(const Texte: String);
begin
 if Texte='' then Exit;
 if GlobalWarnings=Nil then
  begin
   GlobalWarnings:=TStringList.Create;
  {PostMessage(g_Form1.Handle, wm_InternalMessage, wp_Warning, 0);}
  end;
 if GlobalWarnings.IndexOf(Texte)<0 then
  GlobalWarnings.Add(Texte);
end;

procedure GlobalDisplayWarnings;
var
 S: String;
begin
 if GlobalWarnings<>Nil then
  begin
   S:=GlobalWarnings.Text;
   GlobalWarnings.Free;
   GlobalWarnings:=Nil;
   MessageDlg(S, mtWarning, [mbOk], 0);
  end;
end;

 {------------------------}

procedure CopyToolbar(Source, Dest: TToolbar97);
var
 I, J: Integer;
 Btn, BtnSrc: TToolbarButton97;
 Liste: TList;
 SourceCtrl, DestCtrl: TControl;
begin
{Result.Visible:=False;}
 for I:=Dest.ControlCount-1 downto 0 do
  Dest.Controls[I].Free;
 if Source=Nil then
  Exit;
 Dest.Caption:=Source.Caption;
{Dest.CloseButton:=Source.CloseButton;}
 Liste:=TList.Create; try
 for I:=0 to Source.ControlCount-1 do
  begin
   DestCtrl:=Nil;
   SourceCtrl:=Source.Controls[I];
   if SourceCtrl is TToolbarButton97 then
    begin
     BtnSrc:=TToolbarButton97(SourceCtrl);
     Btn:=TToolbarButton97.Create(Dest.Owner);
     Btn.Width:=BtnSrc.Width;
     Btn.Height:=BtnSrc.Height;
     Btn.Caption:=BtnSrc.Caption;
     Btn.DropdownArrow:=False;
     Btn.DropdownMenu:=BtnSrc.DropdownMenu;
     Btn.Glyph.Assign(BtnSrc.Glyph);
     Btn.Hint:=BtnSrc.Hint;
     Btn.OnClick:=BtnSrc.OnClick;
     DestCtrl:=Btn;
    end
   else
    if SourceCtrl is TToolbarSep97 then
     DestCtrl:=TToolbarSep97.Create(Dest.Owner);
   if DestCtrl<>Nil then
    begin
     DestCtrl.Parent:=Dest;
     J:=Source.OrderIndex[SourceCtrl];
     while Liste.Count<=J do
      Liste.Add(Nil);
     Liste[J]:=DestCtrl;
    end;
  end;
 for I:=Liste.Count-1 downto 0 do
  begin
   DestCtrl:=TControl(Liste[I]);
   if DestCtrl<>Nil then
    Dest.OrderIndex[DestCtrl]:=0;
  end;
 finally Liste.Free; end;
end;

function LocalDoAccept(Ac: TControl) : Boolean;
begin
 Result:=False;
 if Ac<>Nil then
  begin
   Result:=True;
   if (Ac is TEnterEdit) and TEnterEdit(Ac).Editing then
    TEnterEdit(Ac).DoAccept
   else
    if (Ac is TEnterComboBox) and TEnterComboBox(Ac).Editing then
     TEnterComboBox(Ac).DoAccept
    else
     if (Ac is TListView) and TListView(Ac).IsEditing then
      TListView(Ac).Selected.CancelEdit
     else
      if (Ac is TMyTreeView) and TMyTreeView(Ac).Editing then
       TMyTreeView(Ac).EndEdit(True)
      else
       Result:=False;
  end;
end;

function GlobalDoAccept{(Sender: TObject)} : Boolean;
begin
 Result:=LocalDoAccept(Screen.ActiveControl);
end;

function GlobalDoCancel{(Sender: TObject)} : Boolean;
var
 Ac: TControl;
{Form: TForm;}
begin
 Result:=False;
{Form:=GetParentForm(Sender as TControl);
 if Form<>Nil then
  begin}
   Ac:={Form}Screen.ActiveControl;
   if Ac<>Nil then
    begin
     Result:=True;
     if (Ac is TEnterEdit) and TEnterEdit(Ac).Cancel then
      {cancelled}
     else
      if (Ac is TEnterComboBox) and TEnterComboBox(Ac).Cancel then
       {cancelled}
      else
       if (Ac is TListView) and TListView(Ac).IsEditing then
        SendMessage(ListView_GetEditControl(TListView(Ac).Handle),
         wm_KeyDown, vk_Escape, 0)
       else
        if (Ac is TMyTreeView) and TMyTreeView(Ac).Editing then
         TMyTreeView(Ac).EndEdit(False)
        else
         Result:=False;
    end;
 {end;}
end;

 {------------------------}

procedure QFileObject.CloseUndo;
begin
 if Flags and ofFileLink <> 0 then
  CloseUndoRoot(Self);
end;

destructor QFileObject.Destroy;
begin
 CloseUndo;
 inherited;
end;

function QFileObject.LoadName: String;
begin
 if Flags and ofFileLink = 0 then
  Result:=Name+TypeInfo
 else
  Result:=Filename;
end;

function QFileObject.ReadStandardFileHeading(Source: TStream; var SourceTaille: Integer) : Integer;
var
 Header: TFileHeaderBinary;
 S: String;
 Lu: Integer;
begin
 Lu:=Source.Read(Header, SizeOf(Header));
 if (Lu < c_FileSignatureSize)
 or (Header.Signature<>c_FileSignatureQQRK) then
  Raise EErrorFmt(5184, [LoadName]);
 case Header.Version of
  c_FileVersionBinary: begin
                    if (Lu<SizeOf(Header))
                    or (Header.TotalFileLength < SizeOf(Header)) then
                     Raise EErrorFmt(5186, [LoadName]);
                    if Header.TotalFileLength > SourceTaille then
                     FileCrashRecoverHack;
                    S:=CharToPas(Header.InfoType);
                    if CompareText(S, TypeInfo) <> 0 then
                     Raise EErrorFmt(5187, [LoadName, S]);
                    SourceTaille:=Header.TotalFileLength - SizeOf(Header);
                    ReadFormat:=rf_Private;
                   end;
  c_FileVersionText: begin
                   Source.Seek(c_FileSignatureSize - Lu, soFromCurrent);
                   Dec(SourceTaille, c_FileSignatureSize);
                   ReadFormat:=rf_AsText;
                  end;
  else
   Raise EErrorFmt(5185, [LoadName]);
 end;
 Result:=ReadFormat;
end;

procedure QFileObject.LoadFile(F: TStream; FSize: Integer);
var
 Info: TFileObjectClassInfo;
begin
 if ReadFormat=rf_Private then
  inherited
 else
  begin
   FileObjectClassInfo(Info);
   if Info.QuArKFileObject then  { object is stored in QuArK style }
    if ReadStandardFileHeading(F, FSize) <> rf_AsText then
     inherited
    else
     LoadObjsFromText(F, FSize)
   else   { object is stored in its own format }
    if ReadFormat>=rf_Default then
     ReadUnformatted(F, FSize)  { as unformatted stand-alone file }
    else
     inherited;
  end;
end;

(*procedure QQuArKFileObject.LoadFile(F: TStream; FSize: Integer);
begin
 if (ReadFormat=rf_Private)
 or (ReadStandardFileHeading(F, FSize) <> rf_AsText) then
  inherited LoadFile(F, FSize)
 else
  LoadObjsFromText(F, FSize);
end;*)

(*function ChargerQuArK(F: TStream; Taille: Integer) : Boolean;
begin
 if (ReadFormat=rf_Private)
 or (ReadStandardFileHeading(F, Taille) <> rf_AsText) then
  Result:=True
 else
  begin
   LoadObjsFromText(F, Taille);
   Result:=False;
  end;
end;*)

procedure QFileObject.LoadObjsFromText(F: TStream; Taille: Integer);
var
 S: String;
begin
 SetLength(S, Taille);
 F.ReadBuffer(S[1], Taille);
 ConstructObjsFromText(Self, PChar(S), Taille);
  { when loading from the Addons path, try to build a cached (compiled) version }
(* if (ExtractFilePath(Filename)=GetApplicationAddonsPath())
 and (ExtractFileExt(Filename)=TypeInfo)... *)
end;

procedure ConstructObjsFromText(Self: QObject; P: PChar; PSize: Integer);
const
 cSeparators = [#13, #10, ' ', #9];
 Granularite = 8192;
var
 NameSpec, Arg, A1: String;
 Level, Q: QObject;
 P1: PChar;
 Ligne, I, J {,IgnoreLevel}: Integer;
 Value: Single;
 ValueI: Integer;
 Prochain: PChar;
 Filename: String; //Used in SyntaxError for display purposes

  procedure Lu(Delta: Integer);
  begin
   Inc(P, Delta);
   while P>Prochain do
    begin
     ProgressIndicatorIncrement;
     Inc(Prochain, Granularite);
    end;
   repeat
    while P^ in cSeparators do
     begin
      case P^ of
      #13:
       begin
        Inc(Ligne);
        Inc(P);
        if P^=#10 then Inc(P);
       end;
      #10:
       begin
        Inc(Ligne);
        Inc(P);
       end;
      else
       Inc(P);
      end;
     end;
    if (P[0]<>'/') or (P[1]<>'/') then Exit;
    while not (P^ in [#13, #10, #0]) do
     Inc(P);
   until False;
  end;

  procedure SyntaxError(Texte: Integer);
  begin
   Raise EErrorFmt(5193, [Filename, Ligne, LoadStr1(Texte)]);
  end;

  function HexVal(C: Char) : Integer;
  begin
   case C of
    '0'..'9': HexVal:=Ord(C) - Ord('0');
    'A'..'Z': HexVal:=Ord(C) - (Ord('A')-10);
    'a'..'z': HexVal:=Ord(C) - (Ord('a')-10);
    else
     begin
      SyntaxError(5199);
      HexVal:=0;
     end;
   end;
  end;

  function StringVal() : string;
  var
    StringPart: String;
    P1: PChar;
  begin
   Result:='';
   repeat
    while P^='$' do
     begin
      Lu(1);
      while P^ in ['0'..'9', 'a'..'f', 'A'..'F'] do
       begin
        Result:=Result+Chr((HexVal(P[0]) shl 4) or HexVal(P[1]));
        Inc(P, 2);
       end;
      Lu(0);
     end;
    if P^<>'"' then
     Break;
    P1:=P;
    repeat
     Inc(P);
     case P^ of
      '"': Break;
      #13, #10, #0: SyntaxError(5198);
     end;
    until False;
    SetString(StringPart, P1+1, P-P1-1);
    Result:=Result+StringPart;
    Lu(1);
   until False;
  end;

  procedure ChoiceList();
  {Decker 2001-06-14
    Reason for procedure: I was sick and tired of always having two different
    specifics (Items, Values) to fill out for a TYP="C", never able to actually
    see if the number of items was consistent with the number of values.
    This procedure parses a text which looks like this:
      ( "valuetext" : "itemtext"
       ,"valuetext" : "itemtext"
       ,"valuetext" : "itemtext" )
    Or like this:
      ( "valuetext"
       ,"valuetext"
       ,"valuetext" )
    And builds these two specifics, which it adds to the current object (Level):
      Items="itemtext"$0D"itemtext"$0D"itemtext"
      Values="valuetext"$0D"valuetext"$0D"valuetext"
  }
  var
    Item, Items: String;
    Value, Values: String;
    OnlyValues: Boolean;
  begin
    Lu(1);
    Items:='';
    Values:='';
    OnlyValues:=True;
    while P^<>')' do
    begin
      if (P^<>'"') then
        SyntaxError(248);
      Value:=StringVal();
      Lu(0);
      if (P^=':') then
      begin
        Lu(1);
        if (P^<>'"') then
          SyntaxError(248);
        Item:=StringVal();
        OnlyValues:=False;
      end
      else
        Item:=Value;
      Items:=Items+Item;
      Values:=Values+Value;
      if (P^=',') then
      begin
        Lu(1);
        Items:=Items+#$0D;
        Values:=Values+#$0D;
      end;
    end;
    Lu(1);
    if (OnlyValues=True) then
      Level.SpecificsAdd('Items='+Values) { All the Values are actually Items. Go figure! }
    else
    begin
      Level.SpecificsAdd('Items='+Items);
      Level.SpecificsAdd('Values='+Values);
    end;
  end;

begin
 Filename:=Self.GetFullName;
 ProgressIndicatorStart(5447, PSize div Granularite);
 try
  Ligne:=1;
  Prochain:=P+Granularite;
  Lu(0);
  if P^<>'{' then SyntaxError(5194);
  Level:=Self;
  {IgnoreLevel:=0;}
  Lu(1);
  repeat
   while P^='}' do
    begin
     Lu(1);
     {if IgnoreLevel>0 then
      Dec(IgnoreLevel)
     else}
      Level.FinalizeFromText;
      if Level=Self then
       begin
        if P^<>#0 then GlobalWarning(LoadStr1(5195));
        Self.FixupAllReferences;
        Exit;
       end
      else
       Level:=Level.FParent;
    end;
   I:=1;
   while P[I] <> '=' do
    begin
     if P[I] in [#13, #10, #0] then
      SyntaxError(5197);
     Inc(I);
    end;
   J:=I+1;
   while P[I-1] in cSeparators do
    Dec(I);
   SetString(NameSpec, P, I);
   Lu(J);
   case P^ of
    '{': begin { New SubElement }
          Lu(1);
          {if IgnoreLevel=0 then}
           Q:=ConstructQObject(NameSpec, Level)
          {else
           Q:=Nil};
          {if Q=Nil then
           Inc(IgnoreLevel)
          else
           begin}
            Level.SubElements.Add(Q);
            Level:=Q;
           {end;}
         end;
    '@': begin { Load this file }
          Lu(1);
          {if IgnoreLevel=0 then}
           Level.LoadedFileLink(NameSpec, 0);
         end;
    '"', '$': { Text/Data Specific }
         begin
          Arg:=StringVal();
          {if IgnoreLevel=0 then}
           Level.SpecificsAdd(NameSpec+'='+Arg);
         end;
    '''':begin { Float number Specific }
          Arg:='';
          Lu(1);
          while P^<>'''' do
           begin
            P1:=P;
            while not (P^ in (['''', #0]+cSeparators)) do
             Inc(P);
            SetString(A1, P1, P-P1);
            try
             Value:=StrToFloat(A1);
            except
             Raise EErrorFmt(5193, [Filename, Ligne, FmtLoadStr1(5209, [A1])]);
            end;
            SetLength(Arg, Length(Arg)+4);  { SizeOf(Single) }
            Move(Value, Arg[Length(Arg)-3], 4);  { SizeOf(Single) }
            Lu(0);
           end;
          Lu(1);
          NameSpec[1]:=Chr(Ord(NameSpec[1]) or chrFloatSpec);
          {if IgnoreLevel=0 then}
           Level.SpecificsAdd(NameSpec+'='+Arg);
         end;
    '|': begin { Integer number Specific }
          Arg:='';
          Lu(1);
          while P^<>'|' do
           begin
            P1:=P;
            while not (P^ in (['|', #0]+cSeparators)) do
             Inc(P);
            SetString(A1, P1, P-P1);
            try
             ValueI:=StrToInt(A1);
            except
             Raise EErrorFmt(5193, [Filename, Ligne, FmtLoadStr1(5229, [A1])]);
            end;
            SetLength(Arg, Length(Arg)+4);  { SizeOf(Integer) }
            Move(ValueI, Arg[Length(Arg)-3], 4);  { SizeOf(Integer) }
            Lu(0);
           end;
          Lu(1);
          if Length(NameSpec)<2 then
           Raise EErrorFmt(5193, [Filename, Ligne, Format('SpecName to small: %s', [NameSpec])]);
          NameSpec[2]:=Chr(Ord(NameSpec[2]) or chrFloatSpec);
          {if IgnoreLevel=0 then}
           Level.SpecificsAdd(NameSpec+'='+Arg);
         end;
    '!': begin { Copy Specifics+SubElements from this Object }
          Lu(1);
          {if IgnoreLevel=0 then}
           if not DoIncludeData(Level, Level, NameSpec) then
            GlobalWarning(FmtLoadStr1(5643, [NameSpec]));
         end;
    '(': begin { Choice list }
          ChoiceList();
         end;
    else
     SyntaxError(5196);
   end;
  until False;
 finally
  ProgressIndicatorStop;
 end;
end;

procedure QFileObject.WriteStandardFileHeading(TotalSize: Integer; F: TStream);
var
 Header: TFileHeaderBinary;
begin
 if TotalSize=0 then
  Header.Signature:=0   { while the whole file is not written yet }
 else
  Header.Signature:=c_FileSignatureQQRK;
 Header.Version:=c_FileVersionBinary;
 Header.TotalFileLength:=TotalSize;
 PasToChar(Header.InfoType, TypeInfo);
 F.WriteBuffer(Header, SizeOf(Header));
end;

function MakeTempFileName(const Tag: String) : String;
var
 Z, R: array[0..MAX_PATH] of Char;
begin
 R[0]:=#0;
 Z[0]:=#0;
 GetTempPath(SizeOf(Z), Z);
 GetTempFileName(Z, PChar(Tag), 0, R);
 if R[0]=#0 then
  Raise Exception.Create(LoadStr1(5659));
 Result:=StrPas(R);
end;

procedure DeleteTempFiles;
var
 Z: array[0..MAX_PATH] of Char;
 TempPath: String;
 List: TStringList;

  procedure ListFiles(const Tag: String);
  var
   F: TSearchRec;
  begin
    List.Clear;
    { Find all files with the *.TMP extension }
    if (FindFirst(TempPath+Tag+'*.TMP', faAnyFile, F) = 0) then
    begin
      repeat
        List.Add(F.Name);
      until FindNext(F)<>0;
    end;
    FindClose(F);
  end;

  procedure DeleteTheseFiles;
  var
   I: Integer;
  begin
   for I:=List.Count-1 downto 0 do
    DeleteFile(TempPath+List[I]);
  end;

begin
 GetTempPath(SizeOf(Z), Z);
 TempPath:=StrPas(Z);
 if TempPath='' then
  Exit;  { error }
 TempPath:=IncludeTrailingPathDelimiter(TempPath);
 List:=TStringList.Create;
 try
  ListFiles(TagToDelete);
  DeleteTheseFiles;
  ListFiles(TagToDelete2);
  if List.Count>0 then
   begin
    if MessageDlg(FmtLoadStr1(5216, [List.Count, TempPath]), mtConfirmation,
     [mbYes,mbNo], 0) = mrYes then
      DeleteTheseFiles;
   end;
 finally
  List.Free;
 end;
end;

procedure QFileObject.LoadFromStream(F: TStream);
begin
 {$IFDEF Debug} FLoading:=True; try {$ENDIF}
 LoadFile(F, F.Size-F.Position);
 {$IFDEF Debug} finally FLoading:=False; end; {$ENDIF}
end;


procedure QFileObject.WriteSiblingsTo(Info1: TInfoEnreg1);
var
 L: TQList;
 I: Integer;
begin
 Info1.Format:=rf_Siblings;
 L:=TQList.Create; try
 L.Capacity:=SubElements.Count;
 for I:=0 to SubElements.Count-1 do
  L.Add(SubElements[I]);
 for I:=0 to L.Count-1 do
  L[I].SaveFile1(Info1);
 finally L.Free; end;
end;

type
 TFileSibling = class(TInfoEnreg1)
                private
                  BasePath: String;
                public
                  procedure WriteSibling(const Path: String; Obj: QObject); override;
                end;

procedure TFileSibling.WriteSibling(const Path: String; Obj: QObject);
var
 I: Integer;
begin
 if Obj is QFileObject then
  begin
   I:=Length(Path);
   while (I>0) and (Path[I]<>'/') do
    Dec(I);
   QFileObject(Obj).SaveInFile(rf_Default, AppendFileToPath(BasePath, Copy(Path, I+1, MaxInt)));
  end
 else
  inherited;
end;

procedure QFileObject.SaveInFile;
const
 Granularite = 16384;
var
 F, Target: TQStream;
 S, TempFile: String;
 TargetFile: TStream;
 I, J: Integer;
 Update: Boolean;
{FileOp: TSHFILEOPSTRUCT;}
 Info1: TFileSibling;
begin
 Update:=AlternateFile='';
 try
  Info1:=TFileSibling.Create; try
  Info1.Format:=rf_Siblings;
  if Update then
   Info1.BasePath:=ExtractFilePath(Filename)
  else
   Info1.BasePath:=ExtractFilePath(AlternateFile);
  SaveFile1(Info1);

  TempFile:=MakeTempFileName(TagToDelete2);
  F:=FileAccessQ(TempFile, [maUnique]);
  F.AddRef; try
  F.Temporary:=True;
  Info1.Format:=Format;
  Info1.F:=F;
  Info1.TransfertSource:=Update;
  SaveFile1(Info1);  { write the actual data }

  if Update then
   begin
    ReadFormat:=Format;
    AlternateFile:=Filename;
   end;

   { figure out if we can actually override the target file }
  Target:=FileAccessQ(AlternateFile, [maNoOpen]);
  if Target<>Nil then
   begin  { there are maybe pending objects in the undo buffer }
    Target.AddRef; try
    ClearUndo(MaxInt, Target);   { load them or clear them }
    finally Target.Release; end;
    FileAccessQ(AlternateFile, [maNoOpen, maUnique]);  { try again }
   end;

   { copy or move the file to the target name }
  AlternateFile:=ExpandFileName(AlternateFile);
  if Update then
   Filename:=AlternateFile;

  S:=Copy(AlternateFile, 1, Pos(PathDelim,AlternateFile));
  if GetDriveType(PChar(S)) = DRIVE_FIXED then
   begin  { target is a fixed drive - we MOVE the file there }
    S:=TempFile;
    F.Temporary:=False;
    try
     F.TemporaryClose;
     DeleteFile(QuickResolveFilename(AlternateFile));
     if not MoveFile(PChar(QuickResolveFilename(TempFile)), PChar(QuickResolveFilename(AlternateFile))) then
      begin
       g_Form1.NoTempDelete:=True;
       Raise EErrorFmt(5516, [AlternateFile, S, ExtractFileName(AlternateFile)]);
      end;
     S:=AlternateFile;
    finally
     if F.RefCount1 > 1 then
      try  { don't bother reopening the file if no other references are pending }
       if not F.ReopenAs(S) then
        Abort;  { throw an exception }
      except { bad error if we couldn't reopen the file }
       g_Form1.NoTempDelete:=True;
       Raise EErrorFmt(5515, [S, ExtractFileName(AlternateFile)]);
      end;
    end;
   end
  else
   try  { target is removeable - we COPY the file there }
    I:=F.Size;
    ProgressIndicatorStart(5450, I div Granularite + 1); try
    TargetFile:=TFileStream.Create(QuickResolveFilename(AlternateFile), fmCreate); try
    F.Position:=0;
    while I>0 do
     begin
      J:=I;
      if J>Granularite then J:=Granularite;
      TargetFile.CopyFrom(F, J);
      ProgressIndicatorIncrement;
      Dec(I, J);
     end;
    finally TargetFile.Free; end;
    finally ProgressIndicatorStop; end;
  (*FileOp.Wnd:=g_Form1.Handle;
    FileOp.wFunc:=FO_COPY;
    FileOp.pFrom:=PChar(TempFile);
    FileOp.pTo:=PChar(AlternateFile);
    FileOp.fFlags:=FOF_NOCONFIRMATION or FOF_SIMPLEPROGRESS;
    S:=LoadStr1(5450);
    FileOp.lpszProgressTitle:=PChar(S);
    if SHFileOperation(FileOp)<>0 then
     Raise EError(5516); *)
   except
    on E: Exception do
     Raise EErrorFmt(5517, [E.Message]);
   end;
  finally F.Release; end;
  finally Info1.Free; end;
  if Update then
   FFlags:=FFlags and not ofModified;
 finally
  FixupAllReferences;
 end;
end;

procedure QFileObject.TrySavingNow;
{var
 F: TQForm1;}
begin
 if FFlags and ofFileLink = 0 then
   Raise EErrorFmt(5531, [Filename]);

 // SilverPaladin - 12/1/03 - Changed the error messages to a warning so that
 // the save of multiple files is not interupted for one file that is not yet
 // supported. 
 try
   SaveInFile(RecommendFormat, '');
 except
   ShowMessage(FmtLoadStr1(5219, [Filename]));
 end;
{while EnumObjectWindow(F) do
  SendMessage(F.Handle, wm_InternalMessage, wp_SetModify, 0);
 if g_Form1.Explorer.Roots.IndexOf(Self)>=0 then
  SendMessage(g_Form1.Handle, wm_InternalMessage, wp_SetModify, 0);}
end;

procedure QFileObject.TryRenamingNow(const nName: String);
var
 F: TQForm1;
begin
 if Name<>nName then
  begin
   Name:=nName;
   F:=Nil;
   while EnumObjectWindow(F) do
    PostMessage(F.Handle, wm_InternalMessage, wp_AfficherObjet, 0);
  end;
end;

function QFileObject.RecommendFormat;
begin
 Result:=ReadFormat;
 if (Result=rf_Private) or (Result=rf_NotSaved) then
  Result:=rf_Default;
end;

procedure ConvertObjsToTextWithComment(Level: QObject; L: TStringList; const Indent: String);
const
 Imprimable = [' '..#126] - ['"'];
 NonBinaire = [#13, #10, #9, ' '..#126];
 Marge      = 78;
var
 S, Arg{, Values}: String;
 P, I, J: Integer;
 Binaire, Guillemet, Dollar{, FloatOk}: Boolean;
 Car: PChar;
 C: Char;
 Q: QObject;
 Value{, Value2}: Single;
 ValueI: Integer;
{ValueL: LongInt absolute Value;
 Value2L: LongInt absolute Value2;}
begin
 Level.Acces;
{Level.BuildReferences;}
 ProgressIndicatorStart(5443, Level.SubElements.Count+1); try
 for J:=0 to Level.Specifics.Count-1 do
  begin
   S:=Level.Specifics[J];
   P:=Pos('=', S);
   Arg:=Indent + Copy(S, 1, P-1) + ' = ';
   if (Ord(S[1])>=chrFloatSpec) and ((Length(S)-P) and 3 = 0) then
    begin  { float Specific }
     Arg[Length(Indent)+1]:=Chr(Ord(S[1]) and not chrFloatSpec);
     C:='''';
     for I:=1 to (Length(S)-P) div 4 do
      begin
       if Length(Arg)>=Marge then
        begin
         L.Add(Arg);
         Arg:=Indent+' ';
        end;
       Move(S[P+I*4-3], Value, 4);  { SizeOf(Single) }
       Arg:=Arg + C + ftos1(Value);
       C:=' ';
      end;
     if C<>' ' then Arg:=Arg+C;
     Arg:=Arg+'''';
    end
   else
    if (Length(S)>1) and (Ord(S[2])>=chrFloatSpec) and ((Length(S)-P) and 3 = 0) then
     begin  { integer Specific }
      Arg[Length(Indent)+2]:=Chr(Ord(S[2]) and not chrFloatSpec);
      C:='|';
      for I:=1 to (Length(S)-P) div 4 do
       begin
        if Length(Arg)>=Marge then
         begin
          L.Add(Arg);
          Arg:=Indent+' ';
         end;
        Move(S[P+I*4-3], ValueI, 4);  { SizeOf(Integer) }
        Arg:=Arg + C + itos(ValueI);
        C:=' ';
       end;
      if C<>' ' then Arg:=Arg+C;
      Arg:=Arg+'|';
     end
    else
     if P=Length(S) then
      Arg:=Arg+'""'   { empty Specific }
//     else if (Length(S)=P+1) and (S[P+1]='!') then
//       Arg:=Arg+'!'
     else
      begin  { normal Specific }
       Binaire:=False;
       for I:=Length(S) downto P+1 do
        if not (S[I] in NonBinaire) then
         begin
          if (I=Length(S)) and (S[I]=#0) then Continue;  { ignore this case }
          Binaire:=True;
          Break;
         end;
     (*FloatOk:=False;
       if Binaire and ((Length(S)-P) and 3 = 0)   { 3 = SizeOf(Single)-1 }
       and (Length(S)-P<=MaxFloatSpecLength) then
        begin
         Values:='''';
         try
          for I:=1 to (Length(S)-P) div 4 do
           begin
            Move(S[P+I*4-3], Value, 4);  { SizeOf(Single) }
            if (Value=0)
            or ((Value>-MaxFloatAccept) and (Value<MaxFloatAccept)
             and ((Value<-MinFloatAccept) or (Value>MinFloatAccept))) then
             begin
             {Value2:=StrToFloat(FloatToStr(Value));
              if ValueL<>Value2L then
               begin
                FloatOk:=False;
                Break;
               end;}
              Values:=Values + ftos1(Value) + ' ';
             end;
           end;
          Values[Length(Values)]:='''';
          Arg:=Arg+Values;
          FloatOk:=True;
         except
          {FloatOk:=False;}
         end;
        end;
       if not FloatOk then*)
        begin
         Guillemet:=False;
         Dollar:=False;
         Car:=PChar(S)+P;
         for I:=P+1 to Length(S) do
          begin
           if Length(Arg)>=Marge then
            begin
             if Guillemet then
              Arg:=Arg+'"';
             L.Add(Arg);
             Arg:=Indent+' ';
             Guillemet:=False;
             Dollar:=False;
            end;
           if Binaire or not (Car^ in Imprimable) then
            begin
             if Guillemet then
              begin
               Arg:=Arg+'"';
               Guillemet:=False;
              end;
             if not Dollar then
              begin
               Arg:=Arg+'$';
               Dollar:=True;
              end;
             Arg:=Arg+IntToHex(Ord(Car^), 2);
            end
           else
            begin
             Dollar:=False;
             if not Guillemet then
              begin
               Arg:=Arg+'"'+Car^;
               Guillemet:=True;
              end
             else
              Arg:=Arg+Car^;
            end;
           Inc(Car);
          end;
         if Guillemet then
          Arg:=Arg+'"';
        end;
      end;
   L.Add(Arg);
  end;
 ProgressIndicatorIncrement;
 if Level.WriteSubElements then
 for J:=0 to Level.SubElements.Count-1 do
  begin
   Q:=Level.SubElements[J];
   if Q.Flags and ofFileLink <> 0 then
    L.Add(Indent + Q.Name + Q.TypeInfo + ' = @')
   else
    begin
     L.Add(Indent + Q.Name + Q.TypeInfo +' =');
     L.Add(Indent + '{');
     ConvertObjsToTextWithComment(Q, L, Indent+'  ');
     L.Add(Indent + '}');
    end;
   ProgressIndicatorIncrement;
  end;
 finally ProgressIndicatorStop; end;
end;

procedure ConvertObjsToText(Self: QObject; L: TStringList; Comment: Boolean);
var
 S: String;
begin
 SetLength(S, c_FileSignatureSize);
 PInteger(@S[1])^:=c_FileSignatureQQRK;
 PInteger(@S[1+SizeOf(LongInt)])^:=c_FileVersionText;
 L.Add(S);
 if Comment then
  begin
   L.Add(FmtLoadStr1(5200, [QuarkVersion, Self.Name+Self.TypeInfo]));
   L.Text:=L.Text;   { #13 --> #13#10 }
  end;
 L.Add('{');
 ConvertObjsToTextWithComment(Self, L, '  ');
 L.Add('}');
end;

function CheckFileSignature(var P: PChar) : Boolean;
begin
 Result:=(PInteger(P)^=c_FileSignatureQQRK) and (PInteger(P+SizeOf(LongInt))^=c_FileVersionText);
 Inc(P, 2*SizeOf(LongInt));
end;

procedure QFileObject.SaveFile(Info: TInfoEnreg1);
var
 L: TStringList;
 Origin, Eof: Integer;
 Info1: TFileObjectClassInfo;
begin
 with Info do case Format of
  rf_Default:            { as stand-alone file }
    begin
     FileObjectClassInfo(Info1);
     if Info1.QuArKFileObject then
      begin    { object is stored in QuArK style }
       Origin:=F.Position;
       WriteStandardFileHeading(0, F);
       Format:=rf_Private;
       inherited SaveFile(Info);
       Format:=rf_Default;
       Eof:=F.Position;
       F.Position:=Origin;
       WriteStandardFileHeading(Eof-Origin, F);
       F.Position:=Eof;
      end
     else     { object is stored as unformatted data }
      SaveUnformatted(F);
    end;
  rf_AsText:
    begin
     { $IFDEF Debug}
     FileObjectClassInfo(Info1);
     if not Info1.QuArKFileObject then Raise InternalE('rf_AsText without QuArKFileObject');
     { $ENDIF}
     L:=TStringList.Create; try
     ConvertObjsToText(Self, L, True);
     L.SaveToStream(F);
     finally L.Free; end;
    end;
  else
   inherited;  { Format rf_Private }
 end;
end;

(*procedure QQuArKFileObject.SaveFile;
var
 L: TStringList;
 S: String;
 Origin, Eof: Integer;
begin
 case Format of
  rf_Default:
    begin
     Origin:=F.Position;
     WriteStandardFileHeading(0, F);
     inherited SaveFile(rf_Private, F);
     Eof:=F.Position;
     F.Position:=Origin;
     WriteStandardFileHeading(Eof-Origin, F);
     F.Position:=Eof;
    end;
  rf_AsText:
    begin
     L:=TStringList.Create; try
     SetLength(S, c_FileSignatureSize);
     PInteger(@S[1])^:=c_FileSignatureQQRK;
     PInteger(@S[1+SizeOf(LongInt)])^:=c_FileVersionText;
     L.Add(S);
     L.Add(FmtLoadStr1(5200, [QuarkVersion, Name+TypeInfo]));
     L.Text:=L.Text;   { #13 --> #13#10 }
     L.Add('{');
     ConvertObjsToTextWithComment(Self, L, '  ');
     L.Add('}');
     L.SaveToStream(F);
     finally L.Free; end;
    end;
  else
   inherited;  { Format rf_Private }
 end;
end;*)

(*function SaveQuArK(Format: Integer; F: TStream) : Boolean;
var
 L: TStringList;
 S: String;
 Origin, Eof: Integer;
begin
 Result:=False;
 case Format of
  rf_Default:
    begin
     Origin:=F.Position;
     WriteStandardFileHeading(0, F);
     SaveFile(rf_Private, F);
     Eof:=F.Position;
     F.Position:=Origin;
     WriteStandardFileHeading(Eof-Origin, F);
     F.Position:=Eof;
    end;
  rf_AsText:
    begin
     L:=TStringList.Create; try
     SetLength(S, c_FileSignatureSize);
     PInteger(@S[1])^:=c_FileSignatureQQRK;
     PInteger(@S[1+SizeOf(LongInt)])^:=c_FileVersionText;
     L.Add(S);
     L.Add(FmtLoadStr1(5200, [QuarkVersion, Name+TypeInfo]));
     L.Text:=L.Text;   { #13 --> #13#10 }
     L.Add('{');
     ConvertObjsToTextWithComment(Self, L, '  ');
     L.Add('}');
     L.SaveToStream(F);
     finally L.Free; end;
    end;
  else
   Result:=True;  { Format rf_Private }
 end;
end;*)

class procedure QFileObject.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 FillChar(Info, SizeOf(Info), 0);
 Info.FileObjectDescriptionText:=TypeInfo;
 Info.DefaultExt:=Copy(Info.FileObjectDescriptionText, 2, MaxInt);
end;

procedure QFileObject.ObjectState(var E: TEtatObjet);
begin
 E.IndexImage:=iiUnknownFile;
 E.MarsColor:=clNavy;
end;

function QFileObject.TestConversionType(I: Integer) : QFileObjectClass;
begin
 if I=1 then
  Result:=QFileObjectClass(ClassType)
 else
  Result:=Nil;
end;

function QFileObject.ConversionFrom(Source: QFileObject) : Boolean;
begin
 Result:=False;
end;

procedure QFileObject.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
begin
end;

function QFileObject.ObtainWindow(nOwner: TComponent; State: TFileObjectWndState) : TQForm1;
var
 I: Integer;
 Obj: TComponent;
 E: TEtatObjet;
begin
 for I:=0 to nOwner.ComponentCount-1 do
  begin
   Obj:=nOwner.Components[I];
   if (Obj is TQForm1) and TQForm1(Obj).AssignObject(Self, State) then
    begin
     Result:=TQForm1(Obj);
     ObjectState(E);
     Result.MarsCap.ActiveEndColor:=E.MarsColor;
     Result.UpdateMarsCap;
     Exit;
    end;
  end;
 if State=cmOwnExplorer then
  Result:=TQFormExplorer.Create(nOwner)
 else
  Result:=OpenWindow(nOwner);
 if (Result=Nil) or not Result.AssignObject(Self, State) then
 {Raise EErrorFmt(5201, [Name]);}
 {Result:=Nil;}
  begin
   Result:=Nil;
   for I:=0 to nOwner.ComponentCount-1 do
    begin
     Obj:=nOwner.Components[I];
     if (Obj is TFQUnknown) and (TQForm1(Obj).FileObject=Nil) then
      Result:=TQForm1(Obj);
    end;
   if Result=Nil then
    Result:=TFQUnknown.Create(nOwner);
   Result.ForcedAssignObject(Self, State);
  end;
 ObjectState(E);
 Result.MarsCap.ActiveEndColor:=E.MarsColor;
 Result.UpdateMarsCap;
end;

function QFileObject.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=Nil;
end;

function QFileObject.OpenInWindow(nParent: TWinControl) : TQForm1;
var
 Info: TFileObjectClassInfo;
begin
 FileObjectClassInfo(Info);
 if wiNeverOpen in Info.WndInfo then
  begin
   Result:=Nil;  { never open }
   Exit;
  end;
 Result:=ObtainWindow(nParent, cmNone);
 if Result=Nil then
  Exit;
 Result.BorderStyle:=bsNone;
 Result.Parent:=nParent;
 Result.Left:=nParent.Width;
 Result.Align:=alClient;
 Result.Attach(nParent);
 PostMessage(Result.Handle, wm_InternalMessage, wp_FormActivate, 0);
end;

procedure QFileObject.OpenStandAloneWindow(ParentPanel: TWinControl; FullParentPanel: Boolean);
var
 F: TQForm1;
 Info: TFileObjectClassInfo;
 WndState: TFileObjectWndState;
 Maximize: Boolean;
begin
 AddRef(+1);
 try
  ProgressIndicatorStart(5452, 0);
  try
   FileObjectClassInfo(Info);
   if wiNeverOpen in Info.WndInfo then
    Exit;  { never open }
   if wiForm1 in Info.WndInfo then
    begin
     g_Form1.SetExplorerRoot(Self);   { display in the main window }
     ActivateNow(g_Form1);
    end
   else
    if (wiWindow in Info.WndInfo)
    or ((wiOwnExplorer in Info.WndInfo) and (ParentPanel=Nil)) then
     begin   { display in a stand-alone window }
      Acces;
      Maximize:=(ParentPanel<>Nil) and FullParentPanel;
      if Maximize or not (wiOwnExplorer in Info.WndInfo) then
       WndState:=cmWindow
      else
       WndState:=cmOwnExplorer;
      Maximize:=Maximize or (wiMaximize in Info.WndInfo);
      F:=ObtainWindow(Application, WndState);
      if F=Nil then Exit;
     {WindowPlacement.ShowCmd:=sw_ShowNormal;}
      F.Attach(ParentPanel);
     {WindowPlacement.Length := SizeOf(WindowPlacement);
      GetWindowPlacement(F.Handle, @WindowPlacement);}
      if ParentPanel<>Nil then
       F.RestoredRect := F.GetSmallPosition
      else
       begin
        if Maximize then
         F.WindowState:=wsMaximized
        else
         F.WindowState:=wsNormal;
        F.RestoredRect := F.DefPosition;
      (*if Maximize then
         begin
          WindowPlacement.ShowCmd:=sw_ShowMaximized;
         {Maximize:=False;}
         end;*)
       end;
      PostMessage(F.Handle, wm_InternalMessage, wp_FormActivate, 0);
      F.Show;
      if Maximize then
       F.WindowState:=wsMaximized;
     {SetWindowPlacement(F.Handle, @WindowPlacement);}
     end
    else
     if Self is QUnknown then
      Raise EErrorFmt(5201, [Name+TypeInfo])
     else
      Raise EErrorFmt(5202, [Name+TypeInfo]);
  finally
   ProgressIndicatorStop;
  end;
 finally
  AddRef(-1);
 end;
end;

{procedure QFileObject.Modif;
begin
 PosteMessageFiches(wp_ModifObjet, LongInt(Self));
end;}

{procedure QFileObject.LienFichier(const theFilename: String);
begin
 FFlags:=FFlags or (ofLienFichier or ofNotLoadedToMemory);
 Filename:=ExpandFileName(theFilename);
end;}

function QFileObject.EnumObjectWindow(var F: TQForm1) : Boolean;
var
 I: Integer;
 Obj: TComponent;
begin
 if F=Nil then
  I:=0
 else
  I:=F.ComponentIndex+1;
 while I<Application.ComponentCount do
  begin
   Obj:=Application.Components[I];
   if (Obj is TQForm1) and (TQForm1(Obj).WndState in StandAloneWndState)
   and (TQForm1(Obj).FileObject = Self) then
    begin
     F:=TQForm1(Obj);
     EnumObjectWindow:=True;
     Exit;
    end;
   Inc(I);
  end;
 EnumObjectWindow:=False;
end;

procedure QFileObject.CopyToClipboard;
var
 Gr: QExplorerGroup;
begin
 Gr:=ClipboardGroup;
 Gr.AddRef(+1); try
 Gr.SubElements.Add(Self);
 Gr.CopierObjets(False);
 finally Gr.AddRef(-1); end;
end;

function QFileObject.CreateOwnExplorer;
begin
 Result:=TFileExplorer.Create(nOwner);
 Result.Width:=175;
end;

 {------------------------}

function SaveObject(FFileObject: QFileObject; AskName, FileType: Integer; ParentForm: TCustomForm; AddToRecentsList: Boolean = true) : QFileObject;
var
 SaveDialog1: TSaveDialog;
 Info, Info1: TFileObjectClassInfo;
 S, FileName, AddToRecents: String;
 I, P, SavingTo, SavingAsText: Integer;
 Dup: QFileObject;
 ConvertClass: QFileObjectClass;
 FilterIndex: Integer;
begin
 Result:=Nil;
 if (AskName=fm_SaveIfModif) and (FFileObject.Flags and ofModified = 0) then
  Exit;  { don't need saving }
 AddToRecents:='';
 FFileObject.AddRef(+1);
 try
  try
   FFileObject.FileObjectClassInfo(Info);
   SavingTo:=FFileObject.RecommendFormat;
   if (AskName=fm_SaveAsFile) or (SavingTo=rf_Private) or (FFileObject.Filename='')
   or ((SavingTo<>rf_AsText) and (Info.FileExt=0)) then
    begin
     S:='';
     SavingAsText:=1;
     FilterIndex:=0;
    {for I:=0 to Info.FileExtCount-1 do}
     if Info.FileExt<>0 then
      begin
       S:=S+LoadStr1(Info.FileExt{[I]})+'|';
       Inc(SavingAsText);
      end;
     I:=1;
     repeat
      ConvertClass:=FFileObject.TestConversionType(I);
      if ConvertClass=Nil then Break;
      if ConvertClass<>FFileObject.ClassType then
       begin
        ConvertClass.FileObjectClassInfo(Info1);
        if Info1.FileExt<>0 then
         begin
          S:=S+LoadStr1(Info1.FileExt)+'|';
          if I=FileType then
           FilterIndex:=SavingAsText;
          Inc(SavingAsText);
         end;
        Info1.FileObjectDescriptionText:='';
        Info1.DefaultExt:='';
       end;
      Inc(I);
     until False;
     if Info.QuArKFileObject then
      FileName:=Info.DefaultExt  { can save directly as text }
     else
      FileName:='qrk';   { must wrap into a .qrk to save as text }
     S:=S+FmtLoadStr1(769, [FileName]);
     SaveDialog1:=TSaveDialog.Create(Application); try
     SaveDialog1.Title:=LoadStr1(770);
     SaveDialog1.Filter:=S;
     SaveDialog1.DefaultExt:=FileName;
     if FilterIndex>0 then
      SaveDialog1.FilterIndex:=FilterIndex
     else
      if SavingTo=rf_AsText then
       SaveDialog1.FilterIndex:=SavingAsText;
     S:=FFileObject.Filename;
     if S<>'' then
      begin
       S:=ExtractFileName(S);
       P:=Length(S);
       while (P>0) and (S[P]<>'.') do Dec(P);
       if P>0 then
        S:=Copy(S,1,P-1);
      end
     else
      S:=FFileObject.Name + FFileObject.TypeInfo;
     while Pos('/',S)>0 do
      S:=Copy(S, Pos('/',S)+1, MaxInt);
     if S<>'' then
      BuildCorrectFileName(S);
     { tigari: .map extension fix }
     S:=Copy(S,1,Length(S)-Length(ExtractFileExt(S)));
     {tiglari}
     SaveDialog1.FileName:=S;
     SaveDialog1.Options:=[ofHideReadOnly, ofOverwritePrompt, ofPathMustExist];

     if ParentForm<>Nil then
      ActivateNow(ParentForm);
     if not SaveDialog1.Execute then
      Abort;

     SavingTo:=SaveDialog1.FilterIndex;
     FileName:=SaveDialog1.FileName;
     finally SaveDialog1.Free; end;
     AddToRecents:=FileName;

     if SavingTo=SavingAsText then
      SavingTo:=rf_AsText
     else
      begin
       if (SavingTo>1) or (Info.FileExt=0) then
        begin
         if Info.FileExt<>0 then
          Dec(SavingTo);
         I:=1;
         repeat
          ConvertClass:=FFileObject.TestConversionType(I);
          if ConvertClass<>FFileObject.ClassType then
           begin
            ConvertClass.FileObjectClassInfo(Info1);
            if Info1.FileExt<>0 then
             Dec(SavingTo);
           end;
          Inc(I);
         until SavingTo=0;
         Result:=ConvertClass.Create(FFileObject.Name, Nil);
         Result.AddRef(+1);
         if not Result.ConversionFrom(FFileObject) then
          Raise EError(5538);
         Result.Flags:=Result.Flags or ofFileLink;
         FFileObject.AddRef(-1);
         FFileObject:=Result;
         FFileObject.AddRef(+1);
         FileType:=0;
        end;
       SavingTo:=rf_Default;
      end;
     if FileType<0 then
      begin  { detach object from parent Explorer }
       Result:=FFileObject.Clone(Nil, False) as QFileObject;
       Result.AddRef(+1);
       Result.Flags:=Result.Flags or ofFileLink;
       FFileObject.AddRef(-1);
       FFileObject:=Result;
       FFileObject.AddRef(+1);
      end;

     FFileObject.Filename:=FileName;
     FileName:=ExtractFileName(FileName);
     S:=FFileObject.TypeInfo;
     if CompareText(Copy(FileName, Length(FileName)-Length(S)+1, MaxInt), S)=0 then
      begin
       SetLength(FileName, Length(FileName)-Length(S));
       FFileObject.TryRenamingNow(FileName);
      end;
    end;
   if (SavingTo<>rf_AsText) or Info.QuArKFileObject then
    FFileObject.SaveInFile(SavingTo, '')
   else
    begin  { must wrap into a .qrk to save as text }
     S:=ChangeFileExt(FFileObject.Filename, '.qrk');
     Dup:=BuildFileRoot(S, Nil);
     Dup.AddRef(+1); try
     Dup.SubElements.Add(FFileObject);
     I:=FFileObject.FFlags and ofFileLink; try
     Dec(FFileObject.FFlags, I);  { remove ofFileLink temporarily }
     Dup.SaveInFile(rf_AsText, '');
     finally FFileObject.FFlags:=FFileObject.FFlags or I; end;
     finally Dup.AddRef(-1); end;
    end;
  except
   if Result<>Nil then Result.AddRef(-1);
   Raise;
  end;
 finally
  FFileObject.AddRef(-1);
 end;
{if ParentForm<>Nil then
  SendMessage(ParentForm.Handle, wm_InternalMessage, wp_SetModify, 0);}
 if AddToRecentsList and (AddToRecents<>'') then
  AddToRecentFiles(AddToRecents);
end;

procedure AddToRecentFiles(const FileName: String);
var
 L: TStringList;
 J: Integer;
 MaxRecentFiles: Integer;
begin   { adds the file to the list of recently opened files }
 L:=TStringList.Create;
 try
  L.Text:=g_SetupSet[ssGeneral].Specifics.Values['RecentFiles'];
  MaxRecentFiles:=Round(SetupSubSet(ssGeneral, 'Display').GetFloatSpec('MaxRecentFiles', 5));
  J:=L.IndexOf(FileName);
  if J>=0 then
   L.Delete(J);
  L.Insert(0, FileName);
  while L.Count>MaxRecentFiles do
   L.Delete(MaxRecentFiles);
  g_SetupSet[ssGeneral].Specifics.Values['RecentFiles']:=StringListConcatWithSeparator(L, $0D);
  UpdateSetup(scMinimal);
 finally
  L.Free;
 end;
end;

procedure ResizeRecentFiles;
var
 L: TStringList;
 MaxRecentFiles: Integer;
 Resized: Boolean;
begin   { resizes the list of recently opened files to the correct number of files }
 L:=TStringList.Create;
 try
  L.Text:=g_SetupSet[ssGeneral].Specifics.Values['RecentFiles'];
  MaxRecentFiles:=Round(SetupSubSet(ssGeneral, 'Display').GetFloatSpec('MaxRecentFiles', 5));
  Resized:=False;
  while L.Count>MaxRecentFiles do
  begin
   L.Delete(MaxRecentFiles);
   Resized:=True;
  end;
  if Resized then
  begin
    g_SetupSet[ssGeneral].Specifics.Values['RecentFiles']:=StringListConcatWithSeparator(L, $0D);
    UpdateSetup(scMinimal);
  end;
 finally
  L.Free;
 end;
end;

procedure RestoreAutoSaved;
const
 TagAtom = '~QuArK-tag-auto-save-%x';
var
 S, S1: String;
 DosError, I: Integer;
 Rec: TSearchRec;
 OldId: Cardinal;
 H: THandle;
begin
 SetLength(S, MAX_PATH);
 S[1]:=#0;
 GetTempPath(MAX_PATH, PChar(S));
 SetLength(S, StrLen(PChar(S)));
 DosError:=FindFirst(AppendFileToPath(S, Format('auto-save-*%s', [Ext])), faAnyFile, Rec);
 try
  while DosError=0 do
   begin
    S1:=Copy(Rec.Name, 11, MaxInt);
    I:=Pos('-', S1);
    if I>0 then
     begin
      SetLength(S1, I-1);
      try
       OldId:=StrToInt('$'+S1);
      except
       OldId:=0;
      end;
      if OldId<>0 then
       begin
        H:=CreateEvent(Nil, False, True, PChar(Format(TagAtom, [OldId])));
        OldId:=WaitForSingleObject(H, 0);
        CloseHandle(H);
        if OldId=0 then
         begin
          S1:=AppendFileToPath(S, Rec.Name);
          case MessageDlg(FmtLoadStr1(5682, [S1]), mtInformation, mbYesNoCancel, 0) of
           mrYes: with TSaveDialog.Create(Application) do
                   try
                    Title:=LoadStr1(770);
                    DefaultExt:=Copy(Ext, 2, MaxInt);  { drop '.' }
                    Filter:='*'+Ext+'|*'+Ext;
                    while Execute and not MoveFile(PChar(S1), PChar(FileName)) do
                     MessageDlg(LoadStr1(4427), mtError, [mbOk], 0);
                   finally
                    Free;
                   end;
           mrNo: DeleteFile(S1);
          end;
         end;
       end;
     end;
    DosError:=FindNext(Rec);
   end;
 finally
  FindClose(Rec);
 end;
 // No comments as to why close handle is commented out...
 // H:=
 CreateEvent(Nil, False, False, PChar(Format(TagAtom, [GetCurrentProcessId])));
// CloseHandle(H);
end;

function GetFileRoot(Q: QObject) : QFileObject;
begin
 while Q.Flags and ofFileLink = 0 do
  begin
   Q:=Q.FParent;
   if Q=Nil then
    begin
     Result:=Nil;
     Exit;
    end;
  end;
 Result:=Q as QFileObject;
end;

procedure QFileObject.OperationInScene(Aj: TAjScene; PosRel: Integer);
var
 F: TQForm1;
 Extra: TCustomForm;
 SendMsg: Integer;
 Q: QObject;
begin
 SendMsg:=0;
 case Aj of
  asModifie: SendMsg:=wp_ObjectModified;
  asRetire: if PosRel=0 then
             SendMsg:=wp_ObjectRemoved;
 end;
 if SendMsg<>0 then
  begin
   if g_WorkingExplorer=Nil then
    Extra:=Nil
   else
    begin
     Extra:=ValidParentForm(g_WorkingExplorer);
     Extra.Perform(wm_InternalMessage, SendMsg, LongInt(Self));
    end;
   Q:=Self;
   repeat
    if Q is QFileObject then
     begin
      F:=Nil;
      while EnumObjectWindow(F) do
       if F<>Extra then
        F.Perform(wm_InternalMessage, SendMsg, LongInt(Self));
      if (g_Form1.Explorer.Roots.Count>0) and (Q=g_Form1.Explorer.Roots[0]) then
       g_Form1.Perform(wm_InternalMessage, SendMsg, LongInt(Self));
     end;
    Q:=Q.FParent;
   until Q=Nil;
  end;
 inherited;
end;

function QFileObject.FindFile(const nName: String) : QFileObject;
var
 Q: QObject;
begin
 Q:=SubElements.FindName(nName);
 if (Q<>Nil) and (Q is QFileObject) then
  Result:=QFileObject(Q)
 else
  Result:=Nil;
end;

function QFileObject.LoadSibling(const nName: String) : QFileObject;
var
 S: String;
begin
 Result:=Nil;
 if Flags and ofFileLink = 0 then
  begin
   if (FParent=Nil) or not (FParent is QFileObject) then Exit;
   Result:=QFileObject(FParent).FindFile(nName);
  end
 else
  begin
   if Filename='' then Exit;
   S:=ExtractFilePath(Filename)+nName;
   StringReplace(S,'/','\',[rfReplaceAll]);
   if FileExists(S) then
    Result:=ExactFileLink(S, Nil, False);
  end;
 Result.AddRef(+1);
end;

procedure QFileObject.ChangeToObjectGameMode;
begin
 ChangeGameModeStr(Specifics.Values['Game'], True);
end;

function QFileObject.GetObjectGameCode: Char;
begin
 Result:=GetGameCode(Specifics.Values['Game']);
end;

procedure QFileObject.SetObjectGameCode(nCode: Char);
begin
 Specifics.Values['Game']:=GetGameName(nCode);
end;

function QFileObject.NeedObjectGameCode: Char;
begin
 Result:=ObjectGameCode;
 if Result=mjAny then
  Raise EError(5574);
end;

 {------------------------}

function qSaveFile(self, args: PyObject) : PyObject; cdecl;
var
 alt: PChar;
 astext: PyObject;
 Format: Integer;
begin
 try
  Result:=Nil;
  alt:='';
  astext:=Nil;
  if not PyArg_ParseTupleX(args, '|sO', [@alt, @astext]) then Exit;
  with QkObjFromPyObj(self) as QFileObject do
   begin
    Acces;
    if astext=Nil then
     Format:=RecommendFormat
    else
     if PyObject_IsTrue(astext) then
      Format:=rf_AsText
     else
      Format:=rf_Default;
    SaveInFile(Format, alt);
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qConversion(self, args: PyObject) : PyObject; cdecl;
var
 convert: PChar;
 obj: PyObject;
 I: Integer;
 ConvertClass: QFileObjectClass;
 Source: QFileObject;
begin
 try
  Result:=Nil;
  convert:=Nil;
  if not PyArg_ParseTupleX(args, '|s', [@convert]) then
   Exit;
  Source:=QkObjFromPyObj(self) as QFileObject;
  if convert=Nil then
   begin
    Result:=PyList_New(0);
    if Result=Nil then Exit;
    I:=1;
    repeat
     ConvertClass:=Source.TestConversionType(I);
     if ConvertClass=Nil then Break;
     obj:=PyString_FromString(PChar(ConvertClass.TypeInfo));
     if obj=Nil then
      begin
       Py_DECREF(Result);
       Exit;
      end;
     PyList_Append(Result, obj);
     Py_DECREF(obj);
     Inc(I);
    until False;
   end
  else
   with BuildFileRoot(Source.Name + convert, Nil) do
    begin
     Result:=@PythonObj;
     Py_INCREF(Result);
     try
      if not ConversionFrom(Source) then
       Raise EErrorFmt(5673, [Source.Name+Source.TypeInfo, Name+TypeInfo]);
     except
      Py_DECREF(Result);
      Raise;
     end;
    end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qOpeninWindow(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with QkObjFromPyObj(self) as QFileObject do
    OpenStandAloneWindow(Nil, False);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..2] of TyMethodDef =
  ((ml_name: 'savefile';    ml_meth: qSaveFile;    ml_flags: METH_VARARGS),
   (ml_name: 'conversion';  ml_meth: qConversion;  ml_flags: METH_VARARGS),
   (ml_name: 'openinnewwindow'; ml_meth: qOpeninWindow; ml_flags: METH_VARARGS));

function QFileObject.PyGetAttr(attr: PChar) : PyObject;
var
 I: Integer;
 S: String;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(MethodTable) to High(MethodTable) do
  if StrComp(attr, MethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(MethodTable[I], @PythonObj);
    Exit;
   end;
 case attr[0] of
  'f': if StrComp(attr, 'filename') = 0 then
        begin
         Result:=PyString_FromString(PChar(Filename));
         Exit;
        end;
  't': if StrComp(attr, 'tempfilename') = 0 then
        begin
         SetLength(S, MAX_PATH);
         S[1]:=#0;
         GetTempPath(MAX_PATH, PChar(S));
         SetLength(S, StrLen(PChar(S)));
         S:=AppendFileToPath(S, Format('auto-save-%x-%x%s', [GetCurrentProcessId, LongInt(Self), TypeInfo]));
         Result:=PyString_FromString(PChar(S));
         Exit;
        end;
 end;
end;

function QFileObject.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
 P: PChar;
begin
 Result:=inherited PySetAttr(attr, value);
 if not Result then
  case attr[0] of
   'f': if StrComp(attr, 'filename') = 0 then
         begin
          P:=PyString_AsString(value);
          if P=Nil then Exit;
          Filename:=P;
          Result:=True;
          Exit;
         end;
  end;
end;

 {------------------------}

function TQForm1.AssignObject;
begin
 Result:=(FileObject=Nil) and (State<>cmOwnExplorer);
 if Result then
  ForcedAssignObject(Q, State);
end;

function TQForm1.GetConfigStr: String;
begin
 GetConfigStr:='';
end;

function TQForm1.GetSpecTbExtra;
begin
 Result:=GetConfigStr;
 if Result<>'' then
  Result:=GetTbExtra(Result, Specific);
end;

{function TQForm1.GetSetupObject : QObject;
var
 S: String;
begin
 S:=GetConfigStr;
 if S='' then
  Result:=Nil
 else
  Result:=SetupSubSetEx(ssToolbars, S, False);
end;}

procedure TQForm1.ReadSetupInformation;
begin
 if Level=scNoToolbars then Exit;
 if Level>=scToolbars then
  begin
   SaveToolbars(False);
   LoadToolbars;
  end
 else
  if Level>=scNormal then
   UpdateToolbarSetup;
end;

procedure TQForm1.ForcedAssignObject(Q: QFileObject; State: TFileObjectWndState);
var
 E: TEtatObjet;
begin
 FFileObject.AddRef(-1);
 FFileObject:=Q;
 FFileObject.AddRef(+1);
 FWndState:=State;
{FModified:=False;}
{if State in StandAloneWndState then
  if Q.FParent=Nil then
   Q.CurrentlySaved:=cs_NotSaved
  else
   Q.CurrentlySaved:=cs_Explorer;}
 Caption:=FileObject.Name;
 FFileObject.ObjectState(E);
 SetFormIcon(E.IndexImage);
end;

function TQForm1.GetViewForm: TCustomForm;
begin
 GetViewForm:=ValidParentForm(FAttachPanel);
end;

procedure TQForm1.SaveToolbars;
var
 S: String;
 DockForm: TQkForm;
 F: TCustomForm;
begin
 S:=GetConfigStr;
 if S<>'' then
  begin
   F:=GetParentForm(Self);
   if not (F is TQkForm) then Exit;
   DockForm:=TQkForm(F);
   DockForm.SavePositionTb(S, DockForm<>Self,
    TControl(DockForm.Perform(wm_InternalMessage, wp_TargetExplorer, 0)));
   if Remove and (DockForm<>Self) then
    DockForm.RemoveSubTbs;
  end;
end;

procedure TQForm1.LoadToolbars;
var
 S: String;
 DockForm: TQkForm;
begin
 S:=GetConfigStr;
 if S<>'' then
  begin
   DockForm:=ValidParentForm(Self) as TQkForm;
   DockForm.RestorePositionTb(S, DockForm<>Self,
    TControl(DockForm.Perform(wm_InternalMessage, wp_TargetExplorer, 0)));
  end;
end;

procedure TQForm1.FormClose(Sender: TObject; var Action: TCloseAction);
{var
 WindowPlacement: TWindowPlacement;}
begin
 SaveToolbars(True);
 if WndState in StandAloneWndState then
  begin
   if not FDisabledAlign then
    begin
     DisableAlign;
     FDisabledAlign:=True;
    end;
  {WindowPlacement.Length := SizeOf(WindowPlacement);
   GetWindowPlacement(Handle, @WindowPlacement);}
   DefPosition := {WindowPlacement.rcNormalPosition;}RestoredRect;
   if (FileObject<>Nil) and (FAttachPanel<>Nil) then
    begin
     SetWindowPos(GetViewForm.Handle, Handle, 0,0,0,0,
      swp_NoActivate or swp_NoMove or swp_NoSize or swp_ShowWindow);
     {WindowPlacement.rcNormalPosition}RestoredRect := GetSmallPosition;
     {PlaySound('RestoreDown', 0, snd_Alias or snd_Async or snd_NoDefault or snd_NoStop);}
     {WindowPlacement.ShowCmd:=sw_ShowNormal;}
    {SetWindowPlacement(Handle, @WindowPlacement);}
     WindowState:=wsNormal;
    end;
  end;
{else
  if FTbToolbar<>Nil then
   with FTbToolbar do
    begin
     FTbDockedTo:=DockedTo;
     FTbPos:=DockPos;
     FTbRow:=DockRow;
     FTbBounds:=FloatingRect;
     FTbToolbar.Free;
     FTbToolbar:=Nil;
    end;}
 if FFileObject<>Nil then
  begin
   if WndState in StandAloneWndState then
    begin
     FFileObject.CloseUndo;
    {MainMenu1.UnMerge(MainObjMenu);}
     CopyToolbar(Nil, MenuToolbar);
     if FAttachPanel<>Nil then
      PostMessage(GetViewForm.Handle, wm_InternalMessage, wp_AfficherObjet, LongInt(FFileObject));
    end
   else
   {g_Form1.Menu.UnMerge(MainObjMenu)};
   FFileObject.AddRef(-1);
   FFileObject:=Nil;
  end;
end;

procedure TQForm1.Close1Click(Sender: TObject);
begin
 Close;
end;

function TQForm1.GetSmallPosition : TRect;
begin
 with AttachPanel do
  begin
   Result:=ClientRect;
   Result.TopLeft:=ClientToScreen(Result.TopLeft);
   Result.BottomRight:=ClientToScreen(Result.BottomRight);
  end;
 AdjustWindowRectEx(Result, GetWindowLong(Handle, GWL_STYLE),
   Menu <> nil, GetWindowLong(Handle, GWL_EXSTYLE));
end;

procedure TQForm1.wmInternalMessage(var Msg: TMessage);
var
 Info: TFileObjectClassInfo;
{S: String;}
begin
 case Msg.wParam of
  wp_FormActivate:
    if FileObject<>Nil then
     begin
      if WndState in StandAloneWndState then
       begin
        if FAttachPanel<>Nil then
         begin
          WindowState:=wsMaximized;
          RestoredRect:=DefPosition;
         end;
        if not (Self is TPyForm) then
         begin
          CopyToolbar(g_Form1.ToolbarMenu1, MenuToolbar);
          MenuToolbar.Visible:=True;
         end;
        Show;
       end;
      if not FInitialized then
       begin
        FInitialized:=True;
        ReadSetupInformation(scNoToolbars);
       end;
      if FDisabledAlign then
       begin
        EnableAlign;
        FDisabledAlign:=False;
       end;
      LoadToolbars;
      PostMessage(Handle, wm_InternalMessage, wp_AfficherObjet, 0);
     end;
  wp_AfficherInfos, wp_AfficherObjet:
    if FileObject<>Nil then
     begin
      Caption:=FileObject.Name;
     {if IsIconic(Handle) then
       MarsCaption1.ApplicationName:=''
      else
       begin}
        FileObject.FileObjectClassInfo(Info);
        MarsCap.AppCaption:=FmtLoadStr1(5123, [Info.FileObjectDescriptionText]);
        UpdateMarsCap;
      {end;}
      RedrawWindow(Handle, Nil, 0, rdw_Invalidate or rdw_Frame);
     end;
  wp_FileMenu:
    Save(Msg.lParam);
  wp_EditMsg:
    Msg.Result:=EditMenuCommand(Msg.lParam);
  wp_ObjectModified:
    if QObject(Msg.lParam)=FileObject then
     PostMessage(Handle, wm_InternalMessage, wp_AfficherObjet, 0);
  wp_ObjectRemoved:
    if QObject(Msg.lParam)=FileObject then
     CloseNow;
(*wp_LoadToolbars:
    begin
     S:=GetConfigStr;
     if S<>'' then RestorePositionTb(S, False);
    end;*)
  wp_SetupChanged:
    ReadSetupInformation(Msg.lParam);
 else
  inherited;
 end;
end;

procedure TQForm1.CloseNow;
begin
 Close;
 if FileObject<>Nil then
  Abort;   { close was cancelled }
end;

procedure TQForm1.FormCreate(Sender: TObject);
var
 C: TColor;
begin
 DefPosition:=BoundsRect;
 MarsCap.ActiveBeginColor:=clNavy;
 MarsCap.ActiveEndColor:=clNavy;
 UpdateMarsCap;
 C:=GetDockColor;
 topdock.Color:=C;
 leftdock.Color:=C;
 rightdock.Color:=C;
 bottomdock.Color:=C;
end;

procedure TQForm1.cmSysColorChange(var Msg: TWMSysCommand);
var
 C: TColor;
begin
 C:=GetDockColor;
 topdock.Color:=C;
 leftdock.Color:=C;
 rightdock.Color:=C;
 bottomdock.Color:=C;
end;

procedure TQForm1.wmSysCommand;
begin
 case Msg.CmdType of
  sc_Minimize:
   begin
    Application.Minimize;
    Exit;
   end;
  sc_Maximize, sc_Restore:
   PostMessage(Handle, wm_InternalMessage, wp_AfficherInfos, 0);
 end;
 inherited;
end;

procedure TQForm1.Save(AskName: Integer);
var
 Dup: QFileObject;
begin
 if FFileObject=Nil then
  Exit;
 if AskName=fm_SaveTagOnly then
  begin
   SaveTag(FFileObject);
   Exit;
  end;
 if (AskName<>fm_SaveAsFile) and (FAttachPanel<>Nil) then
  begin  { non stand-alone object : saved with parent }
   GetViewForm.Perform(wm_InternalMessage, wp_FileMenu, AskName);
   Exit;
  end;
 if (AskName=fm_Save) and (FFileObject.Filename='') then
  begin  { stand-alone newly created object : ask if we must save in QuArK Explorer }
   case MessageDlg(LoadStr1(5601), mtConfirmation, mbYesNoCancel, 0) of
    mrYes: begin
            g_Form1.FileMenu.PopupComponent:=Self;
            g_Form1.Saveinnewentry1Click(Nil);
            Exit;
           end;
    mrNo: ;
    else Abort;
   end;
  end;
 Dup:=SaveObject(FFileObject, AskName, -Ord(FAttachPanel<>Nil), Self);
 if Dup<>Nil then
  Detach(Dup);
end;

procedure TQForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
 Explorer: TQkExplorer;
begin
 GlobalDoAccept{(Self)};
 if (WndState=cmNone) or (FAttachPanel<>Nil) then
  Exit;  { ignore ofModified if attached to a parent }
 if (FFileObject<>Nil) and (FFileObject.Flags and ofModified<>0) then
  begin
   ActivateNow(Self);
   case MessageDlg(LoadStr1(5211), mtConfirmation, mbYesNoCancel, 0) of
    mrYes: begin
            Explorer:=TQkExplorer(
             Perform(wm_InternalMessage, wp_TargetExplorer, 0));
            if Explorer<>Nil then
             Explorer.CloseUndoObjects;
            Save(fm_Save);
           end;
    mrNo: ;
   else CanClose:=False;
   end;
  end;
end;

(*procedure TQForm1.OpenWithExplorer;
var
 Panel2: TPanel;
begin
 if Explorer=Nil then
  begin
   Explorer:=TQkExplorer2.Create(Self);
   Explorer.Width:=160;
   Explorer.Align:=alLeft;
   Explorer.Parent:=Self;
   Panel2:=TPanel.Create(Self);
   Panel2.BeverOuter:=bsNone;
   Panel2.Caption:='';
   Panel2.Align:=alClient;
   Panel2.Parent:=Self;
   Explorer.ViewPanel:=Panel2;
   Explorer.MarsCaption:=MarsCaption1;
   Explorer.ObjToolbar:=ObjToolbar;
  end
 else
  begin
   Explorer.Show;
   Explorer.ViewPanel.Show;
  end;

end;*)

procedure TQForm1.Detach;
var
 OldObj: QFileObject;
begin
 OldObj:=FFileObject;
 if New=Nil then
  begin
   New:=OldObj.Clone(Nil, False) as QFileObject;
   New.AddRef(+1);
  end;
 try
  if FAttachPanel<>Nil then
   begin
    PostMessage(GetViewForm.Handle, wm_InternalMessage, wp_AfficherObjet, LongInt(OldObj));
    FAttachPanel:=Nil;
   end;
  FFileObject.AddRef(-1);
  FFileObject:=Nil;
  if AssignObject(New, WndState) then
   PostMessage(Handle, wm_InternalMessage, wp_FormActivate, 0)
  else
   begin  { must open in another window }
    CloseNow;
    New.OpenStandAloneWindow(Nil, False);
   end;
 finally
  New.AddRef(-1);
 end;
end;

procedure TQForm1.Attach(nViewPanel: TWinControl);
begin
 FAttachPanel:=nViewPanel;
end;

procedure TQForm1.CheckUniqueWindow;
var
 F: TQForm1;
begin
 F:=Nil;
 while FileObject.EnumObjectWindow(F) do
  if F<>Self then
   Raise EError(5527);
end;

function TQForm1.EditMenuCommand(Cmd: Integer) : Integer;
begin
 Result:=edOk;
 case Cmd of
  edEdEnable:   { which edit menu commands are to be enabled ? }
    begin
     Result:=edOk or edCopy;
     {no other menu command available}
    end;
  edCopy:                   { copy }
    if FileObject<>Nil then
     FileObject.CopyToClipboard;
  edGetObject, edGetRoot, edGetMacroObject:
    Result:=GetObjectResult(FFileObject);
 else
  Result:=0;
 end;
end;

function TQForm1.TmpSwapFileObject(New: QFileObject) : QFileObject;
begin
 Result:=FFileObject;
 FFileObject:=New;
end;

end.

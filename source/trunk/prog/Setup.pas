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
{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.7  2000/05/21 13:11:50  decker_dk
Find new shaders and misc.

Revision 1.6  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.5  2000/05/11 22:10:17  alexander
added comment

Revision 1.4  2000/05/04 23:56:01  alexander
added: game enumeration for Soldier of Fortune "E"

}

unit Setup;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Dialogs, Controls, Reg2,
     QkObjects, qmath, QkFileObjects;

const
{FIXME: This should not be constants, but read from
 the games config: entry }
{DECKER: These should be made into bit-values instead. Something like:
  mjQuake     = $00000001;
  mjHexen     = $00000002;
  mjQuake2    = $00000004;
  ...
  mjSOF       = $00000080;
  mjQ3A       = $00000100;
  mjAny       = $FFFFFFFF;
  mjNotQuake1 = (mjAny - mjQuake1);
  mjNotQuake2 = (mjAny - mjQuake2);
 This so it will be easier to implement support for games, which could for
 instance have a mixture of file-formats like .WADs _and_ .SHADERS.
 Of cause this will only allow up to 32 different games, but shouldn't that
 be enough? }
 mjQuake    = '1';
 mjHexen    = '2';
 mjHalfLife = '3';
 mjQuake2   = 'A';
 mjHeretic2 = 'B';
 mjSin      = 'C';
 mjKingPin  = 'D';
 mjSOF      = 'E';
 mjQ3A      = 'a';

 mjAny       = #1;
 mjNotQuake2 = #2;
 mjNotQuake1 = #255;

type
 TListeCouleurs =
  (lcVueXZ, lcVueXY, lcSelXZ, lcSelXY, lcOutOfView, lcAxes, lcGridXZ, lcGridXY, lcGridLines,
   lcBrushEntity, lcDuplicator, lcTag, lcGrayImage, lcBSP, lcDigger, lcBezier);
 TSetupSet =
  (ssGeneral, ssGames, ssMap, ssModel, ssToolbars{, ssTempData});
 TSetupSetArray = array[TSetupSet] of QObject;

const
 SetupSetName : array[TSetupSet] of String =
  ('General', 'Games', 'Map', 'Model', 'Toolbars and Menus'{, 'temp'});

type
  QConfig = class(QObject)
            public
              class function TypeInfo: String; override;
              function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
              procedure EtatObjet(var E: TEtatObjet); override;
            end;

var
 ApplicationPath: String;
 SetupSet: TSetupSetArray;

const  { for SetupChanged }
 scInit      = 0;
 scMinimal   = 1;
 scNormal    = 2;
 scToolbars  = 3;
 scGame      = 4;
 scConfigDlg = 5;
 scAddOns    = 6;
 scMaximal   = 7;
 scNoToolbars= 8;

 {------------------------}

procedure InitSetup;
procedure InitApplicationPath;
procedure SetupChanged(Level: Integer);
function SetupSubSet(Root: TSetupSet; SubSet: String) : QObject;
function SetupSubSetEx(Root: TSetupSet; SubSet: String; Create: Boolean) : QObject;
function SetupGameSet : QObject;
procedure UpdateSetup(Level: Integer);
procedure SaveSetupNow;
function MakeAddOnsList : QFileObject;  { includes the file loaded in Form1 }
procedure UpdateForm1Root;
procedure UpdateAddOnsContent;
procedure CloseAddonsList;  { don't call this when toolboxes are open }
procedure AddAddOn(NewAddOn: QObject);
function GetSetupPath(Path: String; var Spec: String; var Q: QObject) : Boolean;
procedure MakeAssociations(Config: Qobject);
procedure RefreshAssociations(Forced: Boolean);
procedure RemoveAssociations;
function AssociationWithQuArK(const FileExt: String) : Boolean;

 {------------------------}

function CharModeJeu: Char;
function ModeJeuQuake2: Boolean;
function CurrentQuake1Mode: Char;
function CurrentQuake2Mode: Char;
function GetGameName(nMode: Char) : String;
procedure ChangeGameMode(nMode: Char; Confirm: Boolean);
procedure ChangeGameModeStr(const nMode: String; Confirm: Boolean);
function GetGameCode(const nMode: String) : Char;
function MapColors(L: TListeCouleurs) : TColor;
{function GetIncludePath: String;}
function InternalVersion : Single;
function GameModeOk(nMode: Char) : Boolean;

 {------------------------}

implementation

uses QkMapObjects, Travail, Game, QkGroup, QkForm, Qk1,
     ToolBox1, Toolbar1, QkQuakeCtx, Quarkx, Python,
     PyObjects, PyForms, Qk3D, Ed3DFX;

const
 SetupFileName    = 'Setup.qrk';
 DefaultsFileName = 'Defaults.qrk';

var
 {SetupFileName1,} DefaultsFileName1: String;
 AddOns: QFileObject = Nil;
{SetupModified: Boolean;}

 {------------------------}

class function QConfig.TypeInfo: String;
begin
 TypeInfo:=':config';
end;

procedure QConfig.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 if TvParent=Nil then
  E.IndexImage:=iiCfgFolder
 else
  E.IndexImage:=iiCfg;
end;

function QConfig.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 if Q is QConfig then
  Result:=[ieDisplay, ieCanDrop]
 else
  Result:=ieResult[Q is QToolbar];
 if Q.Specifics.Values['Form']='' then
  Exclude(Result, ieDisplay);
end;

 {------------------------}

(*procedure BrowsePath(Q: QObject);
var
 I: Integer;
 S: String;
begin
 if Q is QFileObject then
  begin
   Q.Acces;
   for I:=0 to Q.SubElements.Count-1 do
    BrowsePath(Q.SubElements[I]);
  end
 else
  if Q is QConfig then
   begin
    Q.Acces;
    for I:=0 to Q.Specifics.Count-1 do
     begin
      S:=Q.Specifics[I];
      if Copy(S,1,12) = 'IncludePath=' then
       begin
        S:=Copy(S,13,MaxInt);
        if S='' then Continue;
        if (S[1]<>'\') and (Pos(':',S)=0) then
         S:=ApplicationPath+S;
        CurrentIncludePath:=CurrentIncludePath + ';' + S;
       end;
     end;
   end;
end;*)

(*function GetIncludePath: String;
begin
 if CurrentIncludePath<>'' then  { are we already in a call to GetIncludePath ? }
  GetIncludePath:=CurrentIncludePath
 else
  try
   CurrentIncludePath:=ApplicationPath;
   BrowsePath(SetupQrk);
   Result:=CurrentIncludePath;
  finally
   CurrentIncludePath:='';
  end;
end;*)

(*function OfficialPath(const S: String) : String;
var
 I: Integer;
begin
 Result:=ExpandFileName(S);
 if (Result<>'') and (Result[Length(Result)]='\') then
  SetLength(Result, Length(Result)-1);
 if (Result<>'') and (Result[Length(Result)]=':') then
  Result:=Result+'\';
end;*)

 {------------------------}

function SetupSubSetEx(Root: TSetupSet; SubSet: String; Create: Boolean) : QObject;
begin
 Result:=SetupSet[Root].SubElements.FindName(SubSet+':config');
 if (Result=Nil) and Create then
  begin
   Result:=QConfig.Create(SubSet, SetupSet[Root]);
   SetupSet[Root].SubElements.Add(Result);
  end;
end;

function SetupSubSet(Root: TSetupSet; SubSet: String) : QObject;
begin
 Result:=SetupSet[Root].SubElements.FindName(SubSet+':config');
 if Result=Nil then
  Raise EErrorFmt(5205, [SetupSetName[Root]+':'+SubSet]);
end;

function SetupGameSet : QObject;
begin
 SetupGameSet:=SetupSubSet(ssGames, SetupSet[ssGames].Specifics.Values['GameCfg']);
end;

function GetSetupPath(Path: String; var Spec: String; var Q: QObject) : Boolean;
var
 P: Integer;
 S: TSetupSet;
begin
 Result:=False;
 Q:=Nil;
 P:=Pos(':', Path);
 if P=0 then Exit;
 if P=1 then
  Q:=SetupGameSet
 else
  begin
   for S:=Low(S) to High(S) do
    if CompareText(SetupSetName[S], Copy(Path,1,P-1)) = 0 then
     begin
      Q:=SetupSet[S];
      Break;
     end;
   if Q=Nil then Exit;
  end;
 repeat
  System.Delete(Path, 1, P);
  P:=Pos(':', Path);
  if P=0 then Break;
  Q:=Q.SubElements.FindName(Copy(Path,1,P-1)+':config');
  if Q=Nil then Exit;
 until False;
 Spec:=Path;
 Result:=True;
end;

procedure Mix(Target, Q: QObject);
var
 I, P: Integer;
 S, Spec, Arg: String;
 Test, NewTarget: QObject;
begin 
 Q.Acces;
 for I:=0 to Q.Specifics.Count-1 do
  begin
   S:=Q.Specifics[I];
   P:=Pos('=',S);
   Spec:=Copy(S,1,P-1);
   Arg:=Copy(S,P+1,MaxInt);
 (*if (P>1) and (S[P-1]='+') then  { concat with previous values }
    Arg := Target.Specifics.Values[Spec] + Arg;*)
   Target.Specifics.Values[Spec] := Arg;
   if Arg='' then
    Target.Specifics.Add(Spec+'=');
  end;
 for I:=0 to Q.SubElements.Count-1 do
  begin
   Test:=Q.SubElements[I];
  {if Test is QConfig then}
    begin
     NewTarget:=Target.SubElements.FindName(Test.Name+Test.TypeInfo);
     if NewTarget=Nil then
      begin
       NewTarget:=ConstructQObject(Test.Name+Test.TypeInfo, Target);
       Target.SubElements.Add(NewTarget);
      end;
     Mix(NewTarget, Test);
    end;
  end;
end;

procedure BrowseConfig(Q: QObject);
var
 I: Integer;
 T: TSetupSet;
begin
 if Q is QFileObject then
  begin       
   Q.Acces;
   DebutTravail(5445, Q.SubElements.Count); try
   for I:=0 to Q.SubElements.Count-1 do
    begin
     BrowseConfig(Q.SubElements[I]);
     ProgresTravail;
    end; 
   finally FinTravail; end;
  end
 else
  if Q is QConfig then
   for T:=Low(T) to High(T) do
    if CompareText(Q.Name, SetupSetName[T]) = 0 then
     begin
      if SetupSet[T]=Nil then
       begin
        SetupSet[T]:=QConfig.Create(Q.Name, Nil);
        SetupSet[T].AddRef(+1);
       end;
       { mix Q into this SetupSet }
      Mix(SetupSet[T], Q);
     end;
end;

procedure InitApplicationPath;
var
 Z: array[0..MAX_PATH] of Char;
 I: Integer;
begin
 I:=GetEnvironmentVariable('QUARKPATH', Z, SizeOf(Z));
 if I=0 then
  ApplicationPath:=ExtractFilePath(Application.ExeName)
 else
  begin
   SetString(ApplicationPath, Z, I);
   if ApplicationPath[I]<>'\' then
    ApplicationPath:=ApplicationPath+'\';
  end;
(*{$IFDEF Debug}
 if Application.ExeName='C:\DELPHI\EXE\QUARK5.EXE' then
  ApplicationPath:='d:\delphi\program2\quake\qk5c\';
  //ApplicationPath:='d:\delphi\program2\quake\install\quark\';
 {$ENDIF}*)
end;

procedure InitSetup;
var
 SetupQrk: QFileObject;
 V1, V2: String;
 T: TSetupSet;
 Version: Reel;
begin
 for T:=High(T) downto Low(T) do
  if SetupSet[T]<>Nil then
   begin
    SetupSet[T].AddRef(-1);
    SetupSet[T]:=Nil;
   end;
{SetupSet[ssTempData]:=QConfig.Create(SetupSetName[ssTempData], Nil);
 SetupSet[ssTempData].AddRef(+1);}
 DefaultsFileName1:='';
{ SetupFileName1:=ApplicationPath+SetupFileName;  { default }

  { loads Defaults.qrk }
 try
  SetupQrk:=LienFichierQObject(DefaultsFileName, Nil, False);
  SetupQrk.AddRef(+1);
  try
   DefaultsFileName1:=SetupQrk.NomFichier;
   BrowseConfig(SetupQrk);  { copies this setup data into memory }
  finally
   SetupQrk.AddRef(-1);
  end;                                     
 except
  on E: Exception do
   begin
    Form1.MessageException(E, LoadStr1(5204), [mbOk]);
    Halt(1);   { cannot load Defaults.qrk - fatal error }
    Exit;
   end;
 end;

  { checks loaded data }
 if SetupSet[ssGeneral]<>Nil then
  begin   { checks version }
   V1:=QuarkVersion;
   V2:=SetupSet[ssGeneral].Specifics.Values['Version'];
   if V1 <> V2 then
    begin
     MessageDlg(FmtLoadStr1(5206, [V1, V2]), mtError, [mbOk], 0);
     Halt(1);   { wrong version of Defaults.qrk }
    end;
  end;
 for T:=Low(T) to High(T) do
  if SetupSet[T]=Nil then  { checks ":config" objects }
   begin
    MessageDlg(FmtLoadStr1(5205, [SetupSetName[T]]), mtError, [mbOk], 0);
    Halt(1);   { missing ":config" object }
   end;
 Version:=InternalVersion;

  { loads Setup.qrk over the default configuration }
 try
  SetupQrk:=LienFichierQObject(SetupFileName, Nil, False);
  SetupQrk.AddRef(+1);
  try
  {SetupFileName1:=SetupQrk.NomFichier;}
   BrowseConfig(SetupQrk);  { copies this setup data into memory }
  finally
   SetupQrk.AddRef(-1);
  end;
 except
  { could not load Setup.qrk - this is not an error, continue execution }
 end;

 if SetupSet[ssGeneral].GetFloatSpec('RunVersion', 5.901)<5.9005 then
  RefreshAssociations(True);
 SetupSet[ssGeneral].SetFloatSpec('RunVersion', Version);
 SetupSet[ssGeneral].Specifics.Values['Date']:=DateToStr(Date);
 SetupChanged({scMaximal} {scMinimal} scInit);
end;

procedure SetupChanged;
var
 fnt: PyObject;
 S: String;
{SetupInfo: PyObject;}
begin
 if Level>=scAddOns then
  ClearGameBuffers(False)
 else
  ClearGameBuffer1;
 if (Level>=scAddOns) or (Level=scGame) then
  LibererMemoireTextures;

  { initializes QuArK depending on the setup information }
 Info.DefWhiteOnBlack:=SetupSubSet(ssMap, 'Colors').Specifics.Values['InvertedColors']<>'';
 with SetupSubSet(ssMap, 'Options') do
  begin
   Info.CacherFaces:=Specifics.Values['HideFaces']<>'';
   Info.TexAntiScroll:=IntSpec['TexAntiScroll'];
  end;
 S:=SetupSubSet(ssGeneral, 'Display').Specifics.Values['MarsCaption'];
 if S='?' then
  begin
   if MessageDlg(LoadStr1(5690), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    S:='1'
   else
    S:='';
   SetupSubSet(ssGeneral, 'Display').Specifics.Values['MarsCaption']:=S;
  end;
 SetMarsCapActive(S<>'');

  { stores the setup infos into the Quarkx Python module }
(*SetupInfo:=PyList_New(Ord(High(T))+1); try
 for T:=Low(T) to High(T) do
  PyList_SetItem(SetupInfo, Ord(T), GetPyObj(SetupSet[T]));
 PyDict_SetItemString(QuarkxDict, 'setupset', SetupInfo);
 finally Py_DECREF(SetupInfo); end;*)

  { sends the reset message to all windows }
 PosteMessageFiches(wp_SetupChanged, Level);
 if Level = scInit then Exit;

  { sends the reset message to Python }
 fnt:=PyDict_GetItemString(QuarkxDict, 'setupchanged');
 Py_XDECREF(GetPythonValue(fnt, Py_BuildValueX('(i)', [Level]), True));
end;

procedure StoreDiff(Root, New, Old: QObject);
var
 I, P: Integer;
 S: String;
 Cfg, New1, Old1: QObject;
begin
 Cfg:=ConstructQObject(New.Name+New.TypeInfo, Root);
 Root.SubElements.Add(Cfg);
 for I:=0 to New.Specifics.Count-1 do
  begin
   S:=New.Specifics[I];
   P:=Pos('=', S);
   if Copy(S,P+1,MaxInt) <> Old.Specifics.Values[Copy(S,1,P-1)] then
    Cfg.Specifics.Add(S);  { this Specifics has been modified }
  end;
 for I:=0 to Old.Specifics.Count-1 do
  begin
   S:=Old.Specifics[I];
   P:=Pos('=', S);
   if (P<Length(S)) and (New.Specifics.IndexOfName(Copy(S,1,P-1))<0) then
    Cfg.Specifics.Add(Copy(S,1,P));  { this Specifics has been removed }
  end;
 for I:=0 to New.SubElements.Count-1 do
  begin
   New1:=New.SubElements[I];
   Old1:=Old.SubElements.FindName(New1.Name+New1.TypeInfo);
   if Old1=Nil then
    Cfg.SubElements.Add(New1)   { it's a new sub-item }
   else
    StoreDiff(Cfg, New1, Old1);  { store only differences }
  end;
 for I:=0 to Old.SubElements.Count-1 do
  begin
   Old1:=Old.SubElements[I];
   if New.SubElements.FindName(Old1.Name+Old1.TypeInfo) = Nil then
    begin  { "Old1" has been deleted from the new version }
     New1:=ConstructQObject(Old1.Name+Old1.TypeInfo, Cfg);
     Cfg.SubElements.Add(New1);  { adds an empty object to mean this }
    end;
  end;
 if (Cfg.Specifics.Count=0) and (Cfg.SubElements.Count=0) then
  Root.SubElements.Remove(Cfg);
end;

procedure SaveSetup(Format: Integer; AppPath: String);
var
 SetupQrk: QFileObject;
 T: TSetupSet;
 Q: QObject;
 SetupSet2: TSetupSetArray;
begin
 if DefaultsFileName1='' then Exit;
 SetupSet2:=SetupSet;
 try
  FillChar(SetupSet, SizeOf(SetupSet), 0);  { reset SetupSet }
  try
   SetupQrk:=ExactFileLink(DefaultsFileName1, Nil, False);
   SetupQrk.AddRef(+1);
   try
    BrowseConfig(SetupQrk);  { loads the default setup data }
   finally
    SetupQrk.AddRef(-1);
   end;

   try
     { opens the old setup file }
    SetupQrk:=LienFichierQObject(SetupFileName, Nil, False);
   except
    on EQObjectFileNotFound do  { creates a new setup file if not found }
     SetupQrk:=BuildFileRoot(AppPath+SetupFileName, Nil);
   end;
    { stores the new setup information }
   SetupQrk.AddRef(+1);
   try
    SetupQrk.Acces;
    for T:=Low(T) to {Pred}(High(T)) do  (*{ ignore ssTempData }*)
     begin
      Q:=SetupQrk.SubElements.FindName(SetupSetName[T]+':config');
      if Q<>Nil then
       SetupQrk.SubElements.Remove(Q);  { remove old setup info }
      { stores only the setup data that really changed }
      StoreDiff(SetupQrk, SetupSet2[T], SetupSet[T]);
     end;
    SetupQrk.ReadFormat:=Format;
    SetupQrk.Specifics.Values['Description']:=LoadStr1(5394);
    SetupQrk.TrySavingNow;
   finally
    SetupQrk.AddRef(-1);
   end;

  finally  { frees the temporary loaded SetupSet }
   for T:=High(T) downto Low(T) do
    if SetupSet[T]<>Nil then
     SetupSet[T].AddRef(-1);
  end;
 finally  { restores the saved SetupSet }
  SetupSet:=SetupSet2;
 end;
end;

procedure UpdateSetup;
begin
 SetupChanged(Level);
{SetupModified:=True;}
end;

procedure SaveSetupNow;
begin
 SaveSetup(rf_AsText, ApplicationPath);   { save as text }
end;
(*var
 s: PyObject;
 Path: String;
 P: PChar;
begin
 Path:=ApplicationPath;
 s:=PyDict_GetItemString(QuarkxDict, 'exepath');
 if s<>Nil then
  begin
   P:=PyString_AsString(s);
   if (P<>Nil) and (P^<>#0) then
    begin
     Path:=P;
     if Path[Length(Path)]<>'\' then
      Path:=Path+'\';
    end;
  end;
 SaveSetup(rf_AsText, Path);   { save as text }
end;*)

procedure UpdateForm1Root;
begin
 if AddOns=Nil then Exit;
 if AddOns.Specifics.Values['f1r']<>'' then
  begin
   AddOns.SubElements.Delete(AddOns.SubElements.Count-1);
   AddOns.Specifics.Values['f1r']:='';
   CloseToolBoxes;
  end;
 if Form1.Explorer.Roots.Count>0 then
  begin
   AddOns.SubElements.Add(Form1.Explorer.Roots[0]);
   AddOns.Specifics.Values['f1r']:='1';
  end;
 UpdateAddOnsContent;
end;

procedure AddAddOn(NewAddOn: QObject);
var
 L: TStringList;
 S: String;
begin
 L:=TStringList.Create; try
 L.Text:=SetupGameSet.Specifics.Values['AddOns'];
 L.Add(NewAddOn.Name+NewAddOn.TypeInfo);
 S:=TrimStringList(L, $0D);
 finally L.Free; end;
 SetupGameSet.Specifics.Values['AddOns']:=S;
 if AddOns=Nil then Exit;
 if AddOns.Specifics.Values['f1r']<>'' then
  AddOns.SubElements.Insert(AddOns.SubElements.Count-1, NewAddOn)
 else
  AddOns.SubElements.Add(NewAddOn);
end;

function MakeAddOnsList : QFileObject;
var
 L: TStringList;
 I: Integer;
{Info: PGameBuffer;}
begin
{Info:=GameBuffer(mjAny);}
 if {Info^.}AddOns=Nil then
  begin
   Result:=QExplorerGroup.Create('', Nil);
   Result.AddRef(+1);
   try
    Result.Flags:=Result.Flags or ofFileLink;
    Result.NomFichier:=DefaultsFileName1;
    Result.SubElements.Add(ExactFileLink(DefaultsFileName1, Result, False));
  (*Result.SubElements.Add(LienFichierQObject(
     SetupGameSet.Specifics.Values['Base'], Result));*)
    L:=TStringList.Create; try
    L.Text:=SetupGameSet.Specifics.Values['AddOns'];
    for I:=0 to L.Count-1 do
     try
      Result.SubElements.Add(LienFichierQObject(L[I], Result, False));
     except
      on EQObjectFileNotFound do
       if I=0 then
        GlobalWarning(FmtLoadStr1(5549, [SetupGameSet.Name, L[I]]))
       else
        GlobalWarning(FmtLoadStr1(5557, [L[I]]));
     end;
    finally L.Free; end;
    for I:=0 to Result.SubElements.Count-1 do
     with Result.SubElements[I] do
      Flags:=Flags or ofWarnBeforeChange;
   except
    Result.AddRef(-1);
    Raise;
   end;
  {Info^.}AddOns:=Result;
   UpdateForm1Root;
  end
 else
  Result:={Info^.}AddOns;
 Result.AddRef(+1);
end;

procedure CloseAddonsList;
begin
 AddOns.AddRef(-1);
 AddOns:=Nil;
 UpdateAddOnsContent;
end;

procedure UpdateAddOnsContent;
begin
 ClearQuakeContext;
 PosteMessageFiches(wp_UpdateAddOnsContent, 0);
end;

 {------------------------}

(*type
 TMapColorPalette = array[TListeCouleurs] of TColor;

const
 MapColors_Default : TMapColorPalette =
  ($0080FF80, $0080FFFF, $00008000, $00008080, clSilver, $00F0CAA6, $00008000, $00008080, $00FFD0D0,
   clMaroon, clBlue, clRed, clRed, clTeal, clFuchsia);
 MapColors_B_W : TMapColorPalette =
  (clWhite, clWhite, $0080FF80, $0080FFFF, clSilver, $00F0CAA6, $00008000, $00008080, $00FFD0D0,
   clMaroon, clGreen, $004080FF, clBlue, $00FF8080, clFuchsia);
 MapColors_W_B : TMapColorPalette =
  (clBlack, clBlack, $00006000, $00006060, clGray, $00704A26, $00008000, $00008080, $00800000,
   $0000FFFF, $00FFC000, clRed, $004080FF, $00FFC000, clFuchsia);*)

const
 MapColorNames: array[TListeCouleurs] of String =
  ('ViewXZ',
   'ViewXY',
   'SelXZ',
   'SelXY',
   'Gray',
   'Axis',
   'GridXZ',
   'GridXY',
   'GridLines',
   'BrEnt',
   'Duplicator',
   'Tag',
   'GrayImage',
   'BSP',
   'Digger',
   'Bezier');

function MapColors(L: TListeCouleurs) : TColor;
begin
 Result:=SetupSubSet(ssMap, 'Colors').IntSpec[MapColorNames[L]];
end;

function CharModeJeu: Char;
var
 S: String;
begin
 S:=SetupGameSet.Specifics.Values['Code'];
 if S='' then
  CharModeJeu:=mjQuake
 else
  CharModeJeu:=S[1];
end;

function ModeJeuQuake2: Boolean;
begin
 Result:=CharModeJeu>=mjQuake2;
end;

function CurrentQuake1Mode: Char;
begin
 if CharModeJeu<mjQuake2 then
  Result:=CharModeJeu
 else
  Result:=mjQuake;
end;

function CurrentQuake2Mode: Char;
begin
 if CharModeJeu>mjQuake2 then
  Result:=CharModeJeu
 else
  Result:=mjQuake2;
end;

procedure ChangeGameModeStr(const nMode: String; Confirm: Boolean);
begin
 if CompareText(nMode, SetupGameSet.Name) <> 0 then
  if nMode='' then
   begin
    if Confirm then
     GlobalWarning(FmtLoadStr1(5548, [SetupGameSet.Name]))
   end
  else
   begin
    if SetupSet[ssGames].SubElements.FindName(nMode+':config')=Nil then
     Raise EErrorFmt(5547, [nMode]);
    if Screen.ActiveForm is TToolBoxForm then
     Raise EErrorFmt(5599, [nMode]);
    if Confirm and (MessageDlg(FmtLoadStr1(5543, [nMode]), mtWarning, mbOkCancel, 0) <> mrOk) then
     Abort;
    ClearGameBuffers(True);
    SetupSet[ssGames].Specifics.Values['GameCfg']:=nMode;
   {SetupModified:=True;}
    PosteMessageFiches(wp_SetupChanged, scGame);
   end;
end;

function GetGameCode(const nMode: String) : Char;
var
 Q: QObject;
 S: String;
begin
 if nMode='' then
  GetGameCode:=mjAny
 else
  begin
   Q:=SetupSet[ssGames].SubElements.FindName(nMode+':config');
   if Q=Nil then
    Raise EErrorFmt(5547, [nMode]);
   S:=Q.Specifics.Values['Code'];
   if S='' then
    GetGameCode:=mjQuake
   else
    GetGameCode:=S[1];
  end;
end;

procedure ChangeGameMode(nMode: Char; Confirm: Boolean);
var
 S: String;
begin
 case nMode of
  mjAny: Exit;
  mjNotQuake2: if ModeJeuQuake2 then
                nMode:=mjQuake
               else
                Exit;
  mjNotQuake1: if not ModeJeuQuake2 then
                nMode:=mjQuake2
               else
                Exit; 
 else
   if CharModeJeu=nMode then Exit;
 end;
 S:=GetGameName(nMode);
 if S='' then
  Raise EErrorFmt(5542, [CharModeJeu+nMode]);
 ChangeGameModeStr(S, Confirm);
end;

function GameModeOk(nMode: Char) : Boolean;
begin
 case nMode of
  mjAny: Result:=True;
  mjNotQuake2: Result:=not ModeJeuQuake2;
  mjNotQuake1: Result:=ModeJeuQuake2;
 else
  Result:=CharModeJeu=nMode;
 end;
end;

function GetGameName(nMode: Char) : String;
var
 I: Integer;
 S: String;
begin
 with SetupSet[ssGames] do
  for I:=0 to SubElements.Count-1 do
   begin
    S:=SubElements[I].Specifics.Values['Code'];
    if S='' then
     S:=mjQuake;
    if S[1]=nMode then
     begin  { found game config. }
      Result:=SubElements[I].Name;
      Exit;
     end;
   end;
 Result:='';  
end;

{function GetIncludePath: String;
begin
 if SetupSet[ssGeneral]=Nil then
  Result:=ApplicationPath
 else
  Result:=SetupSet[ssGeneral].Specifics.Values['IncludePath+']
   + ApplicationPath;
end;}

function InternalVersion : Single;
begin
 Result:=SetupSet[ssGeneral].GetFloatSpec('InternalVersion', 0);
end;

const
 RegFileAssocFormat = 'QuArK_%s_file';

function MakeAssociation({Reg: TRegistry2;} const Ext, Command: String) : Boolean;
var
 Reg: TRegistry2;
 S1, S: String;
 QClassPtr: QObjectClass;
 QClassInfo: TFileObjectClassInfo;
 Description: String;
begin
 QClassPtr:=GetRegisteredQObject('.'+Ext);
 Description:=Ext;
 if (QClassPtr<>Nil) and (QClassPtr.InheritsFrom(QFileObject)) then
  begin
   QFileObjectClass(QClassPtr).FileObjectClassInfo(QClassInfo);
   Description:=QClassInfo.NomClasseEnClair;
  end;

 Result:=False;
 Reg:=TRegistry2.Create; try
 Reg.RootKey:=HKEY_CLASSES_ROOT;
 if not Reg.OpenKey('\.'+Ext, True) then Exit;
{if not Reg.ReadString('', S1) or (S1='') then
  begin}
   S1:=Format(RegFileAssocFormat, [Ext]);
   if not Reg.WriteString('', S1) then Exit;
 {end;}
 if not Reg.OpenKey('\'+S1, True) then Exit;
 if not Reg.ReadString('', S) or (S='') then
  Reg.WriteString('', Description);
 if not Reg.OpenKey('shell\open\command', True) then Exit;
 if not Reg.WriteString('', Command) then Exit;
{if (Icon>=0) and Reg.OpenKey('\'+S1+'\DefaultIcon', True) then
  Reg.WriteString('', Format('%s,%d', [Application.ExeName, Icon]));}
 finally Reg.Free; end;
 Result:=True;
end;

function GetRegCommand : String;
begin
 Result:=Application.ExeName+' "%1"';
end;

procedure MakeAssociations(Config: Qobject);
var
 I, P: Integer;
 Command, S, Ext: String;
{Reg: TRegistry2;}
begin
 Command:=GetRegCommand;
{Reg:=TRegistry2.Create; try
 Reg.RootKey:=HKEY_CLASSES_ROOT;}
 for I:=0 to Config.Specifics.Count-1 do
  begin
   S:=Config.Specifics[I];
   P:=Pos('=',S);
   if (P>1) and (S[1]='.') and (P<Length(S)) then
    begin   { must be activated }
     if not MakeAssociation({Reg,} Copy(S, 2, P-2), Command) then
      GlobalWarning(FmtLoadStr1(5616, [Ext]));
    end;
  end;
{finally Reg.Free; end;}
 RefreshAssociations(True);
end;

procedure RefreshAssociations(Forced: Boolean);
var
 I, P: Integer;
 S, S1, Ext, Command: String;
 Active, Activate: Boolean;
 Reg: TRegistry2;

  procedure OpenReg;
  begin
   if Reg=Nil then
    begin
     Reg:=TRegistry2.Create;
     Reg.RootKey:=HKEY_CLASSES_ROOT;
    end;
  end;

begin
 Command:=GetRegCommand;
 with SetupSubSet(ssGeneral, 'File Associations') do
  begin
   Reg:=Nil; try
   for I:=0 to Specifics.Count-1 do
    begin
     S:=Specifics[I];
     P:=Pos('=',S);
     if (P>1) and (S[1]='.') then
      begin
       Activate:=Copy(S,P+1,MaxInt)='!';
       Ext:=Copy(S, 2, P-2);
       OpenReg;
       Active:=Reg.OpenKey('\.'+Ext, False)
        and Reg.ReadString('', S1)
        and (S1<>'') and Reg.OpenKey('\'+S1+'\shell\open\command', False)
        and Reg.ReadString('', S)
        and (CompareText(S, Command) = 0);

       if not Active and Activate then   { auto-associate }
        begin
         Reg.Free;
         Reg:=Nil;
         Active:=MakeAssociation({Reg,} Ext, Command);
        end;
       if Active or (Activate and Forced) then   { set icon and always show icon }
        begin
         OpenReg;
         if Reg.OpenKey('\'+S1, True) then
          begin
           Reg.WriteString('AlwaysShowExt', '');
           P:=Round(GetFloatSpec('i'+Ext, -1));
           if (P>=0) and Reg.OpenKey('DefaultIcon', True) then
            Reg.WriteString('', Format('%s,%d', [Application.ExeName, P]));
          end;
        end;

        {if not Reg.OpenKey('\', False)
         or not Reg.DeleteKey(S1)
         or not Reg.DeleteKey('.'+Ext) then
          GlobalWarning(FmtLoadStr1(5613, [Ext]));}

       end;
    end;
   finally Reg.Free; end;
  end;
end;

procedure RemoveAssociations;
var
 I, P: Integer;
 S, S1, Ext, Command: String;
 Active: Boolean;
 Reg: TRegistry2;
begin
 Command:=GetRegCommand;
 with SetupSubSet(ssGeneral, 'File Associations') do
  begin
   Reg:=TRegistry2.Create; try
   Reg.RootKey:=HKEY_CLASSES_ROOT;
   for I:=0 to Specifics.Count-1 do
    begin
     S:=Specifics[I];
     P:=Pos('=',S);
     if (P>1) and (S[1]='.') then
      begin
       Ext:=Copy(S, 2, P-2);
       Active:=Reg.OpenKey('\.'+Ext, False)
        and Reg.ReadString('', S1)
        and (S1<>'') and Reg.OpenKey('\'+S1+'\shell\open\command', False)
        and Reg.ReadString('', S)
        and (CompareText(S, Command) = 0);

       if Active then   { must un-associate }
        if not Reg.OpenKey('\', False)
        or not Reg.DeleteKey(S1)
        or not Reg.DeleteKey('.'+Ext) then
         GlobalWarning(FmtLoadStr1(5613, [Ext]));
      end;
    end;
   finally Reg.Free; end;
  end;
end;

function AssociationWithQuArK(const FileExt: String) : Boolean;
begin
 Result:=SetupSubSet(ssGeneral, 'File Associations').Specifics.Values[FileExt]<>'';
end;

 {------------------------}

procedure CloseSetupSet;
var
 T: TSetupSet;
begin
 for T:=Low(SetupSet) to High(SetupSet) do
  SetupSet[T].AddRef(-1);
 PyDict_SetItemString(QuarkxDict, 'setupset', Py_None);
end;

initialization
  RegisterQObject(QConfig, 'a');
{$IFDEF Debug}
finalization
  CloseSetupSet;
{$ENDIF}
end.

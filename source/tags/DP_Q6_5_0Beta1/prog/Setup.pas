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
Revision 1.47  2006/08/02 07:17:57  cdunde
To add .md3 model editor 3D view support for Quake 4.

Revision 1.46  2006/07/17 06:58:00  cdunde
To setup RTCW-ET as its own game
with md3 model display support.

Revision 1.45  2006/04/27 06:19:59  cdunde
To setup Quake4 support and code changes for Doom3 and material handling of both.
Related file changes
QkD3.pas
Added counter for phrasing of material list that kept their textures from
displaying and sometimes caused an overload and system lockup.
Added list of "Keywords" for the "Default texture" to display more of them.
QkMap.pas
To allow Quake4 Version 3 .mqp files to be read, previously set to only
allow Doom3 Version 1 .map files to be read and error on Version 2.
This still is the case for Doom3 with the above change for Quake4.
Setup.pas
Add game code "m" to start game support for Quake4.
QkTextures.pas
Added Quake4 game code mjQuake4 in Doom3 material file section
to point to Quake4 material files and display their related textures.

Revision 1.44  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.42  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.41  2004/11/08 22:47:43  alexander
hl2 support started

Revision 1.40  2004/05/21 01:11:11  cdunde
To add support for Sylphis game engine. Code by Harry Kalogirou.

Revision 1.39  2003/07/21 04:42:40  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.38  2002/12/30 18:07:35  decker_dk
Renamed 'GetRegisteredQObject' to 'RequestClassOfType', and moved the 'QObjectClassList.Free' from Qk1.PAS to QkObjectClassList.PAS.

Revision 1.37  2002/12/18 00:49:17  tiglari
Add Genesis3D code

Revision 1.36  2002/06/09 02:17:31  tiglari
add SoF2 gamecode

Revision 1.35  2002/04/28 21:23:49  tiglari
comment out line as suggested by Andy Vincent to stop abnormal termination
  after Python is Finalized

Revision 1.34  2002/04/08 10:58:35  tiglari
Add gamecode for Torque (from Desmond Fletcher)

Revision 1.33  2002/04/04 17:49:06  decker_dk
Added gamecode 'f' for Jedi Knight II support.

Revision 1.32  2002/02/24 13:47:10  decker_dk
Added MOHAA (Medal Of Honor:Allied Assault) with gamecode 'e'.

Revision 1.31  2001/12/05 20:39:17  decker_dk
Added RTCW (Return To Castle Wolfenstein) with gamecode 'd'.

Revision 1.30  2001/10/12 11:03:16  tiglari
Live Pointer Hunt (non-Debug build)

Revision 1.29  2001/10/12 09:11:19  tiglari
Live Pointer Hunt

Revision 1.28  2001/10/10 21:28:42  tiglari
Live Pointer Cleanup: free g_TexExtensions in finalization

Revision 1.27  2001/08/05 05:39:38  tiglari
move the bsptype stuff into qkbsp

Revision 1.26  2001/07/21 01:48:07  tiglari
add/use functions & values defining classes of games

Revision 1.25  2001/06/21 17:34:33  decker_dk
Added preliminary support for 6DX.

Revision 1.24  2001/06/05 18:41:51  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.23  2001/03/20 21:42:24  decker_dk
Updated copyright-header

Revision 1.22  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.21  2001/01/30 19:11:11  decker_dk
Changed to GetApplicationPath().

Revision 1.20  2001/01/21 15:50:28  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.19  2001/01/15 19:22:20  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.18  2000/12/30 15:24:55  decker_dk
- The .MAP exporting entity-numbering, didn't take into account Treeview-
groups. Modified TTreeMapEntity.SaveAsText(), TTreeMapGroup.SaveAsText() and
TTreeMapBrush.SaveAsText().
- Created a "Textures max-dimension" for the 3D views. A lower value requires
less memory for the textures, but will also decrease the texture quality in the
3D views.
- Removed the "Registering..." menuitem

Revision 1.17  2000/11/19 15:31:48  decker_dk
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

Revision 1.16  2000/11/16 19:42:16  decker_dk
- Modified Convex's texture-fileextension alias code, so it won't conflict
with the rest of the existing code.
- Introduced a 'TextureFileExtensions' specific, which will contain the
texture-fileextension aliases, for COnvex's code.
- Implemented solution for extracting texture-links from .PK3 files
('.pakfolder' vs '.zipfolder' problem)
- Replaced the function-names:
  = Q2TexPath    -> GameTexturesPath
  = Q3ShaderPath -> GameShadersPath
- Cleaned up some code here and there.
- Corrected problem with QTextureFile.LoadPaletteInfo not initializing an
PGameBuffer totally. Hmm? May have introduced problem with color-palette
in other windows than the texture-browser-detail.
- Found the place in QkWAD.PAS where the common size of the textures, in the
texture-browser, are controlled/set. Useful for 32x32, 128x128 and so scaling.

Revision 1.15  2000/09/25 19:36:28  decker_dk
Secured gamecode 'c' for mjCrystalSpace.

Revision 1.14  2000/09/18 01:31:48  alexander
added enum for startrek voyager elite force

Revision 1.12  2000/08/25 17:57:52  decker_dk
Comment about possible bug. Look for FIXME

Revision 1.11  2000/08/21 20:45:13  aiv
Added ModelColor

Revision 1.10  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.9  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.8  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

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
 mjQuake        = '1';
 mjHexen        = '2';
 mjHalfLife     = '3';
 mjGenesis3D    = '4';
 {..up to '9' Quake-style games may exist..}

 mjQuake2       = 'A';
 mjHeretic2     = 'B';
 mjSin          = 'C';
 mjKingPin      = 'D';
 mjSOF          = 'E';  { Soldier Of Fortune }
 {..space for more Quake-2 style games..}
 mj6DX          = 'Z';

 mjQ3A          = 'a';  { Quake-3:Arena / Quake-3:Team Arena }
 mjStarTrekEF   = 'b';  { Star Trek:Voyager - Elite Force }
 mjCrystalSpace = 'c';
 mjRTCW         = 'd';  { Return To Castle Wolfenstein }
 mjMOHAA        = 'e';  { Medal Of Honor:Allied Assault }
 mjJK2          = 'f';  { Jedi Knight II - Jedi Outcast }
 mjTorque       = 'g';
 mjSOF2         = 'h';  { Soldier of Fortune II }
 mjSylphis      = 'i';  { Sylphis }
 mjDoom3        = 'j';  { Doom 3 }
 mjHL2          = 'k';  { Half Life 2 }
 mjJA           = 'l';  { Jedi Academy }
 mjQuake4       = 'm';  { Quake 4 }
 mjRTCWET       = 'n';  { Return To Castle Wolfenstein - Enemy Territory}
 mjWildWest     = 'w';  { WildWest }

 mjAny          = #1;
 mjNotQuake2    = #2;
 mjNotQuake1    = #255;


type
 TListeCouleurs =
  (lcVueXZ, lcVueXY, lcSelXZ, lcSelXY, lcOutOfView, lcAxes, lcGridXZ, lcGridXY, lcGridLines,
   lcBrushEntity, lcDuplicator, lcTag, lcGrayImage, lcBSP, lcDigger, lcBezier);
 TModelColors =
  (mcVueXZ, mcVueXY, mcSelXZ, mcSelXY, mcOutOfView, mcAxes, mcGridXZ, mcGridXY, mcGridLines,
   mcTag, mcGrayImage, mcLinear, mcVertices);
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
              procedure ObjectState(var E: TEtatObjet); override;
            end;

var
{DECKER ApplicationPath: String;}
 g_SetupSet: TSetupSetArray;
{--CONVEX-- support for multiple texture formats}
 g_TexExtensions : TStringList = NIL;

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
(*procedure InitApplicationPath;*)
procedure SetupChanged(Level: Integer);
function SetupSubSet(Root: TSetupSet; SubSet: String) : QObject;
function SetupSubSetEx(Root: TSetupSet; SubSet: String; Create: Boolean) : QObject;
function SetupGameSet : QObject;
procedure UpdateSetup(Level: Integer);
procedure SaveSetupNow;
function MakeAddOnsList : QFileObject;  { includes the file loaded in g_Form1 }
procedure UpdateForm1Root;
procedure UpdateAddOnsContent;
procedure CloseAddonsList;  { don't call this when toolboxes are open }
procedure AddAddOn(NewAddOn: QObject);
function GetSetupPath(Path: String; var Spec: String; var Q: QObject) : Boolean;
procedure MakeAssociations(Config: Qobject);
procedure RefreshAssociations(Forced: Boolean);
procedure RemoveAssociations;
function AssociationWithQuArK(const FileExt: String) : Boolean;
function UsesMiptex : boolean; overload;
function UsesMiptex(mj : Char) : boolean; overload;
procedure StoreTexExtensions; {--CONVEX--}

 {------------------------}

function CharModeJeu: Char;
function ModeJeuQuake2: Boolean;
function ModeJeuQuake4: Boolean; 
function ModeJeuRTCWET: Boolean;
function CurrentQuake1Mode: Char;
function CurrentQuake2Mode: Char;
function GetGameName(nMode: Char) : String;
procedure ChangeGameMode(nMode: Char; Confirm: Boolean);
procedure ChangeGameModeStr(const nMode: String; Confirm: Boolean);
function GetGameCode(const nMode: String) : Char;
function GameModeOk(nMode: Char) : Boolean;

function MapColors(L: TListeCouleurs) : TColor;
function ModelColors(L: TModelColors) : TColor;
function InternalVersion : Single;

 {------------------------}

implementation

uses QkMapObjects, Travail, Game, QkGroup, QkForm, Qk1,
     ToolBox1, Toolbar1, QkQuakeCtx, Quarkx, Python, PyMapView,
     PyObjects, PyForms, Qk3D, EdSceneObject, QkObjectClassList, QkApplPaths;

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

procedure QConfig.ObjectState(var E: TEtatObjet);
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

function SetupSubSetEx(Root: TSetupSet; SubSet: String; Create: Boolean) : QObject;
begin
 Result:=g_SetupSet[Root].SubElements.FindName(SubSet+':config');
 if (Result=Nil) and Create then
  begin
   Result:=QConfig.Create(SubSet, g_SetupSet[Root]);
   g_SetupSet[Root].SubElements.Add(Result);
  end;
end;

function SetupSubSet(Root: TSetupSet; SubSet: String) : QObject;
begin
 Result:=g_SetupSet[Root].SubElements.FindName(SubSet+':config');
 if Result=Nil then
  Raise EErrorFmt(5205, [SetupSetName[Root]+':'+SubSet]);
end;

{--CONVEX-begin--}
procedure StoreTexExtensions;
var
 C:Char;
 Idx : Byte;
 S, SubStr : String;
begin
  if g_TexExtensions<>NIL then
   g_TexExtensions.Free;
  g_TexExtensions := TStringList.Create;
  try
  {DECKER - changed key from 'TextureFormat', due to possible conflicts with
   TGameBuffer(...)TextureExt in QkTextures.PAS}
   S := SetupGameSet.Specifics.Values['TextureFileExtensions'];
  except
   S := '';
  end;
  Idx := 1;
  while (Idx <= Length(S)) do
  begin
    SubStr := '';
    C := #0;
    while ((C <> ' ') and (C <> ',') and (Idx <= Length(S))) do
    begin
      C := S[Idx];
      if ((C <> ' ') and (C <> ',')) then
        SubStr := SubStr + C;
      Inc(Idx);
    end;
    g_TexExtensions.Add(SubStr);
  end;
end;
{--CONVEX-end--}

function SetupGameSet : QObject;
begin
 SetupGameSet:=SetupSubSet(ssGames, g_SetupSet[ssGames].Specifics.Values['GameCfg']);
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
      Q:=g_SetupSet[S];
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
   ProgressIndicatorStart(5445, Q.SubElements.Count); try
   for I:=0 to Q.SubElements.Count-1 do
    begin
     BrowseConfig(Q.SubElements[I]);
     ProgressIndicatorIncrement;
    end;
   finally ProgressIndicatorStop; end;
  end
 else
  if Q is QConfig then
   for T:=Low(T) to High(T) do
    if CompareText(Q.Name, SetupSetName[T]) = 0 then
     begin
      if g_SetupSet[T]=Nil then
       begin
        g_SetupSet[T]:=QConfig.Create(Q.Name, Nil);
        g_SetupSet[T].AddRef(+1);
       end;
       { mix Q into this g_SetupSet }
      Mix(g_SetupSet[T], Q);
     end;
end;

(*DECKER
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
   ApplicationPath:=IncludeTrailingPathDelimiter(ApplicationPath);
  end;
end;
/DECKER*)

procedure InitSetup;
var
 SetupQrk: QFileObject;
 V1, V2: String;
 T: TSetupSet;
 Version: TDouble;
begin
 for T:=High(T) downto Low(T) do
  if g_SetupSet[T]<>Nil then
   begin
    g_SetupSet[T].AddRef(-1);
    g_SetupSet[T]:=Nil;
   end;
{g_SetupSet[ssTempData]:=QConfig.Create(SetupSetName[ssTempData], Nil);
 g_SetupSet[ssTempData].AddRef(+1);}
 DefaultsFileName1:='';
{ SetupFileName1:=GetApplicationPath()+SetupFileName;  { default }

  { loads Defaults.qrk }
 try
  SetupQrk:=LienFichierQObject(DefaultsFileName, Nil, False);
  SetupQrk.AddRef(+1);
  try
   DefaultsFileName1:=SetupQrk.Filename;
   BrowseConfig(SetupQrk);  { copies this setup data into memory }
  finally
   SetupQrk.AddRef(-1);
  end;
 except
  on E: Exception do
   begin
    g_Form1.MessageException(E, LoadStr1(5204), [mbOk]);
    Halt(1);   { cannot load Defaults.qrk - fatal error }
    Exit;
   end;
 end;

  { checks loaded data }
 if g_SetupSet[ssGeneral]<>Nil then
  begin   { checks version }
   V1:=QuarkVersion;
   V2:=g_SetupSet[ssGeneral].Specifics.Values['Version'];
   if V1 <> V2 then
    begin
     MessageDlg(FmtLoadStr1(5206, [V1, V2]), mtError, [mbOk], 0);
     Halt(1);   { wrong version of Defaults.qrk }
    end;
  end;
 for T:=Low(T) to High(T) do
  if g_SetupSet[T]=Nil then  { checks ":config" objects }
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
  {SetupFileName1:=SetupQrk.Filename;}
   BrowseConfig(SetupQrk);  { copies this setup data into memory }
  finally
   SetupQrk.AddRef(-1);
  end;
 except
  { could not load Setup.qrk - this is not an error, continue execution }
 end;

 if g_SetupSet[ssGeneral].GetFloatSpec('RunVersion', 5.901)<5.9005 then
  RefreshAssociations(True);
 g_SetupSet[ssGeneral].SetFloatSpec('RunVersion', Version);
 g_SetupSet[ssGeneral].Specifics.Values['Date']:=DateToStr(Date);
 SetupChanged({scMaximal} {scMinimal} scInit);
end;

procedure SetupChanged;
var
 fnt: PyObject;
 S: String;
 I, J: Integer;
{SetupInfo: PyObject;}
begin
 if Level>=scAddOns then
  ClearGameBuffers(False)
 else
  ClearGameBuffer1;
  
 for I:=0 to Screen.FormCount-1 do
  with Screen.Forms[I] do
   for J:=0 to ComponentCount-1 do
    if Components[J] is TPyMapView then
     TPyMapView(Components[J]).DeleteScene;
  
 if (Level>=scAddOns) or (Level=scGame) then
  TTextureManager.FreeNonVisibleTextures;

  { initializes QuArK depending on the setup information }
 g_DrawInfo.DefWhiteOnBlack:=SetupSubSet(ssMap, 'Colors').Specifics.Values['InvertedColors']<>'';
 with SetupSubSet(ssMap, 'Options') do
  begin
   g_DrawInfo.CacherFaces:=Specifics.Values['HideFaces']<>'';
   g_DrawInfo.TexAntiScroll:=IntSpec['TexAntiScroll'];
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
  PyList_SetItem(SetupInfo, Ord(T), GetPyObj(g_SetupSet[T]));
 PyDict_SetItemString(QuarkxDict, 'setupset', SetupInfo);
 finally Py_DECREF(SetupInfo); end;*)

  { sends the reset message to all windows }
 PosteMessageFiches(wp_SetupChanged, Level);
 StoreTexExtensions; {--Convex--}
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
 SetupSet2:=g_SetupSet;
 try
  FillChar(g_SetupSet, SizeOf(g_SetupSet), 0);  { reset g_SetupSet }
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
      StoreDiff(SetupQrk, SetupSet2[T], g_SetupSet[T]);
     end;
    SetupQrk.ReadFormat:=Format;
    SetupQrk.Specifics.Values['Description']:=LoadStr1(5394);
    SetupQrk.TrySavingNow;
   finally
    SetupQrk.AddRef(-1);
   end;

  finally  { frees the temporary loaded g_SetupSet }
   for T:=High(T) downto Low(T) do
    if g_SetupSet[T]<>Nil then
     g_SetupSet[T].AddRef(-1);
  end;
 finally  { restores the saved g_SetupSet }
  g_SetupSet:=SetupSet2;
 end;
end;

procedure UpdateSetup;
begin
 SetupChanged(Level);
{SetupModified:=True;}
end;

procedure SaveSetupNow;
begin
 SaveSetup(rf_AsText, GetApplicationPath());   { save as text }
end;
(*var
 s: PyObject;
 Path: String;
 P: PChar;
begin
 Path:=GetApplicationPath();
 s:=PyDict_GetItemString(QuarkxDict, 'exepath');
 if s<>Nil then
  begin
   P:=PyString_AsString(s);
   if (P<>Nil) and (P^<>#0) then Path:=IncludeTrailingPathDelimiter(P);
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
 if g_Form1.Explorer.Roots.Count>0 then
  begin
   AddOns.SubElements.Add(g_Form1.Explorer.Roots[0]);
   AddOns.Specifics.Values['f1r']:='1';
  end;
 UpdateAddOnsContent;
end;

procedure AddAddOn(NewAddOn: QObject);
var
 L: TStringList;
 S: String;
begin
 L:=TStringList.Create;
 try
  L.Text:=SetupGameSet.Specifics.Values['AddOns'];
  L.Add(NewAddOn.Name+NewAddOn.TypeInfo);
  S:=StringListConcatWithSeparator(L, $0D);
 finally
  L.Free;
 end;
 SetupGameSet.Specifics.Values['AddOns']:=S;
 if AddOns=Nil then
  Exit;
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
    Result.Filename:=DefaultsFileName1;
    Result.SubElements.Add(ExactFileLink(DefaultsFileName1, Result, False));
  (*Result.SubElements.Add(LienFichierQObject(
     SetupGameSet.Specifics.Values['Base'], Result));*)
    L:=TStringList.Create;
    try
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
    finally
     L.Free;
    end;
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
 Result.AddRef(+1); {DECKER - FIXME - Possible cause of "double Result.AddRef(+1)" here?}
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

const
  ModelColorNames: array[TModelColors] of String =
  ('ViewXZ',
   'ViewXY',
   'SelXZ',
   'SelXY',
   'Gray',
   'Axis',
   'GridXZ',
   'GridXY',
   'GridLines',
   'Tag',
   'GrayImage',
   'Linear',
   'Vertices');

function ModelColors(L: TModelColors) : TColor;
begin
 Result:=SetupSubSet(ssModel, 'Colors').IntSpec[ModelColorNames[L]];
end;

{ this should probably be done by direct lookup of codes
  in the game config files }

{ probably should also be done by direct lookup
  from setup files }
function UsesMipTex : boolean; overload;
begin
  Result:=UsesMipTex(CharModeJeu);
end;

function UsesMipTex(mj : Char) : boolean; overload;
begin
  mj:=CharModeJeu;
  if (mj>='1') and (mj<='9') then
    Result:=true
  else
    Result:=false;
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
 Result := CharModeJeu >= mjQuake2;
end;     

function ModeJeuQuake4: Boolean;
begin
 Result := CharModeJeu >= mjQuake4;
end;

function ModeJeuRTCWET: Boolean;
begin
 Result := CharModeJeu >= mjRTCWET;
end;

function CurrentQuake1Mode: Char;
begin
 if CharModeJeu < mjQuake2 then
  Result:=CharModeJeu
 else
  Result:=mjQuake;
end;

function CurrentQuake2Mode: Char;
begin
 if CharModeJeu > mjQuake2 then
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
    if g_SetupSet[ssGames].SubElements.FindName(nMode+':config')=Nil then
     Raise EErrorFmt(5547, [nMode]);
    if Screen.ActiveForm is TToolBoxForm then
     Raise EErrorFmt(5599, [nMode]);
    if Confirm and (MessageDlg(FmtLoadStr1(5543, [nMode]), mtWarning, mbOkCancel, 0) <> mrOk) then
     Abort;
    ClearGameBuffers(True);
    g_SetupSet[ssGames].Specifics.Values['GameCfg']:=nMode;
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
   Q:=g_SetupSet[ssGames].SubElements.FindName(nMode+':config');
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
  mjAny:
   Exit;
  mjNotQuake2:
   if ModeJeuQuake2 then
    nMode:=mjQuake
   else
    Exit;
  mjNotQuake1:
   if not ModeJeuQuake2 then
    nMode:=mjQuake2
   else
    Exit;
 else
   if CharModeJeu=nMode then
    Exit;
 end;
 S:=GetGameName(nMode);
 if S='' then
  Raise EErrorFmt(5542, [CharModeJeu+nMode]);
 ChangeGameModeStr(S, Confirm);
 StoreTexExtensions; {--Convex--}
end;

function GameModeOk(nMode: Char) : Boolean;
begin
 case nMode of
  mjAny:       Result:=True;
  mjNotQuake2: Result:=not ModeJeuQuake2;
  mjNotQuake1: Result:=ModeJeuQuake2;
 else
  Result := CharModeJeu = nMode;
 end;
end;

function GetGameName(nMode: Char) : String;
var
 I: Integer;
 S: String;
begin
 with g_SetupSet[ssGames] do
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
 if g_SetupSet[ssGeneral]=Nil then
  Result:=GetApplicationPath()
 else
  Result:=g_SetupSet[ssGeneral].Specifics.Values['IncludePath+']
   + GetApplicationPath();
end;}

function InternalVersion : Single;
begin
 Result:=g_SetupSet[ssGeneral].GetFloatSpec('InternalVersion', 0);
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
 QClassPtr:=RequestClassOfType('.'+Ext);
 Description:=Ext;
 if (QClassPtr<>Nil) and (QClassPtr.InheritsFrom(QFileObject)) then
  begin
   QFileObjectClass(QClassPtr).FileObjectClassInfo(QClassInfo);
   Description:=QClassInfo.FileObjectDescriptionText;
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
 for T:=Low(g_SetupSet) to High(g_SetupSet) do
  g_SetupSet[T].AddRef(-1);
// PyDict_SetItemString(QuarkxDict, 'setupset', Py_None);
end;

initialization
  RegisterQObject(QConfig, 'a');
finalization
  CloseSetupSet;
{$IFDEF Debug}
//  Clear_g_MemQObject;
{$ENDIF}

  if g_TexExtensions<>NIL then
   g_TexExtensions.Free;

end.

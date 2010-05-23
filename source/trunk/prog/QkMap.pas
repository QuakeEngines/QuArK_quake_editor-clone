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
Revision 1.101  2010/04/16 21:19:26  danielpharos
Move some version-stuff about. quarkpy now also checks the minor version number.

Revision 1.100  2009/08/14 13:36:05  danielpharos
Added some comments to help me fix a problem later.

Revision 1.99  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.98  2009/05/15 11:05:59  danielpharos
Moved SaveMapVersion setting to seperate Files section.

Revision 1.97  2009/04/30 18:25:07  danielpharos
Fixed some small layouting issues with saved VMF files.

Revision 1.96  2009/03/19 18:31:22  danielpharos
Argl! Removed bad comment.

Revision 1.95  2009/03/19 18:23:10  danielpharos
Fixed a double space in bezier MOHAA surfaceparams output.

Revision 1.94  2009/03/14 13:13:53  danielpharos
MOHAA beziers now also output their surfaceparams.

Revision 1.93  2009/02/21 17:10:12  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.92  2009/02/13 22:30:59  danielpharos
Save the two additional MOHAA flags, and restructure code a bit to allow MOHAA's patchDef2 surfaceparms. MOHAA maps should now load properly!

Revision 1.91  2009/02/11 15:59:09  danielpharos
Added missing ResolveMapSaveSettings-calls.

Revision 1.90  2009/02/11 15:54:09  danielpharos
Figured out how to resolve MapFormat.

Revision 1.89  2009/02/11 15:26:31  danielpharos
Figured out what the fallback gamecode needs to be.

Revision 1.88  2009/02/11 15:18:56  danielpharos
Oops.

Revision 1.87  2009/02/11 14:59:57  danielpharos
Restructure some .map file saving code, and added some code for CoD2 (still not working properly though).

Revision 1.86  2009/02/11 14:53:22  danielpharos
TList --> TQList

Revision 1.85  2009/02/11 14:44:45  danielpharos
Read in two additional MOHAA surface flags.

Revision 1.84  2008/11/06 21:11:50  danielpharos
Made type Specifics soft-coded: Will lated be changed into a new, yet-to-be-defined type.

Revision 1.83  2008/11/06 20:18:22  danielpharos
Removed old stuff in preparation for new specifics code.

Revision 1.82  2008/10/23 23:17:31  danielpharos
Fixed CoD1 map output. Should work now.

Revision 1.81  2008/10/23 22:29:42  danielpharos
Fix for CoD1 map format.

Revision 1.80  2008/10/23 21:49:54  danielpharos
Ignore everything we don't understand from CoD1 maps.

Revision 1.79  2008/10/23 19:21:17  danielpharos
Fix mistake prev rev and read all CoD1 surface flags.

Revision 1.78  2008/10/23 18:55:50  danielpharos
Added partial support for loading Call of Duty 2 maps.

Revision 1.77  2008/10/12 11:31:32  danielpharos
Moved 6DX map format to separate file, and re-factored QkMap and QkQuakeMap.

Revision 1.76  2008/10/05 13:51:19  danielpharos
Correct Integer to HDC.

Revision 1.75  2007/07/05 10:19:45  danielpharos
Moved the Quake .map format code to a separate file.

Revision 1.74  2007/06/06 21:27:38  danielpharos
Fix a problem where several things weren't saved in .map files.

Revision 1.73  2007/05/29 13:06:11  danielpharos
Added ignore-what-I-don't-understand support for iwmap 4 .map files, which is used by Call of Duty 2.

Revision 1.72  2007/05/24 20:42:45  danielpharos
Reserved gamecodes for Call of Duty 1 and 2.

Revision 1.71  2007/05/06 21:20:50  danielpharos
Fixed a bunch of missing BrushDef's in Q3 .map files.

Revision 1.70  2007/04/16 11:34:55  danielpharos
Added begin of support for EF2. Changed STVEF naming to be more consistent. Added ForceFaceFlags option.

Revision 1.69  2007/04/15 10:44:41  danielpharos
Doom 3 and Quake 4 will now always export the new mapversion files. Fixed a quote mistake in the brush texture names.

Revision 1.68  2007/04/14 11:25:12  danielpharos
Putted some end-of-line checks back in.

Revision 1.67  2007/04/12 22:41:31  danielpharos
Possible fix for entities disappearing when exporting .map file.

Revision 1.66  2007/04/12 22:18:13  danielpharos
Small fix, mainly for Quake 2.

Revision 1.65  2007/04/12 20:54:07  danielpharos
Another BIG update for Doom 3 and Quake 4: patchdef2 should be saving correctly now.

Revision 1.64  2007/04/12 15:28:11  danielpharos
Minor clean up.

Revision 1.63  2007/04/12 15:04:43  danielpharos
BIG moving around of code. All the .map save routines should now be in QkMap. This will allow easy changes, and will simplify future map format support.

Revision 1.62  2007/04/12 10:51:10  danielpharos
Added brushdef2 loading support.

Revision 1.61  2007/04/12 10:05:21  danielpharos
Improved texture name handling for Doom 3 and Quake 4.

Revision 1.60  2007/04/09 21:44:24  danielpharos
Started work on Doom 3 map version 2 and Quake 4 map version 3.

Revision 1.59  2007/03/25 13:52:25  danielpharos
Moved a few dictionnary words around.

Revision 1.58  2006/04/27 06:19:59  cdunde
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

Revision 1.57  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.55  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.54  2004/11/11 18:43:49  alexander
fixed malformed comment line with 0D inside fooling D5 and not D6

Revision 1.53  2004/11/06 08:18:38  cdunde
Reversed last change to end statement due to compiling problem

Revision 1.52  2004/10/30 14:06:39  alexander
made it compileable

Revision 1.51  2004/05/21 01:11:10  cdunde
To add support for Sylphis game engine. Code by Harry Kalogirou.

Revision 1.50  2002/12/22 05:52:46  tiglari
restoring projecting points to planes, to make lighting work out

Revision 1.49  2002/12/21 09:25:13  tiglari
steamline v220 map-reading

Revision 1.48  2002/12/15 14:05:14  tiglari
improve efficiency of wc33 reading by eliminating unnecessary projections of
 texture points to planes (this should be tested more to check that it's
 really right)

Revision 1.47  2002/05/15 22:04:51  tiglari
fixes to map reading error recording (so that new maps can be created ..)

Revision 1.46  2002/05/15 00:08:38  tiglari
Record Map Errors for possible write to console or elsewhere

Revision 1.45  2002/05/14 21:24:50  tiglari
comment on bad texture scale while reading valve 220 maps

Revision 1.44  2002/05/07 23:23:46  tiglari
Mohaa map reading

Revision 1.43  2002/05/07 09:12:04  tiglari
prevent reversion to Quake mode when loading Torque maps (Desmond Fletcher)

Revision 1.42  2002/04/30 12:27:49  tiglari
Removed switch to Q3A mode when brushdef encountered: any game
 could have a brush-primitives-reading tool written for it.

Revision 1.41  2002/03/27 00:24:49  tiglari
delete/write mapversion 220 specific as needed (removed when map
 read, added back in if written out in V220 format).

Revision 1.40  2002/03/26 22:21:59  tiglari
support UseIntegralVertexes flag

Revision 1.39  2002/03/26 10:16:40  tiglari
Englishification: TPolyedre->TPolyhedron

Revision 1.38  2002/03/26 10:11:30  tiglari
get rid of soDisableEnhTex, soWriteValve220 (obsoleted by OutputMapFormat)

Revision 1.37  2001/07/19 09:49:46  tiglari
rearrange QMap hierarchy to make QHfmFile work properly

Revision 1.34  2001/06/05 18:39:33  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.33  2001/05/06 21:13:37  tiglari
fix mode change to q3a mode when reading stvef maps

Revision 1.32  2001/03/31 04:27:28  tiglari
WC33 mapversion 220 flag now kept, map will be written in 220 if present
 (concurrent update to QkMapPoly.pas)

Revision 1.31  2001/03/20 21:50:54  decker_dk
Updated copyright-header

Revision 1.30  2001/03/20 07:44:05  tiglari
wc33 reading offset fix

Revision 1.29  2001/03/18 01:35:48  tiglari
wc33 map format read (not fully tested, offsets might be off)

Revision 1.28  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.27  2001/02/17 09:15:20  tiglari
brush primitives texture reading working (at least sort of ...)

Revision 1.26  2001/02/17 06:10:12  tiglari
nonfunctional brush primitives reading (syntax OK, but brushes broken)

Revision 1.25  2001/02/07 19:28:43  decker_dk
Fixed problem in ReadSymbol() case '"', where introducing begin-end's created a wrongly statement-sequence. Always ALWAYS remember to put begin-ends at multiple if-statements!!!

Revision 1.24  2001/01/28 17:25:52  decker_dk
Made QMapFile.SaveFile() use the function 'CommentMapLine(string)' to write the .MAP comment-header.

Revision 1.23  2001/01/21 15:49:03  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.22  2001/01/15 19:20:19  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.21  2000/12/10 21:38:36  decker_dk
Able to read exponent-values from .MAP files

Revision 1.20  2000/11/19 15:31:49  decker_dk
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

Revision 1.19  2000/10/26 18:12:40  tiglari
fixed bug with a build flag (disableFP vs. Enhanced Tex mixup)

Revision 1.18  2000/10/26 17:09:52  tiglari
read soEnableBrushPrim

Revision 1.17  2000/09/24 23:45:16  alexander
committed tiglaris .map loading and bezier texture missing fix

Revision 1.16  2000/09/14 18:00:22  decker_dk
Moved QTexture1 and QTexture2 into QkQ1.PAS and QkQ2.PAS

Revision 1.15  2000/08/20 11:16:47  aiv
Removed (not req'd)

Revision 1.14  2000/07/30 11:21:03  tiglari
put in pascal version of map saving flag code from mapquakemenu.py
to make save flags apply when map is saved from  File menu.  Question:
why did Armin put it in the Python in the first place?

Revision 1.13  2000/07/19 18:23:26  decker_dk
Read mapversion 220 maps (WC33 format)

Revision 1.12  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.11  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.10  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.9  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkMap;

interface

uses
  Windows, Messages, Classes, SysUtils, Forms, Dialogs, Controls,
  StdCtrls, EnterEditCtrl, ExtCtrls, TB97,
  QkMapObjects, QkMapPoly,
  QkBsp, qmath, Python,
  { tiglari } QkTextures, { /tiglari}
  QkForm, QkObjects, QkFileObjects, PyMapView;

{ $DEFINE ClassnameLowerCase}
{$DEFINE RemoveEmptySpecs}

type
 TMapFormatTypes = (
     CQType, { Classic Quake1/2/3 }
     QetpType,  { Quark Enhanced Texture Positioning }
     V220Type,  { Valve Mapformat 220 }
     BPType,     { Brush Primitives }
     HL2Type,    {that one used in hl2 tools}
     CoD2Type,   { Call of Duty 2 }
     UnknownType
  );

  TMapSaveSettings = record
    GameCode: Char;
    MapFormat: TMapFormatTypes;
    MapVersion: Integer;
    DecimalPlaces: Integer;
    BrushDefVersion: Integer;
    PatchDefVersion: Integer;
  end;

 QMap = class(QFileObject)
        protected
          function OpenWindow(nOwner: TComponent) : TQForm1; override;
        public
          function TestConversionType(I: Integer) : QFileObjectClass; override;
          function ConversionFrom(Source: QFileObject) : Boolean; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
          function GetOutputMapFileName : String;
          procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
        end;
 QQkm = class(QMap)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
 QMapFile = class(QMap)
            protected
              procedure LoadFile(F: TStream; FSize: Integer); override;
              procedure SaveFile(Info: TInfoEnreg1); override;
            end;

type
  TFQMap = class(TQForm1)
    Panel2: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    EnterEdit1: TEnterEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure EnterEdit1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   {FOldPaint: TCSBPaintEvent;}
    FRoot: TTreeMap;
    procedure ScrollBox1Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    procedure ReadSetupInformation(Level: Integer); override;
  public
    ScrollBox1: TPyMapView;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  end;

 {------------------------}

function GetDefaultMapSaveSettings : TMapSaveSettings;
function ReadEntityList(Racine: TTreeMapBrush; const SourceFile: String; BSP: QBsp) : Char;
procedure SaveAsMapText(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
procedure SaveAsMapTextTTreeMapBrush(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
procedure SaveAsMapTextTTreeMapSpec(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Dest, HxStrings: TStrings; Flags2: Integer; EntityNumber: Integer);
procedure SaveAsMapTextTTreeMapEntity(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
procedure SaveAsMapTextTTreeMapGroup(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
procedure SaveAsMapTextTPolygon(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Brush: TStrings; OriginBrush: PVect; Flags2: Integer);
procedure SaveAsMapTextTFace(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Brush: TStrings; OriginBrush: PVect; Flags2: Integer);
procedure SaveAsMapTextTBezier(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Target: TStrings);
procedure SaveAsMapTextTDuplicator(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);

 {------------------------}

implementation

uses
  Setup, Undo, Quarkx, qmatrices, Qk3D, PyMath, QkQuakeMap, QkApplPaths,
  Graphics, StrUtils, Game, QkExceptions, Travail, QkConsts, Logging,
  PyForms, Bezier, Duplicator, QkPixelSet, Qk6DX, QkVMF, QkSylphis, QkQ2,
  { tiglari } QkSin, { /tiglari } QkBspHulls, MapError, QkObjectClassList;

{$R *.DFM}

 {------------------------}

var
  EntityNoCounting: Integer;

function GetFirstEntityNo: Integer;
begin
 { Exporting .MAP entity numbering scheme. Resets to zero }
 EntityNoCounting := 0;
 Result := EntityNoCounting;
end;

function GetNextEntityNo: Integer;
begin
 { Exporting .MAP entity numbering scheme. Increment by one }
 Inc(EntityNoCounting);
 Result := EntityNoCounting;
end;

function GetDefaultMapSaveSettings : TMapSaveSettings;
begin
  with Result do
  begin
    GameCode := mjAny;
    MapFormat := UnknownType;
    MapVersion := -1;
    DecimalPlaces := -1;
    BrushDefVersion := -1;
    PatchDefVersion := -1;
  end;
end;
       
function GetDecimalPlaces(GameCode: Char): Integer;
begin
 // Rowdy: 17-Feb-2005 it seems that Torque wants more decimal places in .map files,
 //        something to do with an idiosyncracy in Torque's map2dif utility that
 //        converts a .map file written by QuArK to a .dif file (?) that Torque
 //        can use.  cdunde has done some comprehensive testing with Lee Davies
 //        (thanx guys!) and the results are inconclusive, but in order to make
 //        QuArK play a bit nicer with Torque, or at least with map2dif, this
 //        little hack was orchestrated so that each game has a configurable number
 //        of decimal places for polys written to .map files.  The old (hard-coded)
 //        default was 5, so we drop back to that if we read something we don't
 //        understand.  The range is set from 1 to 40 - less than 1 (i.e. 0 (i.e.
 //        integers)) should still be controlled by the existing option to write
 //        only integers to .map files (see the Configutation window), anything
 //        more than 40 decimal places is ludicrously insanely stupendously
 //        silly :-P  So there.  Does this make the longest single comment in the
 //        QuArK source???
 Result := Round(SetupSubSet(ssGames, GetGameName(GameCode)).GetFloatSpec('DecimalPlaces', 5));
 if Result <= 0 then
  Result := 0
 else if Result > 40 then
  Result := 40;
end;

function GetMapFormat(GameCode : Char) : TMapFormatTypes;
var
  S : PChar;
begin
  S:=PChar(SetupSubSet(ssGames, GetGameName(GameCode)).Specifics.Values['OutputMapFormat']);
  if      StrComp(S, 'Classic Quake')    = 0 then Result := CQType
  else if StrComp(S, 'Quark etp')        = 0 then Result := QetpType
  else if StrComp(S, 'Valve 220')        = 0 then Result := V220Type
  else if StrComp(S, 'Brush Primitives') = 0 then Result := BPType
  else if StrComp(S, 'HL2')              = 0 then Result := HL2Type
  else if StrComp(S, 'CoD2')             = 0 then Result := CoD2Type
  else
  begin
    Log(LOG_WARNING,LoadStr1(5702)+'\nDefaulting to Classic Quake',[S]);
    //Raise EErrorFmt(5702, [S]);
    Result:=CQType
  end;
end;

procedure ResolveMapSaveSettings(var MapSaveSettings: TMapSaveSettings);
var
  MapOptionSpecs : TSpecificsList;
begin
  with MapSaveSettings do
  begin
    //Resolve GameCode
    if GameCode=mjAny then
      GameCode:=CharModeJeu;

    //Resolve MapFormat
    if MapFormat=UnknownType then
      MapFormat:=GetMapFormat(GameCode);

    //Resolve MapVersion
    if MapVersion=-1 then
    begin
      MapVersion:=0;
      if GameCode=mjDoom3 then
      begin
        MapOptionSpecs:=SetupSubSet(ssFiles,'MAP').Specifics;
        if MapOptionSpecs.Values['SaveMapVersion'] = '1' then
          MapVersion:=1
        else if MapOptionSpecs.Values['SaveMapVersion'] = '2' then
          MapVersion:=2
        else
          MapVersion:=2;
      end;
      if GameCode=mjQuake4 then
        MapVersion:=3;
    end;

    //Resolve DecimalPlaces
    if DecimalPlaces=-1 then
      DecimalPlaces:=GetDecimalPlaces(GameCode);

    //Resolve BrushDefVersion
    BrushDefVersion:=0;
    if MapVersion>0 then
      BrushDefVersion:=3
    else
      if GameCode=mjCoD then
        BrushDefVersion:=0
      else if GameCode>=mjQ3A then
        //DanielPharos: This should select all Quake3 and better games.
        BrushDefVersion:=1;

    //DanielPharos: At the moment, the only one supported!
    if BrushDefVersion>1 then
      BrushDefVersion:=1;

    //Resolve PatchDefVersion
    PatchDefVersion:=2;
    if MapVersion>2 then
      PatchDefVersion:=3;

  end;
end;

 {------------------------}

function QMap.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 if nOwner=Application then
  Result:=NewPyForm(Self)
 else
  Result:=TFQMap.Create(nOwner);
end;

procedure QMap.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiMap;
 E.MarsColor:=clBlack;
end;

class procedure QMap.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.WndInfo:=[wiWindow, wiMaximize];
 Info.PythonMacro:='displaymap';
end;

function QMap.TestConversionType(I: Integer) : QFileObjectClass;
begin
 case I of
  1: Result:=QQkm;
  2: case CharModeJeu of
     mj6DX:
       Result:=QHmfFile;
     mjHL2:
       Result:=QVMFFile;
     mjSylphis:
       Result:=QCMapFile;
     else
       Result:=QQuakeMapFile;
     end;
 else Result:=Nil;
 end;
end;

function QMap.ConversionFrom(Source: QFileObject) : Boolean;
begin
 Result:=Source is QMap;
 if Result then
  begin
   Source.Acces;
   CopyAllData(Source, False);   { directly copies data }
  end;
end;

function QMap.GetOutputMapFileName : String;
begin
 Result:=Specifics.Values['FileName'];
 if Result='' then
  Result:=Name;
 BuildCorrectFileName(Result);
end;

procedure QMap.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
begin
 if FirstMap='' then
  FirstMap:='*';
 PyList_Append(maplist, @PythonObj);
end;

 {------------------------}

class function QQkm.TypeInfo;
begin
 Result:='.qkm';
end;

class procedure QQkm.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5126);
 Info.FileExt:=775;
 Info.QuArKFileObject:=True;
end;

 {------------------------}

function ReadEntityList(Racine: TTreeMapBrush; const SourceFile: String; BSP: QBsp) : Char;
const
 cSeperators = [' ', #13, #10, Chr(vk_Tab)];
 cExponentChars = ['E', 'e'];
 Granularite = 8192;
 EndOfLine = False;
type
 TSymbols = (sEOF,
             sBracketLeft,
             sBracketRight,
             sCurlyBracketLeft,
             sCurlyBracketRight,
             sSquareBracketLeft,
             sSquareBracketRight,
             sStringToken,
             sStringQuotedToken,
             sNumValueToken,
             sTokenForcedToString);
var
 SymbolType: TSymbols;
 S, S1, Classname: String;
 NumericValue: Double;
 V: array[1..3] of TVect;
 P: TPolyhedron;
 I, J, K, NumericValue1, ContentsFlags: Integer;
 WorldSpawn: Boolean;
 Entite, EntitePoly: TTreeMapSpec;
 L: TSpecificsList;
 LineNoBeingParsed: Integer;
 Juste13{, EndOfLine}, Q2Tex, ReadSymbolForceToText: Boolean;
 HullNum, BrushNum, FaceNum: Integer;
 HullList: TQList;
 Source, Prochain: PChar;
 Entities, MapStructure {Rowdy}, MapStructureB {/Rowdy}: TTreeMapGroup;
 Params: TFaceParams;
 InvPoly, InvFaces: Integer;
 TxCommand: Char;
 OriginBrush: TPolyhedron;
 Facteur: TDouble;
 Delta, Delta1: TVect;
 {Rowdy}
 B: TBezier;
 EntiteBezier: TTreeMapSpec;
 MeshBuf1: TBezierMeshBuf5;
 pCP1: vec5_p;
 {/Rowdy}
 WC33map: Boolean; {Decker}
 SpecIndex: integer; {Decker}
 UAxis, VAxis : TVect;  {wc3.3 stuff/tiglari}
 UShift, VShift: Double;

 { tiglari, for sin stuff }
 ThreeSing: array[0..2] of Single;
 LastValue : Double;

 Flags, Contents : LongInt;
 Q : QPixelSet;
 Header : TQ2MipTex;

 { Rowdy, for Doom 3 and Quake 4 stuff}
 MapVersion: Integer;

 //For MOHAA's patchDef2 detection:
 MOHAAPatchDef2Detected: Boolean;

 function ReadInt(str : string) : LongInt;
 begin
   if str = '' then
     Result:=0
   else
     Result:=StrToInt(str)
 end;

 procedure SetSinFlag;
   {DECKER - since SetFlagPos is only used by SetSinFlag, it can as well be a private-local procedure/}
   procedure SetFlagPos(pol: char; BitPosition: integer; var Flags: LongInt);
    begin
     if pol = '+' then
      Flags:=Flags or (1 shl BitPosition)
     else
      Flags:=Flags and (not (1 shl BitPosition));
   end;
   
 begin
     { The following code was generated by the PERL script flags2.pl
       from the data file sinflags.txt.  Don't modify this by hand. }

    case S[2] of
     'a' :
        case S[3] of
         'd' :
              SetFlagPos(S[1],20,Flags); { add }
         'n' :
              SetFlagPos(S[1],23,Flags); { animate }
        end;
     'c' :
        case S[3] of
         'o' :
            case S[4] of
             'n' :
                case S[5] of
                 's' :
                      SetFlagPos(S[1],14,Flags); { console }
                 'v' :
                      SetFlagPos(S[1],6,Flags); { conveyor }
                end;
             'r' :
                  SetFlagPos(S[1],26,Contents); { corpse }
            end;
         'u' :
            case S[10] of
             '0' :
                  SetFlagPos(S[1],18,Contents); { current_0 }
             '1' :
                  SetFlagPos(S[1],20,Contents); { current_180 }
             '2' :
                  SetFlagPos(S[1],21,Contents); { current_270 }
             '9' :
                  SetFlagPos(S[1],19,Contents); { current_90 }
             'd' :
                  SetFlagPos(S[1],23,Contents); { current_dn }
             'u' :
                  SetFlagPos(S[1],22,Contents); { current_up }
            end;
        end;
     'd' :
        case S[3] of
         'a' :
              SetFlagPos(S[1],17,Flags); { damage }
         'e' :
              SetFlagPos(S[1],27,Contents); { detail }
        end;
     'e' :
          SetFlagPos(S[1],21,Flags); { envmapped }
     'f' :
          SetFlagPos(S[1],2,Contents); { fence }
     'h' :
        case S[3] of
         'a' :
              SetFlagPos(S[1],16,Flags); { hardwareonly }
         'i' :
              SetFlagPos(S[1],8,Flags); { hint }
        end;
     'l' :
        case S[3] of
         'a' :
            case S[4] of
             'd' :
                  SetFlagPos(S[1],29,Contents); { ladder }
             'v' :
                  SetFlagPos(S[1],3,Contents); { lava }
            end;
         'i' :
              SetFlagPos(S[1],0,Flags); { light }
        end;
     'm' :
        case S[3] of
         'a' :
              SetFlagPos(S[1],1,Flags); { masked }
         'i' :
            case S[4] of
             'r' :
                  SetFlagPos(S[1],13,Flags); { mirror }
             's' :
                  SetFlagPos(S[1],6,Contents); { mist }
            end;
         'o' :
            if Length(S)=8 then
              SetFlagPos(S[1],25,Contents) { monster }
            else
              SetFlagPos(S[1],17,Contents); { monsterclip }
        end;
     'n' :
        case S[4] of
         'd' :
              SetFlagPos(S[1],7,Flags); { nodraw }
         'f' :
              SetFlagPos(S[1],5,Flags); { nofilter }
         'm' :
              SetFlagPos(S[1],26,Flags); { nomerge }
         'n' :
              SetFlagPos(S[1],4,Flags); { nonlit }
         'r' :
              SetFlagPos(S[1],19,Flags); { normal }
        end;
     'o' :
          SetFlagPos(S[1],24,Contents); { origin }
     'p' :
        case S[3] of
         'l' :
              SetFlagPos(S[1],16,Contents); { playerclip }
         'r' :
              SetFlagPos(S[1],12,Flags); { prelit }
        end;
     'r' :
        case S[3] of
         'a' :
              SetFlagPos(S[1],22,Flags); { random }
         'i' :
              SetFlagPos(S[1],11,Flags); { ricochet }
         'n' :
              SetFlagPos(S[1],24,Flags); { rndtime }
        end;
     's' :
        case S[3] of
         'k' :
            case S[4] of
             'i' :
                  SetFlagPos(S[1],9,Flags); { skip }
             'y' :
                  SetFlagPos(S[1],2,Flags); { sky }
            end;
         'l' :
              SetFlagPos(S[1],4,Contents); { slime }
         'o' :
              SetFlagPos(S[1],0,Contents); { solid }
         'u' :
            case S[9] of
             '0' :
                  SetFlagPos(S[1],27,Flags); { surfbit0 }
             '1' :
                  SetFlagPos(S[1],28,Flags); { surfbit1 }
             '2' :
                  SetFlagPos(S[1],29,Flags); { surfbit2 }
             '3' :
                  SetFlagPos(S[1],30,Flags); { surfbit3 }
            end;
        end;
     't' :
        case S[8] of
         'a' :
              SetFlagPos(S[1],25,Flags); { translate }
         'u' :
              SetFlagPos(S[1],28,Contents); { translucent }
        end;
     'u' :
          SetFlagPos(S[1],15,Flags); { usecolor }
     'w' :
        case S[3] of
         'a' :
            case S[4] of
             'r' :
                  SetFlagPos(S[1],3,Flags); { warping }
             't' :
                  SetFlagPos(S[1],5,Contents); { water }
             'v' :
                  SetFlagPos(S[1],10,Flags); { wavy }
            end;
         'e' :
              SetFlagPos(S[1],18,Flags); { weak }
         'i' :
              SetFlagPos(S[1],1,Contents); { window }
        end;
    end;

     { We now return to our normal programming. }
 end;
 { /tiglari}

 procedure ReadSymbol(PrevSymbolMustBe: TSymbols);
{
ReadSymbol(PrevSymbolMustBe : TSymbols) reads the next token, and checks
whether the previous is what PrevSymbolMustBe says it should have been.
This can also be checked by examining SymbolType, if there are
several possibilities.

   "SymbolType" contains the kind of token just read :
   "sStringToken": a string token, whose value is in "S"
   "sStringQuotedToken": a quote-delimited string, whose value is in "S"
   "sNumValueToken": a floating-point value, found in "NumericValue"

Call the procedure "ReadSymbol()" to get the next token. The argument to the
procedure is the current token kind again; useful to read e.g. three
floating-point values :

   FirstValue:=NumericValue;
   ReadSymbol(sNumValueToken);
   SecondValue:=NumericValue;
   ReadSymbol(sNumValueToken);
   ThirdValue:=NumericValue;
   ReadSymbol(sNumValueToken);

This way, the procedure "ReadSymbol" checks that the kind of token was really the
expected one.
}
 var
   C: Char;
   Arret: Boolean;

   procedure ReadStringToken();
   begin
     S:='';
     repeat
       S:=S+C;
       C:=Source^;
       if C=#0 then
         Break;
       Inc(Source);
     until C in cSeperators;
     if (C=#13) or ((C=#10) and not Juste13) then
       Inc(LineNoBeingParsed);
     Juste13:=C=#13;
     if (LeftStr(S,1)='"') and (RightStr(S,1)='"') then
     begin
       S:=MidStr(S,2,Length(S)-2);
       SymbolType:=sStringQuotedToken;
     end
     else
       SymbolType:=sStringToken;
   end;

 begin
   repeat
     if (SymbolType<>PrevSymbolMustBe) and (PrevSymbolMustBe<>sEOF) then
       Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(248)]);

     repeat
       C:=Source^;
       if C=#0 then
       begin
         SymbolType:=sEOF;
         Exit;
       end;

       Inc(Source);
       if (C=#13) or ((C=#10) and not Juste13) then
         Inc(LineNoBeingParsed);
       Juste13:=C=#13;
     until not (C in cSeperators);

     while Source>Prochain do
     begin
       ProgressIndicatorIncrement;
       Inc(Prochain, Granularite);
     end;

     if ReadSymbolForceToText then
     begin
       ReadStringToken();
       SymbolType:=sTokenForcedToString;
       Exit;
     end;

     Arret:=True;

     case C of
     '(': SymbolType:=sBracketLeft;
     ')': SymbolType:=sBracketRight;
     '{': SymbolType:=sCurlyBracketLeft;
     '}': SymbolType:=sCurlyBracketRight;
     '[': SymbolType:=sSquareBracketLeft;
     ']': SymbolType:=sSquareBracketRight;

     '"':
       begin
         S:='';
         repeat
           C:=Source^;
           if C in [#0, #13, #10] then
           begin
             if (C=#13) or ((C=#10) and not Juste13) then
               Inc(LineNoBeingParsed);
             Juste13:=C=#13;
             if EndOfLine and (S<>'') and (S[Length(S)-1]='"') then
             begin
               SetLength(S, Length(S)-1);
               Break;
             end
             else
               Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(249)]);
           end;

           Inc(Source);
           if (C='"') and not EndOfLine then
             Break;
           S:=S+C;
         until False;
         SymbolType:=sStringQuotedToken;
       end;

     '-', '0'..'9':
       begin
         if (C='-') and not (Source^ in ['0'..'9','.']) then
           ReadStringToken()
         else
         begin
           S:='';
           repeat
             S:=S+C;
             C:=Source^;
             if C=#0 then
               Break;
             Inc(Source);
           until not (C in ['0'..'9', '.']);

           { Did we encounter a exponent-value? Something like: "1.322e-12"
             Then continue to read the characters }
           if (C in cExponentChars) then
           begin
             repeat
               S:=S+C;
               C:=Source^;
               if C=#0 then
                 Break;
               Inc(Source);
             until not (C in ['0'..'9', '-', '+']);
           end;

           if (C=#0) or (C in cSeperators) then
           begin
             if (C=#13) or ((C=#10) and not Juste13) then
               Inc(LineNoBeingParsed);
             Juste13:=C=#13;
             NumericValue:=StrToFloat(S);
             SymbolType:=sNumValueToken;
           end
           else
             Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(251)]);
         end;
       end;

     '/', ';':
       begin
         if (C=';') or (Source^='/') then
         begin
           if C=';' then
             Dec(Source);

           if (Source[1]='T') and (Source[2]='X') then
             TxCommand:=Source[3];

           Inc(Source);
           repeat
             C:=Source^;
             if C=#0 then
               Break;
             Inc(Source);
           until C in [#13, #10];

           if (C=#13) or ((C=#10) and not Juste13) then
             Inc(LineNoBeingParsed);
           Juste13:=C=#13;
           Arret:=False;
         end
         else
           Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(248)]);
       end;

     else
       ReadStringToken();
     end;
   until Arret;
 end;

 function ReadVect(theLastSymbolForceToText: Boolean): TVect;
 begin
   ReadSymbol(sBracketLeft);
   Result.X:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.Y:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.Z:=NumericValue;
   ReadSymbol(sNumValueToken);
   ReadSymbolForceToText:=theLastSymbolForceToText;
   ReadSymbol(sBracketRight);
   ReadSymbolForceToText:=False;
 end;

 function ReadVect10(theLastSymbolForceToText: Boolean): TVect10;
 begin
   ReadSymbol(sBracketLeft);
   Result.X1:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X2:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X3:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X4:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X5:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X6:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X7:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X8:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X9:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X10:=NumericValue;
   ReadSymbol(sNumValueToken);
   ReadSymbolForceToText:=theLastSymbolForceToText;
   ReadSymbol(sBracketRight);
   ReadSymbolForceToText:=False;
 end;

 function ReadVect7(theLastSymbolForceToText: Boolean): TVect7;
 begin
   ReadSymbol(sBracketLeft);
   Result.X1:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X2:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X3:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X4:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X5:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X6:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.X7:=NumericValue;
   ReadSymbol(sNumValueToken);
   ReadSymbolForceToText:=theLastSymbolForceToText;
   ReadSymbol(sBracketRight);
   ReadSymbolForceToText:=False;
 end;

 function ReadVect5(theLastSymbolForceToText: Boolean): TVect5;
 begin
   ReadSymbol(sBracketLeft);
   Result.X:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.Y:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.Z:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.S:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.T:=NumericValue;
   ReadSymbol(sNumValueToken);
   ReadSymbolForceToText:=theLastSymbolForceToText;
   ReadSymbol(sBracketRight);
   ReadSymbolForceToText:=False;
 end;
 
 function ReadVect4(theLastSymbolForceToText: Boolean): TVect4;
 begin
   ReadSymbol(sBracketLeft);
   Result.X:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.Y:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.Z:=NumericValue;
   ReadSymbol(sNumValueToken);
   Result.D:=NumericValue;
   ReadSymbol(sNumValueToken);
   ReadSymbolForceToText:=theLastSymbolForceToText;
   ReadSymbol(sBracketRight);
   ReadSymbolForceToText:=False;
 end;

 procedure ReadSinSurfaceFlags(Surface: TFace);
 begin
   { tiglari[, sin surf info reading }

   { Here's how you get texture info from its name.
     Note the use of BuildQ2Header to get the default
     fields for the texture.  Something like this is
     needed because Sin maps mark the *difference* between
     the default & what the properties of the face are for
     these fields. }
   { this loads some a sort of index to the texture, but doesn't
      really load it.  The 2nd arg is only useful when editing
      a bsp with the textures in it }
   Q:=GlobalFindTexture(Surface.NomTex, Nil);
   if Q<>Nil then
   begin
     { this does the real loading.  always check if loading happened.
       if Q comes up Nil at the end, all defaults will be 0 }
     Q:=Q.LoadPixelSet;
     if not (Q is QTextureSin) then
       Q:=Nil;
   end;
   Contents:=StrToInt(Q.Specifics.Values['Contents']);
   Flags:=StrToInt(Q.Specifics.Values['Flags']);
   while SymbolType=sStringToken do
   begin  { verbose but fast, c.f. QkSin: QTextureSin.LoadFile }
    if S = 'color' then  { three following values }
     begin
      ReadSymbol(sStringToken);
      ThreeSing[0] := NumericValue;
      S1 := FloatToStrF(NumericValue,ffFixed,7,2);
      ReadSymbol(sNumValueToken);
      ThreeSing[1] := NumericValue;
      S1 := S1+' '+FloatToStrF(NumericValue,ffFixed,7,2);
      ReadSymbol(sNumValueToken);
      ThreeSing[2] := NumericValue;
      S1 := S1+' '+FloatToStrF(NumericValue,ffFixed,7,2);
      ReadSymbol(sNumValueToken);
      Surface.Specifics.Add('color='+S1);
    { Surface.SetFloatsSpec('color', ThreeSing);  }
     end
    else
    if S = 'directstyle' then { following string value }
     begin
      ReadSymbol(sStringToken);
      Surface.Specifics.Add('directstyle='+S);
      ReadSymbol(sStringToken);
     end
    else
    if (S[1] = '+') or (S[1] = '-') then  { no following value }
     begin
       SetSinFlag(); { one big momma of a procedure }
       ReadSymbol(sStringToken);
     end
    else
     begin  { 1 following value, get it and act }
      S1:=S;
      ReadSymbol(sStringToken);
      LastValue:=NumericValue;
      ReadSymbol(sNumValueToken);
      case S1[1] of
        'a' : if S1 = 'animtime' then
                begin
                 Surface.SetFloatSpec('animtime', LastValue);
                end;
        'd' : if S1 = 'direct' then
                begin
                 Surface.Specifics.Add('direct='+IntToStr(Round(LastValue)));
                end
              else
              if S1 = 'directangle' then
                begin
                 Surface.Specifics.Add('directangle='+IntToStr(Round(LastValue)));
                end;
        'f' : if S1 = 'friction' then
                begin
                 Surface.SetFloatSpec('friction', LastValue);
                end;
        'l' : if S1 = 'lightvalue' then { assuming that this is the old Value }
                begin
                  Surface.Specifics.Add('Value='+IntToStr(Round(LastValue)));
                end;
        'n' : if S1 = 'nonlitvalue' then { note name discrepancy }
                begin
                 Surface.SetFloatSpec('nonlit', LastValue);
                end;
        'r' : if S1 = 'restitution' then
                begin
                 Surface.SetFloatSpec('restitution', LastValue);
                end;
        't' : if S1 = 'translucence' then
                begin
                  Surface.SetFloatSpec('translucence', LastValue)
                end
              else
              if S1 = 'trans_mag' then
                begin
                  Surface.SetFloatSpec('trans_mag', LastValue)
                end
              else
              if S1 = 'trans_angle' then
                begin
                 Surface.Specifics.Add('trans_angle='+IntToStr(Round(LastValue)));
                end;
     end
    end
   end;
   { now set Flags & Contents }
   S1 := IntToStr(Flags);
   Surface.Specifics.Values['Contents']:=IntToStr(Contents);
   Surface.Specifics.Values['Flags']:=IntToStr(Flags);
  { /tiglari }
 end;

 { sort of like Sin, but simpler; we just read the flags without
   bothering to check that they're not the same as the defaults }

 procedure ReadMohaaSurfaceParms(Surface: TTexturedTreeMap);
 var
   Val: String;
 begin
   while SymbolType=sStringToken do
   begin // the +/- surfaceparms
     if (S[1] = '+') or (S[1] = '-') then // read 'surfaceparm' and then a surfaceparm
     begin
       if (Copy(S,2,Length(S)-1)='surfaceparm') then
       begin
         if S[1]='+' then
           Val:='1'
         else
           Val:='0';
         ReadSymbol(sStringToken);
         if Symboltype=sStringToken then
         begin
           Surface.Specifics.Values['_esp_'+S]:=Val
         end;
         ReadSymbol(sStringToken);
       end;
     end
     else
     // surfaceLight (int), surfaceColor (3f), surfaceAngle (int), surfaceDensity (int), subdivisions (1f), tesselation (1f)
     if S='surfaceLight' then
     begin
       ReadSymbol(sStringToken);
       Surface.Specifics.Values['surfaceLight']:=S;
       ReadSymbol(sNumValueToken);
     end
     else
     if S='surfaceColor' then
     begin
       ReadSymbol(sStringToken);
       ThreeSing[0]:=NumericValue;
       ReadSymbol(sNumValueToken);
       ThreeSing[1]:=NumericValue;
       ReadSymbol(sNumValueToken);
       ThreeSing[2]:=NumericValue;
       ReadSymbol(sNumValueToken);
       Surface.SetFloatsSpec('surfaceColor',ThreeSing);
     end
     else
     if S='surfaceAngle' then
     begin
       ReadSymbol(sStringToken);
       Surface.Specifics.Values['surfaceAngle']:=S;
       ReadSymbol(sNumValueToken);
     end
     else
     if s='surfaceDensity' then
     begin
       ReadSymbol(sStringToken);
       Surface.Specifics.Values['surfaceDensity']:=S;
       ReadSymbol(sNumValueToken);
     end
     else
     if s='subdivisions' then
     begin
       ReadSymbol(sStringToken);
       Surface.SetFloatSpec('subdivisions',NumericValue);
       ReadSymbol(sNumValueToken);
     end
     else
     if s='tesselation' then
     begin
       ReadSymbol(sStringToken);
       Surface.SetFloatSpec('tesselation',NumericValue);
       ReadSymbol(sNumValueToken);
     end
     else
       Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(265)]);  // unexpected surface attribute
   end;
 end;


 procedure ReadSquareTex4 (var Axis : TVect; var Shift : Double);
 begin
   ReadSymbol(sSquareBracketLeft);
   Axis.X:=NumericValue;
   ReadSymbol(sNumValueToken);
   Axis.Y:=NumericValue;
   ReadSymbol(sNumValueToken);
   Axis.Z:=NumericValue;
   ReadSymbol(sNumValueToken);
   Shift:=NumericValue;
   ReadSymbol(sNumValueToken);
 end;

(* The wc3.3 220 map format texture rep is quite close
  to the texinfo_t data structure used by qbsp.  This
  consists of 2 axes lying in one of the three planes
  normal to the axes, plus offsets, & the formula for
  computing the texture coordinate of a point xyz on
  a face is:
   u = x * u_axis.x + y * u_axis.y + z * u_axis.z + u_offset
   v = x * v_axis.x + y * v_axis.y + z * v_axis.z + v_offset
  (Max McGuire's Quake2 BSP file format tutorial on
   www.flipcode.com)

  However wc3.3 does *not* seem to require the texture-vectors
  to lie in an axis plane, and if you write with that assumption
  (projecting the points), things get distorted.

  U/V Axis/Shift are straight from the 4-vectors, param[3]
  is rot which is ignored (implicit from the axes), while
  param[4,5] are UV scales.  Diferent from the bsp-format
  is that the axes are normalized to length 1, and you
  divide by the scale to get the .bsp-version of the axis.
  (Zoner's HL tools source, textures.cpp) *)

 procedure WC33Params(Surface: TFace);
 var
  PP0, PP1, PP2, NP0, NP1, NP2, PlanePoint, TexNorm : TVect;
 begin
   PP0:=VecSum(VecScale(-UShift*Params[4], UAxis),VecScale(-VShift*Params[5], VAxis));
   PP1:=VecSum(PP0,VecScale(Params[4]*128,UAxis));
    { note p.i.t.a sign-flip }
   PP2:=VecSum(PP0,VecScale(-Params[5]*128,VAxis));
   with Surface do
   begin
     TexNorm:=Cross(UAxis,VAxis);
     Normalise(TexNorm);
     PlanePoint:=VecScale(Dist, Normale);
     (* could perhaps be optimized by 'partial evaluation' *)
     try
       NP0:=ProjectPointToPlane(PP0, TexNorm, PlanePoint, Normale);
       NP1:=ProjectPointToPlane(PP1, TexNorm, PlanePoint, Normale);
       NP2:=ProjectPointToPlane(PP2, TexNorm, PlanePoint, Normale);
       SetThreePointsEx(NP0,NP1,NP2,Normale);
     except
       g_MapError.AddText('Problem with texture scale of face '+IntToStr(FaceNum)+ ' in brush '+IntToStr(BrushNum)+' in hull '+IntToStr(HullNum+1));
     end;
  end;
 end;

 procedure ReadCOD2Flags;
 begin
   //FIXME: We need to do this right. At the moment, we're just dumping the data!
   if (SymbolType<>sStringToken) then
     Exit;
   if S='contents' then
   begin
     // "contents"
     ReadSymbol(sStringToken);
     // "details;"
     ReadSymbol(sStringToken);
   end;
   if S='toolFlags' then
   begin
     // "toolFlags"
     ReadSymbol(sStringToken);
     // *whatever*
     ReadSymbol(sStringToken);
   end;
 end;

 procedure ReadCOD2SurfaceParams;
 begin
   //FIXME: We need to do this right. At the moment, we're just dumping the data!

   //Lightmap name...
   ReadSymbol(sStringToken);
   //Param 1
   ReadSymbol(sNumValueToken);
   //Param 2
   ReadSymbol(sNumValueToken);
   //Param 3
   ReadSymbol(sNumValueToken);
   //Param 4
   ReadSymbol(sNumValueToken);
   //Param 5
   ReadSymbol(sNumValueToken);
   //Param 6
   ReadSymbol(sNumValueToken);
 end;

 procedure ReadPatchDef2;
 var
   I, J: Integer;
   V5: TVect5;
   TexPath: String;
 begin
   ReadSymbol(sStringToken); // lbrace follows "patchDef2"
   ReadSymbol(sCurlyBracketLeft); // texture follows lbrace

   { DanielPharos: We've got a problem here...
   Map versions 2 and higher explicitly put 'textures/' in front
   of the paths! All we can do for the moment is cutting that off.
   In the future, somebody should change QuArK's behaviour to where
   you can set if this path gets prefixed. }
   if MapVersion>1 then
   begin
     TexPath:=IncludeTrailingPathDelimiter(GameTexturesPath);
     if LowerCase(LeftStr(S,Length(TexPath)))=TexPath then
       S:=RightStr(S,Length(S)-Length(TexPath));
   end;

   if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
     S:=LowerCase(S);
   Q2Tex:=Q2Tex or (Pos('/',S)<>0);

   B:=TBezier.Create(LoadStr1(261), EntiteBezier); // 261 = "bezier"
   EntiteBezier.SubElements.Add(B); //&&&
   B.NomTex:=S;   { here we get the texture-name }

   if MapVersion>1 then
     ReadSymbol(sStringQuotedToken) // lparen follows texture
   else
     ReadSymbol(sStringToken); // lparen follows texture

   // now comes 5 numbers which tell how many control points there are
   // use ReadVect5 which is the same as ReadVect but expects 5 numbers
   // and we only need the X and Y values
   //V5:=ReadVect5(False);
   //Can't use ReadVect5 here, since MOHAA dumps surface flags inside the vector...!
   ReadSymbol(sBracketLeft);
   V5.X:=NumericValue;
   ReadSymbol(sNumValueToken);
   V5.Y:=NumericValue;
   ReadSymbol(sNumValueToken);
   V5.Z:=NumericValue;
   ReadSymbol(sNumValueToken);
   V5.S:=NumericValue;
   ReadSymbol(sNumValueToken);
   V5.T:=NumericValue;
   ReadSymbol(sNumValueToken);
   if SymbolType = sStringToken then
   begin
     MOHAAPatchDef2Detected := True;
     ReadMohaaSurfaceParms(B);
   end
   else
     MOHAAPatchDef2Detected := False;
   ReadSymbol(sBracketRight);
   // Nr 1: Width (many lines of control points there are)
   // Nr 2: Height (how many control points on each line)
   // Nr 3: HorzSubdivisions (?)
   // Nr 4: VertSubdivisions (?)
   // Nr 5: ?

   MeshBuf1.W := Round(V5.X);
   MeshBuf1.H := Round(V5.Y);

   GetMem(MeshBuf1.CP, MeshBuf1.W * MeshBuf1.H * SizeOf(vec5_t));
   try
     ReadSymbol(sBracketLeft); // lparen follows vect5
     for I:=0 to MeshBuf1.W-1 do
     begin
       pCP1:=MeshBuf1.CP;
       Inc(pCP1, I);
       ReadSymbol(sBracketLeft); // read the leading lparen for the line
       for J:=1 to MeshBuf1.H do
       begin
         V5:=ReadVect5(False);
         pCP1^[0]:=V5.X;
         pCP1^[1]:=V5.Y;
         pCP1^[2]:=V5.Z;
         pCP1^[3]:=V5.S;
         pCP1^[4]:=V5.T;
         Inc(pCP1, MeshBuf1.W);
       end;
       ReadSymbol(sBracketRight); // read the trailing rparen for the line
     end;
     ReadSymbol(sBracketRight);  { rparen which finishes all the lines of control points }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the patchDef2 }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }

     B.ControlPoints:=MeshBuf1;
     B.AutoSetSmooth;
   finally
     FreeMem(MeshBuf1.CP);
   end;
 end;
 
 
 procedure ReadPatchDef3;
 var
   I, J: Integer;
   V5: TVect5;
   V7: TVect7;
   TexPath: String;
 begin
   ReadSymbol(sStringToken); // lbrace follows "patchDef3"
   ReadSymbol(sCurlyBracketLeft); // texture follows lbrace

   { DanielPharos: We've got a problem here...
   Map versions 2 and higher explicitly put 'textures/' in front
   of the paths! All we can do for the moment is cutting that off.
   In the future, somebody should change QuArK's behaviour to where
   you can set if this path gets prefixed. }
   if MapVersion>1 then
   begin
     TexPath:=IncludeTrailingPathDelimiter(GameTexturesPath);
     if LowerCase(LeftStr(S,Length(TexPath)))=TexPath then
       S:=RightStr(S,Length(S)-Length(TexPath));
   end;

   if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
     S:=LowerCase(S);
   Q2Tex:=Q2Tex or (Pos('/',S)<>0);

   B:=TBezier.Create(LoadStr1(261), EntiteBezier); // 261 = "bezier"
   EntiteBezier.SubElements.Add(B); //&&&
   B.NomTex:=S;   { here we get the texture-name }

   if MapVersion>1 then
     ReadSymbol(sStringQuotedToken) // lparen follows texture
   else
     ReadSymbol(sStringToken); // lparen follows texture

   // now comes 7 numbers which tell how many control points there are
   // use ReadVect7 which is the same as ReadVect but expects 7 numbers
   // and we only need the first and second values
   V7:=ReadVect7(False);
   // Nr 1: Width (many lines of control points there are)
   // Nr 2: Height (how many control points on each line)
   // Nr 3: HorzSubdivisions (?)
   // Nr 4: VertSubdivisions (?)
   // Nr 5: ?
   // Nr 6: ?
   // Nr 7: ?

   MeshBuf1.W := Round(V7.X1);
   MeshBuf1.H := Round(V7.X2);

   GetMem(MeshBuf1.CP, MeshBuf1.W * MeshBuf1.H * SizeOf(vec5_t));
   try
     ReadSymbol(sBracketLeft); // lparen follows vect7
     for I:=0 to MeshBuf1.W-1 do
     begin
       pCP1:=MeshBuf1.CP;
       Inc(pCP1, I);
       ReadSymbol(sBracketLeft); // read the leading lparen for the line
       for J:=1 to MeshBuf1.H do
       begin
         V5:=ReadVect5(False);
         pCP1^[0]:=V5.X;
         pCP1^[1]:=V5.Y;
         pCP1^[2]:=V5.Z;
         pCP1^[3]:=V5.S;
         pCP1^[4]:=V5.T;
         Inc(pCP1, MeshBuf1.W);
       end;
       ReadSymbol(sBracketRight); // read the trailing rparen for the line end
     end;
     B.ControlPoints:=MeshBuf1;
     B.AutoSetSmooth;

     ReadSymbol(sBracketRight);  { rparen which finishes all the lines of control points }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the patchDef3 }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }
   finally
     FreeMem(MeshBuf1.CP);
   end;
 end;

 //FIXME: This is more or less a dummy procedure...
 procedure ReadPatchTerrainDef3;
 var
   I, J: Integer;
   V7: TVect7;
   V10: TVect10;
   TexPath: String;
 begin
   ReadSymbol(sStringToken); // lbrace follows "patchTerrainDef3"
   ReadSymbol(sCurlyBracketLeft); // texture follows lbrace

   { DanielPharos: We've got a problem here...
   Map versions 2 and higher explicitly put 'textures/' in front
   of the paths! All we can do for the moment is cutting that off.
   In the future, somebody should change QuArK's behaviour to where
   you can set if this path gets prefixed. }
   if MapVersion>1 then
   begin
     TexPath:=IncludeTrailingPathDelimiter(GameTexturesPath);
     if LowerCase(LeftStr(S,Length(TexPath)))=TexPath then
       S:=RightStr(S,Length(S)-Length(TexPath));
   end;

   if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
     S:=LowerCase(S);
   Q2Tex:=Q2Tex or (Pos('/',S)<>0);

//   B:=TBezier.Create(LoadStr1(261), EntiteBezier); // 261 = "bezier"
//   EntiteBezier.SubElements.Add(B); //&&&
//   B.NomTex:=S;   { here we get the texture-name }

   if MapVersion>1 then
     ReadSymbol(sStringQuotedToken) // lparen follows texture
   else
     ReadSymbol(sStringToken); // lparen follows texture

   // now comes 7 numbers which tell how many control points there are
   // use ReadVect7 which is the same as ReadVect but expects 7 numbers
   // and we only need the first and second values
   V7:=ReadVect7(False);
   // Nr 1: Width (many lines of control points there are)
   // Nr 2: Height (how many control points on each line)
   // Nr 3: HorzSubdivisions (?)
   // Nr 4: VertSubdivisions (?)
   // Nr 5: ?
   // Nr 6: ?
   // Nr 7: ?

   MeshBuf1.W := Round(V7.X1);
   MeshBuf1.H := Round(V7.X2);

   GetMem(MeshBuf1.CP, MeshBuf1.W * MeshBuf1.H * SizeOf(vec5_t));
   try
     ReadSymbol(sBracketLeft); // lparen follows vect7
     for I:=0 to MeshBuf1.W-1 do
     begin
       pCP1:=MeshBuf1.CP;
       Inc(pCP1, I);
       ReadSymbol(sBracketLeft); // read the leading lparen for the line
       for J:=1 to MeshBuf1.H do
       begin
         V10:=ReadVect10(False);
         pCP1^[0]:=V10.X1;
         pCP1^[1]:=V10.X2;
         pCP1^[2]:=V10.X3;
         pCP1^[3]:=V10.X4;
         pCP1^[4]:=V10.X5;
         //FIXME: What are the other values? (6-9 look like color-RGBA)
         Inc(pCP1, MeshBuf1.W);
       end;
       ReadSymbol(sBracketRight); // read the trailing rparen for the line end
     end;
//     B.ControlPoints:=MeshBuf1; //FIXME: Doesn't work...!
//     B.AutoSetSmooth;

     ReadSymbol(sBracketRight);  { rparen which finishes all the lines of control points }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the patchTerrainDef3 }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }
   finally
     FreeMem(MeshBuf1.CP);
   end;
 end;

 //FIXME: This is more or less a dummy procedure...
 procedure ReadPatchDef5;
 var
   I, J: Integer;
   V7: TVect7;
   V10: TVect10;
   TexPath: String;
 begin
   ReadSymbol(sStringToken); // lbrace follows "patchDef5"
   ReadSymbol(sCurlyBracketLeft); // texture follows lbrace

   { DanielPharos: We've got a problem here...
   Map versions 2 and higher explicitly put 'textures/' in front
   of the paths! All we can do for the moment is cutting that off.
   In the future, somebody should change QuArK's behaviour to where
   you can set if this path gets prefixed. }
   if MapVersion>1 then
   begin
     TexPath:=IncludeTrailingPathDelimiter(GameTexturesPath);
     if LowerCase(LeftStr(S,Length(TexPath)))=TexPath then
       S:=RightStr(S,Length(S)-Length(TexPath));
   end;

   if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
     S:=LowerCase(S);
   Q2Tex:=Q2Tex or (Pos('/',S)<>0);

//   B:=TBezier.Create(LoadStr1(261), EntiteBezier); // 261 = "bezier"
//   EntiteBezier.SubElements.Add(B); //&&&
//   B.NomTex:=S;   { here we get the texture-name }

   if MapVersion>1 then
     ReadSymbol(sStringQuotedToken) // lparen follows texture
   else
     ReadSymbol(sStringToken); // lparen follows texture

   // now comes 7 numbers which tell how many control points there are
   // use ReadVect7 which is the same as ReadVect but expects 7 numbers
   // and we only need the first and second values
   V7:=ReadVect7(False);
   // Nr 1: Width (many lines of control points there are)
   // Nr 2: Height (how many control points on each line)
   // Nr 3: HorzSubdivisions (?)
   // Nr 4: VertSubdivisions (?)
   // Nr 5: ?
   // Nr 6: ?
   // Nr 7: ?


   MeshBuf1.W := Round(V7.X1);
   MeshBuf1.H := Round(V7.X2);

   GetMem(MeshBuf1.CP, MeshBuf1.W * MeshBuf1.H * SizeOf(vec5_t));
   try
     ReadSymbol(sBracketLeft); // lparen follows vect7
     for I:=0 to MeshBuf1.W-1 do
     begin
       pCP1:=MeshBuf1.CP;
       Inc(pCP1, I);
       ReadSymbol(sBracketLeft); // read the leading lparen for the line
       for J:=1 to MeshBuf1.H do
       begin
         V10:=ReadVect10(False);
         pCP1^[0]:=V10.X1;
         pCP1^[1]:=V10.X2;
         pCP1^[2]:=V10.X3;
         pCP1^[3]:=V10.X4;
         pCP1^[4]:=V10.X5;
         //FIXME: What are the other values? (6-9 look like color-RGBA)
         Inc(pCP1, MeshBuf1.W);
       end;
       ReadSymbol(sBracketRight); // read the trailing rparen for the line end
     end;
//     B.ControlPoints:=MeshBuf1;
//     B.AutoSetSmooth;

     ReadSymbol(sBracketRight);  { rparen which finishes all the lines of control points }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the patchDef5 }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }
   finally
     FreeMem(MeshBuf1.CP);
   end;
 end;

 procedure ReadBrush;
 var
   I: Integer;
   Surface: TFace;
 begin
  P:=TPolyhedron.Create(LoadStr1(138), EntitePoly);
  EntitePoly.SubElements.Add(P);
  ContentsFlags:=0;
  while SymbolType <> sCurlyBracketRight do  { read the faces }
   begin
    TxCommand:=#0; { Reset the QuArK-special '//TX1' '//TX2' indicator to not-found }
    Inc(FaceNum);
    V[1]:=ReadVect(False);
    V[2]:=ReadVect(False);
    V[3]:=ReadVect(True);
    Surface:=TFace.Create(LoadStr1(139), P);
    P.SubElements.Add(Surface);
    Surface.SetThreePoints(V[1], V[3], V[2]);

    if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
      S:=LowerCase(S);
    Q2Tex:=Q2Tex or (Pos('/',S)<>0);

    Surface.NomTex:=S;   { here we get the texture-name }
    ReadSymbol(sTokenForcedToString);
    {DECKER}
    if (SymbolType=sSquareBracketLeft) then
    begin
      { WorldCraft3.3 map-format version 220 encountered }
      WC33map:=True;
      {Params[1]:=ReadSquareTex4();} {Read some texture scale/position thingy}
      ReadSquareTex4(UAxis, UShift);
      ReadSymbol(sSquareBracketRight);
     { Params[2]:=ReadSquareTex4(); }{Read some texture scale/position thingy}
      ReadSquareTex4(VAxis, VShift);
      ReadSymbol(sSquareBracketRight);
      for I:=3 to 5 do {Read the last three values}
        { Rot, UScale, VScale; Rot always 0 }
       begin
        Params[I]:=NumericValue;
        ReadSymbol(sNumValueToken);
       end;
    end
    else
    begin
      for I:=1 to 5 do
       begin
        Params[I]:=NumericValue;
        ReadSymbol(sNumValueToken);
       end;
      if charmodejeu=mjGenesis3D then
      begin
        if PointsToPlane(Surface.Normale)='X' then
           Params[4]:=-Params[4];
      end;
    end;
    {/DECKER}
    if Result=mjCOD2 then
    begin
      //What is this 6th number...?
      NumericValue1:=Round(NumericValue);
      ReadSymbol(sNumValueToken);

      ReadCOD2SurfaceParams;
    end
    else
    begin
      if SymbolType=sNumValueToken then
       begin
        NumericValue1:=Round(NumericValue);
        ReadSymbol(sNumValueToken);
        if SymbolType<>sNumValueToken then
         Result:=mjHexen  { Hexen II : ignore la luminosité de radiation }
         //Could also be bad Call of Duty 2 file (when the 'iwmap 4' header is missing)
        else
         begin  { Quake 2, Heretic2 and Mohaa : read the three fields }
          ContentsFlags:=NumericValue1;
          Surface.Specifics.Values['Contents']:=IntToStr(NumericValue1);
          Surface.Specifics.Values['Flags']:=IntToStr(Round(NumericValue));
          ReadSymbol(sNumValueToken);
          Surface.Specifics.Values['Value']:=IntToStr(Round(NumericValue));
          ReadSymbol(sNumValueToken);
          if Result=mjQuake then
            Result:=mjNotQuake1;
          if SymbolType=sStringToken then // Mohaa
          begin
            Result:=mjMOHAA;
            ReadMohaaSurfaceParms(Surface);
          end
          else if SymbolType=sNumValueToken then //CoD1
          begin
            Result:=mjCoD;
            //FIXME: What does this number mean?
            ReadSymbol(sNumValueToken);
          end;
         end;
       end
      else
       if SymbolType=sStringToken then
        begin  { Sin : extra surface flags as text }
         Result:=mjSin;
         ReadSinSurfaceFlags(Surface); {DECKER - moved to procedure to increase readability}
        end;
    end;
    if not Surface.LoadData then
     Inc(InvFaces)
    else
     case TxCommand of   { "//TX#" means that the three points already define the texture params themselves }
      '1': ;
      '2': Surface.TextureMirror:=True;
     else
      if WC33Map then
        WC33Params(Surface)
      else
       with Surface do
        SetFaceFromParams(Normale, Dist, Params);
     end;
   end;

  ReadSymbol(sCurlyBracketRight);

  if not P.CheckPolyhedron then
   Inc(InvPoly)
  else
   if ContentsFlags and ContentsOrigin <> 0 then
    OriginBrush:=P;
 end;


 procedure ReadBrushDef;
 var
   R1, R2, TexS, TexT, Tex0, P0, P1, P2, ZVect : TVect;
   Denom : Double;
   Matrix : TMatrixTransformation;
   Surface: TFace;
   TexPath: String;
 begin
  ReadSymbol(sStringToken); // lbrace follows "brushDef"
  ReadSymbol(sCurlyBracketLeft); // data follows lbrace
  P:=TPolyhedron.Create(LoadStr1(138), EntitePoly);
  EntitePoly.SubElements.Add(P);
  ContentsFlags:=0;
  while SymbolType <> sCurlyBracketRight do  { read the faces }
  begin
    TxCommand:=#0; { Reset the QuArK-special '//TX1' '//TX2' indicator to not-found }
    Inc(FaceNum);
    V[1]:=ReadVect(False);
    V[2]:=ReadVect(False);
    V[3]:=ReadVect(False);
    ReadSymbol(sBracketLeft);
    R1:=ReadVect(False);
    R2:=ReadVect(False);
    ReadSymbolForceToText:=true;
    ReadSymbol(sBracketRight);
    ReadSymbolForceToText:=false;
    Surface:=TFace.Create(LoadStr1(139), P);
    P.SubElements.Add(Surface);
    Surface.SetThreePoints(V[1], V[3], V[2]);
    if not Surface.LoadData then
      ShowMessage('LoadData failure');
     { set relevant attributes }
    { get 3points expressed in AxisBase coordinates
       (see infobase|Src|Topics|Scale|Brush primitives) }
    Denom:=R1.X*R2.Y-R1.Y*R2.X;
    P0.X:=(-R1.Z*R2.Y+R1.Y*R2.Z)/Denom;      {-a13*a22+a12*a23}
    P0.Y:=-(R1.X*R2.Z-R1.Z*R2.X)/Denom;       {-(a11*a23-a13*a21)}
    P0.Z:=0.0;
    P1.X:=(-R1.Z*R2.Y+R2.Y+R1.Y*R2.Z)/Denom; {-a13*a22+a22+a12*a23}
    P1.Y:=-(R1.X*R2.Z-R1.Z*R2.X+R2.X)/Denom;  {-(a11*a23-a13*a21+a21)}
    P1.Z:=0.0;
    P2.X:=(R1.Y*R2.Z+R1.Y-R1.Z*R2.Y)/Denom;  {a12*a23+a12-a13*a22}
    P2.Y:=-(-R1.Z*R2.X+R1.X*R2.Z+R1.X)/Denom; {-(-a13*a21+a11*a23+a11)}
    P2.Z:=0.0;
    { Convert to map space }
    GetAxisBase(Surface.Normale, TexS, TexT);
    Tex0:=VecScale(Surface.Dist, Surface.Normale);
    ZVect.X:=0; ZVect.Y:=0; ZVect.Z:=1;
    Matrix:=MatrixFromCols(TexS, TexT, ZVect);
    P0:=VecSum(MatrixMultByVect(Matrix,P0),Tex0);
    P1:=VecSum(MatrixMultByVect(Matrix,P1),Tex0);
    P2:=VecSum(MatrixMultByVect(Matrix,P2),Tex0);

    { DanielPharos: We've got a problem here...
    Map versions 2 and higher explicitly put 'textures/' in front
    of the paths! All we can do for the moment is cutting that off.
    In the future, somebody should change QuArK's behaviour to where
    you can set if this path gets prefixed. }
    if MapVersion>1 then
    begin
      TexPath:=IncludeTrailingPathDelimiter(GameTexturesPath);
      if LowerCase(LeftStr(S,Length(TexPath)))=TexPath then
        S:=RightStr(S,Length(S)-Length(TexPath));
    end;

    if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
      S:=LowerCase(S);
    Q2Tex:=Q2Tex or (Pos('/',S)<>0);

    Surface.NomTex:=S;   { here we get the texture-name }
    Surface.SetThreePointsUserTex(P0,P1,P2,nil);
    ReadSymbol(sTokenForcedToString);
    if SymbolType=sNumValueToken then
    begin
      NumericValue1:=Round(NumericValue);
      ReadSymbol(sNumValueToken);
      ContentsFlags:=NumericValue1;
      Surface.Specifics.Values['Contents']:=IntToStr(NumericValue1);
      Surface.Specifics.Values['Flags']:=IntToStr(Round(NumericValue));
      ReadSymbol(sNumValueToken);
      Surface.Specifics.Values['Value']:=IntToStr(Round(NumericValue));
      ReadSymbol(sNumValueToken);
      Result:=mjNotQuake1;
    end
  end;
  ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brushDef }
  ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }
 end;

 procedure ReadBrushDef3;
 var
   R1, R2, TexS, TexT, Tex0, P0, P1, P2, ZVect : TVect;
   Denom : Double;
   texname : String;
   Plane : TVect4;
   normal : TVect;
   dist : double;
   texparm : TFaceParams;
   Matrix : TMatrixTransformation;
   Surface: TFace;
   TexPath: String;
 begin
  ReadSymbol(sStringToken); // lbrace follows "brushDef3"
  ReadSymbol(sCurlyBracketLeft); // texture follows lbrace
  P:=TPolyhedron.Create(LoadStr1(138), EntitePoly);
  EntitePoly.SubElements.Add(P);
  ContentsFlags:=0;
  while SymbolType <> sCurlyBracketRight do  { read the faces }
  begin
    TxCommand:=#0; { Reset the QuArK-special '//TX1' '//TX2' indicator to not-found }
    Inc(FaceNum);

    Plane := ReadVect4(False);
    ReadSymbol(sBracketLeft);

    normal.X := Plane.X;
    normal.Y := Plane.Y;
    normal.Z := Plane.Z;
    dist := -Plane.D;

    R1 := ReadVect(False);
    R2 := ReadVect(False);

    V[1] := R1;
    V[2] := R2;

    ReadSymbolForceToText:=true;
    ReadSymbol(sBracketRight);
    texname := S;
    ReadSymbolForceToText:=false;

    ReadSymbol(sTokenForcedToString);

    if MapVersion<3 then
    begin
      v[3].x := NumericValue;
      ReadSymbol(sNumValueToken);
      v[3].y := NumericValue;
      ReadSymbol(sNumValueToken);
      v[3].z := NumericValue;
      ReadSymbol(sNumValueToken);
    end
    else
    begin
      v[3].x := 0;
      v[3].y := 0;
      v[3].z := 0;
    end;

    Surface:=TFace.Create(LoadStr1(139), P);
    P.SubElements.Add(Surface);
    Surface.SetThreePoints(V[1], V[3], V[2]);

    texparm[1] := 0;
    texparm[2] := 0;
    texparm[3] := 0;
    texparm[4] := 0;
    texparm[5] := 0;
    Surface.SetFaceFromParams(normal, dist, texparm);
    if not Surface.LoadData then
      ShowMessage('LoadData failure');

    Denom:=R1.X*R2.Y-R1.Y*R2.X;
    P0.X:=(-R1.Z*R2.Y+R1.Y*R2.Z)/Denom;      {-a13*a22+a12*a23}
    P0.Y:=-(R1.X*R2.Z-R1.Z*R2.X)/Denom;       {-(a11*a23-a13*a21)}
    P0.Z:=0.0;
    P1.X:=(-R1.Z*R2.Y+R2.Y+R1.Y*R2.Z)/Denom; {-a13*a22+a22+a12*a23}
    P1.Y:=-(R1.X*R2.Z-R1.Z*R2.X+R2.X)/Denom;  {-(a11*a23-a13*a21+a21)}
    P1.Z:=0.0;
    P2.X:=(R1.Y*R2.Z+R1.Y-R1.Z*R2.Y)/Denom;  {a12*a23+a12-a13*a22}
    P2.Y:=-(-R1.Z*R2.X+R1.X*R2.Z+R1.X)/Denom; {-(-a13*a21+a11*a23+a11)}
    P2.Z:=0.0;
    { Convert to map space }
    GetAxisBase(Surface.Normale, TexS, TexT);
    {GetAxisBase(normal, TexS, TexT);    }
    Tex0:=VecScale(Surface.Dist, Surface.Normale);
    ZVect.X:=0; ZVect.Y:=0; ZVect.Z:=1;
    Matrix:=MatrixFromCols(TexS, TexT, ZVect);
    P0:=VecSum(MatrixMultByVect(Matrix,P0),Tex0);
    P1:=VecSum(MatrixMultByVect(Matrix,P1),Tex0);
    P2:=VecSum(MatrixMultByVect(Matrix,P2),Tex0);

    { DanielPharos: We've got a problem here...
    Map versions 2 and higher explicitly put 'textures/' in front
    of the paths! All we can do for the moment is cutting that off.
    In the future, somebody should change QuArK's behaviour to where
    you can set if this path gets prefixed. }
    if MapVersion>1 then
    begin
      TexPath:=IncludeTrailingPathDelimiter(GameTexturesPath);
      if LowerCase(LeftStr(S,Length(TexPath)))=TexPath then
        S:=RightStr(S,Length(S)-Length(TexPath));
    end;

    if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
      texname:=LowerCase(texname);
    Q2Tex:=Q2Tex or (Pos('/',texname)<>0);

    Surface.NomTex:=texname;   { here we get the texture-name }
    Surface.SetThreePointsUserTex(P0,P1,P2,nil);
  end;
  ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brushDef3 }
  ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }
 end;

 //FIXME: This is more or less a dummy procedure...
 procedure ReadCurve;
 var
   texname : String;
   lightmapname : String;
   CurveParams : array[1..4] of double;
   I,J: Integer;
 begin
   ReadSymbol(sStringToken); // lbrace follows "curve"
   ReadSymbol(sCurlyBracketLeft); // data follows lbrace
   ReadCOD2Flags;
   texname:=S;
   ReadSymbol(sStringToken);
   lightmapname:=S;
   ReadSymbol(sStringToken);
   for I:=1 to 4 do
   begin
     CurveParams[I]:=NumericValue;
     ReadSymbol(sNumValueToken);
   end;

   for I:=1 to Round(CurveParams[1]) do
   begin
     ReadSymbol(sBracketLeft);
     for J:=1 to Round(CurveParams[2]) do
     begin
       //v --> vertices
       ReadSymbol(sStringToken);
       //NR 1
       ReadSymbol(sNumValueToken);
       //NR 2
       ReadSymbol(sNumValueToken);
       //NR 3
       ReadSymbol(sNumValueToken);
       if (SymbolType=sStringToken) and (S='c') then
       begin
         //c --> color
         ReadSymbol(sStringToken);
         //NR 1
         ReadSymbol(sNumValueToken);
         //NR 2
         ReadSymbol(sNumValueToken);
         //NR 3
         ReadSymbol(sNumValueToken);
         //NR 4
         ReadSymbol(sNumValueToken);
       end;
       //t --> texture
       ReadSymbol(sStringToken);
       //NR 1
       ReadSymbol(sNumValueToken);
       //NR 2
       ReadSymbol(sNumValueToken);
       //NR 3
       ReadSymbol(sNumValueToken);
       //NR 4
       ReadSymbol(sNumValueToken);
     end;
     ReadSymbol(sBracketRight);
   end;
   ReadSymbol(sCurlyBracketRight); // rbrace which finishes the curve
   ReadSymbol(sCurlyBracketRight); // rbrace which finishes the brush
 end;

 //FIXME: This is more or less a dummy procedure...
 procedure ReadMesh;
 var
   texname : String;
   lightmapname : String;
   I,J: Integer;
   MeshParams : array[1..4] of double;
 begin
   ReadSymbol(sStringToken); // lbrace follows "mesh"
   ReadSymbol(sCurlyBracketLeft); // data follows lbrace
   ReadCOD2Flags;
   texname:=S;
   ReadSymbol(sStringToken);
   lightmapname:=S;
   ReadSymbol(sStringToken);

   // now comes 4 numbers which tell how many control points there are
   // use ReadVect4 which is the same as ReadVect but expects 4 numbers
   // and we only need the X and Y values
   for I:=1 to 4 do
   begin
     MeshParams[I]:=NumericValue;
     ReadSymbol(sNumValueToken);
   end;
   // Nr 1: Width (many lines of control points there are)
   // Nr 2: Height (how many control points on each line)
   // Nr 3: ?
   // Nr 4: ?

   for I:=1 to Round(MeshParams[1]) do
   begin
     ReadSymbol(sBracketLeft);
     for J:=1 to Round(MeshParams[2]) do
     begin
       //v --> vertices
       ReadSymbol(sStringToken);
       //NR 1
       ReadSymbol(sNumValueToken);
       //NR 2
       ReadSymbol(sNumValueToken);
       //NR 3
       ReadSymbol(sNumValueToken);
       if (SymbolType=sStringToken) and (S='c') then
       begin
         //c --> color
         ReadSymbol(sStringToken);
         //NR 1
         ReadSymbol(sNumValueToken);
         //NR 2
         ReadSymbol(sNumValueToken);
         //NR 3
         ReadSymbol(sNumValueToken);
         //NR 4
         ReadSymbol(sNumValueToken);
       end;
       //t --> texture
       ReadSymbol(sStringToken);
       //NR 1
       ReadSymbol(sNumValueToken);
       //NR 2
       ReadSymbol(sNumValueToken);
       //NR 3
       ReadSymbol(sNumValueToken);
       //NR 4
       ReadSymbol(sNumValueToken);
       if (SymbolType=sStringToken) and (S='f') then
       begin
         //f --> flags
         ReadSymbol(sStringToken);
         //NR
         ReadSymbol(sNumValueToken);
       end;
     end;
     ReadSymbol(sBracketRight);
   end;
   ReadSymbol(sCurlyBracketRight); // rbrace which finishes the mesh
   ReadSymbol(sCurlyBracketRight); // rbrace which finishes the brush
 end;

begin
  ProgressIndicatorStart(5451, Length(SourceFile) div Granularite);
  try
    Source:=PChar(SourceFile);
    Prochain:=Source+Granularite;   //point at which progress marker will be ticked
    Result:=mjQuake;     //Result is info about what game the map is for
    Q2Tex:=False;
    WC33map:=False; {Decker}
    g_MapError.Clear;
    ReadSymbolForceToText:=False;    //ReadSymbol is not to expect text
    LineNoBeingParsed:=1;
    InvPoly:=0;
    InvFaces:=0;
    Juste13:=False;
   {EndOfLine:=False;}
    HullList:=Nil;
    L:=TSpecificsList.Create;
    try
     WorldSpawn:=False;  //we haven't seen the worldspawn entity yet
     Entities:=TTreeMapGroup.Create(LoadStr1(136), Racine);
     Racine.SubElements.Add(Entities);
     MapStructure:=TTreeMapGroup.Create(LoadStr1(137), Racine);
     Racine.SubElements.Add(MapStructure);
     {Rowdy}
     MapStructureB:=Nil;
     (*** commented out by Armin : only create the group if actually needed
      *  MapStructureB:=TTreeMapGroup.Create(LoadStr1(262), Racine);
      *  Racine.SubElements.Add(MapStructureB);
      *)
     {/Rowdy}
     ReadSymbol(sEOF);
     while SymbolType<>sEOF do { when ReadSymbol's arg is sEOF, it's not really `expected'.
                   The first real char ought to be {.  If it is, it
                   will become C in ReadSymbol, and SymbolType will sCurlyBracketLeft }
      begin
      { if the thing just read wasn't {, the ReadSymbol call will bomb.
        Otherwise, it will pull in the next chunk (which ought to be
        a quoted string), and set SymbolType to the type of what it got. }

       // ... except for Doom 3, where we might have a "version 1" or "version 2" line BEFORE
       // the first "}" ...
       if (SymbolType=sStringToken) then
       begin
        if (CompareText(S,'Version')=0) then
        begin
         ReadSymbol(sStringToken); // get the map version number // NumValueToken);
         if SymbolType<>sNumValueToken then
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(251)]); // invalid number
         MapVersion := Trunc(NumericValue+0.5);
         case MapVersion of
         1: Result:=mjDoom3;   // this is a Doom 3 Version 1 map
         2: Result:=mjDoom3;   // this is a Doom 3 Version 2 map
         3: Result:=mjQuake4;  // this is a Quake 4 Version 3 map
            //Right now all Quake 4 maps are Version 3 to my knowledge
         else
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(267)]); // can't read this map version
         end;
         ReadSymbol(sNumValueToken);
        end
        else if (CompareText(S,'iwmap')=0) then
        begin
         //FIXME:
         LogAndWarn('Warning: Call of Duty 2 maps are only partially supported. Brushes will load, but curves and meshes will not!');
         ReadSymbol(sStringToken); // get the map version number // NumValueToken);
         if SymbolType<>sNumValueToken then
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(251)]); // invalid number
         MapVersion := Trunc(NumericValue+0.5);
         case MapVersion of
         4: Result:=mjCOD2;   // this is a Call of Duty 2 map
         else
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(267)]); // can't read this map version
         end;
         ReadSymbol(sNumValueToken);
        end;
       end;

       ReadSymbol(sCurlyBracketLeft);
       L.Clear;
       Classname:='';
       HullNum:=-1;
       { pull in the quoted-string attribute-value pairs }
       while SymbolType=sStringQuotedToken do
        begin
         S1:=S;  { S is where ReadSymbol sticks quoted strings }
        {EndOfLine:=True;}
         ReadSymbol(sStringQuotedToken);
        {EndOfLine:=False;}
         if SymbolType=sStringQuotedToken then
          { SpecClassname is `classname', defined in QKMapObjects }
          if CompareText(S1, SpecClassname)=0 then
           {$IFDEF ClassnameLowerCase}
           Classname:=LowerCase(S)
           {$ELSE}
           Classname:=S
           {$ENDIF}
          else
           begin
            { this looks like adding an attribute-value pair to L }
            L.Add(S1+'='+S);
            { stuff for dealing with model attributes in BSP entity lists }
            if (BSP<>Nil) and (CompareText(S1, 'model')=0) and (S<>'') and (S[1]='*') then
             begin
              Val(Copy(S,2,MaxInt), HullNum, I);
              if I<>0 then
               HullNum:=-1;
             end;
           end;
         ReadSymbol(sStringQuotedToken);
        end;

       if Classname = ClassnameWorldspawn then
        begin
         // only one worldspawn allowed
         if WorldSpawn then
          Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(252)]);
         Entite:=Racine;
         EntitePoly:=MapStructure;
         {Rowdy}
         EntiteBezier:=MapStructureB;
         {/Rowdy}
         WorldSpawn:=True;
         HullNum:=0;
         Racine.Name:=ClassnameWorldspawn;
        end
       else
        begin
         BrushNum:=-1;
         if (SymbolType<>sCurlyBracketLeft) and (HullNum=-1) then
          Entite:=TTreeMapEntity.Create(Classname, Entities)
         else
          Entite:=TTreeMapBrush.Create(Classname, Entities);
         Entities.SubElements.Add(Entite);
         EntitePoly:=Entite;
         {Rowdy}
         EntiteBezier:=Entite;
         {/Rowdy}
        end;

       OriginBrush:=Nil;
       if BSP<>Nil then    { only relevant if we're reading a BSP }
        begin
         if HullNum>=0 then
          begin
           if HullList=Nil then
            HullList:=TQList.Create;
           for I:=HullList.Count to HullNum do
            HullList.Add(Nil);
           HullList[HullNum]:=EntitePoly;
          end;
        end
       else
        while SymbolType = sCurlyBracketLeft do  {read a brush}
         begin
          Inc(BrushNum);
          FaceNum:=-1;
          ReadSymbol(sCurlyBracketLeft);
          if SymbolType=sStringToken then
           begin
             if LowerCase(s)='patchdef2' then
             begin
               // A patchDef2 means it is at least a Quake 3 map
               if Result=mjQuake then
                Result:=mjQ3A;
              { Armin: create the MapStructureB group if not already done }
               if EntiteBezier=Nil then
               begin
                 MapStructureB:=TTreeMapGroup.Create(LoadStr1(262), Racine);
                 Racine.SubElements.Add(MapStructureB);
                 EntiteBezier:=MapStructureB;
               end;
               ReadPatchDef2();
               if MOHAAPatchDef2Detected then
                 Result:=mjMOHAA;
             end
             else if LowerCase(s)='patchdef3' then
             begin
               // A patchDef3 means it is at least a Doom 3 map
               if Result=mjQuake then
                 Result:=mjDoom3;
               { Armin: create the MapStructureB group if not already done }
               if EntiteBezier=Nil then
               begin
                 MapStructureB:=TTreeMapGroup.Create(LoadStr1(262), Racine);
                 Racine.SubElements.Add(MapStructureB);
                 EntiteBezier:=MapStructureB;
               end;
               ReadPatchDef3();
             end
             else if LowerCase(s)='patchterraindef3' then
             begin
               // A patchTerrainDef3 means it is at least a Call of Duty 1 map
               if Result=mjQuake then
                 Result:=mjCoD;
               { Armin: create the MapStructureB group if not already done }
               if EntiteBezier=Nil then
               begin
                 MapStructureB:=TTreeMapGroup.Create(LoadStr1(262), Racine);
                 Racine.SubElements.Add(MapStructureB);
                 EntiteBezier:=MapStructureB;
               end;
               ReadPatchTerrainDef3();
             end
             else if LowerCase(s)='patchdef5' then
             begin
               // A patchDef5 means it is at least a Call of Duty 1 map
               if Result=mjQuake then
                 Result:=mjCoD;
               { Armin: create the MapStructureB group if not already done }
               if EntiteBezier=Nil then
               begin
                 MapStructureB:=TTreeMapGroup.Create(LoadStr1(262), Racine);
                 Racine.SubElements.Add(MapStructureB);
                 EntiteBezier:=MapStructureB;
               end;
               ReadPatchDef5();
             end
             else if LowerCase(s)='brushdef' then
             begin
               // A brushDef means it is at least a Quake 3 map
               if Result=mjQuake then
                 Result:=mjQ3A;
               ReadBrushDef();
             end
             else if LowerCase(s)='brushdef2' then
             begin
               // A brushDef2 means it is at least a Doom 3 map (not sure about this)
               if Result=mjQuake then
                 Result:=mjDoom3;
               ReadBrushDef3();
               // The difference between brushdef2 and brushdef3 is minimal...
             end
             else if LowerCase(s)='brushdef3' then
             begin
               // A brushDef3 means it is at least a Doom 3 map
               if Result=mjQuake then
                 Result:=mjDoom3;
               ReadBrushDef3();
             end
             else if LowerCase(s)='curve' then
             begin
               // A curve means it is a least a Call of Duty 2 (maybe 1?) map
               if Result=mjQuake then
                 Result:=mjCOD2;
               ReadCurve();
             end
             else if LowerCase(s)='mesh' then
             begin
               // A mesh means it is a least a Call of Duty 2 (maybe 1?) map
               if Result=mjQuake then
                 Result:=mjCOD2;
               ReadMesh();
             end
             else if LowerCase(s)='contents' then
             begin
               // A "contents detail;" means it is a least a Call of Duty 2 (maybe 1?) map
               ReadCOD2Flags;
               if Result=mjQuake then
                 Result:=mjCOD2;
               ReadBrush;
             end
             else
               raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(260)]);
           end
          else
           ReadBrush;
         end;

       if (OriginBrush<>Nil) and (EntitePoly<>MapStructure) then
        begin
         V[1].X:=MaxInt;
         V[1].Y:=MaxInt;
         V[1].Z:=MaxInt;
         V[2].X:=-MaxInt;
         V[2].Y:=-MaxInt;
         V[2].Z:=-MaxInt;
         OriginBrush.ChercheExtremites(V[1], V[2]);
         if V[1].X<V[2].X then
          begin
           Delta.X:=0.5*(V[1].X+V[2].X);
           Delta.Y:=0.5*(V[1].Y+V[2].Y);      { center of the 'origin brush' }
           Delta.Z:=0.5*(V[1].Z+V[2].Z);
           for I:=0 to EntitePoly.SubElements.Count-1 do
            with EntitePoly.SubElements[I] do
             for J:=0 to SubElements.Count-1 do
              with SubElements[J] as TFace do
               if GetThreePoints(V[1], V[2], V[3]) and LoadData then
                begin
                 Facteur:=Dot(Normale, Delta);
                 Delta1.X:=Delta.X - Normale.X*Facteur;
                 Delta1.Y:=Delta.Y - Normale.Y*Facteur;    { Delta1 is Delta forced in the plane of the face }
                 Delta1.Z:=Delta.Z - Normale.Z*Facteur;
                 for K:=1 to 3 do
                  begin
                   V[K].X:=V[K].X + Delta1.X;
                   V[K].Y:=V[K].Y + Delta1.Y;
                   V[K].Z:=V[K].Z + Delta1.Z;
                  end;
                 SetThreePoints(V[1], V[2], V[3]);
                end;
          end;
        end;

       if (WC33map) then
       begin
         { Remove the spec/arg "mapversion" from worldspawn,
           since QuArK will write it depending on whether the
           game config is set to write this format }
         SpecIndex := L.IndexOfName('mapversion');
         if (SpecIndex >= 0) then
           L.Delete(SpecIndex);
       end;

      {Entite.Item.Text:=Classname;}
       Entite.Specifics.Assign(L);
      {Entite.SpecificsChange;}
       ReadSymbol(sCurlyBracketRight);
      end;

     if HullList<>Nil then
      for I:=0 to HullList.Count-1 do
       begin
        EntitePoly:=TTreeMapSpec(HullList[I]);
        if EntitePoly<>Nil then
         EntitePoly.SubElements.Add(TBSPHull.CreateHull(BSP, I, EntitePoly as TTreeMapGroup));
       end;

     if not WorldSpawn then
      Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(255)]);

    finally
     L.Free;
     HullList.Free;
    end;

    Racine.FixupAllReferences;
  finally
    ProgressIndicatorStop;
  end;

  if (Result=mjQuake) and Q2Tex then
    Result:=mjNotQuake1;

  case Result of
  mjNotQuake1:
    Result:=CurrentQuake2Mode;
  mjQuake:
    begin
      Result:=CurrentQuake1Mode;
      if Result=mjHexen then
        Result:=mjQuake
      else if CharModeJeu=mj6DX then
        Result:=mj6DX
      else if CharModeJeu=mjCrystalSpace then
        Result:=mjCrystalSpace
      else if CharModeJeu=mjTorque then
        Result:=mjTorque 
      else if CharModeJeu=mjSylphis then
        Result:=mjSylphis;

    end;
    mjQ3A:
    { FIXME:  barf coding, the idea is that  if Q3a mode
      is detected, we stay in the current game mode if it's
      one of the Q3A-format games, otherwise switch to Q3A }
    begin
     if CharModeJeu=mjEF2 then
       Result:=CharModeJeu
     else if CharModeJeu=mjSOF then
       Result:=CharModeJeu
     else if CharModeJeu=mjSTVEF then
       Result:=CharModeJeu
     else if CharModeJeu=mjRTCW then
       Result:=CharModeJeu
     else if CharModeJeu=mjRTCWET then
       Result:=CharModeJeu
     else if CharModeJeu=mjNEXUIZ then
       Result:=CharModeJeu
     else if CharModeJeu=mjWarsow then
       Result:=CharModeJeu;
    end;
  end;

  if InvFaces>0 then
    GlobalWarning(FmtLoadStr1(257, [InvFaces]));
  if InvPoly>0 then
    GlobalWarning(FmtLoadStr1(256, [InvPoly]));
end;

procedure Valve220MapParams(MapSaveSettings: TMapSaveSettings; HL2: boolean; const Normale: TVect; const F: TFace; var S: String);
var
  Plan: Char;
  Axis, P0, P1, P2, PP0, PP1, PP2, Origin, D1, D2:TVect;

  S1, S2, UOff, VOff : Double;
  Dot22, Dot23, Dot33, Mdet,aa, bb, dd : Double; // from zoner's
  QV0, QV1, UAxis, VAxis : TVect; // from Zoners
  DecimalPlaces : Integer;

  procedure write4vect(const V: TVect; D : Double; var S: String);
  begin
    S:=S+' [ ';
    S:=S+FloatToStrF(V.X, ffFixed, 20, DecimalPlaces)+' ';
    S:=S+FloatToStrF(V.Y, ffFixed, 20, DecimalPlaces)+' ';
    S:=S+FloatToStrF(V.Z, ffFixed, 20, DecimalPlaces)+' ';
    S:=S+FloatToStrF(D, ffFixed, 20, DecimalPlaces)+' ';
    S:=S+'] ';
  end;

  procedure write4vectHL2(const V: TVect; D : Double; var S: String);
  begin
    S:=S+'[';
    S:=S+FloatToStrF(V.X, ffFixed, 20, DecimalPlaces)+' ';
    S:=S+FloatToStrF(V.Y, ffFixed, 20, DecimalPlaces)+' ';
    S:=S+FloatToStrF(V.Z, ffFixed, 20, DecimalPlaces)+' ';
    S:=S+FloatToStrF(D, ffFixed, 20, DecimalPlaces);
    S:=S+']';
  end;

begin
  DecimalPlaces := MapSaveSettings.DecimalPlaces;

  Plan:=PointsToPlane(Normale);
  case Plan of
   'X' : Axis := MakeVect(1, 0, 0);
   'Y' : Axis := MakeVect(0, 1, 0);
   'Z' : Axis := MakeVect(0, 0, 1);
  end;

  Origin:=MakeVect(0,0,0);


  F.GetThreePointsT(PP0, PP1, PP2);
(*
   this code seems to show that the results of
     GetThreePointsT are the same as getting the
     simulated 3points, and swapping P2 and P1 when
     the mirror bit is set

  F.SimulateEnhTex(V0, V1, V2, Mirror);
  if Mirror then
  begin
   // ShowMessage('Mirror');
    V2b:=V2;
    V2:=V1;
    V1:=V2b;
  end;
  if VecLength(VecDiff(V0,P0))>rien then
  begin
    ShowMessage('P0 discrepancy');
  end;
   if VecLength(VecDiff(V1,P1))>rien then
  begin
    ShowMessage('P1 discrepancy');
  end;
   if VecLength(VecDiff(V2,P2))>rien then
  begin
    ShowMessage('P2 discrepancy');
  end;

 *)

  P0:=ProjectPointToPlane(PP0, Axis, Origin, Axis);
  P1:=ProjectPointToPlane(PP1, Axis, Origin, Axis);
  P2:=ProjectPointToPlane(PP2, Axis, Origin, Axis);

  // D1|D2 = Zoner's TexPt[0|1]
  D1:= VecScale(1.0/128.0, VecDiff(P1, P0));
  D2:= VecScale(1.0/128.0, VecDiff(P2, P0));
 {
        dot22 = DotProduct(TexPt[0], TexPt[0]);
        dot23 = DotProduct(TexPt[0], TexPt[1]);
        dot33 = DotProduct(TexPt[1], TexPt[1]);
        mdet = dot22 * dot33 - dot23 * dot23;
 }

  Dot22:=Dot(D1, D1);
  Dot23:=Dot(D1, D2);
  Dot33:=Dot(D2, D2);
  Mdet:= Dot22*Dot33 - Dot23*Dot23;

 {
         mdet = 1.0 / mdet;
         aa = dot33 * mdet;
         bb = -dot23 * mdet;
         dd = dot22 * mdet;
  }

  Mdet := 1.0/MDet;
  aa:=Dot33*MDet;
  bb:=-Dot23*Mdet;
  dd:= Dot22*Mdet;

(*
     for (j = 0; j < 3; j++)
     {
       side->td.vects.quark.vects[0][j] = aa * TexPt[0][j] + bb * TexPt[1][j];
       side->td.vects.quark.vects[1][j] = -( /*cc */ bb * TexPt[0][j] + dd * TexPt[1][j]);
     }
*)
  QV0:=VecSum(VecScale(aa, D1), VecScale(bb, D2));
  QV1:=VecSum(VecScale(-bb, D1), VecScale(-dd, D2));

  {
    side->td.vects.quark.vects[0][3] = -DotProduct(side->td.vects.quark.vects[0], side->planepts[0]);
    side->td.vects.quark.vects[1][3] = -DotProduct(side->td.vects.quark.vects[1], side->planepts[0]);
  }

  UOff:=-Dot(QV0,P0);
  VOff:=-Dot(QV1,P0);

  { up do this point, QV0,UOff and QV1,VOff seem to be identical to the
     quark.vects structure in zoner's }

  UAxis:=QV0;
  VAxis:=QV1;


  Normalise(QV0, S1);
  Normalise(QV1, S2);


  if HL2 then
  begin
    S1:=1.0/S1;
    S2:=1.0/S2;

    S:=S+#13#10'    "uaxis" "';
    write4vectHL2(QV0, UOff, S);
    S:=S+' '+FloatToStrF(S1, ffFixed, 20, DecimalPlaces)+'"';
    S:=S+#13#10'    "vaxis" "';
    write4vectHL2(QV1, VOff, S);
    S:=S+' '+FloatToStrF(S2, ffFixed, 20, DecimalPlaces)+'"';
  end
  else
  begin
//  if (veclength(qv0)>1) or (veclength(qv1)>1) then
//    Raise InternalE('veclength(qv0)>1) or (veclength(qv1)>1');
    write4vect(QV0, UOff, S);
    write4vect(QV1, VOff, S);

(*
    write4vect(D1, -PP0.X/S1, S);
    write4vect(D2, PP0.Y/S2, S);
*)

    S1:=1.0/S1;
    S2:=1.0/S2;

    S:=S+' 0 ';
    S:=S+' '+FloatToStrF(S1, ffFixed, 20, DecimalPlaces);
    { sign flip engineered into Scale }
    S:=S+' '+FloatToStrF(S2, ffFixed, 20, DecimalPlaces);
  end;

end;

procedure CoD2MapParams(MapSaveSettings: TMapSaveSettings; const Normale: TVect; const F: TFace; var S: String);
var
  PT: TThreePoints;
  Mirror: Boolean;
  DecimalPlaces: Integer;
begin
  DecimalPlaces := MapSaveSettings.DecimalPlaces;

  with F do
   begin
    SimulateEnhTex(PT[1], PT[3], PT[2], Mirror); {doesn't scale}

    //FIXME: WRONG!
    S:=S+' ';
    S:=S+FloatToStrF(PT[1].X, ffFixed, 20, DecimalPlaces);
    S:=S+' ';
    S:=S+FloatToStrF(PT[1].Y, ffFixed, 20, DecimalPlaces);
    S:=S+' ';
    S:=S+FloatToStrF(PT[2].X, ffFixed, 20, DecimalPlaces);
    S:=S+' ';
    S:=S+FloatToStrF(PT[2].Y, ffFixed, 20, DecimalPlaces);
    S:=S+' ';
    S:=S+FloatToStrF(PT[3].X, ffFixed, 20, DecimalPlaces);
    S:=S+' ';
    S:=S+FloatToStrF(PT[3].Y, ffFixed, 20, DecimalPlaces);
   end;
  //FIXME: ...
end;


procedure ApproximateParams(MapSaveSettings: TMapSaveSettings; const Normale: TVect; const V: TThreePoints; var Params: TFaceParams; Mirror: Boolean);
var
  PX, PY: array[1..3] of TDouble;
  A, P2, S, C: TDouble;
  I: Integer;
  Plan: Char;
begin
  Plan:=PointsToPlane(Normale);
  for I:=1 to 3 do
    case Plan of
      'X': begin
             PX[I]:=V[I].Y;
             PY[I]:=V[I].Z;
           end;
      'Y': begin
             PX[I]:=V[I].X;
             PY[I]:=V[I].Z;
           end;
      'Z': begin
             PX[I]:=V[I].X;
             PY[I]:=V[I].Y;
           end;
  end;
  if not Mirror then
  begin
    P2:=PX[2]; PX[2]:=PX[3]; PX[3]:=P2;
    P2:=PY[2]; PY[2]:=PY[3]; PY[3]:=P2;
  end;
  PY[3]:=PY[3]-PY[1];
  PX[2]:=PX[2]-PX[1];
  PY[2]:=PY[2]-PY[1];
  if Abs(PY[2])<rien then
    Params[3]:=0
  else
  begin
    A:=AngleXY(PX[2], PY[2]);
    S:=Sin(A);
    C:=Cos(A);
   {PX[2]:=Sqrt(Sqr(PX[2])+Sqr(PY[2]));}
    PX[2]:=PX[2]*C + PY[2]*S;
    Params[3]:=A*(180/pi);

    PX[3]:=PX[3]-PX[1];
    PY[3]:=PY[3]*C - PX[3]*S;

    P2:=PX[1];
    PX[1]:=P2*C + PY[1]*S;
    PY[1]:=PY[1]*C - P2*S;
  end;
  Params[4]:=PX[2] / EchelleTexture;
  if MapSaveSettings.GameCode=mjGenesis3d then
  begin
    Params[3]:=Round(Params[3]);
    if Plan='Y' then
      Params[3]:=-Params[3];
  end;
  Params[5]:=PY[3] / EchelleTexture;  { approximation is here : we ignore the angle that the third vector may do }
  if Abs(Params[4])<rien2 then A:=1 else A:=1/Params[4];
  Params[1]:=-PX[1]*A;
  if Abs(Params[5])<rien2 then A:=1 else A:=1/Params[5];
  Params[2]:=PY[1]*A;
  if MapSaveSettings.GameCode=mjGenesis3d then
  begin
    if Plan='X' then
      Params[4]:=-Params[4]
  end
end;

 {------------------------}

procedure QMapFile.LoadFile(F: TStream; FSize: Integer);
var
 Racine: TTreeMapBrush;
 ModeJeu: Char;
 Source: String;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      SetLength(Source, FSize);
      F.ReadBuffer(Source[1], FSize);
      Racine:=TTreeMapBrush.Create('', Self);
      Racine.AddRef(+1);
      try
        ModeJeu:=ReadEntityList(Racine, Source, Nil);
        SubElements.Add(Racine);
        Specifics.Values['Root']:=Racine.Name+Racine.TypeInfo;
        ObjectGameCode:=ModeJeu;
      finally
        Racine.AddRef(-1);
      end;
     end;
 else
  inherited;
 end;
end;

procedure QMapFile.SaveFile(Info: TInfoEnreg1);
var
 Dest, HxStrings: TStringList;
 Root: QObject;
 List: TQList;
 saveflags : Integer;
 MapOptionSpecs : TSpecificsList;
 MapSaveSettings: TMapSaveSettings;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      Root:=SubElements.FindName(Specifics.Values['Root']);
      if (Root=Nil) or not (Root is TTreeMapBrush) then
       Raise EError(5558);
      Root.LoadAll;
      HxStrings:=Nil;
      List:=TQList.Create;
      Dest:=TStringList.Create;
      try
       if Specifics.IndexOfName('hxstrings')>=0 then
        begin
         HxStrings:=TStringList.Create;
         HxStrings.Text:=Specifics.Values['hxstrings'];
        end;

       { .MAP comment header, which explains that this .MAP has been written
         by QuArK, for this game, and then QuArK's webpage. }
       Dest.Add(CommentMapLine(FmtLoadStr1(176, [QuarkVersion])));
       Dest.Add(CommentMapLine(FmtLoadStr1(177, [SetupGameSet.Name])));
       Dest.Add(CommentMapLine(FmtLoadStr1(178, [])));
       Dest.Add('');

       MapSaveSettings:=GetDefaultMapSaveSettings;
       MapSaveSettings.GameCode:=CharModeJeu;
       MapSaveSettings.MapVersion:=0;
       MapOptionSpecs:=SetupSubSet(ssFiles,'MAP').Specifics;
       if CharModeJeu=mjDoom3 then
       begin
         // Rowdy: write an extra line to indicate we are using version 1 .map file
         // format or Doom 3's default version 2.
         if MapOptionSpecs.Values['SaveMapVersion'] = '1' then
         begin
           MapSaveSettings.MapVersion:=1;
           Dest.Add('Version 1');
         end
         else if MapOptionSpecs.Values['SaveMapVersion'] = '2' then
         begin
           MapSaveSettings.MapVersion:=2;
           Dest.Add('Version 2');
         end
         else
         begin
           MapSaveSettings.MapVersion:=2;
           Dest.Add('Version 2');  //Default to map version 2
         end;
         Dest.Add('');
       end
       else if CharModeJeu=mjQuake4 then
       begin
         MapSaveSettings.MapVersion:=3;
         Dest.Add('Version 3');
       end
       else if CharModeJeu=mjCoD2 then
       begin
         Dest.Add('iwmap 4');
       end;
       Dest.Text:=Dest.Text;   { #13 -> #13#10 }

       saveflags:=0;
       if MapOptionSpecs.Values['IgnoreToBuild']<>'' then
         saveflags:=saveflags or soIgnoreToBuild;
       if MapOptionSpecs.Values['DisableFPCoord']<>'' then
         saveflags:=saveflags or soDisableFPCoord;
       if MapOptionSpecs.Values['UseIntegralVertices']<>'' then
         saveflags:=saveflags or soUseIntegralVertices;
       saveflags:=saveflags or IntSpec['saveflags']; {merge in selonly}

       //FIXME: ObjectGameCode is not always defined...
       SaveAsMapText(Root, MapSaveSettings, List, Dest, saveflags, HxStrings);
       Dest.SaveToStream(F);
       if HxStrings<>Nil then
        Specifics.Values['hxstrings']:=HxStrings.Text;
      finally
       Dest.Free;
       List.Free;
       HxStrings.Free;
      end;
     end;
 else
  inherited;
 end;
end;

 {------------------------}

function AlmostIntegral(const V: TVect; var V2: TVect) : boolean;
var
  DV: TVect;
begin
  V2:=MakeVect(Round(V.X), Round(V.Y), Round(V.Z));
  DV:=VecDiff(V, V2);
  if sqr(DV.X)+sqr(DV.Y)+sqr(DV.Z)<0.0001 then
    Result:=true
  else
    Result:=false;
end;

function EqualVect(V1, V2 : TVect) : boolean;
begin
  if (V1.X=V2.X) and (V1.Y=V2.Y) and (V1.Z=V2.Z) then
    Result:=true
  else
    Result:=false;
end;

function CoLinear(V1, V2, V3: TVect) : boolean ;
var
  D1, D2: TVect;
begin
  if EqualVect(V1,V2) or EqualVect(V2,V3) or
      EqualVect(V1, V3) then
    Result:=true
  else
  begin
    D1:=VecDiff(V2,V1);
    D2:=VecDiff(V3,V2);
    Normalise(D1); Normalise(D2);
    if Dot(D1,D2)>0.999 then
      Result:=true
    else
      Result:=false;
  end;
end;

function InvertDenom(P0, P1, P2 : TVect) : Double;
begin
 Result:=-P2.X*P1.Y + P2.X*P0.Y + P2.Y*P1.X
   - P2.Y*P0.X + P0.X*P1.Y - P0.Y*P1.X;
end;

{ based on information about Q3R brush primitives format
  provided by Timothee Besset }
procedure GetPXPY(const Normal: TVect; const V: TThreePoints; var PX, PY: array of Double; const Dist : Double);
var
  texS, texT, texO, P0, P1, P2: TVect;
  D : Double;
begin
  { get basis vectors for affine plane of face }
  GetAxisBase(Normal, texS, texT);
  { origin of plane's coordindate system }
  texO:=VecScale(Dist, Normal);
  { get the texture points.  V has been provided by
     Face.GetThreePointsUserTex, so if texture scale
     is 1:1, (P1-P0) will be texture width, P2-P0
     texture height.  In written out map, for 1:1
     texture scale these #'s will be 128 }

    P0:=V[1];
    P2:=V[3]; P1:=V[2];

   { redo threepoints in plane coordinate system }
   P0:=CoordShift(P0, texO, texS, texT);
   P1:=CoordShift(P1, texO, texS, texT);
   P2:=CoordShift(P2, texO, texS, texT);

   { Now solve the equation system produced
     where PX, PY are to be row 1 and row 2
     of the homogenous matrix that will map
     (0,0), (1,0) and (0,-1) onto P0, P1, P2
     respectively (note sign swap) }

   D:=InvertDenom(P0, P1, P2);

   PX[1]:=(P2.Y-P0.Y)/D;
   PX[2]:=(P0.X-P2.X)/D;
   PX[3]:=(-P2.Y*P0.X+P2.X*P0.Y)/D;
   PY[1]:=(P1.Y-P0.Y)/D;
   PY[2]:=(P0.X-P1.X)/D;
   PY[3]:=(-P0.X*P1.Y+P0.Y*P1.X)/D;
end;

procedure MohaaSurfaceParms(F: TTexturedTreeMap; var S: String);
var
  Q: QPixelSet;
  S1, SpecStr, Spec, Val, Val2: String;
  I, Pozzie: Integer;
  Single3: array[1..3] of Single;
  { tiglari }
  rval : Single; { for Value/lightvalue }
begin
  Q := GlobalFindTexture(F.NomTex,Nil);  { find the Texture Link object }
  if Q<>Nil then Q:=Q.LoadPixelSet;      { load it }
  for I := 0 to F.Specifics.Count-1 do
  begin
    SpecStr:=F.Specifics.Strings[I];
    if Copy(SpecStr,0,5)='_esp_' then
    begin
      Pozzie:=Pos('=', SpecStr);
      Spec:=Copy(SpecStr,6,Pozzie-6);
      Val:=Copy(SpecStr,Pozzie+1,Maxint);
      { we only want to write it if it's different from the shader's spec }
      Val2:=Q.Specifics.Values['_esp_'+Spec];
      if Val='1' then  // the face is positively specified
      begin
        if Val2<>'1' then  // the shader is not specified
          S:=S+' +surfaceparm '+Spec
      end
      else
      if Val2='1' then //the face is not positively specified and the shader is
          S:=S+' -surfaceparm '+Spec;
    end;
  end;
  { tiglari - now write the other face properties }
  S1:=F.Specifics.Values['surfaceLight'];
  if (S1<>'') then
    S:=S+' surfaceLight '+S1;
  if F.GetFloatsSpec('surfaceColor',Single3) then
  begin
    S:=S+' surfaceColor '+FloatToStrF(Single3[1], ffFixed, 20, 6);
    S:=S+' '+FloatToStrF(Single3[2], ffFixed, 20, 6);
    S:=S+' '+FloatToStrF(Single3[3], ffFixed, 20, 6);
  end;
  S1:=F.Specifics.Values['surfaceAngle'];
  if (S1<>'') then
    S:=S+' surfaceAngle '+S1;
  S1:=F.Specifics.Values['surfaceDensity'];
  if (S1<>'') then
    S:=S+' surfaceDensity '+S1;
  rval:=F.GetFloatSpec('subdivisions',0);
  if (rval<>0) then
    S:=S+' subdivisions '+FloatToStrF(rval, ffFixed, 20, 6);
  rval:=F.GetFloatSpec('tesselation',0);
  if (rval<>0) then
    S:=S+' tesselation '+FloatToStrF(rval, ffFixed, 20, 6);
end;

 {------------------------}

procedure SaveAsMapText(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
begin
  ResolveMapSaveSettings(MapSaveSettings);

  if ObjectToSave is TTreeMapBrush then
    //TTreeMapBrush needs to be checked before TTreeMapGroup, because it is a child of it!
    SaveAsMapTextTTreeMapBrush(ObjectToSave, MapSaveSettings, Negatif, Texte, Flags2, HxStrings)
  else if ObjectToSave is TDuplicator then
    //TDuplicator needs to be checked before TTreeMapEntity, because it is a child of it!
    SaveAsMapTextTDuplicator(ObjectToSave, MapSaveSettings, Negatif, Texte, Flags2, HxStrings)
  else if ObjectToSave is TTreeMapEntity then
    //TTreeMapEntity needs to be checked before TTreeMapSpec, because it is a child of it!
    SaveAsMapTextTTreeMapEntity(ObjectToSave, MapSaveSettings, Negatif, Texte, Flags2, HxStrings)
  else if ObjectToSave is TTreeMapGroup then
    //TTreeMapGroup needs to be checked before TTreeMapSpec, because it is a child of it!
    SaveAsMapTextTTreeMapGroup(ObjectToSave, MapSaveSettings, Negatif, Texte, Flags2, HxStrings)
  else if ObjectToSave is TTreeMapSpec then
    //SaveAsMapTextTTreeMapSpec(ObjectToSave, MapSaveSettings, Texte, HxStrings, Flags, I);
  else if ObjectToSave is TPolyedre then
    //SaveAsMapTextTPolygon(ObjectToSave, MapSaveSettings, Texte, OriginBrush, Flags2)
  else if ObjectToSave is TFace then
    //SaveAsMapTextTFace(ObjectToSave, MapSaveSettings, Texte, OriginBrush, Flags2)
  else if ObjectToSave is TBezier then
    //SaveAsMapTextTBezier(ObjectToSave, MapSaveSettings, Texte)
  else
    raise InternalE(LoadStr1(5525));
end;

procedure SaveAsMapTextTTreeMapBrush(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
var
 Polyedres: TQList;
 I, J: Integer;
 V1, V2: TVect;
 OriginBrush: PVect;
begin
 ResolveMapSaveSettings(MapSaveSettings);

 with TTreeMapBrush(ObjectToSave) do
 begin

 { If this is the first time we're called, soOutsideWorldspawn is not set,
   and we have to reset the entity-numbering-scheme to zero (zero = worldspawn) }
 if (Flags2 and soOutsideWorldspawn = 0) then
 begin
  { Do some checking of the spec/args in worldspawn,
    to set up special .MAP writing methods.
    Note, that specifics that starts with a ';'-character
    will not be written to the .MAP file! }
  { this is nuked, maps will now be written based on OutputMapFormat
  if Specifics.Values['mapversion']='220' then
    Flags2:=Flags2 + soWriteValve220, except we'll leave 6dx the same for now
  else }
  if (Specifics.Values['mapversion']='6DX') or (Specifics.Values[';mapversion']='6DX') then
    Flags2:=Flags2 + soWrite6DXHierarky;

  if MapSaveSettings.MapFormat=V220Type then
    Specifics.Values['mapversion']:='220';
  I := GetFirstEntityNo;
 end
 else
  I := GetNextEntityNo;
 if Flags2 and soBSP = 0 then
   Texte.Add(CommentMapLine('Entity '+IntToStr(I)));
 SaveAsMapTextTTreeMapSpec(ObjectToSave, MapSaveSettings, Texte, HxStrings, Flags, I);
 if Flags2 and soBSP = 0 then
  begin
   Polyedres:=TQList.Create;
   try
    ListePolyedres(Polyedres, Negatif, Flags2 and not soDirectDup, 1);
    OriginBrush:=Nil;
    if (Flags2 and soOutsideWorldspawn <> 0) and (CharModeJeu>=mjQuake2) then
     for I:=Polyedres.Count-1 downto 0 do
      with TPolyedre(Polyedres[I]) do
       if CheckPolyhedron and (Faces.Count>0)
       and (StrToIntDef(PSurface(Faces[0]).F.Specifics.Values['Contents'], 0) and ContentsOrigin <> 0) then
        begin
         V1.X:=MaxInt;
         V1.Y:=MaxInt;
         V1.Z:=MaxInt;
         V2.X:=-MaxInt;
         V2.Y:=-MaxInt;
         V2.Z:=-MaxInt;
         ChercheExtremites(V1, V2);
         if V1.X<V2.X then
          begin
           V1.X:=0.5*(V1.X+V2.X);
           V1.Y:=0.5*(V1.Y+V2.Y);      { center of the 'origin brush' }
           V1.Z:=0.5*(V1.Z+V2.Z);
           OriginBrush:=@V1;
          end;
         Break;
        end;
    for I:=0 to Polyedres.Count-1 do
     begin
      Texte.Add(CommentMapLine('Brush '+IntToStr(I)));
      SaveAsMapTextTPolygon(TPolyedre(Polyedres[I]), MapSaveSettings, Texte, OriginBrush, Flags2);
     end;
    { proceed with Bezier patches }
    I:=Polyedres.Count-1;
    Polyedres.Clear;
    ListeBeziers(Polyedres, Flags2);
    for J:=0 to Polyedres.Count-1 do
    begin
     I:=I+1;
     Texte.Add(CommentMapLine('Bezier '+IntToStr(J)+' (Brush '+IntToStr(I)+')'));
     SaveAsMapTextTBezier(TBezier(Polyedres[J]), MapSaveSettings, Texte);
    end;
   finally
    Polyedres.Free;
   end;
  end;
  // Does 6DX support work correctly?
  if ((Flags2 and soWrite6DXHierarky) <> 0) then
  begin
   { For 6DX support }
    if (Flags2 and soOutsideWorldspawn) = 0 then
    begin
      Texte.Add('}');
      SaveAsMapTextTTreeMapGroup(ObjectToSave, MapSaveSettings, Negatif, Texte, Flags2 or soOutsideWorldspawn, HxStrings);
    end
    else
    begin
      SaveAsMapTextTTreeMapGroup(ObjectToSave, MapSaveSettings, Negatif, Texte, Flags2 or soOutsideWorldspawn, HxStrings);
      Texte.Add('}');
    end;
  end
  else
  begin
    Texte.Add('}');
    SaveAsMapTextTTreeMapGroup(ObjectToSave, MapSaveSettings, Negatif, Texte, Flags2 or soOutsideWorldspawn, HxStrings);
  end;

 end;
end;

procedure SaveAsMapTextTTreeMapSpec(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Dest, HxStrings: TStrings; Flags2: Integer; EntityNumber: Integer);
const
 LineStarts: array[Boolean] of String = (' "', '"');
var
 S, Msg, LineStart,outputname: String;
 P1, I, J, P, hashpos: Integer;
 typedspecs:Bool;
 DoneNameSpecific: boolean; // Rowdy: for Doom 3
begin
 ResolveMapSaveSettings(MapSaveSettings);

 with TTreeMapSpec(ObjectToSave) do
 begin

 DoneNameSpecific:=False;
 if Flags2 and soBsp=0 then
   Dest.Add(CommentMapLine(Ancestry));
 if (MapSaveSettings.MapFormat=HL2Type) then
   if (Name = 'worldspawn') then
     Dest.Add(LineStart+'world')
   else
     Dest.Add(LineStart+'entity');
 Dest.Add('{');
 LineStart:=LineStarts[Flags2 and soBSP <> 0];
 Dest.Add(LineStart+SpecClassname+'" "'+Name+'"');
 typedspecs:=false;
 for J:=0 to Specifics.Count-1 do
 begin
   S:=Specifics[J];

   // process untyped specifics
   hashpos:=Pos('#', S);
   if (MapSaveSettings.MapFormat<>HL2Type) or ((hashpos=0) or (hashpos=1)) then
   begin
     if (S<>'') and (S[1]<>';') and (Ord(S[1])<chrFloatSpec) then
     begin
       P:=Pos('=', S);
       Msg:=Copy(S, P+1, 255);
       // special processing for hxstrings
       if (S[1]='#') and (HxStrings<>Nil) then
       begin
         I:=0;
         while (I<HxStrings.Count) and (Msg<>HxStrings[I]) do
          Inc(I);
         if I=HxStrings.Count then
          HxStrings.Add(Msg);
         Msg:=IntToStr(I+1);
         P1:=2;
         Dec(P);
       end
       else
         P1:=1;
       {$IFDEF RemoveEmptySpecs}
       if Msg<>'' then
       begin
       {$ENDIF}
         Dest.Add(LineStart+Copy(S, P1, P-1)+'" "'+Msg+'"');
         // Rowdy: Doom 3 requires each entity to have a unique "name" specific
         // and we can use the entity number for that
         if LowerCase(Copy(S, P1, P-1)) = 'name' then
           DoneNameSpecific:=True;
       {$IFDEF RemoveEmptySpecs}
       end;
       {$ENDIF}
     end;
   end
   else
     typedspecs:=true;
 end; // for J:=...

 if typedspecs then
 begin
   Dest.Add('  connections');
   Dest.Add('  {');
   for J:=0 to Specifics.Count-1 do
   begin
     S:=Specifics[J];
     P:=Pos('=', S);
     Msg:=Copy(S, P+1, 255);

// not needed in map file
//     hashpos:=Pos('input#',S);
//     if hashpos <> 0 then
//       Dest.Add('   "'+Copy(S, 7, P-7)+'" "'+Msg+'"');

     hashpos:=Pos('output#',S);
     if hashpos <> 0 then
     begin
       outputname:=Copy(S, 8, P-8);
       for i := length(outputname) downto 1 do
       begin
         if outputname[i] in ['0'..'9'] then
           setlength(outputname,i-1)
         else
           break;
       end;
       Dest.Add('   "'+outputname+'" "'+Msg+'"');
     end;

   end;
   Dest.Add('  }');
 end;

  // Rowdy: Doom 3 entity names: use the entity class + '_' + entity number
  // e.g. if entity 17 was a light, the name specific would be 'light_17'
  if (CharModeJeu=mjDoom3) and not(DoneNameSpecific) then
   Dest.Add(LineStart+'name" "'+Name+'_'+IntToStr(EntityNumber)+'"');

 end;
end;

procedure SaveAsMapTextTTreeMapEntity(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
var
 EntityNumber: Integer;
begin
 ResolveMapSaveSettings(MapSaveSettings);

 with TTreeMapEntity(ObjectToSave) do
 begin

 EntityNumber:=0;
 if Flags2 and soBSP=0 then
  begin
   EntityNumber:=GetNextEntityNo;
   Texte.Add(CommentMapLine('Entity '+IntToStr(EntityNumber)));
  end;
 SaveAsMapTextTTreeMapSpec(ObjectToSave, MapSaveSettings, Texte, HxStrings, Flags2, EntityNumber);
 Texte.Add('}');

 end;
end;

procedure SaveAsMapTextTTreeMapGroup(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
var
 I: Integer;
 T: TTreeMap;
begin
 ResolveMapSaveSettings(MapSaveSettings);

 with TTreeMapGroup(ObjectToSave) do
 begin

 if (Flags2 and soIgnoreToBuild <> 0)
 and (ViewFlags and vfIgnoreToBuildMap <> 0) then
  Exit;
 if Odd(SelMult) then
  Flags2:=Flags2 and not soSelOnly;
 for I:=0 to SubElements.Count-1 do
  begin
   T:=TTreeMap(SubElements[I]);
   // Is this right?
   if not ((Flags2 and soSelOnly <> 0) and ControleSelection(T)) then
   begin
    SaveAsMapText(T, MapSaveSettings, Negatif, Texte, Flags2, HxStrings);
   end;
  end;

 end;
end;

procedure SaveAsMapTextTPolygon(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Brush: TStrings; OriginBrush: PVect; Flags2: Integer);
var
 J: Integer;
 Q: QObject;
 S:String;
 { BrushPrim, Valve220Map : Boolean }
 MapFormat: TMapFormatTypes;
begin
 ResolveMapSaveSettings(MapSaveSettings);

 with TPolyhedron(ObjectToSave) do
 begin

 if g_DrawInfo.ConstruirePolyedres and not CheckPolyhedron then Exit;
 { these means brutally round off the threepoints, whatever they are }
 MapFormat:=MapSaveSettings.MapFormat;
 Brush.Add(CommentMapLine(Ancestry));

 if MapFormat=HL2Type then
  Brush.Add(' solid');

 Brush.Add(' {');

 if MapFormat=BPType then
 begin
  case MapSaveSettings.BrushDefVersion of
  0: ;
  1: Brush.Add(' brushDef');
  2: Brush.Add(' brushDef2');
  3: Brush.Add(' brushDef3');
  end;
  Brush.Add(' {');
 end;
 if g_DrawInfo.ConstruirePolyedres then
  for J:=0 to Faces.Count-1 do
  begin
   if MapFormat=HL2Type then
     Brush.Add('   side'#13#10'   {');
   SaveAsMapTextTFace(PSurface(Faces[J])^.F, MapSaveSettings, Brush, OriginBrush, Flags2);
   if MapFormat=HL2Type then
   begin
     S:=PSurface(Faces[J])^.F.Specifics.values['lightmapscale'];
     if S<>'' then
       Brush.Add('    "lightmapscale" "'+S+'"')
     else
       Brush.Add('    "lightmapscale" "16"');
     Brush.Add('   }');
   end
  end
 else
  for J:=0 to SubElements.Count-1 do
   begin
    Q:=SubElements[J];
    if Q is TFace then
     SaveAsMapTextTFace(TFace(Q), MapSaveSettings, Brush, OriginBrush, Flags2);
   end;
 if MapFormat=BPType then
   Brush.Add(' }');
 Brush.Add(' }');

 end;
end;

procedure SaveAsMapTextTFace(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Brush: TStrings; OriginBrush: PVect; Flags2: Integer);
const
 TxField: array[Boolean, Boolean] of String =
  ((' //TX1', ' //TX2'),
   (' ;TX1',  ' ;TX2' ));
var
 F: TFace;
 WriteIntegers, UseIntegralVertices, ExpandThreePoints : Boolean;
 S, S1, S2, S3: String;
 I, R, J, K : Integer;
 P, PT, VT: TThreePoints;
 Params: TFaceParams;
 Delta1, V, V2: TVect;
 Facteur: TDouble;
 FS: PSurface;
 PX, PY: array[1..3] of Double;
 { tiglari }
 rval : Single; { for Value/lightvalue }
 Q: QPixelSet;
 TextureName: String;
 Mirror, EtpMirror: Boolean;
 DecimalPlaces: Integer;
 type
   FlagDef = record
    name: Pchar;
    pos:  integer;
   end;

 const
   FlagsTable : array[0..30] of FlagDef =
    ((name: 'light'; pos: 0),
     (name: 'masked'; pos: 1),
     (name: 'sky'; pos: 2),
     (name: 'warping'; pos: 3),
     (name: 'nonlit'; pos: 4),
     (name: 'nofilter'; pos: 5),
     (name: 'conveyor'; pos: 6),
     (name: 'nodraw'; pos: 7),
     (name: 'hint'; pos: 8),
     (name: 'skip'; pos: 9),
     (name: 'wavy'; pos: 10),
     (name: 'ricochet'; pos: 11),
     (name: 'prelit'; pos: 12),
     (name: 'mirror'; pos: 13),
     (name: 'console'; pos: 14),
     (name: 'usecolor'; pos: 15),
     (name: 'hardwareonly'; pos: 16),
     (name: 'damage'; pos: 17),
     (name: 'weak'; pos: 18),
     (name: 'normal'; pos: 19),
     (name: 'add'; pos: 20),
     (name: 'envmapped'; pos: 21),
     (name: 'random'; pos: 22),
     (name: 'animate'; pos: 23),
     (name: 'rndtime'; pos: 24),
     (name: 'translate'; pos: 25),
     (name: 'nomerge'; pos: 26),
     (name: 'surfbit0'; pos: 27),
     (name: 'surfbit1'; pos: 28),
     (name: 'surfbit2'; pos: 29),
     (name: 'surfbit3'; pos: 30));

   ContentsTable : array[0..20] of FlagDef =
    ((name: 'solid'; pos: 0),
     (name: 'window'; pos: 1),
     (name: 'fence'; pos: 2),
     (name: 'lava'; pos: 3),
     (name: 'slime'; pos: 4),
     (name: 'water'; pos: 5),
     (name: 'mist'; pos: 6),
     (name: 'playerclip'; pos: 16),
     (name: 'monsterclip'; pos: 17),
     (name: 'current_0'; pos: 18),
     (name: 'current_90'; pos: 19),
     (name: 'current_180'; pos: 20),
     (name: 'current_270'; pos: 21),
     (name: 'current_up'; pos: 22),
     (name: 'current_dn'; pos: 23),
     (name: 'origin'; pos: 24),
     (name: 'monster'; pos: 25),
     (name: 'corpse'; pos: 26),
     (name: 'detail'; pos: 27),
     (name: 'translucent'; pos: 28),
     (name: 'ladder'; pos: 29));

  {tiglari}
  function CheckFieldDefault(const spec, linkspec:String; const Q:QPixelSet) : String;
  begin
    { fixed? by Armin }
   Result:=F.Specifics.Values[spec];
   if Result<>'' then   { if spec was found in the face, we are done }
    Exit;
   if Q<>Nil then   { is there a texture link to look into for default flags ? }
    Result:=Q.Specifics.Values[linkspec];   { yes }
   if Result='' then
    Result:='0';      { if not found at all, supply a numeric default }
  end;
  {/tiglari}

  procedure StashFloatFlag(const spec : String; const places : integer);
  var val : Single;
  begin
    val:=F.GetFloatSpec(spec, -1);
    if val >= 0 then
      if val <> Q.GetFloatSpec(spec, -1) then
        S:=S+' '+spec+' '+FloatToStrF(val, ffFixed, 7, places);
  end;

  procedure StashIntFlag(const spec : String);
  var val : string;
  begin
    val:=F.Specifics.Values[spec];
    if val<>'' then
      if val<>Q.Specifics.Values[spec] then
        S:=S+' '+spec+' '+val+'.0'
  end;

  procedure StashStrFlag(const spec : String);
  var val : string;
  begin
    val:=F.Specifics.Values[spec];
    if val <>'' then
       if val<>Q.Specifics.Values[spec] then
         S:=S+' '+spec+' '+val;
  end;


  procedure WriteNonDefaultFlags(const specstring : String; const defaults: LongInt;
                                  const table: array of FlagDef);
  var
   len, i: integer;
   specif, pozzies, neggos: LongInt;
  begin
    if specstring<>'' then
    begin
      specif:=StrToInt(specstring);
      len:=High(table);
      pozzies:=specif and (not defaults);
      neggos:=(not specif) and defaults;
      for i:=0 to len do
      begin
        if (pozzies shr table[i].pos) and 1=1 then
           S:=S+' +'+table[i].name
        else if (neggos shr table[i].pos) and 1=1 then
           S:=S+' -'+table[i].name
      end
    end
  end;
  { /tiglari }

  procedure write3vect(const P: array of Double; var S: String);
{
  var
   I: Integer;
   R: Double;
}
  begin
   S:=S+'( ';
   S:=S+FloatToStrF(P[1], ffFixed, 20, DecimalPlaces)+' ';
   S:=S+FloatToStrF(P[2], ffFixed, 20, DecimalPlaces)+' ';
   S:=S+FloatToStrF(P[3], ffFixed, 20, DecimalPlaces)+' ';

{   for I:=1 to 3 do
   begin
     R:=P[I]/EchelleTexture;
     S:=S+FloatToStrF(R, ffFixed, 20, DecimalPlaces)+' ';
   end;
}   S:=S+') ';
  end;

begin
 ResolveMapSaveSettings(MapSaveSettings);

 DecimalPlaces := MapSaveSettings.DecimalPlaces;
 
 F:=TFace(ObjectToSave);
 if F.GetThreePoints(P[1], P[3], P[2]) and F.LoadData then
{
     if BrushPrim then
       F.GetThreePointsBP(P[1], P[3], P[2]);
}
  begin

   if (MapSaveSettings.MapFormat=QetpType) or (MapSaveSettings.MapFormat=V220Type) then
   begin
     F.SimulateEnhTex(P[1], P[3], P[2], EtpMirror); {doesn't scale}

{
     if EtpMirror then
     begin
       V:=P[2];
       P[2]:=P[3];
       P[3]:=V;
     end;
}

   end;
   
    { these means brutally round off the threepoints, whatever they are }
 WriteIntegers:= {$IFDEF WriteOnlyIntegers} True {$ELSE} Flags2 and soDisableFPCoord <> 0 {$ENDIF};

// UseIntegralVertices:=(MapFormat=BPType) or (MapFormat=V220Type) or (Flags2 and soDisableEnhTex<>0);
// ExpandThreePoints:=WriteIntegers and UseIntegralVertices;

 UseIntegralVertices:=(MapSaveSettings.MapFormat<>QetpType) and (Flags2 and soUseIntegralVertices<>0);
 ExpandThreePoints:=false; { abandon this heroic but foolish measure.  The
   idea was to force threepoints to integers with less distortion, in aid
   of easier commerce between QuArK and Radiant, but it's just a Bad Idea. }

   if OriginBrush<>Nil then
    begin
     Delta1:=OriginBrush^;
     Facteur:=Dot(F.Normale, Delta1);
     Delta1.X:=Delta1.X - F.Normale.X*Facteur;
     Delta1.Y:=Delta1.Y - F.Normale.Y*Facteur;    { force Delta1 in the plane of the face }
     Delta1.Z:=Delta1.Z - F.Normale.Z*Facteur;
     for I:=1 to 3 do
      begin
       P[I].X:=P[I].X-Delta1.X;
       P[I].Y:=P[I].Y-Delta1.Y;
       P[I].Z:=P[I].Z-Delta1.Z;
      end;
    end;
   S:='  ';
   { experiment }
   if UseIntegralVertices then
   begin
     { wacko crap to get the vertexes }
     { FS is first of a linked list of structures
        specifying vertex cycles }
     FS:=F.FaceOfPoly;
     K:=1;
     while Assigned(FS) do {FS is not Nil, why not say it that way?
                            imitating vertices(of) code below) }
     begin
       K:=1;
       for J:=0 to FS^.prvVertexCount-1 do {one more than # vertexes useed }
       { try to find some that are almost integers }
       begin
         V:=FS^.prvVertexTable[J]^.P;  { an actual vertex }
         if AlmostIntegral(V,V2) then
         begin
           if (K=2) and EqualVect(VT[1],V2) then
             continue;
           if (K=3) and CoLinear(VT[1],VT[2],V2) then
             continue;
           VT[K]:=V2;
           K:=K+1;
           if K>3 then break;
         end;
       end;
       if K>3 then break;
       FS:=FS^.NextF;
     end;
     if K>3 then {we got some nearly integral threepoints
       so lets usem }
     begin
       P[1]:=VT[1];
       { if normal would be wrong, exchange }
       if Dot(Cross(VecDiff(VT[2],VT[1]),VecDiff(VT[3],VT[1])),F.Normale)>0 then
       begin
          P[2]:=VT[3]; P[3]:=VT[2];
       end
       else
       begin
         P[2]:=VT[2]; P[3]:=VT[3]
       end
     end
(* We could use 1 or two integral ones if they are found, but
   atm I'm not convinced it's worth it.
     else if K>1 then  { use the first integral vertex as P[1] }
     begin
       P[1]:=VT[1];
       VT[2]:=VecSum(P[1],VecScale(100,VecDiff(P[2],P[1])));
       VT[3]:=VecSum(P[1],VecScale(100,VecDiff(P[3],P[1])));
         if Dot(Cross(VecDiff(VT[2],VT[1]),VecDiff(VT[3],VT[1])),F.Normale)>0 then
         begin
            P[2]:=VT[3]; P[3]:=VT[2];
         end
         else
         begin
           P[2]:=VT[2]; P[3]:=VT[3]
         end
     end
*)
   else if ExpandThreePoints then
   (* slower but more accurate alternative, suggested by gage144
      on Quake3World editing forum: make a 2d grid of points on
      the axis plane closest to the face plane, project them onto
      the face, then use the three projected points that are nearest
      to being integral, rounded to integral *)
   begin
      P[2]:=VecSum(P[1],VecScale(100,VecDiff(P[2],P[1])));
      P[3]:=VecSum(P[1],VecScale(100,VecDiff(P[3],P[1])));
   end
   end;

   {start writing out a face}

   if MapSaveSettings.MapFormat=HL2Type then
     S:= S+ '  "plane" "';

   for I:=1 to 3 do
    with P[I] do
     begin
      if MapSaveSettings.MapFormat=HL2Type then
        S:=S+'('
      else
        S:=S+'( ';
      R:=Round(X);

      if WriteIntegers or (Abs(X-R) < rien) then
       S:=S+IntToStr(R)+' '
      else
       S:=S+FloatToStrF(X, ffFixed, 20, DecimalPlaces)+' ';
      R:=Round(Y);

      if WriteIntegers or (Abs(Y-R) < rien) then
       S:=S+IntToStr(R)+' '
      else
       S:=S+FloatToStrF(Y, ffFixed, 20, DecimalPlaces)+' ';
      R:=Round(Z);

      if WriteIntegers or (Abs(Z-R) < rien) then
       S:=S+IntToStr(R)
      else
       S:=S+FloatToStrF(Z, ffFixed, 20, DecimalPlaces);

      if MapSaveSettings.MapFormat=HL2Type then
        S:=S+') '
      else
        S:=S+' ) ';

     end;

   if MapSaveSettings.MapFormat=HL2Type then
     S[Length(S)]:='"'; //Change last space into quote

   {start writing out a texture coordinates}
   if MapSaveSettings.MapFormat=BPType then
    with F do
     begin
      GetThreePointsUserTex(PT[1], PT[2], PT[3],Nil);
      GetPXPY(Normale, PT, PX, PY, Dist);
      S:=S+'( ';
      write3vect(PX,S);
      write3vect(PY,S);
      S:=S+') ';
     end;


   {start writing out a texture name and tx coordinates}
   with F do
    begin

     {texture name}
     TextureName:=NomTex;
     if MapSaveSettings.MapFormat=HL2Type then
       S:= S+#13#10'    "material" "';

     if MapSaveSettings.MapVersion>1 then
       TextureName:='"'+ConcatPaths([GameTexturesPath, TextureName])+'"';

     if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
       TextureName:=UpperCase(TextureName);
     if not (SetupGameSet.Specifics.Values['TextureNameDontReverseSlashes']<>'') then
       TextureName:=ReverseSlashes(TextureName);

     S:=S+TextureName;

     if MapSaveSettings.MapFormat=HL2Type then
       S:= S+'"';

     {texture coordinates}
     case MapSaveSettings.MapFormat of
      V220Type:
        Valve220MapParams(MapSaveSettings,False,Normale, F, S);
      HL2Type:
        Valve220MapParams(MapSaveSettings,True,Normale, F, S);
      CoD2Type:
        CoD2MapParams(MapSaveSettings,Normale, F, S);

      else {case}
        if not (MapSaveSettings.MapFormat=BPType) then
        begin
          SimulateEnhTex(PT[1], PT[3], PT[2], Mirror); {doesn't scale}

          ApproximateParams(MapSaveSettings, Normale, PT, Params, Mirror); {does scale}
          for I:=1 to 2 do
            S:=S+' '+IntToStr(Round(Params[I]));
          for I:=3 to 5 do
          begin
            R:=Round(Params[I]);
            if Abs(R-Params[I])<rien then
              S:=S+' '+IntToStr(R)
            else
              S:=S+' '+FloatToStrF(Params[I], ffFixed, 20, DecimalPlaces);
          end;
      end;
     end;{case MapFormat}
   end; {with f}

   if MapSaveSettings.GameCode=mjHexen then
     S:=S+' -1'
   else
   if MapSaveSettings.GameCode=mjSin then
   { tiglari: Sin/KP/SOF/Q2 code below manages the content/
      flags/value information in textures.  It's complicated
      because there is in general default info in the textures
      which can be overridden in the faces, Sin is the most
      complex. }
   begin
      Q := GlobalFindTexture(F.NomTex,Nil);
      if Q<>Nil then
      begin { see comments to QkMap on what's going on here }
       Q:=Q.LoadPixelSet;
       if not (Q is QTextureSin) then
         Q:=Nil;
      end;
      if Q=Nil then
        Q:=QTextureSin.Create('', Nil);
      Q.AddRef(+1);
      try
       { these function below updates S }
       StashFloatFlag('friction',2);     { for flags stored as floats }
       StashFloatFlag('restitution',2);
       StashIntFlag('direct');        { stash string as float }
       StashIntFlag('directangle');
       StashStrFlag('directstyle');   { stash string as string }
       StashFloatFlag('translucence',2);
       StashFloatFlag('trans_mag',2);
       StashFloatFlag('animtime', 2);
       StashIntFlag('trans_angle');
       StashStrFlag('color');
       WriteNonDefaultFlags(F.Specifics.Values['Flags'],StrToInt(Q.Specifics.Values['Flags']), FlagsTable);
       WriteNonDefaultFlags(F.Specifics.Values['Contents'],StrToInt(Q.Specifics.Values['Flags']), ContentsTable);
       { maybe the internal storage of these should be changed to fit
         the Sin file format }
       S1:=F.Specifics.Values['Value'];
       if S1<>'' then
         if Q.Specifics.Values['Value']<>S1 then
           S:=S+' lightvalue '+F.Specifics.Values['Value'];
     {  StashFloatFlag('nonlitvalue', 2);   { Nun functiona }
       rval:=F.GetFloatSpec('nonlit', -1);
       if rval >= 0 then
         if Q.GetFloatSpec('nonlit', -1)<>rval then
           S:=S+' nonlitvalue '+FloatToStrF(rval, ffFixed, 7, 2);
      finally
        Q.AddRef(-1);
      end;
   end
   else  { kp seems to need field values
             written into the map.  Alex write code to
             put the c, f, v flags into the texture link }
   if (MapSaveSettings.GameCode=mjKingPin) then
   begin
     Q := GlobalFindTexture(F.NomTex,Nil);  { find the Texture Link object }
     if Q<>Nil then Q.Acces;              { load it (but not the texture it points to !) }
     S1:=CheckFieldDefault('Contents','c', Q);
     S2:=CheckFieldDefault('Flags','f', Q);
     S3:=CheckFieldDefault('Value','v', Q);
     S:=S+' '+S1+' '+S2+' '+S3;
   end
   else {for me, SOF seems to behave like Q2, but for other
      people, default flags seem to be written into the map
      to work, so that's what happens here }
   if (MapSaveSettings.GameCode=mjSOF) then
   begin
      Q := GlobalFindTexture(F.NomTex,Nil);  { find the Texture Link object }
      if Q<>Nil then Q:=Q.LoadPixelSet;      { load it, since the default flags
              are in the actual texture, not the link.) }
      S1:=CheckFieldDefault('Contents','Contents', Q);
      S2:=CheckFieldDefault('Flags','Flags', Q);
      S3:=CheckFieldDefault('Value','Value', Q);
      S:=S+' '+S1+' '+S2+' '+S3;
   end
   else
   if (MapSaveSettings.GameCode=mjCOD) then
   begin
     //FIXME: Output right flags
     S:=S+' 0 0 0 0';
   end
   else
   if (MapSaveSettings.GameCode=mjCOD2) then
   begin
     //FIXME: Output right flags
     S:=S+' X 0 0 0 0 0 0';
   end
   else
    { and in Q2, default flags get written into the map
      automatically, no wuccaz (<- wuccin furries) }
    {\tiglari}
    {Decker - FIXME: Until we figure out how to clearly handle
     MOHAA's face-flags, we now just write them to the .MAP so
     the MOHAA-Q3MAP.EXE will be happy.}
   begin
      S1:=F.Specifics.Values['Contents'];
      S2:=F.Specifics.Values['Flags'];
      S3:=F.Specifics.Values['Value'];
      if (SetupGameSet.Specifics.Values['ForceFaceFlags']='1')
      or (S1<>'') or (S2<>'') or (S3<>'') then
      //Decker - force write face-flags for certain games (MOHAA and EF2)
      begin
        if S1='' then S1:='0';
        if S2='' then S2:='0';
        if S3='' then S3:='0';
        S:=S+' '+S1+' '+S2+' '+S3;

        if MapSaveSettings.GameCode=mjMOHAA then
          MohaaSurfaceParms(F, S);

      end;
   end;

   if (MapSaveSettings.MapFormat=QetpType) then
     S:=S+TxField[(MapSaveSettings.GameCode>='A') and (MapSaveSettings.GameCode<='Z'), EtpMirror];

   Brush.Add(S);
  end;
end;

procedure SaveAsMapTextTBezier(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Target: TStrings);
var
 cp: TBezierMeshBuf5;
 I, J, K, R: Integer;
 S: String;
 Value: PSingle;
 TextureName: String;
 DecimalPlaces: Integer;
begin
 ResolveMapSaveSettings(MapSaveSettings);

 DecimalPlaces := MapSaveSettings.DecimalPlaces;

 with TBezier(ObjectToSave) do
 begin

 cp:=ControlPoints;
 if (cp.W>1) and (cp.H>1) then
  begin   { ignore Bezier lines (with only 1 row or 1 column of control points) }
   Target.Add(CommentMapLine(Ancestry));
   Target.Add(' {');
   case MapSaveSettings.PatchDefVersion of
   2: Target.Add('  patchDef2');
   3: Target.Add('  patchDef3');
   end;
   Target.Add('  {');

   {texture name}
   TextureName:=NomTex;

   if MapSaveSettings.MapVersion>1 then
     TextureName:='"'+ConcatPaths([GameTexturesPath, TextureName])+'"';

   if SetupGameSet.Specifics.Values['TextureNameUppercase']<>'' then
     TextureName:=UpperCase(TextureName);
   if not (SetupGameSet.Specifics.Values['TextureNameDontReverseSlashes']<>'') then
     TextureName:=ReverseSlashes(TextureName);

   Target.Add('   ' + TextureName);

   { We should start saving the other values too, once we've
     figured out what they actually are... }
   if MapSaveSettings.GameCode=mjMOHAA then
   begin
     case MapSaveSettings.PatchDefVersion of
     2: S:=Format('   ( %d %d 0 0 0', [cp.W, cp.H]);
     3: S:=Format('   ( %d %d 0 0 0 0 0', [cp.W, cp.H]);
     end;
     MohaaSurfaceParms(TBezier(ObjectToSave), S);
     S:=S+' )';
     Target.Add(S);
   end
   else
     case MapSaveSettings.PatchDefVersion of
     2: Target.Add(Format('   ( %d %d 0 0 0 )', [cp.W, cp.H]));
     3: Target.Add(Format('   ( %d %d 0 0 0 0 0 )', [cp.W, cp.H]));
     end;
   Target.Add('   (');
   for J:=0 to cp.W-1 do
    begin
     Value:=@cp.CP^[5*J];
     S:='    ( ';
     for I:=1 to cp.H do
      begin
       S:=S+'( ';
       for K:=1 to 5 do
        begin
         R:=Round(Value^);
         if {WriteIntegers or} (Abs(Value^-R) < rien) then
          S:=S+IntToStr(R)+' '
         else
          S:=S+FloatToStrF(Value^, ffFixed, 20, DecimalPlaces)+' ';
         Inc(Value);
        end;
       S:=S+') ';
       Inc(Value, 5*(cp.W-1));
      end;
     Target.Add(S+')');
    end;
   Target.Add('   )');
   Target.Add('  }');
   Target.Add(' }');
  end;

 end;
end;

procedure SaveAsMapTextTDuplicator(ObjectToSave: QObject; MapSaveSettings: TMapSaveSettings; Negatif: TQList; Texte: TStrings; Flags2: Integer; HxStrings: TStrings);
var
 I: Integer;
begin
 ResolveMapSaveSettings(MapSaveSettings);

 with TDuplicator(ObjectToSave) do
 begin

{if (Specifics.Values['out']<>'')
 and (FParent<>Nil) and (TvParent.TvParent=Nil) then
  GlobalWarning(LoadStr1(230));}  { FIXME: do various map tests globally }
 for I:=0 to LengthBuildImages-1 do
   SaveAsMapText(ItemToSave(I), MapSaveSettings, Negatif, Texte, Flags2, HxStrings);
  
 end;
end;

 {------------------------}

function TFQMap.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QMap) and (State<>cmWindow) and inherited AssignObject(Q, State);
end;

procedure TFQMap.ReadSetupInformation(Level: Integer);
begin
 inherited;
 ScrollBox1.Invalidate;
 ScrollBox1.Color:=MapColors(lcVueXY);
end;

procedure TFQMap.Button1Click(Sender: TObject);
begin
 with ValidParentForm(Self) as TQkForm do
  ProcessEditMsg(edOpen);
end;

procedure TFQMap.wmInternalMessage(var Msg: TMessage);
var
 S: String;
 Min, Max, D: TVect;
 Root: QObject;
 M: TMatrixTransformation;
begin
 if Msg.wParam=wp_AfficherObjet then
  begin
   if FileObject=Nil then
    S:=''
   else
    begin
     FileObject.Acces;
     S:=FileObject.Specifics.Values['Game'];
     if S='' then
      S:=LoadStr1(182)
     else
      S:=FmtLoadStr1(181, [S]);
    end;
   Label1.Caption:=S;
   if FileObject<>Nil then
    S:=(FileObject as QMap).GetOutputMapFileName;
   EnterEdit1.Text:=S;
   if FileObject=Nil then Exit;
   S:=FileObject.Specifics.Values['Root'];
   if S='' then Exit;  { no data }
   Root:=FileObject.SubElements.FindName(S);
   if (Root=Nil) or not (Root is TTreeMap) then Exit;  { no data }
   CheckTreeMap(TTreeMap(Root));
   Root.ClearAllSelection;

   Min.X:=-10;
   Min.Y:=-10;
   Min.Z:=-10;
   Max.X:=+10;
   Max.Y:=+10;
   Max.Z:=+10;
   TTreeMap(Root).ChercheExtremites(Min, Max);

   D.X:=(ScrollBox1.ClientWidth-20)/(Max.X-Min.X);
   D.Y:=(ScrollBox1.ClientHeight-18)/(Max.Y-Min.Y);
   if D.Y<D.X then D.X:=D.Y;
   ScrollBox1.MapViewProj.Free;
   ScrollBox1.MapViewProj:=Nil;
  {ScrollBox1.MapViewProj:=GetTopDownAngle(0, D.X, False);}
   M:=MatriceIdentite;
   M[1,1]:=D.X;
   M[2,2]:=-D.X;
   M[3,3]:=-D.X;
   ScrollBox1.MapViewProj:=GetMatrixCoordinates(M);
   ScrollBox1.HorzScrollBar.Range:=ScrollBox1.ClientWidth;
   ScrollBox1.VertScrollBar.Range:=ScrollBox1.ClientHeight;
   D.X:=(Min.X+Max.X)*0.5;
   D.Y:=(Min.Y+Max.Y)*0.5;
   D.Z:=(Min.Z+Max.Z)*0.5;
   FRoot:=TTreeMap(Root);
   ScrollBox1.CentreEcran:=D;
  end
 else
  inherited;
end;

procedure TFQMap.EnterEdit1Accept(Sender: TObject);
var
 Q: QMap;
 S: String;
begin
 Q:=FileObject as QMap;
 S:=EnterEdit1.Text;
 Undo.Action(Q, TSpecificUndo.Create(LoadStr1(615), 'FileName',
  S, sp_AutoSuppr, Q));
end;

procedure TFQMap.FormCreate(Sender: TObject);
begin
 inherited;
 ScrollBox1:=TPyMapView.Create(Self);
 ScrollBox1.MapViewObject^.Parent:=Nil;
 ScrollBox1.Parent:=Panel2;
 ScrollBox1.Align:=alClient;
{FOldPaint:=ScrollBox1.OnPaint;}
 ScrollBox1.OnPaint:=ScrollBox1Paint;
end;

procedure TFQMap.ScrollBox1Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
var
 Pen: HPen;
 Brush: HBrush;
begin
 if FRoot=Nil then Exit;
{FOldPaint(Sender, PaintInfo);}
 Canvas.Handle:=DC;
 try
  SetupWhiteOnBlack(g_DrawInfo.DefWhiteOnBlack);
  ScrollBox1.MapViewProj.SetAsCCoord(DC);
  Pen:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_Pen));
  Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_Brush));
  g_DrawInfo.GreyBrush:=CreatePen(ps_Solid, 0, MapColors(lcOutOfView));
  try
   FRoot.Dessiner;
  finally
   SelectObject(g_DrawInfo.DC, Brush);
   SelectObject(g_DrawInfo.DC, Pen);
   DeleteObject(g_DrawInfo.GreyBrush);
  end;
 finally
  Canvas.Handle:=0;
 end;
end;

procedure TFQMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 FRoot:=Nil;
 inherited;
end;

 {------------------------}

initialization
  RegisterQObject(QQkm, 'y');
end.

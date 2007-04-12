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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkFileObjects, TB97, QkObjects, CursorScrollBox, ExtCtrls, StdCtrls,
  QkForm, QkMapObjects, QkBsp, EnterEditCtrl, PyMapView, PyMath,
  qmatrices, Python, { tiglari } QkTextures, QkSin { /tiglari};

{ $DEFINE TexUpperCase}
{ $DEFINE ClassnameLowerCase}

type

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
 QBaseMapFile = class(QMap)
            protected
              procedure LoadFile(F: TStream; FSize: Integer); override;
              procedure SaveFile(Info: TInfoEnreg1); override;
            end;

 QMapFile = class(QBaseMapFile)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         end;

 QHmfFile = class(QBaseMapFile)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         end;

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
    procedure ScrollBox1Paint(Sender: TObject; DC: {HDC}Integer; const rcPaint: TRect);
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    procedure ReadSetupInformation(Level: Integer); override;
  public
    ScrollBox1: TPyMapView;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  end;

 {------------------------}

function ReadEntityList(Racine: TTreeMapBrush; const SourceFile: String; BSP: QBsp) : Char;

 {------------------------}

implementation

uses Qk1, QkQme, QkMapPoly, qmath, Travail, Setup,
  Qk3D, QkBspHulls, Undo, Game, Quarkx, PyForms, QkPixelSet {Rowdy}, Bezier {/Rowdy},
  QkQ2, QkObjectClassList, MapError, StrUtils;

{$R *.DFM}

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
  2: if CharModeJeu=mj6DX then
       Result:=QHmfFile
         else
       Result:=QMapFile;
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
 FinDeLigne = False;
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
 StringValue: String;
 NumericValue: Double;
 V: array[1..3] of TVect;
 P: TPolyhedron;
 Surface: TFace;
 I, J, K, NumericValue1, ContentsFlags: Integer;
 WorldSpawn: Boolean;
 Entite, EntitePoly: TTreeMapSpec;
 L: TStringList;
 LineNoBeingParsed: Integer;
 Juste13{, FinDeLigne}, Q2Tex, ReadSymbolForceToText: Boolean;
 HullNum, BrushNum, FaceNum: Integer;
 HullList: TList;
 Source, Prochain: PChar;
 Entities, MapStructure {Rowdy}, MapStructureB {/Rowdy}: TTreeMapGroup;
 Params: TFaceParams;
 InvPoly, InvFaces: Integer;
 TxCommand: Char;
 OriginBrush: TPolyhedron;
 Facteur: TDouble;
 Delta, Delta1: TVect;
 {Rowdy}
 V5: TVect5;
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
     if (C=#13) or ((C=#10) {and not Juste13}) then
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
             if FinDeLigne and (S<>'') and (S[Length(S)]='"') then
             begin
               SetLength(S, Length(S)-1);
               Break;
             end
             else
               Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(249)]);
           end;

           Inc(Source);
           if (C='"') and not FinDeLigne then
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
             if (C=#13) or ((C=#10) {and not Juste13}) then
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

           if (C=#13) or ((C=#10) {and not Juste13}) then
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
 
 function ReadVect7(theLastSymbolForceToText: Boolean): TVect5;
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
   ReadSymbol(sNumValueToken);
   ReadSymbol(sNumValueToken);
   ReadSymbolForceToText:=theLastSymbolForceToText;
   ReadSymbol(sBracketRight);
   ReadSymbolForceToText:=False;
 end;

 {Rowdy}
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
 {/Rowdy}
 
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

 procedure ReadPatchDef2;
 var
   I, J: Integer;
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
     if LowerCase(LeftStr(S,9))='textures/' then
       S:=RightStr(S,Length(S)-9);
   end;

   {$IFDEF TexUpperCase}
   S:=LowerCase(S);
   {$ENDIF}
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
   V5:=ReadVect5(False);
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
     if LowerCase(LeftStr(S,9))='textures/' then
       S:=RightStr(S,Length(S)-9);
   end;

   {$IFDEF TexUpperCase}
   S:=LowerCase(S);
   {$ENDIF}
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
   V5:=ReadVect7(False);
   // Nr 1: Width (many lines of control points there are)
   // Nr 2: Height (how many control points on each line)
   // Nr 3: HorzSubdivisions (?)
   // Nr 4: VertSubdivisions (?)
   // Nr 5: ?
   // Nr 6: ?
   // Nr 7: ?

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
       ReadSymbol(sBracketRight); // read the trailing rparen for the line end
     end;
     B.ControlPoints:=MeshBuf1;
     B.AutoSetSmooth;

     ReadSymbol(sBracketRight);  { rparen which finishes all the lines of control points }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the patchDef2 }
     ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }
   finally
     FreeMem(MeshBuf1.CP);
   end;
 end;


 procedure ReadBrushDef;
 var
   R1, R2, TexS, TexT, Tex0, P0, P1, P2, ZVect : TVect;
   Denom : Double;
   Matrix : TMatrixTransformation;
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
      if LowerCase(LeftStr(S,9))='textures/' then
         S:=RightStr(S,Length(S)-9);
    end;

    {$IFDEF TexUpperCase}
    S:=LowerCase(S);
    {$ENDIF}
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
  ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the patchDef2 }
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
      if LowerCase(LeftStr(texname,9))='textures/' then
         texname:=RightStr(texname,Length(texname)-9);
    end;

    {$IFDEF TexUpperCase}
    texname:=LowerCase(texname);
    {$ENDIF}
    Q2Tex:=Q2Tex or (Pos('/',texname)<>0);
    Surface.NomTex:=texname;   { here we get the texture-name }
    Surface.SetThreePointsUserTex(P0,P1,P2,nil);
  end;
  ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the patchDef2 }
  ReadSymbol(sCurlyBracketRight);    { rbrace which finishes the brush }
 end;

 procedure ReadSinSurfaceFlags;
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
      Surface.SpecificsAdd('color='+S1);
    { Surface.SetFloatsSpec('color', ThreeSing);  }
     end
    else
    if S = 'directstyle' then { following string value }
     begin
      ReadSymbol(sStringToken);
      Surface.SpecificsAdd('directstyle='+S);
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
                 Surface.SpecificsAdd('direct='+IntToStr(Round(LastValue)));
                end
              else
              if S1 = 'directangle' then
                begin
                 Surface.SpecificsAdd('directangle='+IntToStr(Round(LastValue)));
                end;
        'f' : if S1 = 'friction' then
                begin
                 Surface.SetFloatSpec('friction', LastValue);
                end;
        'l' : if S1 = 'lightvalue' then { assuming that this is the old Value }
                begin
                  Surface.SpecificsAdd('Value='+IntToStr(Round(LastValue)));
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
              if S1 =  'trans_mag' then
                begin
                  Surface.SetFloatSpec('trans_mag', LastValue)
                end
              else
              if S1 = 'trans_angle' then
                begin
                 Surface.SpecificsAdd('trans_angle='+IntToStr(Round(LastValue)));
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

 procedure ReadMohaaSurfaceParms;
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
     // surfaceLight (int), surfaceColor (3f), surfaceAngle (int), tesselation (1f)
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

procedure WC33Params;
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

begin
  ProgressIndicatorStart(5451, Length(SourceFile) div Granularite);
  try
    Source:=PChar(SourceFile);
    Prochain:=Source+Granularite;   { point at which progress marker will be ticked}
    Result:=mjQuake;     { Into Result is but info about what game the map is for }
    Q2Tex:=False;
    WC33map:=False; {Decker}
    g_MapError.Clear;
    ReadSymbolForceToText:=False;    { ReadSymbol is not to expect text}
    LineNoBeingParsed:=1;
    InvPoly:=0;
    InvFaces:=0;
    Juste13:=False;
   {FinDeLigne:=False;}
    HullList:=Nil;
    L:=TStringList.Create;
    try   { L and HullList get freed by finally, regardless of exceptions }
     WorldSpawn:=False;  { we haven't seen the worldspawn entity yet }
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
       if (SymbolType=sStringToken) and (CompareText(S,'Version')=0) then
        begin
         ReadSymbol(sStringToken); // get the map version number // NumValueToken);
         if SymbolType<>sNumValueToken then
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(251)]); // invalid number
         MapVersion := Trunc(NumericValue+0.5);
         case MapVersion of
         1: Result:=mjDoom3;   // this is a Doom 3 Version 1 map
         2: Result:=mjDoom3;   // this is a Doom 3 Version 2 map
         3: Result:=mjQuake4;  // this is a Quake 4 Version 3 map
            //Right now all Quake 4 maps are Version 3
         else
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(267)]); // can't read this map version
         end;
         ReadSymbol(sNumValueToken);
        end;

       ReadSymbol(sCurlyBracketLeft);
       L.Clear;
       Classname:='';
       HullNum:=-1;
       { pull in the quoted-string attribute-value pairs }
       while SymbolType=sStringQuotedToken do
        begin
         S1:=S;  { S is where ReadSymbol sticks quoted strings }

         // NOT HERE ---
         // Rowdy: the thing read might not be a { as Doom 3 appears to expect "Version" followed by
         // "1" or "2" (all without quotes) to indicate what version the .map file is.
         // Barf right now if it is a Doom 3 Map, especially version 2
         //if CompareText(s1, 'version') = 0 then
         //begin
         //  ReadSymbol(sNumValueToken);
         //  MapVersion := NumericValue;
         //  if MapVersion <> 1 then
         //    raise EErrorFmt(266, [LineNoBeingParsed, LoadStr1(260)]); // don't read Doom 3 version 2 maps
         //end;

        {FinDeLigne:=True;}
         ReadSymbol(sStringQuotedToken);
        {FinDeLigne:=False;}
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
         { only one worldpsawn allowed }
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
            HullList:=TList.Create;
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
               ReadPatchDef2(); {DECKER - moved to local-procedure to increase readability}
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
               ReadPatchDef3(); //DECKER - moved to local-procedure to increase readability
             end
             else if LowerCase(s)='brushdef' then
             begin
               // A brushDef means it is at least a Quake 3 map
               if Result=mjQuake then
                 Result:=mjQ3A;
               ReadBrushDef(); //DECKER - moved to local-procedure to increase readability
             end
             else if LowerCase(s)='brushdef2' then
             begin
               // A brushDef2 means it is at least a Doom 3 map (not sure about this)
               if Result=mjQuake then
                 Result:=mjDoom3;
               ReadBrushDef3(); //DECKER - moved to local-procedure to increase readability
               // The difference between brushdef2 and brushdef3 is minimal...
             end
             else if LowerCase(s)='brushdef3' then
             begin
               // A brushDef3 means it is at least a Doom 3 map
               if Result=mjQuake then
                 Result:=mjDoom3;
               ReadBrushDef3();
             end
             else
               raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(260)]);
           end
          else
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
              {$IFDEF TexUpperCase}
              S:=LowerCase(S);
              {$ENDIF}
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
              if SymbolType=sNumValueToken then
               begin
                NumericValue1:=Round(NumericValue);
                ReadSymbol(sNumValueToken);
                if SymbolType<>sNumValueToken then
                 Result:=mjHexen  { Hexen II : ignore la luminosité de radiation }
                else
                 begin  { Quake 2, Heretic2 and Mohaa : read the three fields }
                  ContentsFlags:=NumericValue1;
                  Surface.Specifics.Values['Contents']:=IntToStr(NumericValue1);
                  Surface.Specifics.Values['Flags']:=IntToStr(Round(NumericValue));
                  ReadSymbol(sNumValueToken);
                  Surface.Specifics.Values['Value']:=IntToStr(Round(NumericValue));
                  ReadSymbol(sNumValueToken);
                  Result:=mjNotQuake1;
                  if SymbolType=sStringToken then // Mohaa
                    ReadMohaaSurfaceParms
                 end;
               end
              else
               if SymbolType=sStringToken then
                begin  { Sin : extra surface flags as text }
                 Result:=mjSin;
                 ReadSinSurfaceFlags(); {DECKER - moved to procedure to increase readability}
                end;

              if not Surface.LoadData then
               Inc(InvFaces)
              else
               case TxCommand of   { "//TX#" means that the three points already define the texture params themselves }
                '1': ;
                '2': Surface.TextureMirror:=True;
               else
                if WC33Map then
                  WC33Params
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
         {Rowdy}
           end; // end of not patchDef2
         {/Rowdy}
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
        Result:=mjQuake;
      if CharModeJeu=mjTorque then
        Result:=mjTorque; 
      if CharModeJeu=mjSylphis then
       Result:=mjSylphis

    end;
    mjQ3A:
    { FIXME:  barf coding, the idea is that  if Q3a mode
      is detected, we stay in the current game mode if it's
      one of the Q3A-format games, otherwise switch to Q3A }
    begin
     if CharModeJeu=mjStarTrekEF  then
       Result:=CharModeJeu;
    end;
  end;

  if InvFaces>0 then
    GlobalWarning(FmtLoadStr1(257, [InvFaces]));
  if InvPoly>0 then
    GlobalWarning(FmtLoadStr1(256, [InvPoly]));
end;

 {------------------------}

class function QHmfFile.TypeInfo;
begin
 Result:='.hmf';
end;

class procedure QHmfFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5180);
 Info.FileExt:=808;
end;

 {------------------------}

class function QMapFile.TypeInfo;
begin
 Result:='.map';
end;

class procedure QMapFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5142);
 Info.FileExt:=784;
end;

procedure QBaseMapFile.LoadFile(F: TStream; FSize: Integer);
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

procedure QBaseMapFile.SaveFile(Info: TInfoEnreg1);
var
 Dest, HxStrings: TStringList;
 Racine: QObject;
 List: TQList;
 saveflags : Integer;
 MapOptionSpecs : TStringList;
  { Rowdy, for Doom 3 and Quake 4 stuff}
// MapVersion: Integer;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      Racine:=SubElements.FindName(Specifics.Values['Root']);
      if (Racine=Nil) or not (Racine is TTreeMapBrush) then
       Raise EError(5558);
      Racine.LoadAll;
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

//       MapVersion:=0;
       MapOptionSpecs:=SetupSubSet(ssMap,'Options').Specifics;
       if ObjectGameCode=mjDoom3 then
       begin
         // Rowdy: write an extra line to indicate we are using version 1 .map file
         // format or Doom 3's default version 2.
         if MapOptionSpecs.Values['SaveMapVersion'] = '1' then
         begin
//           MapVersion:=1;
           Dest.Add('Version 1');
         end
         else if MapOptionSpecs.Values['SaveMapVersion'] = '2' then
         begin
//           MapVersion:=2;
           Dest.Add('Version 2');
         end
         else
         begin
//           MapVersion:=2;
           Dest.Add('Version 2');  //Default to map version 2
         end;
         Dest.Add('');
       end;
       if ObjectGameCode=mjQuake4 then
       begin
//         MapVersion:=3;
         Dest.Add('Version 3');
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

     { TTreeMap(Racine).SaveAsText(List, Dest, IntSpec['saveflags'], HxStrings); }
       TTreeMap(Racine).SaveAsText(List, Dest, saveflags, HxStrings);
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
 Racine: QObject;
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
   Racine:=FileObject.SubElements.FindName(S);
   if (Racine=Nil) or not (Racine is TTreeMap) then Exit;  { no data }
   CheckTreeMap(TTreeMap(Racine));
   Racine.ClearAllSelection;

   Min.X:=-10;
   Min.Y:=-10;
   Min.Z:=-10;
   Max.X:=+10;
   Max.Y:=+10;
   Max.Z:=+10;
   TTreeMap(Racine).ChercheExtremites(Min, Max);

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
   FRoot:=TTreeMap(Racine);
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

procedure TFQMap.ScrollBox1Paint(Sender: TObject; DC: Integer; const rcPaint: TRect);
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

initialization
  RegisterQObject(QQkm, 'y');
  RegisterQObject(QMapFile, 'x');
  RegisterQObject(QHmfFile, 'x');
end.

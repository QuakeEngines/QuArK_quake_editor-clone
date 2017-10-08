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
Revision 1.45  2017/10/06 14:13:39  danielpharos
Big reworking to push game-specific BSP loading into the game files.

Revision 1.44  2017/10/01 09:54:40  danielpharos
Shader stage sub-data can have multiple levels. Skip them all (for now).

Revision 1.43  2017/10/01 09:32:52  danielpharos
Skip sub-data in shader stages to prevent syntax errors when reading these shaders.

Revision 1.42  2012/09/05 18:06:10  danielpharos
Move implementation of FloatSpec internally to QkObjects.

Revision 1.41  2009/07/15 10:38:00  danielpharos
Updated website link.

Revision 1.40  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.39  2008/09/29 21:45:30  danielpharos
Soft-coded 'maps' directory (not in Python yet).

Revision 1.38  2008/09/06 15:57:08  danielpharos
Moved exception code into separate file.

Revision 1.37  2008/08/26 16:21:51  danielpharos
Added filename of broken shader/material file to error-message.

Revision 1.36  2008/04/26 15:31:54  danielpharos
Changed the way the default shader image is found to be more like the new Doom 3 code.

Revision 1.35  2007/08/14 16:32:59  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.34  2007/05/06 21:21:43  danielpharos
Changed DDS support to be EF2-specific.

Revision 1.33  2007/05/05 22:17:52  cdunde
To add .dds Texture Browser loading from .pk3 files.

Revision 1.32  2007/03/25 13:52:24  danielpharos
Moved a few dictionnary words around.

Revision 1.31  2006/06/23 07:27:26  cdunde
To reverse 1.30 change, Q3 did not like that one!
Stopped shader texture from displaying in the game.

Revision 1.30  2006/05/08 06:40:36  cdunde
To make same kind of change for Q3 as Rowdy
just did for D3 and Q4 in QkD3.pas.

Revision 1.29  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.27  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.26  2004/05/14 05:48:55  cdunde
To fix comment symbol errors causing shader textures not to show up in texture Browser. Correction by Rowdy

Revision 1.25  2003/10/26 00:33:50  silverpaladin
Added Missing Shader Texture texture  To try to prevent Access Violations in 3d views

Revision 1.24  2003/08/12 15:57:01  silverpaladin
Added support for block comments in shader files

Revision 1.23  2002/06/18 00:55:51  tiglari
add .png to .jpg texture support.  If any more image formats start being used
  for textures, the strategy for doing this will need to be rethought

Revision 1.22  2002/05/05 10:21:16  tiglari
Reading editable surfaceparms from MOHAA shaders

Revision 1.21  2001/03/20 21:44:19  decker_dk
Updated copyright-header

Revision 1.20  2001/01/21 15:49:48  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.19  2001/01/15 19:21:04  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.18  2000/09/10 12:57:06  alexander
disabled defaultimage caching (cache is not easily initialized)

Revision 1.17  2000/08/22 11:39:21  tiglari
'q' specific processing added to shaders,for specifying image in .qrk files

Revision 1.16  2000/08/19 07:30:47  tiglari
Cacheing of DefaultImage for Shaders

Revision 1.15  2000/07/18 19:38:00  decker_dk
Englishification - Big One This Time...

Revision 1.14  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.13  2000/07/06 02:47:58  alexander
attempt to improve shader handling

Revision 1.12  2000/06/24 02:50:35  tiglari
bad solution for adjust w. min. distortion for shaders

Revision 1.11  2000/06/12 18:41:59  decker_dk
more control on shaders being valid

Revision 1.10  2000/05/21 13:11:50  decker_dk
Find new shaders and misc.

Revision 1.9  2000/05/14 15:06:56  decker_dk
Charger(F,Taille) -> LoadFile(F,FSize)
ToutCharger -> LoadAll
ChargerInterne(F,Taille) -> LoadInternal(F,FSize)
ChargerObjTexte(Q,P,Taille) -> ConstructObjsFromText(Q,P,PSize)

Revision 1.8  2000/04/29 15:13:30  decker_dk
Allow other than PAK#.PAK files

Revision 1.7  2000/04/24 09:54:54  arigo
Q3 shaders, once more

Revision 1.6  2000/04/22 08:54:23  arigo
Shader stage attributes were not written correctly

Revision 1.5  2000/04/18 18:47:57  arigo
Quake 3 : auto export shaders
}

unit QkQ3;

interface

uses
  SysUtils, Windows, Classes, QkZip2, QkFileObjects, QkObjects, QkText,
  QkTextures, QkBsp, Setup, QkWad, QkPixelSet;

type
  Q_CFile = class(QCfgFile)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         procedure ObjectState(var E: TEtatObjet); override;
        end;
  Q_HFile = class(QCfgFile)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         procedure ObjectState(var E: TEtatObjet); override;
        end;
  Q3Pak = class(QZipPak)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
  QShader = class(QPixelSet)
            protected
              DefaultImageCache : QPixelSet;
            public
              class function TypeInfo: String; override;
              {procedure DataUpdate;}
              function DumpString : String;
              function DefaultImage : QPixelSet;
              {procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;}
              function GetSize : TPoint; override;
              procedure SetSize(const nSize: TPoint); override;
              function Description : TPixelSetDescription; override;
              function SetDescription(const PSD: TPixelSetDescription;
                                      Confirm: TSDConfirm) : Boolean; override;
              function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
              procedure ListDependencies(L: TStringList); override;
            end;
  QShaderStage = class(QPixelSet)
                 public
                   class function TypeInfo: String; override;
                   function ContainsImageReference : Boolean;
                   function ProvidesSomeImage : QPixelSet;
                   function LoadPixelSet : QPixelSet; override;
                   function Description : TPixelSetDescription; override;
                   function SetDescription(const PSD: TPixelSetDescription;
                                           Confirm: TSDConfirm) : Boolean; override;
                 end;
  QShaderFile = class(QWad)
                protected
                  procedure SaveFile(Info: TInfoEnreg1); override;
                  procedure LoadFile(F: TStream; FSize: Integer); override;
                public
                  class function TypeInfo: String; override;
                  class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                  function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                end;
 QBsp3FileHandler = class(QBspFileHandler)
  public
   procedure LoadBsp(F: TStream; StreamSize: Integer); override;
   procedure SaveBsp(Info: TInfoEnreg1); override;
   function GetEntryName(const EntryIndex: Integer) : String; override;
   function GetLumpEdges: Integer; override;
   function GetLumpEntities: Integer; override;
   function GetLumpFaces: Integer; override;
   function GetLumpLeafs: Integer; override;
   function GetLumpLeafFaces: Integer; override;
   function GetLumpModels: Integer; override;
   function GetLumpNodes: Integer; override;
   function GetLumpPlanes: Integer; override;
   function GetLumpSurfEdges: Integer; override;
   function GetLumpTexInfo: Integer; override;
   function GetLumpTextures: Integer; override;
   function GetLumpVertexes: Integer; override;
 end;

implementation

uses QuarkX, QkExceptions, Game, Travail, QkObjectClassList, Logging;

const
 LUMP_ENTITIES = 0;
 LUMP_SHADERS = 1;
 LUMP_PLANES = 2;
 LUMP_NODES = 3;
 LUMP_LEAFS = 4;
 LUMP_LEAFSURFACES = 5;
 LUMP_LEAFBRUSHES = 6;
 LUMP_MODELS = 7;
 LUMP_BRUSHES = 8;
 LUMP_BRUSHSIDES = 9;
 LUMP_DRAWVERTS = 10;
 LUMP_DRAWINDEXES = 11;
 LUMP_FOGS = 12;
 LUMP_SURFACES = 13;
 LUMP_LIGHTMAPS = 14;
 LUMP_LIGHTGRID = 15;
 LUMP_VISIBILITY = 16;

 HEADER_LUMPS = 17;

type
 TBspEntries = record
               EntryPosition: LongInt;
               EntrySize: LongInt;
              end;
 TBsp3Header = record
           Signature: LongInt;
           Version: LongInt;
           Entries: array[0..HEADER_LUMPS-1] of TBspEntries;
          end;

const
 Bsp3EntryNames : array[0..HEADER_LUMPS-1] of String =
   (              {Actually a 'FilenameExtension' - See TypeInfo()}
    'entities'    + '.a.bsp3'   // eBsp3_entities
   ,'texinfo'     + '.b.bsp3'   // eBsp3_texinfo
   ,'planes'      + '.c.bsp3'   // eBsp3_planes
   ,'nodes'       + '.d.bsp3'   // eBsp3_nodes
   ,'leafs'       + '.e.bsp3'   // eBsp3_leafs
   ,'leaffaces'   + '.f.bsp3'   // eBsp3_leaffaces
   ,'leafbrushes' + '.g.bsp3'   // eBsp3_leafbrushes
   ,'models'      + '.h.bsp3'   // eBsp3_models
   ,'brushes'     + '.i.bsp3'   // eBsp3_brushes
   ,'brushsides'  + '.j.bsp3'   // eBsp3_brushsides
   ,'vertexes'    + '.k.bsp3'   // eBsp3_vertexes
   ,'meshvertexes'+ '.l.bsp3'   // eBsp3_meshvertexes
   ,'effects'     + '.m.bsp3'   // eBsp3_effects
   ,'faces'       + '.n.bsp3'   // eBsp3_faces
   ,'lighting'    + '.o.bsp3'   // eBsp3_lighting
   ,'lightvol'    + '.p.bsp3'   // eBsp3_lightvol
   ,'visibility'  + '.q.bsp3'   // eBsp3_visibility
   );

type

  QBsp3   = class(QFileObject)  protected class function TypeInfo: String; override; end;
  QBsp3a  = class(QZText)       protected class function TypeInfo: String; override; end;

class function QBsp3 .TypeInfo; begin TypeInfo:='.bsp3';                       end;
class function QBsp3a.TypeInfo; begin TypeInfo:='.a.bsp3'; {'entities.a.bsp3'} end;

{------------------------}

procedure Q_HFile.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiText;
// E.MarsColor:=clWhite;
end;

class function Q_HFile.TypeInfo;
begin
 Result:='.h';
end;

class procedure Q_HFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5174);
 Info.FileExt:=803;
end;

{------------------------}

procedure Q_CFile.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiText;
// E.MarsColor:=clWhite;
end;

class function Q_CFile.TypeInfo;
begin
 Result:='.c';
end;

class procedure Q_CFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5173);
 Info.FileExt:=802;
end;

{------------------------}

class function Q3Pak.TypeInfo;
begin
 Result:='.pk3';
end;

class procedure Q3Pak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5170);
 Info.FileExt:=798;
end;

{------------------------}

class function QShader.TypeInfo;
begin
 Result:=':shader';
end;

function QShader.DefaultImage : QPixelSet;
var
 Q: QObject;
 I: Integer;
 ValidStage: QPixelSet;
 DefaultImageName: array[0..1] of String;
 DefaultImageIndex: Integer;
 ImageFileName: String;
 Size: TPoint;
 V: array [1..2] of Single;
 TexExt: String;
begin
 Acces;
 Result:=Nil;
 {tiglari}
 {alex disabled this because it led to crashes
  (it is never initilialized)
 if DefaultImageCache<>Nil then
 begin

   result:=DefaultImageCache
 end
 else
 begin
 /alex}
 {/tiglari}

 TexExt:=SetupGameSet.Specifics.Values['TextureFormat'];
 if ReverseLink<>nil then
   DefaultImageName[0]:=ReverseLink.Specifics.Values['e'];
 if DefaultImageName[0]<>'' then
 begin
   ImageFileName:=Specifics.Values[DefaultImageName[0]];
   Log(LOG_VERBOSE,'attempting to load '+ImageFileName);
   try
     Result:=NeedGameFile(ImageFileName, '') as QPixelSet
   except
     Result:=nil;
   end;
 end
 else
 begin
   DefaultImageIndex:=0;
   DefaultImageName[0]:=Specifics.Values['q']; // look at the q specific (QTextureLnk.LoadPixelSet)
   DefaultImageName[1]:=Specifics.Values['qer_editorimage'];

   while ((Result=nil) and (DefaultImageIndex<2)) do
   begin
     if (DefaultImageName[DefaultImageIndex]<>'') then
     begin
       ImageFileName:=DefaultImageName[DefaultImageIndex];
       Log(LOG_VERBOSE,'attempting to load '+ImageFileName);
       try
         if (ExtractFileExt(ImageFileName)='') then
           Result:=NeedGameFile(ImageFileName+TexExt, '') as QPixelSet
         else
           Result:=NeedGameFile(ImageFileName, '') as QPixelSet;
       except
         Result:=nil;
       end;
     end;
     if Result=nil then
       DefaultImageIndex:=DefaultImageIndex+1;
   end;
 end;

 { examines all shaderstages for existing images }
 if Result=Nil then
 begin
   for I:=0 to SubElements.Count-1 do
   begin
     Q:=SubElements[I];
     if Q is QShaderStage then
     begin
       { Skip over $lightmap and those not containing images }
       if QShaderStage(Q).ContainsImageReference then
       begin
         try
           ValidStage:=QShaderStage(Q).ProvidesSomeImage;
           { Missing a texture, shader invalid? Return NIL }
           if not (ValidStage=Nil) then
             { Set to first valid stage, so something is displayed in the texture-browser }
             Result:=ValidStage;
             break;
         except
           Result:=NIL;
         end;
       end;
     end;
   end;
 end;
   
 { If no image could be found yet, try the shader-name itself }
 if Result=Nil then
 begin
   Log(LOG_VERBOSE,'attempting to load '+Name+TexExt);
   try
     Result:=NeedGameFile(Name+TexExt, '') as QPixelSet;
   except
     Result:=nil;
   end;
 end;
 
 { tiglari }
 {alex
 end;
 DefaultImageCache:=Result;
 /alex}
 { /tiglari }

 {tiglari: giving shaders a size.  a presumably
  horrible place to do it, but doesn't work when
  shaders are being loaded }
 if Result<>Nil then
 begin
   Size:=Result.GetSize;
   V[1]:=Size.X;
   V[2]:=Size.Y;
   SetFloatsSpec('Size',V);
 end
 else
   Raise EErrorFmt(5695, [self.name]);
 {/tiglari}
end;

(*procedure QShader.DataUpdate;
var
 Image: QPixelSet;
begin
  { we only want to set the correct size of this shader,
    based on the first valid stage size }
 Image:=DefaultImage;
 if Image<>Nil then
  try
   Image.Acces;
   SetSize(Image.GetSize);
  except
   {nothing}
  end;
end;*)

function QShader.DumpString : String;
var
 I, K: Integer;
 Q: QObject;

  procedure DumpSpec(const Spec, Indent: String);
  var
   J: Integer;
  begin
   J:=Pos('=', Spec);
     { ignore specifics that cannot be written as text }
   if (J>0) and not IsFloatSpec(Spec) then //FIXME: and not IsIntSpec(Spec)
    Result:=Result + Indent + Copy(Spec,1,J-1) + TrimRight(' ' + Copy(Spec,J+1,MaxInt)) + #13#10;
      { dump the specific as a shader or stage attribute }
  end;

begin
 Result:=Name + #13#10'{'#13#10;
 Acces;
 for I:=0 to Specifics.Count-1 do  { attributes }
  DumpSpec(Specifics[I], chr(vk_Tab));
 for K:=0 to SubElements.Count-1 do  { stages }
  begin
   Q:=SubElements[K];
   Q.Acces;
    { stage intro }
   Result:=Result + chr(vk_Tab) + '{'#13#10;
(* DECKER
   { include 'map' attribute from the name of the stage }
   if Q.Name <> LoadStr1(5699) then
    Result:=Result + chr(vk_Tab) + chr(vk_Tab) + 'map ' + Q.Name + #13#10;
*)
   for I:=0 to Q.Specifics.Count-1 do  { stage attributes }
    DumpSpec(Q.Specifics[I], chr(vk_Tab) + chr(vk_Tab));
    { stage end }
   Result:=Result + chr(vk_Tab) + '}'#13#10;
  end;
 { shader end }
 Result:=Result + '}'#13#10#13#10;
end;

(*procedure QShader.OperationInScene(Aj: TAjScene; PosRel: Integer);
begin
 inherited;
 if Aj=asModifie then
  DataUpdate;
end;*)

function QShader.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[Q is QShaderStage];
end;

function QShader.GetSize : TPoint;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5534, ['Size']);
 Image.Acces;
 Result:=Image.GetSize;
end;

function QShader.Description : TPixelSetDescription;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5695, [Name]);
 Result:=Image.Description;
end;

procedure QShader.SetSize;
begin
 Raise EError(5696);
end;

function QShader.SetDescription;
begin
 Raise EError(5696);
end;

procedure QShader.ListDependencies(L: TStringList);
var
 I: Integer;
 S, SpecialStage: String;
begin
  Acces;
  SpecialStage:=LoadStr1(5699);
  for I:=0 to SubElements.Count-1 do
  begin
    S:=SubElements[I].Name;
    { to do: check for animated stages }
    if (S<>'') and (S[1]<>'$') and (S<>SpecialStage) then
      L.Add(#255+S);   { #255 means it is not a texture name but directly a file name }
  end;
end;

 {------------------------}

class function QShaderStage.TypeInfo;
begin
 Result:=':shstg';
end;

function QShaderStage.ContainsImageReference : Boolean;
begin
 if (Name='') or (Name[1]='$') then
  Result:=False
 else
  Result:=True;
end;

function QShaderStage.ProvidesSomeImage : QPixelSet;
begin
 Result:=Nil;
 if ContainsImageReference then
 begin
  if Name=LoadStr1(5699) then   { complex stage }
   Result:=Nil   { to do: check for animated stages }
  else
   Result:=NeedGameFile(Name, '') as QPixelSet;
 end;
end;

function QShaderStage.LoadPixelSet : QPixelSet;
begin
 Result:=ProvidesSomeImage;
 if Result=Nil then
  Raise EErrorFmt(5697, [Name]);
 Result.Acces;
end;

function QShaderStage.Description : TPixelSetDescription;
begin
 Result:=LoadPixelSet.Description;
end;

function QShaderStage.SetDescription(const PSD: TPixelSetDescription;
                                     Confirm: TSDConfirm) : Boolean;
begin
 Raise EError(5696);
end;

 {------------------------}

class function QShaderFile.TypeInfo;
begin
 Result:='.shader';
end;

class procedure QShaderFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5175);
 Info.FileExt{Count}:=804;
end;

function QShaderFile.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 if Q is QShader then
  Result:=ieResult[True] + [ieListView]
 else
  Result:=ieResult[False];
end;

procedure QShaderFile.LoadFile(F: TStream; FSize: Integer);
const
 ProgressStep = 4096;
var
 ComplexStage, Data: String;
 Source, NextStep: PChar;
 Shader: QShader;
 Stage: QShaderStage;
 I, LineNumber: Integer;
 SectionComment, Comment: Boolean;
 V: array[1..2] of Single;
 masked: boolean; { Mohaa (tiglari): I'm guessing that this means that some
                    of the surfaceparms should be loaded into the editor as
                    flags.  'surfaceparm' appears an various other contexts
                    where it doesn't seem to set checks in the Mohradiant
                    surf inspector }
 EditableSurfaceParms : boolean;
 NestDepth: Integer;

  procedure SyntaxError;
  begin
   Raise EErrorFmt(5694, [Self.GetFullName(), LineNumber]);
  end;

  procedure SkipSpaces;
  begin
   repeat
    while Source^ in [' ', Chr(vk_Tab)] do
     Inc(Source);
    if Source^=#13 then
     begin
      Inc(LineNumber);
      Inc(Source);
      if Source^=#10 then Inc(source);
     end
    else
     if Source^=#10 then
      begin
       Inc(LineNumber);
       Inc(Source);
      end
     else
      Break;
   until False;
  end;

  function ReadLine : String;
  var
   P1, P2: PChar;
  begin
   P1:=Source;
   while not (Source^ in [#13, #10, #0]) do
    Inc(Source);
   P2:=Source;
   // if the string ends with sp or tab, remove them
   while (P2>P1) and (P2[-1] in [' ', Chr(vk_Tab)]) do
    Dec(P2);
   SetString(Result, P1, P2-P1);
  end;

  procedure ReadAttribute(Target: QObject);
  var
   P1: PChar;
   Spec: String;
  begin
   P1:=Source;
   while not (Source^ in [' ', Chr(vk_Tab), #13, #10, #0]) do
    Inc(Source);
   SetString(Spec, P1, Source-P1);
   while Source^ in [' ', Chr(vk_Tab)] do
    Inc(Source);

    { decker:
      FIXME: we insert the attribute directly into the object's specifics/args list.
      It will create duplicated specifics and specifics with no corresponding argument.
      In any of these two situations, code that edit the object might mess things up.
      TO DO: when shaders editing is implemented, ensure all the way that we can edit
      a "raw" specifics/args list, without disturbing the order of the specifics,
      without removing empty ones, and supporting duplicated specifics.
    }

   {tiglari: this solution may seem horrible, but it involves less special-game
    coding and attendant mess than the others I can come up with.  It is a
    basic design defect of QuArK that the face-specifics used by QuArK
    specifically (v, tv etc) aren't systematically separated from the ones
    associated with particular games, so here we're using an '_esp_' prefix
    to achieve this effect.  Requires some corresponding footwork in mapmgr.py
    and mapmenus.py. }

   if EditableSurfaceParms then
 //   if CharModeJeu=mjMohaa then
   begin
     if (Spec='qer_keyword') and (ReadLine='masked')then
       masked:=true
     else
     if (masked and (Spec='surfaceparm')) then
       Target.Specifics.Add('_esp_'+Readline+'='+'1');
   end;
   Target.Specifics.Add(Spec+'='+ReadLine);
  end;

begin
 EditableSurfaceParms:=SetupGameSet.Specifics.Values['EditableSurfaceParms']<>'';

 case ReadFormat of
  rf_Default: begin  { as stand-alone file }
      ProgressIndicatorStart(5453, FSize div ProgressStep); try
      SetLength(Data, FSize);
      Source:=PChar(Data);
      F.ReadBuffer(Source^, FSize);  { read the whole file at once }

       { preprocess comments }
      Comment:=False;
      SectionComment:=False;
      for I:=0 to FSize-2 do
      begin
        if (Source[I]='/') and (Source[I+1]='/') then
          begin
           Comment:=True;
           {Rowdy: handle comments starting with //*****... by replacing
                   both // in the start of comment marker with spaces,
                   so we do not see /* as the next character sequence}
           Source[I] := ' ';
           Source[I+1] := ' ';
           {/Rowdy}
         end
        else if (Source[I]='/') and (Source[I+1]='*') then
          SectionComment := TRUE
        else if (Source[I]='*') and (Source[I+1]='/') then
        begin
          SectionComment := FALSE;
          Source[I] := ' ';
          Source[I+1] := ' ';
        end
        else begin
          if Source[I] in [#13,#10]
          then Comment:=False;
        end;

        if (Comment or SectionComment) then
          Source[I]:=' ';
      end;

      NextStep:=Source+ProgressStep;
      LineNumber:=1;
      ComplexStage:=LoadStr1(5699);
      repeat
        { read one shader definition per loop }
       masked:= false;   // mohaa
       SkipSpaces;
       if Source^=#0 then Break;    { end of file }
       Shader:=QShader.Create(ReadLine, Self);    { new shader object }
       SubElements.Add(Shader);
       SkipSpaces;
       if Source^=#0 then Break;    { end of file }
       if Source^<>'{' then SyntaxError;
       Inc(Source);
       repeat
         { read one shader attribute or stage per loop }
        SkipSpaces;
        if Source^='}' then Break;   { end of shader }
        if Source^='{' then
         begin   { shader stage }
          Inc(Source);
          Stage:=QShaderStage.Create(ComplexStage, Shader);
          Shader.SubElements.Add(Stage);
          repeat
            { read one stage attribute per loop }
           SkipSpaces;
           if Source^='}' then Break;   { end of stage }
           if Source^='{' then
            begin   { stage data. For example: texEnvCombine, nvRegCombiners }
             //FIXME: Currently cannot handle this; skip it!
             NestDepth:=1;
             repeat
              Inc(Source);
              if Source^='{' then
               Inc(NestDepth);
              if Source^='}' then
               Dec(NestDepth);
             until NestDepth=0;
             Inc(Source);   { skip the closing brace }
            end
           else
            ReadAttribute(Stage);
          until False;
          Inc(Source);   { skip the closing brace }

          { remove the 'map' attribute and use it to set the name of the stage }
          if Stage.Specifics.Values['map']<>'' then
          begin
           Stage.Name:=Stage.Specifics.Values['map'];
(* DECKER
           Stage.Specifics.Values['map']:='';
*)
          end
          else
          {DECKER - try 'clampmap' instead }
          if Stage.Specifics.Values['clampmap']<>'' then
           Stage.Name:=Stage.Specifics.Values['clampmap']
          else
          {DECKER - try 'animmap' instead }
          if Stage.Specifics.Values['animmap']<>'' then
          begin
           Stage.Name:=Stage.Specifics.Values['animmap'];
           { jump over the number and take the first filename in the 'animmap' list }
           Stage.Name:=Copy(Stage.Name, Pos(' ', Stage.Name)+1, 999);
           SetLength(Stage.Name, Pos(' ', Stage.Name)-1);
          end;
         end
        else   { shader attribute }
         ReadAttribute(Shader);
       until False;
       Inc(Source);   { skip the closing brace }
       { Shader.DataUpdate;   { shader ready }
       { tiglari:  tried to give it a real
         size here but failed.  Now in DefaultImage }

        V[1]:=128;
        V[2]:=128;
        SetFloatsSpec('Size',V);
        {/tiglari}

        { progress bar stuff }
       while Source>=NextStep do
        begin
         ProgressIndicatorIncrement;
         Inc(NextStep, ProgressStep);
        end;
      until False;
      finally ProgressIndicatorStop; end;
     end;
 else inherited;
 end;
end;

procedure QShaderFile.SaveFile(Info: TInfoEnreg1);
var
 I: Integer;
 Q: QObject;
 Data: String;
begin
 with Info do case Format of
  rf_Default: begin  { as stand-alone file }
      for I:=0 to SubElements.Count-1 do
       begin
        Q:=SubElements[I];
        if Q is QShader then
         begin
           { write this shader definition into the string Data }
          Data:=QShader(Q).DumpString;
           { dump Data to the stream }
          F.WriteBuffer(PChar(Data)^, Length(Data));
         end;
       end;
     end;
 else inherited;
 end;
end;

 {------------------------}

procedure QBsp3FileHandler.LoadBsp(F: TStream; StreamSize: Integer);
var
 Header: TBsp3Header;
 Origine: LongInt;
 Q: QObject;
 I: Integer;
begin
  if StreamSize < SizeOf(Header) then
    Raise EError(5519);

  Origine:=F.Position;
  F.ReadBuffer(Header, SizeOf(Header));

  for I:=0 to HEADER_LUMPS-1 do
  begin
    if Header.Entries[I].EntrySize < 0 then
      Raise EErrorFmt(5509, [84]);

    if Header.Entries[I].EntrySize = 0 then
      Header.Entries[I].EntryPosition := SizeOf(Header)
    else
    begin
      if Header.Entries[I].EntryPosition < SizeOf(Header) then
        Raise EErrorFmt(5509, [85]);

      if Header.Entries[I].EntryPosition+Header.Entries[I].EntrySize > StreamSize then
      begin
        Header.Entries[I].EntrySize := StreamSize - Header.Entries[I].EntryPosition;
        GlobalWarning(LoadStr1(5641));
      end;
    end;

    F.Position:=Origine + Header.Entries[I].EntryPosition;
    Q:=OpenFileObjectData(F, Bsp3EntryNames[I], Header.Entries[I].EntrySize, FBsp);
    FBsp.SubElements.Add(Q);
    LoadedItem(rf_Default, F, Q, Header.Entries[I].EntrySize);
  end;
end;

procedure QBsp3FileHandler.SaveBsp(Info: TInfoEnreg1);
var
  Header: TBsp3Header;
  Origine, Fin: LongInt;
  Zero: Integer;
  Q: QObject;
  I: Integer;
begin
  ProgressIndicatorStart(5450, HEADER_LUMPS);
  try
    Origine := Info.F.Position;
    Info.F.WriteBuffer(Header, SizeOf(Header));  { updated later }

    { write .bsp entries }
    for I:=0 to HEADER_LUMPS-1 do
    begin
      Q := FBsp.BspEntry[I];
      Header.Entries[I].EntryPosition := Info.F.Position;

      Q.SaveFile1(Info);   { save in non-QuArK file format }

      Header.Entries[I].EntrySize := Info.F.Position - Header.Entries[I].EntryPosition;
      Dec(Header.Entries[I].EntryPosition, Origine);

      Zero:=0;
      Info.F.WriteBuffer(Zero, (-Header.Entries[I].EntrySize) and 3);  { align to 4 bytes }

      ProgressIndicatorIncrement;
    end;

    { update header }
    Fin := Info.F.Position;

    Info.F.Position := Origine;
    Header.Signature := cSignatureBspQ2;
    Header.Version := cVersionBspQ3;
    Info.F.WriteBuffer(Header, SizeOf(Header));

    Info.F.Position := Fin;
  finally
    ProgressIndicatorStop;
  end;
end;

function QBsp3FileHandler.GetEntryName(const EntryIndex: Integer) : String;
begin
  if (EntryIndex<0) or (EntryIndex>=HEADER_LUMPS) then
    raise InternalE('Tried to retrieve name of invalid BSP lump!');
  Result:=Bsp3EntryNames[EntryIndex];
end;

function QBsp3FileHandler.GetLumpEdges: Integer;
begin
  Result:=-1;
end;

function QBsp3FileHandler.GetLumpEntities: Integer;
begin
  Result:=LUMP_ENTITIES;
end;

function QBsp3FileHandler.GetLumpFaces: Integer;
begin
  Result:=LUMP_SURFACES;
end;

function QBsp3FileHandler.GetLumpLeafs: Integer;
begin
  Result:=LUMP_LEAFS;
end;

function QBsp3FileHandler.GetLumpLeafFaces: Integer;
begin
  Result:=LUMP_LEAFSURFACES;
end;

function QBsp3FileHandler.GetLumpModels: Integer;
begin
  Result:=LUMP_MODELS;
end;

function QBsp3FileHandler.GetLumpNodes: Integer;
begin
  Result:=LUMP_NODES;
end;

function QBsp3FileHandler.GetLumpPlanes: Integer;
begin
  Result:=LUMP_PLANES;
end;

function QBsp3FileHandler.GetLumpSurfEdges: Integer;
begin
  Result:=-1;
end;

function QBsp3FileHandler.GetLumpTexInfo: Integer;
begin
  Result:=LUMP_SHADERS;
end;

function QBsp3FileHandler.GetLumpTextures: Integer;
begin
  Result:=-1;
end;

function QBsp3FileHandler.GetLumpVertexes: Integer;
begin
  Result:=LUMP_DRAWVERTS;
end;

 {------------------------}

initialization
  RegisterQObject(Q3Pak, 's');
  RegisterQObject(Q_CFile, 's');
  RegisterQObject(Q_HFile, 's');
  RegisterQObject(QShader, 'a');
  RegisterQObject(QShaderStage, 'a');
  RegisterQObject(QShaderFile, 'p');

  RegisterQObject(QBsp3,  '!');
  RegisterQObject(QBsp3a, 'a');
end.

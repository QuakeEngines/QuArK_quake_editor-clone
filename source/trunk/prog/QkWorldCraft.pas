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
unit QkWorldCraft;

interface

uses
  Windows, SysUtils, Classes, QkFileObjects, QkObjects, QkMap;

type
 QRmfMapFile = class(QMapFile)
        protected
          procedure LoadFile(F: TStream; FSize: Integer); override;
          procedure SaveFile(Info: TInfoEnreg1); override;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

implementation

uses QuarkX, QkObjectClassList, QkExceptions, Logging, Setup,
  QkMapObjects, QkMapPoly, MapError, qmath {//FIXME};

type
  RMFVisGroup = packed record //@ UNTESTED
    m_szName: array[0..127] of Char;
    m_rgbColor: array[0..3] of Byte;
    m_dwID: DWORD;
    m_bVisible: Bool;
  end;

  RMFTexture_21 = packed record //@ UNTESTED
    texture: array[0..MAX_PATH-1] of Byte;
    rotate: Single;
    shift: array[0..1] of Single;
    scale: array[0..1] of Single;
    smooth: Byte;
    material: Byte;
    q2surface: DWORD;
    q2contents: DWORD;
    q2value: DWORD;
  end;

  RMFTexture_33 = packed record //@ UNTESTED
    texture: array[0..MAX_PATH-1] of Byte; 
    UAxis: array[0..3] of Single;
    VAxis: array[0..3] of Single;
    rotate: Single;
    scale: array[0..1] of Single;
    smooth: Byte;
    PADDING1: Byte;
    material: Byte;
    PADDING2: Byte;
    q2surface: DWORD;
    q2contents: DWORD;
    nLightmapScale: Integer;
  end;

//@: See http://www.inxbus.net/hldoc/d6/d51/loadsave__rmf_8cpp-source.html
const
//VERSION:  $CD, $CC, $0C, $40,
  RMFVersion: Single = 3.7;
  RMFLastCompatVersion: Single = 0.3;
  RmfID: array[0..2] of Byte = (Byte('R'), Byte('M'), Byte('F'));
  old_group_bytes: Integer = 134;

{------------------------}

class function QRmfMapFile.TypeInfo;
begin
 Result:='.rmf';
end;

class procedure QRmfMapFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5150);
 Info.FileExt:=827;
end;

procedure QRmfMapFile.LoadFile(F: TStream; FSize: Integer);

  function ReadRMFString(var F: TStream; FSize: Integer) : String;
  var
    StringLength: Byte;
  begin
    FillChar(StringLength, SizeOf(Byte), 0);
    F.ReadBuffer(StringLength, 1); //1 = SizeOf(Byte)
    if StringLength > 128 then
      Log(LOG_INFO, LoadStr1(5787));
    SetLength(Result, StringLength);
    if StringLength>0 then
    begin
      F.ReadBuffer(Result[1], StringLength);
      while Result[Length(Result)] = #0 do
      begin
        SetLength(Result, Length(Result)-1);
        if Length(Result) = 0 then
          Exit;
      end;
    end;
  end;

var
 Racine: TTreeMapBrush;
 Entite, EntitePoly: TTreeMapSpec;
 Entities, MapStructure: TTreeMapGroup;
 ModeJeu: Char;

 RMFVersion: Single;
 RMFHeader: array[0..2] of Byte;
 VisGroup: RMFVisGroup;
 DummyByte: Byte;
 DummyInteger: Integer;
 DummySingle: Single;
 DummyString: String;
 DummyVector: array[0..2] of Single;
 DummyBOOL: BOOL;
 DummyWORD: WORD;
 DummyDWORD: DWORD;
 i: Integer;

 procedure LoadMapClass; forward;
 procedure LoadEditGameClass; forward;

 procedure LoadMapEntity;
 begin
   //FIXME
   //if (SymbolType<>sCurlyBracketLeft) and (HullNum=-1) then
   //  Entite:=TTreeMapEntity.Create(Classname, Entities)
   //else
     Entite:=TTreeMapBrush.Create('CMapEntity', Entities); //FIXME
   Entities.SubElements.Add(Entite);
   EntitePoly:=Entite;

   LoadMapClass;
   LoadEditGameClass;

   F.ReadBuffer(DummyWORD, SizeOf(WORD)); //flags
   F.ReadBuffer(DummyVector[0], 3*SizeOf(Single)); //origin

   if RMFVersion < 0.5 then
     DummyVector[2]:=-DummyVector[2];

   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //complex (unused)
 end;

 procedure LoadMapGroup;
 begin
   LoadMapClass;
 end;

 procedure LoadMapHelper;
 begin
   //@
 end;

 procedure LoadMapDisp;
 begin
   //@
 end;

 procedure LoadMapFace(P: TPolyhedron);
 var
   OldTex: RMFTexture_21;
   OldTex33: RMFTexture_33;
   LoadPoints: array[0..255, 0..2] of Single;
   LoadPlanePoints: array[0..2, 0..2] of Single;
   i: Integer;
   Surface: TFace;
 begin
   Surface:=TFace.Create(LoadStr1(139), P);
   P.SubElements.Add(Surface);

   FillChar(OldTex, SizeOf(OldTex), 0);
   FillChar(OldTex33, SizeOf(OldTex33), 0);
   FillChar(LoadPoints, SizeOf(LoadPoints), 0);
   FillChar(LoadPlanePoints, SizeOf(LoadPlanePoints), 0);
   if RMFVersion < 0.9 then
   begin
     // Read the name
     F.ReadBuffer(OldTex.texture[0], 16);

     // Ensure name is ASCIIZ
     OldTex.texture[16]:=0;

     // Read the rest - skip the name
     F.ReadBuffer(OldTex.rotate, SizeOf(OldTex.rotate) + SizeOf(OldTex.shift) + SizeOf(OldTex.scale));

     Surface.NomTex:=CharToPas(OldTex.texture);
   end
   else if RMFVersion < 1.2 then
   begin
     // Didn't have smooth/material groups
     F.ReadBuffer(OldTex.texture[0], 40);
     F.ReadBuffer(OldTex.texture[0], SizeOf(OldTex.texture) - (MAX_PATH) + SizeOf(OldTex.rotate) + SizeOf(OldTex.shift) + SizeOf(OldTex.scale));

     Surface.NomTex:=CharToPas(OldTex.texture);
   end
   else if RMFVersion < 1.7 then
   begin
     // No quake2 fields yet and smaller texture size.
     F.ReadBuffer(OldTex.texture[0], 40);
     F.ReadBuffer(OldTex.rotate, SizeOf(OldTex) - 3*SizeOf(Integer) - MAX_PATH);

     Surface.NomTex:=CharToPas(OldTex.texture);
   end
   else if RMFVersion < 1.8 then
   begin
     // Texture name field changed from 40 to MAX_PATH in size.
     F.ReadBuffer(OldTex.texture[0], 40);
     F.ReadBuffer(OldTex.rotate, SizeOf(OldTex) - MAX_PATH);

     Surface.NomTex:=CharToPas(OldTex.texture);
   end
   else if RMFVersion < 2.2 then
   begin
     F.ReadBuffer(OldTex.texture[0], SizeOf(OldTex));

     Surface.NomTex:=CharToPas(OldTex.texture);
   end
   else
   begin
     //
     // After 3.3 the alignment of vec4_t's changed. We never save the new format,
     // since RMF is no longer being revved.
     //
     F.ReadBuffer(OldTex33.texture[0], SizeOf(OldTex33));
     //@

     Surface.NomTex:=CharToPas(OldTex33.texture);
   end;

   if RMFVersion < 0.6 then
   begin
     F.ReadBuffer(DummySingle, SizeOf(DummySingle)); //light
     //@
   end;
   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //size
   if DummyInteger>256 then
     Raise EErrorFmt(5779, [LoadStr1(5783)]);
   F.ReadBuffer(LoadPoints[0][0], DummyInteger*3*SizeOf(Single));
   if RMFVersion < 2.2 then
     for i := 0 to DummyInteger-1 do
       LoadPoints[i][2]:=-LoadPoints[i][2];
   Surface.SetThreePoints(MakeVect(LoadPoints[0][0], LoadPoints[0][1], LoadPoints[0][2]), MakeVect(LoadPoints[2][0], LoadPoints[2][1], LoadPoints[2][2]), MakeVect(LoadPoints[1][0], LoadPoints[1][1], LoadPoints[1][2]));  //FIXME
   if not Surface.LoadData then
     raise InternalE('LoadData failure');   //FIXME: ERROR!

   if RMFVersion >= 0.7 then
   begin
     F.ReadBuffer(LoadPlanePoints[0][0], SizeOf(LoadPlanePoints));
     //@
   end;

   if (RMFVersion >= 3.4) and (RMFVersion <= 3.6) then
   begin
     if RMFVersion >= 3.5 then
     begin
       F.ReadBuffer(DummyInteger, SizeOf(Integer)); //nLoadHasMapDisp;
       DummyBool:=(DummyInteger<>0);
     end
     else
       F.ReadBuffer(DummyBOOL, SizeOf(DummyBOOL)); //bHasMapDisp;

     if DummyBool then
       LoadMapDisp;
   end;
   //@
 end;

 procedure LoadMapSolid;
 var
   i: Integer;
   P: TPolyhedron;
 begin
   LoadMapClass;

   P:=TPolyhedron.Create(LoadStr1(138), EntitePoly);
   EntitePoly.SubElements.Add(P);

   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Number of faces
   for i := 0 to DummyInteger-1 do
     LoadMapFace(P);
     //@
 end;

 procedure LoadMapClass;
 var
   i: Integer;
 begin
   if RMFVersion < 1.0 then
   begin
     // kill group information .. unfortunate
     F.ReadBuffer(DummyInteger, SizeOf(Integer));
     F.Seek(DummyInteger, 1);
   end
   else
   begin
     F.ReadBuffer(DummyDWORD, SizeOf(DWORD)); //ID
     //@
   end;

   //Object color
   F.ReadBuffer(DummyByte, SizeOf(Byte));
   F.ReadBuffer(DummyByte, SizeOf(Byte));
   F.ReadBuffer(DummyByte, SizeOf(Byte));

   //Load children
   F.ReadBuffer(DummyInteger, SizeOf(Integer));
   for i := 0 to DummyInteger-1 do
   begin
     DummyString:=ReadRMFString(F, FSize); //Name of pChild
     if DummyString = 'CMapEntity' then
       LoadMapEntity
     else if DummyString = 'CMapGroup' then
       LoadMapGroup
     else if DummyString = 'CMapHelper' then //used?
       LoadMapHelper
     else if DummyString = 'CMapSolid' then
       LoadMapSolid
     else
       Raise EErrorFmt(5779, [FmtLoadStr1(5784, [DummyString])]);
   end;
 end;

 procedure LoadKeyValue;
 begin
   DummyString:=ReadRMFString(F, FSize); //Key
   DummyString:=ReadRMFString(F, FSize); //Value
   //@
 end;

 procedure LoadEditGameClass;
 var
   i: Integer;
 begin
   DummyString:=ReadRMFString(F, FSize);
   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Angle
   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Spawnflags
   if DummyString[1] = #0 then
     Log(LOG_INFO, 'RMF: Invalid EditGameClass name.'); //@
   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Size
   for i := 0 to DummyInteger-1 do
   begin
     LoadKeyValue;
     //@
   end;
   //@ OLD angle...?
   if RMFVersion >= 1.5 then
   begin
     //Dummy timeline information (unused)
     F.ReadBuffer(DummyBOOL, SizeOf(DummyBOOL));
     F.ReadBuffer(DummyInteger, SizeOf(DummyInteger));
     F.ReadBuffer(DummyInteger, SizeOf(DummyInteger));
   end;
 end;

 procedure LoadMapPath;
 var
   DummyBuffer: array[0..127] of Char;
   DummyInteger2: Integer;
   i, k: Integer;
 begin
   F.ReadBuffer(DummyBuffer, 128); //128 = SizeOf(DummyBuffer) //Name
   F.ReadBuffer(DummyBuffer, 128); //128 = SizeOf(DummyBuffer) //Class
   F.ReadBuffer(DummyInteger, SizeOf(DummyInteger)); //Direction

   F.ReadBuffer(DummyInteger, SizeOf(DummyInteger)); //Number of nodes
   for i := 0 to DummyInteger-1 do
   begin
     F.ReadBuffer(DummyVector[0], 3*SizeOf(Single)); //position
     F.ReadBuffer(DummyDWORD, SizeOf(DWORD)); //ID
     if RMFVersion >= 1.6 then
     begin
       F.ReadBuffer(DummyBuffer, 128); //128 = SizeOf(DummyBuffer) //Name
       F.ReadBuffer(DummyInteger2, SizeOf(DummyInteger2)); //size
       for k := 0 to DummyInteger2-1 do
         LoadKeyValue;
     end;
   end;
 end;

begin
 case ReadFormat of
  rf_Default: begin  { as stand-alone file }
      Racine:=TTreeMapBrush.Create('', Self);
      Racine.AddRef(+1);
      try
        ModeJeu:=mjHalfLife;
        g_MapError.Clear;

        Entities:=TTreeMapGroup.Create(LoadStr1(136), Racine);
        Racine.SubElements.Add(Entities);
        MapStructure:=TTreeMapGroup.Create(LoadStr1(137), Racine);
        Racine.SubElements.Add(MapStructure);

        //LoadMapWorld:
        if FSize<SizeOf(RMFVersion) then
          Raise EError(5519);
        F.ReadBuffer(RMFVersion, SizeOf(RMFVersion));
        if (RMFVersion < RMFLastCompatVersion) or (RMFVersion > RMFVersion) then
          Raise EErrorFmt(5779, [LoadStr1(5780)]);
        if RMFVersion >= 0.8 then
        begin
          F.ReadBuffer(RMFHeader, SizeOf(RMFHeader));
          if not CompareMem(@RMFHeader, @RmfID, SizeOf(RmfID)) then
            Raise EErrorFmt(5779, [LoadStr1(5781)]);
        end;

        // load groups
        if RMFVersion >= 1.0 then
        begin
          F.ReadBuffer(DummyInteger, SizeOf(Integer));
          for i := 0 to DummyInteger-1 do
          begin
            F.ReadBuffer(VisGroup, SizeOf(VisGroup));
            //@ Read in "visgroups"
          end;
        end;

        // make sure it's a CMapWorld
        DummyString:=ReadRMFString(F, FSize);
        if DummyString<>'CMapWorld' then
          Raise EErrorFmt(5779, [FmtLoadStr1(5782, [DummyString, 'CMapWorld'])]);
        Racine.Name:=DummyString;
        Entite:=Racine;
        EntitePoly:=MapStructure;

        // load children & local data
        LoadMapClass;

        // load ceditgameclass & CMapClass
        LoadEditGameClass;

        if RMFVersion < 1.0 then
        begin
          //Not supported at all; skip this data
          F.ReadBuffer(DummyInteger, SizeOf(Integer));
          F.Seek(old_group_bytes * DummyInteger, 1);
        end;

        // load paths
        if RMFVersion >= 1.1 then
        begin
          F.ReadBuffer(DummyInteger, SizeOf(Integer));
          for i := 0 to DummyInteger-1 do
            LoadMapPath;
        end;

        // read camera
        if RMFVersion < 1.4 then
        begin
          F.ReadBuffer(DummyVector[0], 3*SizeOf(Single)); //unused
          F.ReadBuffer(DummyVector[0], 3*SizeOf(Single)); //unused
        end;

        SubElements.Add(Racine);
        Specifics.Values['Root']:=Racine.Name+Racine.TypeInfo;
        ObjectGameCode:=ModeJeu;
      finally
        Racine.AddRef(-1);
      end;

      //@     Racine.FixupAllReferences;?

     end;
 else
  inherited;
 end;
end;

procedure QRmfMapFile.SaveFile(Info: TInfoEnreg1);
begin
 with Info do case Format of
  rf_Default: begin  { as stand-alone file }
      raise EQObjectSavingNotSupported.Create('Saving RMF files is currently not supported.');
     end;
 else
  inherited;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(QRmfMapFile, 'x');
end.


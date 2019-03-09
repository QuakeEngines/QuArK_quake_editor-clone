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

const //@
  RMFMaxVersion: Single = 3.7;
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

  function ReadRMFString : String;
  var
    StringLength: Byte;
  begin
    F.ReadBuffer(StringLength, SizeOf(Byte));
    if StringLength >= 128 then
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
 ModeJeu: Char;

 BrushNum, FaceNum: Integer;

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

 procedure LoadMapClass(var obj : TTreeMap); forward;
 procedure LoadEditGameClass(var obj : TTreeMap); forward;

 procedure LoadMapEntity(parent: TTreeMap);
 var
   Entite: TTreeMapSpec;
   Origin: array[0..2] of Single;
   OldPos: Integer;
   IsBrush: Boolean;
 begin
   //Ugly hack; we need to know if there's children for this entity
   OldPos:=F.Position;
   if RMFVersion < 1.0 then
   begin
     F.ReadBuffer(DummyInteger, SizeOf(Integer));
     F.Seek(DummyInteger + 3 * SizeOf(Byte), soFromCurrent);
   end
   else
     F.Seek(SizeOf(DWORD) + 3 * SizeOf(Byte), soFromCurrent);
   F.ReadBuffer(DummyInteger, SizeOf(Integer));
   F.Seek(OldPos, soFromBeginning);
   IsBrush:=(DummyInteger<>0);
   if IsBrush then
     Entite:=TTreeMapBrush.Create('CMapEntity', parent) //Temp name
   else
     Entite:=TTreeMapEntity.Create('CMapEntity', parent); //Temp name
   parent.SubElements.Add(Entite);

   LoadMapClass(TTreeMap(Entite));
   LoadEditGameClass(TTreeMap(Entite));

   F.ReadBuffer(DummyWORD, SizeOf(WORD)); //flags
   F.ReadBuffer(Origin[0], 3*SizeOf(Single));
   if not IsBrush then
     TTreeMapEntity(Entite).Origin:=MakeVect(Origin[0], Origin[1], Origin[2]);

   if RMFVersion < 0.5 then
     DummyVector[2]:=-DummyVector[2];

   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //complex (unused)
 end;

 procedure LoadMapGroup(parent: TTreeMap);
 var
   Entite: TTreeMapSpec;
 begin
   Entite:=TTreeMapGroup.Create('func_group', parent); //Slightly naughty hardcoded name
   parent.SubElements.Add(Entite);

   LoadMapClass(TTreeMap(Entite));
 end;

 procedure LoadMapHelper(parent: TTreeMap);
 begin
   //@
 end;

 procedure LoadMapDisp();
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
   Params: TFaceParams;
   UAxis, VAxis : TVect;
   UShift, VShift: Double;
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
   end
   else if RMFVersion < 1.2 then
   begin
     // Didn't have smooth/material groups
     F.ReadBuffer(OldTex.texture[0], 40);
     F.ReadBuffer(OldTex.texture[0], SizeOf(OldTex.texture) - (MAX_PATH) + SizeOf(OldTex.rotate) + SizeOf(OldTex.shift) + SizeOf(OldTex.scale));
   end
   else if RMFVersion < 1.7 then
   begin
     // No quake2 fields yet and smaller texture size.
     F.ReadBuffer(OldTex.texture[0], 40);
     F.ReadBuffer(OldTex.rotate, SizeOf(OldTex) - 3*SizeOf(Integer) - MAX_PATH);
   end
   else if RMFVersion < 1.8 then
   begin
     // Texture name field changed from 40 to MAX_PATH in size.
     F.ReadBuffer(OldTex.texture[0], 40);
     F.ReadBuffer(OldTex.rotate, SizeOf(OldTex) - MAX_PATH);
   end
   else if RMFVersion < 2.2 then
   begin
     F.ReadBuffer(OldTex.texture[0], SizeOf(OldTex));
   end
   else
   begin
     //
     // After 3.3 the alignment of vec4_t's changed. We never save the new format,
     // since RMF is no longer being revved.
     //
     F.ReadBuffer(OldTex33.texture[0], SizeOf(OldTex33));
     //@
   end;

   if RMFVersion < 1.8 then
   begin
     OldTex.texture[40]:=0;
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

   // If reading from a pre-2.2 RMF file, copy the texture info from the old format.
   if RMFVersion < 2.2 then
   begin
     Surface.NomTex:=CharToPas(OldTex.texture);

      //FIXME: OldTex.smooth
      //FIXME: OldTex.material
      //FIXME: OldTex.q2surface
      //FIXME: OldTex.q2contents
     Params[1]:=OldTex.shift[0];
     Params[2]:=OldTex.shift[1];
     Params[3]:=OldTex.rotate;
     Params[4]:=OldTex.scale[0];
     Params[5]:=OldTex.scale[1];
     with Surface do
       SetFaceFromParams(Normale, Dist, Params);
   end
   else
   begin
     Surface.NomTex:=CharToPas(OldTex33.texture);

     //FIXME: OldTex33.smooth
     //FIXME: OldTex33.material
     //FIXME: OldTex33.q2surface @@@Deze kunnen al!
     //FIXME: OldTex33.q2contents
     //FIXME: OldTex33.nLightmapScale
     //FIXME if texture.nLightmapScale == 0 then Use Default Value from GameConfig

     UAxis.x := OldTex33.UAxis[0];
     UAxis.y := OldTex33.UAxis[1];
     UAxis.z := OldTex33.UAxis[2];
     UShift := OldTex33.UAxis[3];

     VAxis.x := OldTex33.VAxis[0];
     VAxis.y := OldTex33.VAxis[1];
     VAxis.z := OldTex33.VAxis[2];
     VShift := OldTex33.VAxis[3];

     Params[3]:=OldTex33.rotate;
     Params[4]:=OldTex33.scale[0];
     Params[5]:=OldTex33.scale[1];
     if not WC22Params(Surface, Params, UAxis, VAxis , UShift, VShift) then
       g_MapError.AddText('Problem with texture scale of face '+IntToStr(FaceNum)+ ' in brush '+IntToStr(BrushNum)); //FIXME: Move to dict!
   end;

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
       LoadMapDisp();
   end;
   //@
 end;

 procedure LoadMapSolid(parent: TTreeMap);
 var
   i: Integer;
   P: TPolyhedron;
 begin
   P:=TPolyhedron.Create(LoadStr1(138), parent);
   parent.SubElements.Add(P);

   LoadMapClass(TTreeMap(P));
   FaceNum:=0;

   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Number of faces
   for i := 0 to DummyInteger-1 do
   begin
     LoadMapFace(P);
     FaceNum:=FaceNum+1;
   end;
   //@
 end;

 procedure LoadMapClass(var obj : TTreeMap);
 var
   i: Integer;
 begin
   BrushNum:=0;
   if RMFVersion < 1.0 then
   begin
     // kill group information .. unfortunate
     F.ReadBuffer(DummyInteger, SizeOf(Integer));
     F.Seek(DummyInteger, soFromCurrent);
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
     DummyString:=ReadRMFString(); //Name of pChild
     if DummyString = 'CMapEntity' then
       LoadMapEntity(obj)
     else if DummyString = 'CMapGroup' then
       LoadMapGroup(obj)
     else if DummyString = 'CMapHelper' then //used?
       LoadMapHelper(obj)
     else if DummyString = 'CMapSolid' then
     begin
       LoadMapSolid(obj);
       BrushNum:=BrushNum+1;
     end
     else
       Raise EErrorFmt(5779, [FmtLoadStr1(5784, [DummyString])]);
   end;
 end;

 procedure LoadKeyValue(var obj : TTreeMap);
 begin
   DummyString:=ReadRMFString(); //Key
   DummyString:=ReadRMFString(); //Value
   //@
 end;

 procedure LoadEditGameClass(var obj : TTreeMap);
 var
   ClassName: String;
   i: Integer;
 begin
   ClassName:=ReadRMFString();
   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Angle
   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Spawnflags
   if ClassName[1] = #0 then
     Log(LOG_INFO, 'RMF: Invalid EditGameClass name.'); //@
   obj.Name:=ClassName;
   F.ReadBuffer(DummyInteger, SizeOf(Integer)); //Size
   for i := 0 to DummyInteger-1 do
   begin
     LoadKeyValue(obj);
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

 procedure LoadMapPath(parent : TTreeMap);
 var
   Entite: TTreeMapSpec;
   DummyBuffer: array[0..127] of Char;
   DummyInteger2: Integer;
   i, k: Integer;
 begin
   Entite:=TTreeMapEntity.Create('CMapPath', parent); //Temp name
   parent.SubElements.Add(Entite);

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
         LoadKeyValue(TTreeMap(Entite));
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

        //LoadMapWorld:
        if FSize<SizeOf(RMFVersion) then
          Raise EError(5519);
        F.ReadBuffer(RMFVersion, SizeOf(RMFVersion));
        if (RMFVersion < RMFLastCompatVersion) or (RMFVersion > RMFMaxVersion) then
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
        DummyString:=ReadRMFString();
        if DummyString<>'CMapWorld' then
          Raise EErrorFmt(5779, [FmtLoadStr1(5782, [DummyString, 'CMapWorld'])]);
        Racine.Name:=DummyString;

        // load children & local data
        LoadMapClass(TTreeMap(Racine));

        // load ceditgameclass & CMapClass
        LoadEditGameClass(TTreeMap(Racine));

        if RMFVersion < 1.0 then
        begin
          //Not supported at all; skip this data
          F.ReadBuffer(DummyInteger, SizeOf(Integer));
          F.Seek(old_group_bytes * DummyInteger, soFromCurrent);
        end;

        // load paths
        if RMFVersion >= 1.1 then
        begin
          F.ReadBuffer(DummyInteger, SizeOf(Integer));
          for i := 0 to DummyInteger-1 do
            LoadMapPath(TTreeMap(Racine));
        end;

        // read camera
        if RMFVersion < 1.4 then
        begin
          F.ReadBuffer(DummyVector[0], 3*SizeOf(Single)); //unused
          F.ReadBuffer(DummyVector[0], 3*SizeOf(Single)); //unused
        end;

        Racine.FixupAllReferences;

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

procedure QRmfMapFile.SaveFile(Info: TInfoEnreg1);
begin
 with Info do case Format of
  rf_Default: begin  { as stand-alone file }
      raise EQObjectSavingNotSupported.Create('Saving RMF files is currently not supported.'); //FIXME: Move to dict!
     end;
 else
  inherited;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(QRmfMapFile, 'x');
end.


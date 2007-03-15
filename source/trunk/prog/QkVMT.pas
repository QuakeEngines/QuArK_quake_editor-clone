(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor - Vtf texture loader
Copyright (C) Alexander Haarer

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

}

unit QkVMT;

interface
uses Windows, Classes, QkWad, QkPixelSet, QkObjects, QkFileObjects, QkVTFLib;

type
  QVMT = class(QPixelSet)
         protected
           DefaultImageCache : QPixelSet;
         public
           procedure SaveFile(Info: TInfoEnreg1); override;
           procedure LoadFile(F: TStream; FSize: Integer); override;
           class function TypeInfo: String; override;
           class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
           function DefaultImage : QPixelSet;
           function GetSize : TPoint; override;
           procedure SetSize(const nSize: TPoint); override;
           function Description : TPixelSetDescription; override;
           function SetDescription(const PSD: TPixelSetDescription;
                                   Confirm: TSDConfirm) : Boolean; override;
         end;

{-------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging, QkVTF, StrUtils;

var
  VMTLoaded: Boolean;

procedure Fatal(x:string);
begin
  Log(LOG_CRITICAL,'load vmt %s',[x]);
  Windows.MessageBox(0, pchar(X), 'Fatal Error', MB_TASKMODAL or MB_ICONERROR or MB_OK);
  Raise Exception.Create(x);
end;

class function QVMT.TypeInfo: String;
begin
 TypeInfo:='.vmt';
end;

class procedure QVMT.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5716);
  Info.FileExt:=815;
  Info.WndInfo:=[wiWindow];
end;

function QVMT.DefaultImage : QPixelSet;
var
 Image: QPixelSet;
 i:integer;
 noimage:boolean;
begin
  Result :=nil;
  Acces;
  noimage:=true;
  for I:=0 to SubElements.Count-1 do
  begin
    if SubElements[I] is qpixelset then
    begin
      image := SubElements[I] as qpixelset;
      Result := image;
      noimage:=false;
      break;
    end;
  end;
  if noimage then
    Raise EErrorFmt(5695, [Name])
end;

function QVMT.GetSize : TPoint;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5534, ['Size']);
 Image.Acces;
 Result:=Image.GetSize;
end;

function QVMT.Description : TPixelSetDescription;
var
 Image: QPixelSet;
begin
  image:=defaultimage;
  if assigned(image ) then
    Result := image.description
  else
    Raise EErrorFmt(5695, [Name]);
end;

procedure QVMT.SetSize;
begin
 Raise EError(5696);
end;

function QVMT.SetDescription;
begin
 Raise EError(5696);
end;

procedure QVMT.LoadFile(F: TStream; FSize: Integer);
var
  RawBuffer: String;
  VMTMaterial: Cardinal;
  VTFImage: QVTF;
  S, Path: String;
  I: integer;
  GameDir: String;
  SteamAppsDir: String;

  //NodeLevel: Cardinal;
  NodeType: VMTNodeType;
  NodeName: String;
  NodeValueString: String;
  NodeValueInteger: Cardinal;
  NodeValueSingle: Single;
begin
  Log(LOG_VERBOSE,'load vmt %s',[self.name]);;
  VTFImage:=nil;
  case ReadFormat of
    1: begin  { as stand-alone file }
      if (not VMTLoaded) or ReloadNeeded then
      begin
        if ReloadNeeded then
          VMTLoaded:=false;
        if not LoadVTFLib then
          Raise EErrorFmt(5718, [GetLastError]);
        VMTLoaded:=true;
      end;

      SetLength(RawBuffer, F.Size);
      F.Seek(0, 0);
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

      if vlCreateMaterial(@VMTMaterial)=false then
        Fatal('Unable to load VMT file. Call to vlCreateMaterial failed.');

      if vlBindMaterial(VMTMaterial)=false then
      begin
        vlDeleteMaterial(VMTMaterial);
        Fatal('Unable to load VMT file. Call to vlBindMaterial failed.');
      end;

      if vlMaterialLoadLump(Pointer(RawBuffer), Length(RawBuffer), false)=false then
      begin
        vlDeleteMaterial(VMTMaterial);
        Fatal('Unable to load VMT file. Call to vlMaterialLoadLump failed. Please make sure the file is a valid VMT file, and not damaged or corrupt.');
      end;

      if vlMaterialGetFirstNode=false then
      begin
        vlDeleteMaterial(VMTMaterial);
        Fatal('Unable to load VMT file. Call to vlMaterialGetFirstNode failed.');
      end;
      //NodeLevel:=0;

      repeat
        NodeName:=vlMaterialGetNodeName;
        NodeType:=vlMaterialGetNodeType;
        case NodeType of
        NODE_TYPE_GROUP:
          begin
            //NodeLevel:=NodeLevel+1;
            //...
          end;
        NODE_TYPE_GROUP_END:
          begin
            //NodeLevel:=NodeLevel-1;
            //...
          end;
        NODE_TYPE_STRING:
          begin
            NodeValueString:=vlMaterialGetNodeString;
            Specifics.Add(NodeName+'='+NodeValueString);
          end;
        NODE_TYPE_INTEGER:
          begin
            NodeValueInteger:=vlMaterialGetNodeInteger;
            Specifics.Add(NodeName+'='+IntToStr(NodeValueInteger));
          end;
        NODE_TYPE_SINGLE:
          begin
            NodeValueSingle:=vlMaterialGetNodeSingle;
            Specifics.Add(NodeName+'='+FloatToStr(NodeValueSingle));
          end;
        end;
      until vlMaterialGetNextNode=false;

      vlDeleteMaterial(VMTMaterial);

      path:='hl2/materials/';
      //DanielPharos: Not the way to go!

      GameDir:=GetGameDir;
      I:=pos('\',GameDir);
      if I>0 then
        SteamAppsDir:=LeftStr(GameDir, I)
      else
      begin
        I:=pos('/',GameDir);
        if I>0 then
          SteamAppsDir:=LeftStr(GameDir, I)
        else
          SteamAppsDir:=GameDir+'\';
      end;

      S:=Specifics.Values['%tooltexture'];
      if (VTFImage=nil) and (s<>'') then
        try
          Log(LOG_VERBOSE,'attempting to load $basetexture '+S);
//          if (self.Protocol<>'') then
//            VTFImage:=NeedGameFileBase(SteamAppsDir+p.name+'.gcf', path + '/' + s + '.vtf') as QVTF
//          else
//            VTFImage:=NeedGameFileBase(GetGameDir, path + '/' + s + '.vtf') as QVTF;
            VTFImage:=NeedGameFileBase(GetGameDir, path + s + '.vtf') as QVTF;
        except
          VTFImage:=nil;
      end;

      S:=Specifics.Values['$basetexture'];
      if (VTFImage=nil) and (s<>'') then
        try
          Log(LOG_VERBOSE,'attempting to load $basetexture '+S);
//          if (self.Protocol<>'') then
//            VTFImage:=NeedGameFileBase(SteamAppsDir+p.name+'.gcf', path + '/' + s + '.vtf') as QVTF
//          else
//            VTFImage:=NeedGameFileBase(GetGameDir, path + '/' + s + '.vtf') as QVTF;
            VTFImage:=NeedGameFileBase(GetGameDir, path + s + '.vtf') as QVTF;
        except
          VTFImage:=nil;
      end;

      if VTFImage<>nil then
      begin
        Log(LOG_VERBOSE,'image found '+S);
        VTFImage.Acces;
        SubElements.Add(VTFImage);
        VTFImage.fparent:=self;
      end
      else
        Log(LOG_WARN,'no image found in material '+self.name);

    end;
    else
      inherited;
  end;
end;

procedure QVMT.SaveFile(Info: TInfoEnreg1);
var
  RawBuffer: String;
  VMTMaterial, OutputSize: Cardinal;
begin
 Log(LOG_VERBOSE,'save vmt %s',[self.name]);
 with Info do case Format of
  1:
  begin  { as stand-alone file }

    if (not VMTLoaded) or ReloadNeeded then
    begin
      if ReloadNeeded then
        VMTLoaded:=false;
      if not LoadVTFLib then
        Raise EErrorFmt(5718, [GetLastError]);
      VMTLoaded:=true;
    end;

    if vlCreateMaterial(@VMTMaterial)=false then
      Fatal('Unable to save VMT file. Call to vlCreateMaterial failed.');

    if vlBindMaterial(VMTMaterial)=false then
      Fatal('Unable to save VMT file. Call to vlBindMaterial failed.');


    {Do stuff}

    if vlMaterialSaveLump(Pointer(RawBuffer), Length(RawBuffer), @OutputSize)=false then
      Fatal('Unable to save VMT file. Call to vlMaterialSaveLump failed.');

    F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);

    vlDeleteMaterial(VMTMaterial);
  end
 else inherited;
 end;
end;

{-------------------}


initialization
begin
  RegisterQObject(QVMT, 'v');
end;

finalization
  UnloadVTFLib(true);
end.

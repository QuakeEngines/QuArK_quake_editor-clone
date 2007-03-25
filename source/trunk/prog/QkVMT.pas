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
Revision 1.5  2007/03/20 21:18:04  danielpharos
Fix for 1.4

Revision 1.4  2007/03/20 21:07:00  danielpharos
Even textures for cstrike should now load correctly. GCF file access through VMT files is (still) broken.

Revision 1.3  2007/03/20 20:38:07  danielpharos
VTF textures should now load correctly from VMT files.

Revision 1.2  2007/03/19 15:27:54  danielpharos
Added a few more keywords to find a texture for a VMT file.

Revision 1.1  2007/03/15 22:19:13  danielpharos
Re-did the entire VMT file loading! It's using the VTFLib now. Saving VMT files not supported yet.


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
 I: integer;
 S: String;
 GameDir: String;
 SteamAppsDir: String;
 SteamDirectory: String;
 SteamDirectoryLength: Integer;
 TexturePath: String;
 TexturePath2: String;
 TexturePath3: String;
 GCFFilename: String;
 Size: TPoint;
 V: array [1..2] of Single;
begin
  Acces;
  Result:=nil;
  {if DefaultImageCache<>Nil then
  begin
    result:=DefaultImageCache
  end
  else
  begin}

  GCFFilename:='source materials';
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

  SteamDirectory:=SetupSubSet(ssGames,'Half-Life2').Specifics.Values['Directory'];
  if (RightStr(SteamDirectory,1)='\') or (RightStr(SteamDirectory,1)='/') then
    SteamDirectoryLength:=Length(SteamDirectory)
  else
    SteamDirectoryLength:=Length(SteamDirectory)+1;
  TexturePath:=RightStr(self.filename,Length(self.filename)-SteamDirectoryLength);

  I:=pos('\',TexturePath);
  if I>0 then
    TexturePath:=LeftStr(TexturePath, I-1)
  else
  begin
    I:=pos('/',GameDir);
    if I>0 then
      TexturePath:=LeftStr(TexturePath, I-1);
  end;

  TexturePath2:=RightStr(self.filename,length(self.filename)-SteamDirectoryLength-Length(TexturePath)-1);

  I:=pos('\',TexturePath2);
  if I=0 then
    I:=pos('/',TexturePath2);
  TexturePath3:=RightStr(TexturePath2,Length(TexturePath2)-I);

  I:=pos('\',TexturePath3);
  if I=0 then
    I:=pos('/',TexturePath3);
  TexturePath3:=RightStr(TexturePath3,Length(TexturePath3)-I);

  TexturePath2:=LeftStr(TexturePath2,Length(TexturePath2)-Length(TexturePath3));

  I:=pos('\',TexturePath3);
  if I=0 then
    I:=pos('/',TexturePath3);
  TexturePath3:=LeftStr(TexturePath3,I);

  S:=Specifics.Values['%tooltexture'];
  if (s<>'') then
  begin
    Log(LOG_VERBOSE,'attempting to load $basetexture '+S);
    try
      if (self.Protocol<>'') then
        Result:=NeedGameFileBase(SteamAppsDir+GCFFilename+'.gcf', TexturePath2 + s + '.vtf') as QPixelSet
      else
        Result:=NeedGameFileBase(TexturePath, TexturePath2 + s + '.vtf') as QPixelSet;
    except
      Result:=nil;
    end;
  end;

  S:=Specifics.Values['$basetexture'];
  if (Result=nil) and (s<>'') then
  begin
    Log(LOG_VERBOSE,'attempting to load $basetexture '+S);
    try
      if (self.Protocol<>'') then
        Result:=NeedGameFileBase(SteamAppsDir+GCFFilename+'.gcf', TexturePath2 + s + '.vtf') as QPixelSet
      else
        Result:=NeedGameFileBase(TexturePath, TexturePath2 + s + '.vtf') as QPixelSet;
    except
      Result:=nil;
    end;
  end;

  S:=Specifics.Values['$Material'];
  if (Result=nil) and (s<>'') then
  begin
    Log(LOG_VERBOSE,'attempting to load $Material '+S);
    try
      if (self.Protocol<>'') then
        Result:=NeedGameFileBase(SteamAppsDir+GCFFilename+'.gcf', TexturePath2 + s + '.vtf') as QPixelSet
      else
        Result:=NeedGameFileBase(TexturePath, TexturePath2 + s + '.vtf') as QPixelSet;
    except
      Result:=nil;
    end;
  end;

  S:=Specifics.Values['$dudvmap'];
  if (Result=nil) and (s<>'') then
  begin
    Log(LOG_VERBOSE,'attempting to load $dudvmap '+S);
    try
      if (self.Protocol<>'') then
        Result:=NeedGameFileBase(SteamAppsDir+GCFFilename+'.gcf', TexturePath2 + s + '.vtf') as QPixelSet
      else
        Result:=NeedGameFileBase(TexturePath, TexturePath2 + s + '.vtf') as QPixelSet;
    except
      Result:=nil;
    end;
  end;

  S:=Specifics.Values['$envmap'];
  if (Result=nil) and (s<>'') then
  begin
    Log(LOG_VERBOSE,'attempting to load $envmap '+S);
    try
      if (self.Protocol<>'') then
        Result:=NeedGameFileBase(SteamAppsDir+GCFFilename+'.gcf', TexturePath2 + s + '.vtf') as QPixelSet
      else
        Result:=NeedGameFileBase(TexturePath, TexturePath2 + s + '.vtf') as QPixelSet;
    except
      Result:=nil;
    end;
  end;

  if (Result=nil) then
  begin
    Log(LOG_VERBOSE,'attempting to load '+TexturePath2+TexturePath3+self.Name+'.vtf');
    try
      if (self.Protocol<>'') then
        Result:=NeedGameFileBase(SteamAppsDir+GCFFilename+'.gcf', TexturePath2 + self.name + '.vtf') as QPixelSet
      else
        Result:=NeedGameFileBase(TexturePath, TexturePath2 + TexturePath3 + self.name + '.vtf') as QPixelSet;
    except
      Result:=nil;
    end;
  end;
  //DefaultImageCache:=Result;

  {tiglari: giving shaders a size.  a presumably
  horrible place to do it, but doesn't work when
  shaders are being loaded }
  if Result<>Nil then
  begin
    Log(LOG_VERBOSE,'image found '+S);
    Size:=Result.GetSize;
    V[1]:=Size.X;
    V[2]:=Size.Y;
    SetFloatsSpec('Size',V);
  end
  else
  begin
    Log(LOG_WARN,'no image found in material '+self.name);
    Raise EErrorFmt(5695, [self.name]);
  end;
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
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5695, [Name]);
 Result:=Image.Description;
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

  //NodeLevel: Cardinal;
  NodeType: VMTNodeType;
  NodeName: String;
  NodeValueString: String;
  NodeValueInteger: Cardinal;
  NodeValueSingle: Single;
begin
  Log(LOG_VERBOSE,'load vmt %s',[self.name]);;
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

    //DanielPharos: Do saving stuff...!
    RawBuffer:='';

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

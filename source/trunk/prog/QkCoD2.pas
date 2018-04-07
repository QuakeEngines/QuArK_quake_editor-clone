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
unit QkCoD2;

interface

uses
  Classes, QkZip2, QkBsp, QkFileObjects, QkObjects, QkPixelset;

type
  QCoD2Pak = class(QZipPak)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
  QCoD2Bsp = class(QBsp)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
  QCoD2Material = class(QPixelSet)
        private
         texture_name: String;
        protected
         procedure SaveFile(Info: TInfoEnreg1); override;
         procedure LoadFile(F: TStream; FSize: Integer); override;
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         class function CanLoadBlankFileExt(const Filename: String; nParent: QObject): Boolean; override;
         function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
         function ProvidesSomeImage : QPixelSet;
         function LoadPixelSet : QPixelSet; override;
         function Description : TPixelSetDescription; override;
         function SetDescription(const PSD: TPixelSetDescription;
                                 Confirm: TSDConfirm) : Boolean; override;
        end;

implementation

uses SysUtils, Windows, Quarkx, QkExceptions, QkObjectClassList,
  Setup, Game, Logging, QkTextures, QkApplPaths, ExtraFunctionality;

type
  //Based on: https://github.com/CptAsgard/CoD2Unity/blob/master/Assets/cod2materialfiles.txt
  TCoD2MaterialHeader = packed record
     offset_material: DWORD;      //index to the material name
     offset_color_texture: DWORD; //index to color texture name
     unknown1: DWORD;             //UNKNOWN
     size_texture: DWORD;         //texture size
     unknown2: DWORD;             //UNKNOWN, padding?
     unknown3, unknown4: Word;    //UNKNOWN, texture UV?
     unknown5, unknown6: Word;    //UNKNOWN, texture ST?
     texture_size_x, texture_size_y: Word; //texture size dimensions
     unknown7: DWORD;             //UNKNOWN, padding?
     unknown8: DWORD;             //UNKNOWN
     unknown9: DWORD;             //UNKNOWN
     unknown10: DWORD;            //UNKNOWN
     unknown11: DWORD;            //UNKNOWN
     unknown12: DWORD;            //UNKNOWN
     offset_techniqueset: DWORD;  //index to techniqueset name
     unknown13: DWORD;            //UNKNOWN
     offset_shader: DWORD;        //index to shader name
     offset_colormap_tag: DWORD;  //index to 'colorMap'
     unknown14: DWORD;            //UNKNOWN, shader slot index?
     offset_colormap: DWORD;      //index to color map name
     offset_normalmap_tag: DWORD; //index to 'normalMap'
     unknown15: DWORD;            //UNKNOWN, shader slot index?
     offset_normalmap: DWORD;     //index to normal map name
     offset_specularmap_tag: DWORD; //index to 'specularMap'
     unknown16: DWORD;            //UNKNOWN, shader slot index?
     offset_specularmap: DWORD;   //index to specular texture
  end;

{------------------------}

class function QCoD2Pak.TypeInfo;
begin
 Result:='.iwd';
end;

class procedure QCoD2Pak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5147);
 Info.FileExt:=821;
end;

{------------------------}

class function QCoD2Bsp.TypeInfo;
begin
 Result:='.d3dbsp';
end;

class procedure QCoD2Bsp.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5151);
 Info.FileExt:=828;
end;

 {------------------------}

class function QCoD2Material.TypeInfo;
begin
 //DanielPharos: The person that "forgot" to add a file extension
 //should be fired! QuArK currently cannot handle this properly,
 //so directly opening a CoD2 material file will not work.
 Result:='';
end;

class procedure QCoD2Material.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5152);
 Info.FileExt:=829;
end;

class function QCoD2Material.CanLoadBlankFileExt(const Filename: String; nParent: QObject): Boolean;
begin
 Result:=False;
 if CharModeJeu=mjCoD2 then
   while (nParent<>nil) do
   begin
     if IncludeTrailingPathDelimiter(nParent.Name)=GameShadersPath then
     begin
       Result:=True;
       break;
     end;
     nParent:=nParent.FParent;
   end;
end;

function QCoD2Material.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 (*if Q is QShader then
  Result:=ieResult[True] + [ieListView]
 else*)
  Result:=ieResult[False];
end;

procedure QCoD2Material.LoadFile(F: TStream; FSize: Integer);
var
  header: TCoD2MaterialHeader;
  org: LongInt;
  S: String;
  C: Char;

  function ReadString(): String;
  begin
    repeat
      f.read(C, 1);
      if C=#0 then
        break;
      Result:=Result+C;
    until (false);
  end;

begin
 case ReadFormat of
  rf_Default: begin  { as stand-alone file }
    ObjectGameCode:=mjCoD2;
    if FSize<SizeOf(TCoD2MaterialHeader) then
      Raise EError(5519);
    org:=F.position;
    f.readbuffer(header, sizeof(header));
    f.seek(org+header.offset_material, soFromBeginning);
    S:=ReadString(); //@
    f.seek(org+header.offset_color_texture, soFromBeginning);
    texture_name:=ReadString(); //@
    //@
   end
   else inherited;
 end;
end;

procedure QCoD2Material.SaveFile(Info: TInfoEnreg1);
begin
 with Info do case Format of
  rf_Default: begin  { as stand-alone file }
      raise EQObjectSavingNotSupported.Create('Saving CoD2 material files is currently not supported!');
     end;
 else inherited;
 end;
end;

function QCoD2Material.ProvidesSomeImage : QPixelSet;
var
 Filename: String;
begin
 Result:=Nil;
 if texture_name<>'' then
 begin
   Filename:=ConcatPaths([GameTexturesPath, texture_name+SetupGameSet.Specifics.Values['TextureFormat']]);
   Log(LOG_VERBOSE,'attempting to load '+Filename);
   Result:=NeedGameFile(Filename, '') as QPixelSet;
 end;
end;

function QCoD2Material.LoadPixelSet : QPixelSet;
begin
 Result:=ProvidesSomeImage;
 if Result=Nil then
  Raise EErrorFmt(5697, [Name]);
 Result.Acces;
end;

function QCoD2Material.Description : TPixelSetDescription;
begin
 Result:=LoadPixelSet.Description;
end;

function QCoD2Material.SetDescription(const PSD: TPixelSetDescription;
                                      Confirm: TSDConfirm) : Boolean;
begin
 Raise EError(5696);
end;

 {------------------------}

initialization
  RegisterQObject(QCoD2Pak, 's');
  RegisterQObject(QCoD2Bsp, 's');
  RegisterQObject(QCoD2Material, 'p');
end.


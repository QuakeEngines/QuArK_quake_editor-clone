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
unit QkIwi;

interface

uses Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
     QkDevIL;

type
  QIwi = class(QImage)
        protected
          class function FileTypeDevIL : DevILType; override;
//FIXME          procedure SaveFileDevILSettings; override;
          class function FormatName : String; override;
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {--------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging, QkExceptions;

class function QIwi.FormatName : String;
begin
 Result:='IWI';
end;

class function QIwi.TypeInfo: String;
begin
 TypeInfo:='.iwi';
end;

class procedure QIwi.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5231);
  Info.FileExt:=824;
  Info.WndInfo:=[wiWindow];
end;

class function QIwi.FileTypeDevIL : DevILType;
begin
  Result:=IL_IWI;
end;

(*procedure QIwi.SaveFileDevILSettings;
var
  Setup: QObject;
  Flag: ILint;
begin
  inherited;

  //FIXME
end;*)

procedure QIwi.LoadFile(F: TStream; FSize: Integer);
begin
  Log(LOG_VERBOSE,'Loading IWI file: %s',[self.name]);
  case ReadFormat of
  rf_Default: begin  { as stand-alone file }
    LoadFileDevIL(F, FSize)
  end;
  else
    inherited;
  end;
end;

procedure QIwi.SaveFile(Info: TInfoEnreg1);
begin
 Log(LOG_VERBOSE,'Saving IWI file: %s',[self.name]);
 with Info do
  case Format of
  rf_Default: begin  { as stand-alone file }
    raise exception.create('Cannot save IWI files (yet)');
    //FIXME

    SaveFileDevIL(Info)
  end
  else
    inherited;
  end;
end;

 {--------------------}

initialization
  RegisterQObject(QIwi, 'k');
end.

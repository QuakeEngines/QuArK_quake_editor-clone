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
}

unit QkPNG;

interface

uses Classes, QkImages, QkObjects, QkFileObjects;

type
  QPng = class(QImages)
        protected
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

{-------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, PNGImage;

class function QPng.TypeInfo: String;
begin
 TypeInfo:='.png';
end;

class procedure QPng.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5181);
  Info.FileExt:=810;
  Info.WndInfo:=[wiWindow];
end;

procedure QPng.LoadFile(F: TStream; FSize: Integer);
var
  png: TPNGImage;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      png:=TPNGImage.Create;
      try
        png.LoadFromStream(F);
        PasteBitmap(GameBuffer(mjAny), png);
      finally
        png.Free;
      end;
    end;
    else
      inherited;
  end;
end;

procedure QPng.SaveFile(Info: TInfoEnreg1);
(*
var
  png: TPngImage;
*)
begin
  with Info do
    case Format of
      1: begin  { as stand-alone file }
        raise exception.create('Sorry, saving .png files is unsupported at the moment!');
      end;
    else
      inherited;
    end;
end;

{-------------------}

initialization
  RegisterQObject(QPng, 'k');
end.

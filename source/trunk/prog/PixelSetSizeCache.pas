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
unit PixelSetSizeCache;

interface

uses Windows, Classes, QkObjects;

type
  TPixelSetSizeCache = class
  protected
    mAltSrc: QObject;
    mTexNames: TStringList;
    mTexSizes: array of TPoint;
  public
    constructor Create(AltSrc: QObject);
    destructor Destroy; override;
    function GetSize(const TexName: String): TPoint;
  end;

implementation

uses QkPixelSet, QkTextures;

constructor TPixelSetSizeCache.Create(AltSrc: QObject);
begin
  mTexNames:=TStringList.Create();
  if AltSrc<>nil then
  begin
    mAltSrc:=AltSrc;
    mAltSrc.AddRef(+1);
  end;
end;

destructor TPixelSetSizeCache.Destroy();
begin
  if mAltSrc<>nil then
    mAltSrc.AddRef(-1);
  mTexNames.Free;
  inherited;
end;

function TPixelSetSizeCache.GetSize(const TexName: String): TPoint;
var
  I: Integer;
  Q: QPixelSet;
begin
  I:=mTexNames.IndexOf(TexName);
  if I=-1 then
  begin
    Result.X:=0;
    Result.Y:=0;
    Q:=GlobalFindTexture(TexName, mAltSrc);
    if Q<>nil then
      try
        Result:=Q.GetSize();
      except
        { do nothing }
      end;
    mTexNames.Add(TexName);
    SetLength(mTexSizes, Length(mTexSizes)+1);
    mTexSizes[Length(mTexSizes)-1]:=Result;
  end
  else
    Result:=mTexSizes[I];
end;

end.

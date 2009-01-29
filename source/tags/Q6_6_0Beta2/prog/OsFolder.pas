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
Revision 1.11  2001/03/20 21:47:27  decker_dk
Updated copyright-header

Revision 1.10  2001/02/05 11:38:57  tiglari
framework for filters in texture-list building (don't work yet,
  just the preliminaries I hope)

Revision 1.9  2001/02/04 18:30:56  tiglari
filter shaders by shaderlist.txt

Revision 1.8  2001/02/04 01:41:00  tiglari
changed visibility of QOsFolder.ReadFolder

Revision 1.7  2001/02/01 20:46:26  decker_dk
added revision history
}

unit OsFolder;

interface

uses
  ToolBoxGroup, QkObjects, Dialogs, Classes, QkFileObjects, QuickWal, QkWad;

type
  QOsFolder = class(QTextureList)
    protected
      procedure LoadFile(F: TStream; FSize: Integer); override;
    public
      procedure FinalizeFromText; override;
      procedure ReadFolder;
      function WriteSubElements : Boolean; override;
      class function TypeInfo: String; override;
 end;


implementation

uses QkObjectClassList, Toolbox1, Game, QkTextures,
  SysUtils;

class function QOsFolder.TypeInfo;
begin
 TypeInfo:='.osfolder';
end;

procedure QOsFolder.FinalizeFromText;
begin;
  ReadFolder;
end;

procedure QOsFolder.LoadFile(F: TStream; FSize: Integer);
begin
  inherited;
  ReadFolder;
end;

procedure QOsFolder.ReadFolder;
var
 Base : String;
 allshaders : boolean;
begin
  Base:=Specifics.Values['path'];
  allshaders:=Specifics.Values['allshaders']='1';
  if Specifics.Values['build']='1' then
    BuildTextureFolders(Base, QObject(Self))
  else
    MergeTextureFolders(Base, QObject(Self), allshaders, Specifics.Values['filter']);
end;

function QOsFolder.WriteSubElements;
begin
  Result:=False;
end;


initialization
  RegisterQObject(QOsFolder, 'a');

end.

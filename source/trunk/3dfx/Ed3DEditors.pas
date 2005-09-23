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



unit Ed3DEditors;

interface

uses EdSceneObject
    ,Ed3DFX
    ,EdOpenGL
  {$IFDEF QUARK_DIRECT3D}
    ,EdDirect3D
  {$ENDIF}
    ;

 {------------------------}

procedure Close3DEditors;
procedure Free3DEditors;

 {------------------------}

implementation

procedure Close3DEditors;
begin
{$IFDEF QUARK_DIRECT3D}
  CloseDirect3DEditor;
{$ENDIF}
  CloseOpenGLEditor;
  Close3DFXEditor;
end;

procedure Free3DEditors;
begin
{$IFDEF QUARK_DIRECT3D}
  FreeDirect3DEditor;
{$ENDIF}
  FreeOpenGLEditor;
  Free3DFXEditor;
  TTextureManager.FreeNonVisibleTextures;
end;

end.

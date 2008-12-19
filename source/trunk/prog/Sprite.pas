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
Revision 1.5  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.3  2001/06/05 18:41:51  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.2  2001/03/20 21:41:57  decker_dk
Updated copyright-header
}

unit Sprite;

interface

uses Windows, SysUtils, Classes, Python, qmath, qmatrices, PyMath, QkObjects,
     Quarkx, Setup, Qk3D, QkImages, QkMdlObject;

type
  QSprite = class(Q3DObject)
  public
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
    Function GetSkinDescr: String;
    Function Skin0: QImage;
    procedure AddTo3DScene(Scene: TObject); override;
    procedure BuildRefList(L: TQList); virtual;
    procedure ChercheExtremites(var Min, Max: TVect); override;
    procedure GetVertices(var p: vec3_p);
    function Triangles(var P: PComponentTris) : Integer;
  end;

implementation

uses EdSceneObject, PyMapView, QkObjectClassList;

class function QSprite.TypeInfo;
begin
  TypeInfo:=':sprite';
end;

procedure QSprite.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiSpriteFile;
end;

procedure QSprite.BuildRefList(L: TQList);
begin
  L.Add(Self);
end;

function vec3(x,y,z: Integer): vec3_t;
begin
  result[0]:=x; result[1]:=y; result[2]:=z;
end;

Function QSprite.GetSkinDescr: String;
begin
  Result:=':'+FParent.Name+':0';
end;

Function QSprite.Skin0: QImage;
begin
  Result:=QImage(Subelements[0]);
  if result=nil then
    FParent.Acces;
end;

function QSprite.Triangles(var P: PComponentTris) : Integer;
var
  p_o: PComponentTris;
  size: tpoint;
begin
  size:=Skin0.getsize;
  getmem(p_o, sizeof(TComponentTris)*2);
  fillchar(p_o^, sizeof(TComponentTris)*2, #0);
  p:=p_o;
  p_o^[0].VertexNo:=0; P^[0].S:=0; P^[0].T:=0;
  p_o^[1].VertexNo:=1; P^[1].S:=size.x; P^[1].T:=0;
  p_o^[2].VertexNo:=2; P^[2].S:=0; P^[2].T:=size.y;
  inc(p_o);
  p_o^[0].VertexNo:=1; P^[0].S:=size.x; P^[0].T:=0;
  p_o^[1].VertexNo:=2; P^[1].S:=0; P^[1].T:=size.y;
  p_o^[2].VertexNo:=3; P^[2].S:=size.x; P^[2].T:=size.y;
  result:=2;
end;

procedure QSprite.GetVertices(var p: vec3_p);
var
  size: tpoint;
  p_o: vec3_p;
begin
  getmem(p_o, sizeof(vec3_t)*4);
  size:=Skin0.getsize;
  p:=p_o;
  p_o^:=vec3(0,0,0); inc(p_o);
  p_o^:=vec3(size.x,0,0);inc(p_o);
  p_o^:=vec3(0,size.y,0);inc(p_o);
  p_o^:=vec3(size.x,size.y,0);
end;

procedure QSprite.AddTo3DScene(Scene: TObject);
var
  Info: PSpriteInfo;
  size: tpoint;
begin
  size:=Skin0.getsize;
  New(Info);
  FillChar(Info^, SizeOf(TSpriteInfo), 0);
  Info^.Base:=Self;
  Info^.Alpha:=255;
  Info^.VertexCount:=4;
  Info^.Width:=size.x;
  Info^.Height:=size.y;
  GetVertices(Info^.Vertices);
  AddRef(+1);
  TSceneObject(Scene).AddSprite(Info);
end;

procedure QSprite.ChercheExtremites(var Min, Max: TVect);
var
  I: Integer;
  P: vec3_p;
begin
  GetVertices(P);
  for I:=1 to 4 do begin
    if P^[0] < Min.X then
      Min.X:=P^[0];
    if P^[1] < Min.Y then
      Min.Y:=P^[1];
    if P^[2] < Min.Z then
      Min.Z:=P^[2];
    if P^[0] > Max.X then
      Max.X:=P^[0];
    if P^[1] > Max.Y then
      Max.Y:=P^[1];
    if P^[2] > Max.Z then
      Max.Z:=P^[2];
    Inc(P);
  end;
end;

initialization
  RegisterQObject(QSprite, 'a');
end.

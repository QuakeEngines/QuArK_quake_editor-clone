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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.5  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.4  2007/02/02 00:47:49  danielpharos
Fixed a few copyright headers

Revision 1.3  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.1  2005/07/30 23:04:44  alexander
introduced dispface class for faces with displacement mapping
vmf loader sets some displacement info
}

unit DispFace;

interface
uses SysUtils, Windows, Classes, Graphics,
     QkObjects, Qk3D, QkMapObjects, qmath, qmatrices,
     QkExplorer, Setup, QkTextures, Python, PyMath,QkMappoly;
type
 TControlPoints3 = {array of} vec3_t;
 PControlPoints3 = {^TBezierControlPoints3;} vec3_p;
 TMeshBuf3 = record
   W, H: Integer;  { number of points stored in buffer }
   CP: PControlPoints3;
 end;

 TDispFace     = class(TFace)
   normals : array of array of tvect;
   dists : array of array of double;
   disppower: integer;
   meshsize:integer;
   FMeshCache: TMeshBuf3;
   public
     procedure setpower(pwr: integer);
     procedure addnormals(normalpoints : array of double);
     procedure adddists(row : integer; distances : array of double);
     function PyGetAttr(attr: PChar) : PyObject;    override;
 end;


implementation
uses math;

function TDispFace.PyGetAttr(attr: PChar) : PyObject;
var
 I, J: Integer;
 list: PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'd': if StrComp(attr, 'dists') = 0 then
       begin
         Result:=PyList_New(0);
         for i:=0 to meshsize-1 do
         begin
           list:=PyList_New(0);
           for j:=0 to meshsize-1 do
           begin
             PyList_Append(list ,PyFloat_FromDouble(dists[i,j]));
           end;
           PyList_Append(Result, list);
           Py_DECREF(list);
         end;
       end;

  end;
end;

procedure TDispFace.addnormals(normalpoints : array of double);
begin
end;

procedure TDispFace.setpower(pwr: integer);
var
  i:integer;
begin
  disppower:=pwr;
  meshsize:=round(power(2,disppower)+1);
  setlength(dists,meshsize);
  for i:=0 to meshsize-1 do
    setlength(dists[i],meshsize);
end;

procedure TDispFace.adddists(row : integer; distances : array of double);
var
  i:integer;
begin
  for i:=Low(distances) to High(distances) do
    dists[row,i] :=distances[i];
end;

end.

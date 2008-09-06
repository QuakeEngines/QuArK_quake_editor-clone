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


unit QkExceptions;

interface

uses SysUtils;

function GetExceptionMessage(E: Exception) : String;
function EError(Res: Integer) : Exception;
function EErrorFmt(Res: Integer; Fmt: array of const) : Exception;

 {-------------------}

implementation

uses Quarkx;

 {-------------------}

function GetExceptionMessage(E: Exception) : String;
var
 I: Integer;
begin
 Result:=E.Message;
 I:=Pos('//', Result);
 if I>0 then
  begin
   SetLength(Result, I);
   Result[I]:='.';
  end
 else
  Result:=Result+'.';
end;

function EError(Res: Integer) : Exception;
begin
 PythonCodeEnd;
 EError:=Exception.Create(LoadStr1(Res));
end;

function EErrorFmt(Res: Integer; Fmt: array of const) : Exception;
begin
 PythonCodeEnd;
 EErrorFmt:=Exception.Create(FmtLoadStr1(Res, Fmt));
end;

end.

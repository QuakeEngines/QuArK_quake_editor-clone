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
Revision 1.7  2005/09/28 10:49:03  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2001/06/05 18:43:13  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.4  2001/03/20 21:35:06  decker_dk
Updated copyright-header
}

unit PyMacros;

interface

uses Graphics, QkObjects, PyForms, Quarkx, Python;

type
 QPyMacro = class(QObject)
            public
              class function TypeInfo: String; override;
              procedure ObjectState(var E: TEtatObjet); override;
              function RunMacro(const Macro: String) : Boolean;
              function RunMacro1(const Macro: String) : PyObject;
            end;

 {------------------------}

implementation

uses Travail, QkObjectClassList;

 {------------------------}

class function QPyMacro.TypeInfo;
begin
 Result:=':py';
end;

procedure QPyMacro.ObjectState;
begin
 inherited;
 E.IndexImage:=iiPython;
 E.MarsColor:=clGreen;
end;

function QPyMacro.RunMacro(const Macro: String) : Boolean;
var
 o: PyObject;
begin
 ProgressIndicatorStart(0,0); try
 o:=RunMacro1(Macro);
 Result:=o<>Nil;
 Py_XDECREF(o);
 finally ProgressIndicatorStop; end;
 PythonCodeEnd;
end;

function QPyMacro.RunMacro1(const Macro: String) : PyObject;
begin
 Acces;
 Result:=CallMacro(@PythonObj, Macro);
end;

 {------------------------}

initialization
  RegisterQObject(QPyMacro, 'a');
end.

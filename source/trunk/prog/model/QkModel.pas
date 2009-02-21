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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.8  2008/11/06 20:16:06  danielpharos
Renamed function to concatenate paths, and start using it.

Revision 1.7  2008/09/29 21:45:32  danielpharos
Soft-coded 'maps' directory (not in Python yet).

Revision 1.6  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.4  2001/03/20 21:37:04  decker_dk
Updated copyright-header

Revision 1.3  2000/10/11 19:01:08  aiv
Small updates
}

unit QkModel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, QkObjects, QkFileObjects,
  QkForm, QkImages, Python, Game, QkPCX, QkTextures, qkmodelroot, Forms,
  PyForms;

type
  QModel = class(QFileObject)
  protected
    function OpenWindow(nOwner: TComponent) : TQForm1; override;
  public
    function ConversionFrom(Source: QFileObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
    procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
    function GetRoot : QModelRoot;
    function TestConversionType(I: Integer) : QFileObjectClass; override;
  end;

implementation

uses QkQkl, QkMdl, QkMd2, QkMd3, form_model, QkHr2, QkApplPaths;

function QModel.TestConversionType(I: Integer) : QFileObjectClass;
begin
  case I of
    1: Result:=QQkl;
    2: Result:=QMd3File;
    3: Result:=QMd2File;
    4: Result:=QMdlFile;
    5: Result:=QHr2Model;
    else Result:=Nil;
  end;
end;

function QModel.GetRoot : QModelRoot;
var
 S: String;
 Q: QObject;
begin
  Result:=Nil;
  S:=Specifics.Values['Root'];
  if S<>'' then begin
    Q:=SubElements.FindName(S);
    if (Q<>Nil) and (Q is QModelRoot) then
      Result:=QModelRoot(Q);
  end;
end;

function QModel.OpenWindow;
begin
  if nOwner=Application then
    Result:=NewPyForm(Self)
  else
    Result:=TFQMdl.Create(nOwner);
end;

procedure QModel.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiModel;
  E.MarsColor:=$00400080;
end;

class procedure QModel.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.WndInfo:=[wiWindow, wiMaximize{, wiOwnExplorer}];
  Info.PythonMacro:='displaymdl';
end;

function QModel.ConversionFrom(Source: QFileObject) : Boolean;
begin
  Result:=Source is QModel;
  if Result then begin
    Source.Acces;
    CopyAllData(Source, False);   { directly copies data }
  end;
end;

procedure QModel.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
var
  S: String;
  filename: PyObject;
begin
  Acces;
  S:=Specifics.Values['FileName'];
  if S='' then
    S:=Name;
  BuildCorrectFileName(S);
  S:=ConcatPaths([GameModelPath, S+TypeInfo]);
  SaveInFile(rf_Default, OutputFile(S));
  filename:=PyString_FromString(PChar(S));
  PyList_Append(extracted, filename);
  Py_DECREF(filename);
end;

end.


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

uses QkQkl, QkMdl, QkMd2, QkMd3, form_model, QkHr2, QkApplPaths, Setup;

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
  //DanielPharos: Workaround: Set the gamemode (if not set yet)
  //to the current gamemode, so that annoying warning doesn't show
  if Specifics.Values['Game'] = '' then
    Specifics.Values['Game'] := GetGameName(CharModeJeu);
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
  try
    PyList_Append(extracted, filename);
  finally
    Py_DECREF(filename);
  end;
end;

end.

{
$Header$
----------- REVISION HISTORY ------------
$Log$
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

uses QkQkl, QkMdl, QkMd2, QkMd3, form_model, QkHr2;

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
  S:=GameModelPath+S+TypeInfo;
  SaveInFile(rf_Default, OutputFile(S));
  filename:=PyString_FromString(PChar(S));
  PyList_Append(extracted, filename);
  Py_DECREF(filename);
end;

end.
 
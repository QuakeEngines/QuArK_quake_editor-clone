unit QkQkl;

interface

uses
  QkObjects, QkFileObjects, QkForm, QkImages, Python, Game, QkModel;

type
  QQkl = class(QModel)
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;

implementation

uses QuarkX, Setup, PyForms, Undo;

class function QQkl.TypeInfo;
begin
  Result:='.qkl';
end;

class procedure QQkl.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.NomClasseEnClair:=LoadStr1(5143);
  Info.FileExt:=785;
  Info.QuArKFileObject:=True;
end;

initialization
  RegisterQObject(QQkl, 'w');
end.

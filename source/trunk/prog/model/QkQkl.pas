{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.3  2001/01/15 19:23:18  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

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

uses QuarkX, Setup, PyForms, Undo, QkObjectClassList;

class function QQkl.TypeInfo;
begin
  Result:='.qkl';
end;

class procedure QQkl.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5143);
  Info.FileExt:=785;
  Info.QuArKFileObject:=True;
end;

initialization
  RegisterQObject(QQkl, 'w');
end.

{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

unit QkSkinGroup;

interface

uses
  QkObjects, QkFileObjects, QkForm, QkImages, Python, Game, QkMdlObject;

type
  QSkinGroup = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
  end;

implementation

uses QkObjectClassList;

class function QSkinGroup.TypeInfo;
begin
  TypeInfo:=':sg';
end;

initialization
  RegisterQObject(QSkinGroup,  'a');
end.
 
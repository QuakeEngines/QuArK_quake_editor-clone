{
$Header$
----------- REVISION HISTORY ------------
$Log$
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

class function QSkinGroup.TypeInfo;
begin
  TypeInfo:=':sg';
end;

initialization
  RegisterQObject(QSkinGroup,  'a');
end.
 
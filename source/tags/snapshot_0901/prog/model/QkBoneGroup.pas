unit QkBoneGroup;

interface

uses
  QkObjects, QkFileObjects, QkForm, QkImages, Python, Game, QkMdlObject;

type
  QBoneGroup = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
  end;

implementation

class function QBoneGroup.TypeInfo;
begin
  TypeInfo:=':bg';
end;

initialization
  RegisterQObject(QBoneGroup,  'a');
end.
 
unit QkMiscGroup;

interface

uses
  QkObjects, QkFileObjects, QkForm, QkImages, Python, Game, QkMdlObject;

type
  QMiscGroup = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
  end;

implementation

class function QMiscGroup.TypeInfo;
begin
  TypeInfo:=':mg';
end;

initialization
  RegisterQObject(QMiscGroup,  'a');
end.
 
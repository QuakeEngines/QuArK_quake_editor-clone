unit QkModelTag;

interface

uses QkMdlObject, QkObjects;

type
  QModelTag = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
  end;

implementation

class function QModelTag.TypeInfo;
begin
  TypeInfo:=':tag';
end;

procedure QModelTag.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiModelTag;
end;

initialization
  RegisterQObject(QModelTag,   'a');
end.
 
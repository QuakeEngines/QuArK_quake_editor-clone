unit QkModelBone;

interface

uses QkMdlObject, QkObjects;

type
  QModelBone = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
  end;

implementation

class function QModelBone.TypeInfo;
begin
  TypeInfo:=':bone';
end;

procedure QModelBone.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiModelBone;
end;

initialization
  RegisterQObject(QModelBone,  'a');
end.
 
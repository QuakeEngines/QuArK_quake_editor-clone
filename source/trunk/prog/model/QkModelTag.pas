{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

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

uses QkObjectClassList;

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
 
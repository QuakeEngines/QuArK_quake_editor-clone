{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

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

uses QkObjectClassList;

class function QBoneGroup.TypeInfo;
begin
  TypeInfo:=':bg';
end;

initialization
  RegisterQObject(QBoneGroup,  'a');
end.
 
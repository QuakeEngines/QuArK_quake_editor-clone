{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

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

uses QkObjectClassList;

class function QMiscGroup.TypeInfo;
begin
  TypeInfo:=':mg';
end;

initialization
  RegisterQObject(QMiscGroup,  'a');
end.
 
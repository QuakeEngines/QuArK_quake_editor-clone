{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

unit QkFrameGroup;

interface

uses
  QkObjects, QkFileObjects, QkForm, QkImages, Python, Game, QkMdlObject;

type
  QFrameGroup = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
  end;

implementation

uses QkObjectClassList;

class function QFrameGroup.TypeInfo;
begin
  TypeInfo:=':fg';
end;

initialization
  RegisterQObject(QFrameGroup, 'a');
end.

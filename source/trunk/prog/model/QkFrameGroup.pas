{
$Header$
----------- REVISION HISTORY ------------
$Log$
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

class function QFrameGroup.TypeInfo;
begin
  TypeInfo:=':fg';
end;

initialization
  RegisterQObject(QFrameGroup, 'a');
end.

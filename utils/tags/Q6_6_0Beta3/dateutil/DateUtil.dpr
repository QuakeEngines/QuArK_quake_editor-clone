program DateUtil;

uses
  Forms,
  UnitDateUtil in 'UnitDateUtil.pas'  {FormDateUtil};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DateUtil';
  Application.CreateForm(TFormDateUtil, FormDateUtil);
  Application.Run;
end.

unit JpegFileWrapper;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, PasJpeg;

type
  TJpegFileWrapper = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    Procedure Load(input,output: TStream; nocolors:Integer; callback: JPEG_ProgressMonitor); // jpeg to bitmap
    Procedure Save(input,output: TStream; quality:Integer; callback: JPEG_ProgressMonitor); // bitmap to jpeg
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

Procedure TJpegFileWrapper.Load(input,output: TStream; nocolors:Integer; callback: JPEG_ProgressMonitor);
begin
  try
    LoadJPEG(input,output, false, nocolors, callback);
  except
    on E: Exception do
      raise Exception.Createfmt('Exception "%s" in TJpegFileWrapper.Load',[E.message]);
  end;
end;

Procedure TJpegFileWrapper.Save(input,output: TStream; quality:Integer; callback: JPEG_ProgressMonitor);
begin
  StoreJPEG(input,output,false,quality, callback);
end;

procedure Register;
begin
  RegisterComponents('Exemples', [TJpegFileWrapper]);
end;

end.

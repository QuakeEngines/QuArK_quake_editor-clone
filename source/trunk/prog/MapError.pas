unit MapError;

interface

type

TMapError = class
 public
  procedure Clear;
  procedure AddText(const Text: String);
  function Text : String;
 protected
  S: String;
end;

var
g_MapError : TMapError;

implementation

procedure TMapError.Clear;
begin
  S:='';
end;

procedure TMapError.AddText(const Text: String);
begin
  S:=S+Text+'\n';
end;

function TMapError.Text: String;
begin
  Result:=S;
  S:='';
end;

initialization

g_MapError:=TMapError.Create;
g_MapError.Clear();

finalization

g_MapError.free;

end.


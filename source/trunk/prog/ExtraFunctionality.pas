unit ExtraFunctionality;

interface
uses SysUtils;
function ConvertPath(const s: string):string;

implementation

function ConvertPath(const s: string):string;
begin
  {$IFDEF LINUX}
  result:=StringReplace(s,'\',PathDelim,[rfReplaceAll]);
  {$ELSE}
  result:=StringReplace(s,'/',PathDelim,[rfReplaceAll]);
  {$ENDIF}
end;

end.
 
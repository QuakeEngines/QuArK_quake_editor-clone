unit ExtraFunctionality;

interface

function IncludeTrailingBackslash(const S: string): string;

implementation

function IncludeTrailingBackslash(const S: string): string;
begin
  Result := S;
  if Result[length(result)]<>'\' then Result := Result + '\';
end;

end.
 
unit ExtraFunctionality;

interface

function IncludeTrailingBackslash(s: string): string;
function IsPathDelimiter(const S: string; Index: Integer): Boolean;

implementation

function IsPathDelimiter(const S: string; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = '\')
    and (ByteType(S, Index) = mbSingleByte);
end;

function IncludeTrailingBackslash(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then Result :=
Result + '\';
end;

end.
 
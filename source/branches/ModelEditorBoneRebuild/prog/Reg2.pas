(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) QuArK Development Team

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.7  2007/02/28 08:50:50  danielpharos
Fixed a range check error when trying to read non-existing values from the registry.

Revision 1.6  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.4  2003/09/06 00:55:50  silverpaladin
Casting Cleanup

Revision 1.3  2001/03/20 21:42:44  decker_dk
Updated copyright-header

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit Reg2;

interface

uses Windows, Classes, SysUtils, Registry;

type
 TRegistry2 = class(TRegistry)
              public
              {OnWrite: TNotifyEvent;
               DontWrite: Boolean;
               Tag: Integer;}
               function ReadInteger(const Name: string; var Value: Integer) : Boolean;
               function ReadString(const Name: string; var Value: String) : Boolean;
               function WriteString(const Name, Value: string) : Boolean;
               function WriteInteger(const Name: string; Value: Integer) : Boolean;
               function ReadOpenKey(const KeyName: String) : Boolean;
              end;

implementation

function TRegistry2.ReadString;
var
  Len: Integer;
  DataType: Cardinal;
  Courant: String;
begin
  Result:=False;
  Len := GetDataSize(Name);
  if Len > 0 then
   begin
    SetString(Courant, nil, Len);
    DataType := REG_NONE;
    if (RegQueryValueEx(CurrentKey, PChar(Name), nil, @DataType, PByte(Courant),
     @Len) = ERROR_SUCCESS)
    and ((DataType = REG_SZ) or (DataType = REG_EXPAND_SZ)) then
     begin
      SetLength(Courant, StrLen(PChar(Courant)));
      Value:=Courant;
      Result:=True;
     end;
   end;
end;

function TRegistry2.WriteString;
var
 Courant: String;
begin
 if {not DontWrite and}
 (not ReadString(Name, Courant) or (Value<>Courant)) then
  begin
  {if Assigned(OnWrite) then
    OnWrite(Self);
   if not DontWrite then}
    Result:=RegSetValueEx(CurrentKey, PChar(Name), 0, REG_SZ,
     PChar(Value), Length(Value)+1)=ERROR_SUCCESS;
  end
 else
  Result:=True;
end;

function TRegistry2.ReadInteger;
var
  Courant: Integer;
  DataType, Len: Cardinal;
begin
 Len:=SizeOf(Courant);
 DataType := REG_NONE;
 if (RegQueryValueEx(CurrentKey, PChar(Name), nil, @DataType, PByte(@Courant),
  @Len) <> ERROR_SUCCESS) or (DataType <> REG_DWORD) then
   Result:=False
  else
   begin
    Value:=Courant;
    Result:=True;
   end;
end;

function TRegistry2.WriteInteger;
var
 Courant: Integer;
begin
 if {not DontWrite and}
 (not ReadInteger(Name, Courant) or (Courant<>Value)) then
  begin
  {if Assigned(OnWrite) then
    OnWrite(Self);
   if not DontWrite then}
    Result:=RegSetValueEx(CurrentKey, PChar(Name), 0, REG_DWORD,
     @Value, SizeOf(Integer))=ERROR_SUCCESS;
  end
 else
  Result:=True;
end;

{function TRegistry2.OpenKey(const KeyName: String; Create: Boolean) : Boolean;
begin
 if Create and (DontWrite or Assigned(OnWrite)) then
  begin
   Result:=OpenKey(KeyName, False);
   if not Result then
    begin
     if DontWrite then Exit;
     if Assigned(OnWrite) then OnWrite(Self);
     if DontWrite then Exit;
     Result:=OpenKey(KeyName, True);
    end;
  end
 else
  OpenKey:=inherited OpenKey(KeyName, Create);
end;}

function TRegistry2.ReadOpenKey(const KeyName: String) : Boolean;
var
  TempKey: HKey;
  S: string;
  Relative: Boolean;
begin
 Result:=False;
 if KeyName='' then Exit;
 S:=KeyName;
 Relative:=S[1]<>'\';
 if not Relative then Delete(S, 1, 1);
 TempKey:=0;
 Result:=RegOpenKeyEx(GetBaseKey(Relative), PChar(S), 0, KEY_READ, TempKey) = ERROR_SUCCESS;
 if Result then
  begin
   if (CurrentKey<>0) and Relative then S:=CurrentPath+'\'+S;
   ChangeKey(TempKey, S);
  end;
end;

end.

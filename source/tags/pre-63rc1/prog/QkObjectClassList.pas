(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2001/03/20 21:45:22  decker_dk
Updated copyright-header

Revision 1.1  2001/01/21 15:49:30  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.
}

unit QkObjectClassList;

interface

uses Classes, QkObjects;

procedure RegisterQObject(Q: QObjectClass; Prior: Char);
function GetRegisteredQObject(const Ext: String) : QObjectClass;
function ConstructQObject(const Name: String; nParent: QObject) : QObject;
function NeedClassOfType(const nTypeInfo: String) : QObjectClass;
procedure ListFileExt(L: TStrings);
procedure BuildFileExt(L: TStrings);

var
 QObjectClassList: TStringList = Nil;

 {------------------------}

implementation

uses
 SysUtils,
 {$IFDEF Debug} MemTester, {$ENDIF}
 QkFileObjects, Quarkx;


 {------------------------}

procedure RegisterQObject(Q: QObjectClass; Prior: Char);
{ (Comment by Decker 2001-01-21)
 Function to register a QkObject-decendant into QuArK's huge class-list.
 'Q' is the class which should be registered.
 'Prior' is a letter, which will be used to prefix 'Q's TypeInfo with, and
 then sorted by 'Prior+Q.TypeInfo()' - Why it needs to be sorted I dont know.
}
begin
  if QObjectClassList=Nil then
  begin
    QObjectClassList:=TStringList.Create;
    QObjectClassList.Sorted:=True;
  end;

  QObjectClassList.AddObject(Prior+Q.TypeInfo, TObject(Q));
end;

function GetRegisteredQObject(const Ext: String) : QObjectClass;
var
  J: Integer;
begin
  for J:=0 to QObjectClassList.Count-1 do
  begin
    if CompareText(Copy(QObjectClassList[J], 2, MaxInt), Ext) = 0 then
    begin
      Result:=QObjectClass(QObjectClassList.Objects[J]);
      Exit;
    end;
  end;
  Result:=Nil;
end;

function ConstructQObject(const Name: String; nParent: QObject) : QObject;
{ (Comment by Decker 2001-01-21
 This function, will create an object based on the 'TypeInfo/FilenameExtension'
 of the 'Name'. E.q. if 'Name' contains "MyTexture.WAL", it will create an object
 of the registered class which 'TypeInfo()' function return ".WAL".
 The new object will belong to, and be child of 'nParent'.
}
var
  I: Integer;
  S: String;
begin
  I := QObjectClassList.Count;

  repeat
    Dec(I);
    S := QObjectClassList[I];
  until (Length(S)-1 <= Length(Name))
    and (StrIComp(@Name[Length(Name)-Length(S)+2], PChar(S)+1) = 0);

  Result := QObjectClass(QObjectClassList.Objects[I]).Create(Copy(Name, 1, Length(Name)-Length(S)+1), nParent);
end;

function NeedClassOfType(const nTypeInfo: String) : QObjectClass;
var
  I: Integer;
  S: String;
begin
  for I:=0 to QObjectClassList.Count-1 do
  begin
    S:=QObjectClassList[I];
    if StrIComp(PChar(nTypeInfo), PChar(S)+1) = 0 then
    begin
      Result:=QObjectClass(QObjectClassList.Objects[I]);
      Exit;
    end;
  end;
  Raise EErrorFmt(5644, [nTypeInfo]);
end;

procedure ListFileExt(L: TStrings);
var
  I: Integer;
  C: QObjectClass;
  Info: TFileObjectClassInfo;
  S: String;
begin
  for I:=QObjectClassList.Count-1 downto 0 do
  begin
    S:=QObjectClassList[I];
    if S[1]='a' then
      Continue;
    C:=QObjectClass(QObjectClassList.Objects[I]);
    if C.InheritsFrom(QFileObject) then
    begin
      QFileObjectClass(C).FileObjectClassInfo(Info);
      if Info.FileExt<>0 then
      begin
        L.Add(Info.DefaultExt);
        L.Add(LoadStr1(Info.FileExt));
      end;
    end;
  end;
end;

procedure BuildFileExt(L: TStrings);
var
  I: Integer;
  C: QObjectClass;
  Info: TFileObjectClassInfo;
  AllTypes1, AllTypes2, S: String;
begin
  AllTypes1:='';
  AllTypes2:='';
  for I:=QObjectClassList.Count-1 downto 0 do
  begin
    S:=QObjectClassList[I];
    if S[1]='a' then
      Continue;
    C:=QObjectClass(QObjectClassList.Objects[I]);
    if C.InheritsFrom(QFileObject) then
    begin
      QFileObjectClass(C).FileObjectClassInfo(Info);
      if Info.FileExt<>0 then
      begin
        L.Add(LoadStr1(Info.FileExt));
        if Info.DefaultExt<>'' then
        begin
          if AllTypes1<>'' then
          begin
            AllTypes1:=AllTypes1+' ';
            AllTypes2:=AllTypes2+';';
          end;
          AllTypes1:=AllTypes1+Info.DefaultExt;
          AllTypes2:=AllTypes2+'*.'+Info.DefaultExt;
        end;
      end;
    end;
  end;
  L.Insert(0, FmtLoadStr1(768, [AllTypes1, AllTypes2]));
end;

end.

(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

{

$Header$
 ----------- REVISION HISTORY ------------
$Log$

}


unit QkInclude;

interface

uses Classes, SysUtils, QkObjects;

type
{QInclude = class(QObject)
            protected
              class function TypeInfo: String; override;
              function RemoveReference : Boolean; override;
            public
            end;}
 QIncluded = class(QObject)
             public
               class function TypeInfo: String; override;
             end;

 {------------------------}

function DoIncludeData(Target, LookFrom: QObject; const InclName: String) : Boolean;
function FindIncludeData1(LookFrom: QObject; const InclName: String) : QObject;

 {------------------------}

implementation

uses QkGroup, Setup;

 {------------------------}

function FindIncludeData1(LookFrom: QObject; const InclName: String) : QObject;
var
 FullMatch: Boolean;
 Match: String;

  function Parcourir(Parent: QObject) : Boolean;
  var
   I: Integer;
   Q, N: QObject;
  begin
   for I:=0 to Parent.SousElements.Count-1 do
    begin
     Q:=Parent.SousElements[I];
     if (FullMatch and (CompareText(Q.Name+Q.TypeInfo, Match) = 0))
     or (not FullMatch and (Q is QIncluded) and (CompareText(Q.Name, InclName) = 0)) then
      begin
       Q.Acces;   { found it }
       if FullMatch then
        begin
         N:=QIncluded.Create('', Nil);
         N.SousElements.Add(Q);
         FindIncludeData1:=N;
        end
       else
        FindIncludeData1:=Q;
       Result:=True;
       Exit;
      end;
     if (Q is QIncluded) or (Q is QExplorerGroup) then
      begin
       Q.Acces;     { browse recursively }
       if Parcourir(Q) then
        begin
         Result:=True;
         Exit;
        end;
      end;
    end;
   Result:=False;
  end;

var
 AddOns: QObject;
begin
 FullMatch:=(Length(InclName)>2) and (InclName[1]='(') and (InclName[Length(InclName)]=')');
 if FullMatch then
  Match:=Copy(InclName, 2, Length(InclName)-2);
 FindIncludeData1:=Nil;
 while (LookFrom<>Nil) and not Parcourir(LookFrom) do
  LookFrom:=LookFrom.FParent;
 if Result=Nil then
  begin
   AddOns:=MakeAddonsList; try
   Parcourir(AddOns);
   finally AddOns.AddRef(-1); end;
  end;
end;

function DoIncludeData(Target, LookFrom: QObject; const InclName: String) : Boolean;
var
 J: Integer;
 Q, N: QObject;
begin
 Q:=FindIncludeData1(LookFrom, InclName);
 Q.AddRef(+1); try
 Result:=Q<>Nil;
 if Result then
  begin
    { copy all data from Q into Target }
   for J:=0 to Q.Specifics.Count-1 do
    Target.Specifics.Add(Q.Specifics[J]);
   for J:=0 to Q.SousElements.Count-1 do
    begin
     N:=Q.SousElements[J].Clone(Target, False);
    {if (IncludePos<0) or (IncludePos>=Target.SousElements.Count) then}
      Target.SousElements.Add(N)
    {else
      begin
       Target.SousElements.Insert(IncludePos, N);
       Inc(IncludePos);
      end};
    end;
  end;
 finally Q.AddRef(-1); end; 
end;

 {------------------------}

(*class function QInclude.TypeInfo: String;
begin
 TypeInfo:=':i';
end;

function QInclude.RemoveReference : Boolean;
{var
 IncludePos: Integer;}
begin
{if FParent=Nil then
  IncludePos:=-1
 else
  IncludePos:=FParent.SousElements.IndexOf(Self);}
 { this object is immediately removed if DoIncludeData succeeded }
 RemoveReference:=DoIncludeData(FParent, FParent, Name);
end;*)

 {------------------------}

class function QIncluded.TypeInfo: String;
begin
 TypeInfo:=':incl';
end;

 {------------------------}

initialization
{ RegisterQObject(QInclude, 'a'); }
  RegisterQObject(QIncluded, 'a');
end.

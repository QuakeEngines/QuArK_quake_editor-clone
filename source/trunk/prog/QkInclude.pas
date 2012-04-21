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

http://quark.sourceforge.net/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.10  2009/07/15 10:38:00  danielpharos
Updated website link.

Revision 1.9  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.8  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.6  2001/06/14 18:54:23  decker_dk
Moved the FullMatch check '('+name+')' to DoIncludeData so it can be used in .QRK files too.
Added functionality to include one's name-equal. To be used in the Half-Life .QRK files soon.

Revision 1.5  2001/03/20 21:45:50  decker_dk
Updated copyright-header

Revision 1.4  2001/01/21 15:49:03  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.3  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
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
function FindIncludeData1(LookFrom: QObject; const InclName: String; FullMatch: Boolean; Original: QObject = nil) : QObject;

 {------------------------}

implementation

uses QkGroup, QkQuakeCtx, Setup, QkObjectClassList;

 {------------------------}

function FindIncludeData1(LookFrom: QObject; const InclName: String; FullMatch: Boolean; Original: QObject = nil) : QObject;

  function Parcourir(Parent: QObject) : Boolean;
  var
   I: Integer;
   Q, N: QObject;
  begin
   for I:=Parent.SubElements.Count-1 downto 0 do
    begin
     Q:=Parent.SubElements[I];
     if ((FullMatch and (CompareText(Q.Name+Q.TypeInfo, InclName) = 0))
     or (not FullMatch and (Q is QIncluded) and (CompareText(Q.Name, InclName) = 0)))
     and (Q<>Original) then
      begin
       Q.Acces;   { found it }
       if FullMatch then
        begin
         N:=QIncluded.Create('', Nil);
         N.SubElements.Add(Q);
         FindIncludeData1:=N;
        end
       else
        FindIncludeData1:=Q;
       Result:=True;
       Exit;
      end;
     if (Q is QIncluded) or (Q is QExplorerGroup) or (Q is QQuakeCtx) then
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
 Q, Q2, N: QObject;
 FullMatch: Boolean;
 MatchName: String;
begin
 FullMatch:=((InclName[1]='(') and (InclName[Length(InclName)]=')'));
 if FullMatch=True then
  MatchName:=Copy(InclName, 2, Length(InclName)-2)
 else
  MatchName:=InclName;
 Q:=FindIncludeData1(LookFrom, MatchName, FullMatch, Target);
 Q.AddRef(+1); try
 Result:=Q<>Nil;
 if Result then
  begin
   if FullMatch=True then
    Q2:=Q.SubElements[0]
   else
    Q2:=Q;
   { copy all data from Q2 into Target }
   for J:=0 to Q2.Specifics.Count-1 do
    Target.Specifics.Add(Q2.Specifics[J]);
   for J:=0 to Q2.SubElements.Count-1 do
    begin
     N:=Q2.SubElements[J].Clone(Target, False);
    {if (IncludePos<0) or (IncludePos>=Target.SubElements.Count) then}
      Target.SubElements.Add(N)
    {else
      begin
       Target.SubElements.Insert(IncludePos, N);
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
  IncludePos:=FParent.SubElements.IndexOf(Self);}
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

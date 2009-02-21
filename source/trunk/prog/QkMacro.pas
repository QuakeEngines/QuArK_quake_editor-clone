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
Revision 1.11  2009/02/17 17:11:11  danielpharos
Improved some bad error raising.

Revision 1.10  2008/09/06 15:56:58  danielpharos
Moved exception code into separate file.

Revision 1.9  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.7  2001/06/05 18:39:33  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.6  2001/03/20 21:45:49  decker_dk
Updated copyright-header

Revision 1.5  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.4  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.3  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkMacro;

interface

uses Windows, SysUtils, Classes, QkObjects;

const
 SpecDesc = ';desc';   { description that appears in italic in toolboxes }
 SpecIncl = ';incl';   { includes data and process the macros again }
 SpecCopy = ';copy';   { like ;copy but after macros are processed (macros in the copied data are not processed) }

 {------------------------}

procedure ProcessMacros(Q, Source: QObject);
procedure DrawMapMacros(Entity: QObject; Macros, Entities: TQList);

 {------------------------}

implementation

uses QkFileObjects, Setup, QkInclude, qmath, Qk3D, Quarkx, QkExceptions;

 {------------------------}

(*procedure FindFreeMacro(var S: String; Next: Boolean);
var
 Form4: TForm4;
 L: TQList;
 TestI, J, K, P, P2: Integer;
 Test, nArg, SpecArg, PreviousArg: String;
 Q: QObject;
 Used: Boolean;
begin
 P:=Pos('%d', S);
 if P=0 then
  begin
   P:=Pos('%s', S);
   if P=0 then Exit;
   TestI:=0;
   Test:='a';
  end
 else
  begin
   TestI:=1;
   Test:='1';
  end;
 Form4:=GetDefaultForm4;
 if Form4=Nil then Exit;
 L:=Form4.GetEntityList;
 nArg:='';
 repeat
  PreviousArg:=nArg;
  nArg:=Copy(S, 1, P-1) + Test + Copy(S, P+2, MaxInt);
  Used:=False;
  for J:=0 to L.Count-1 do
   begin
    Q:=L[J];
    for K:=0 to Q.Specifics.Count-1 do
     begin
      SpecArg:=Q.Specifics[K];
      P2:=Pos('=', SpecArg);
      if (P2>0) and (P2=Length(SpecArg)-Length(nArg))
      and (CompareText(Copy(SpecArg, P2+1, MaxInt), nArg) = 0) then
       begin
        Used:=True;
        Break;
       end;
     end;
    if Used then Break;
   end;
  if not Used then
   begin
    if not Next and (PreviousArg<>'') then
     S:=PreviousArg
    else
     S:=nArg;
    Exit;
   end;
  if TestI=0 then
   begin
    J:=Length(Test);
    while (J>0) and (Test[J]='z') do
     begin
      Test[J]:='a';
      Dec(J);
     end;
    if J=0 then
     Test:='a'+Test
    else
     Test[J]:=Succ(Test[J]);
   end
  else
   begin
    Inc(TestI);
    Test:=IntToStr(TestI);
   end;
 until False;
end;*)

function Process1(Q, Source: QObject; const S: String) : String;
var
 I, J: Integer;
 S1, MacroStr: String;
 Q1: QObject;
begin
 try
  for I:=1 to Length(S) do
   if S[I]='[' then
    if S[I+1]='<' then
     begin
      MacroStr:=Copy(S, I+2, MaxInt);
      J:=Pos('>', MacroStr);
      if J=0 then Raise EError(5580);
      if S[J+1]<>']' then Raise EError(5581);
      Result:=Copy(S,1,I-1)+Copy(MacroStr,1,J-1)+Process1(Q, Source, Copy(MacroStr,J+2,MaxInt));
      Exit;
     end
    else
     begin
      MacroStr:=Process1(Q, Source, Copy(S, I+2, MaxInt));
      J:=Pos(']', MacroStr);
      if J=0 then Raise EError(5578);
      Result:=Copy(MacroStr, J+1, MaxInt);
      MacroStr:=Copy(MacroStr, 1, J-1);
      case S[I+1] of
       ':': if GetSetupPath(MacroStr, S1, Q1) then
             MacroStr:=Q1.Specifics.Values[S1]
            else
             MacroStr:='';
      {'$': FindFreeMacro(MacroStr, True);
       '£': FindFreeMacro(MacroStr, False);}
       '~': MacroStr:=Source.Specifics.Values[MacroStr];
      else
       Raise EError(5579);
      end;
      Result:=Copy(S,1,I-1)+MacroStr+Result;
      Exit;
     end;
 except
  on E: Exception do
   GlobalWarning(FmtLoadStr1(5577, [S, GetExceptionMessage(E)]));
 end;
 Result:=S;
end;

(*procedure ProcessBrackets(Q, Source: QObject);
var
 I, J: Integer;
 S: String;
begin
 Q.Acces;
 for I:=0 to Q.Specifics.Count-1 do
  begin
   S:=Q.Specifics[I];
   J:=Pos('=', S);
   if (J>3) and (S[J-2]='[') and (S[J-1]=']') then   { process macro }
    Q.Specifics[I]:=Copy(S, 1, J-3)+'='+Process1(Q, Source, Copy(S, J+1, MaxInt));
  end;
 for I:=0 to Q.SubElements.Count-1 do
  ProcessBrackets(Q.SubElements[I], Source);
end;*)

procedure ProcessMacros(Q, Source: QObject);
var
 I, J: Integer;
 L: TStringList;
 S: String;
begin
 Q.Acces;
 Q.Specifics.Values[SpecDesc]:='';
 repeat
  for I:=0 to Q.Specifics.Count-1 do
   begin
    S:=Q.Specifics[I];
    J:=Pos('=', S);
    if (J>3) and (S[J-2]='[') and (S[J-1]=']') then   { process macro }
     Q.Specifics[I]:=Copy(S, 1, J-3)+'='+Process1(Q, Source, Copy(S, J+1, MaxInt));
   end;
  for I:=0 to Q.SubElements.Count-1 do
   ProcessMacros(Q.SubElements[I], Source);

  S:=Q.Specifics.Values[SpecIncl];
  if S='' then Break;
  Q.Specifics.Values[SpecIncl]:='';
  L:=TStringList.Create; try
  L.Text:=S;
  for J:=0 to L.Count-1 do
   DoIncludeData(Q, Source, L[J]);
  finally L.Free; end;
 until False;

 S:=Q.Specifics.Values[SpecCopy];
 if S='' then Exit;
 Q.Specifics.Values[SpecCopy]:='';
 L:=TStringList.Create; try
 L.Text:=S;
 for J:=0 to L.Count-1 do
  DoIncludeData(Q, Source, L[J]);
 finally L.Free; end;
end;

(*var
 I, J: Integer;
begin
 S:=Q.Specifics.Values[SpecIncl];
 if S<>'' then
  begin
   Q.Specifics.Values[SpecIncl]:='';
   L:=TStringList.Create; try
   L.Text:=S;
   for J:=0 to L.Count-1 do
    DoIncludeData(Q, Gr.SubElements[I], L[J]);
   finally L.Free; end;
  end;
 S:=Q.Specifics.Values[SpecTexture];
 if S<>'' then
  begin
   ReplaceWithDefaultTex(Q, S, SetupGameSet.Specifics.Values['TextureDef']);
   Q.Specifics.Values[SpecTexture]:='';
  end;
end;*)

procedure DrawMapMacros(Entity: QObject; Macros, Entities: TQList);

  procedure MapMacros(Q: QObject);

    function SelectPen : HPen;
    var
     S: String;
     Width: Integer;
     Color: TColorRef;
    begin
     S:=Q.Specifics.Values['width'];
     if S='' then
      Width:=2
     else
      Width:=Round(ReadNumValueEx(S));
     S:=Q.Specifics.Values['color'];
     if S='' then
      Color:=MapColors(lcAxes)
     else
      Color:=vtocol(ReadVector(S));
     SelectPen:=SelectObject(g_DrawInfo.DC,
      CreatePen(ps_Solid, Width, Color));
    end;

  var
   S, Arg: String;
   I, J: Integer;
   Test, Macro: QObject;
   V1, V2: TVect;
   R: TDouble;
   Pt1, Pt2, Pt3, Pt4, Pt5: TPoint;
   Pen: HPen;
  begin
   try
    Q.Acces;
    if CompareText(Q.Name, 'DrawMap')=0 then
     begin
      S:=Q.Specifics.Values['Spec'];
      if S='' then Exit;
      Arg:=Q.Specifics.Values['Arg'];
      if ((Arg='') and (Entity.Specifics.IndexOfName(S)>=0))
      or ((Arg<>'') and (CompareText(Entity.Specifics.Values[S],Arg)=0)) then
       begin  { "Entity" has the matching Specific }
        for J:=0 to Q.SubElements.Count-1 do
         begin
          Macro:=Q.SubElements[J].Clone(Nil, False); try
          ProcessMacros(Macro, Entity);
          MapMacros(Macro);
          finally Macro.Free; end;
         end;
       end;
      Exit;
     end;
    if CompareText(Q.Name, 'find')=0 then
     begin
      S:=Q.Specifics.Values['Spec'];
      if S='' then Exit;
      Arg:=Q.Specifics.Values['Arg'];
      for I:=0 to Entities.Count-1 do   { search for matching entities }
       begin
        Test:=Entities[I];
        if ((Arg='') and (Test.Specifics.IndexOfName(S)>=0))
        or ((Arg<>'') and (CompareText(Test.Specifics.Values[S],Arg)=0)) then
         begin  { found an entity }
          for J:=0 to Q.SubElements.Count-1 do
           begin
            Macro:=Q.SubElements[J].Clone(Nil, False); try
            ProcessMacros(Macro, Test);
            MapMacros(Macro);
            finally Macro.Free; end;
           end;
         end;
       end;
      Exit;
     end;
    if CompareText(Q.Name, 'Circle')=0 then
     begin
      V1:=ReadVector(Q.Specifics.Values['center']);
      Pt1:=Proj(V1);
      R:=ReadNumValueEx(Q.Specifics.Values['radius']);
      J:=Round(R*g_pProjZ);
      Pen:=SelectPen;
      Ellipse(g_DrawInfo.DC, Pt1.X-J, Pt1.Y-J, Pt1.X+J, Pt1.Y+J);
      DeleteObject(SelectObject(g_DrawInfo.DC, Pen));
      Exit;
     end;
    if CompareText(Q.Name, 'Arrow')=0 then
     begin
      V1:=ReadVector(Q.Specifics.Values['from']);
      Pt1:=Proj(V1);
      V2:=ReadVector(Q.Specifics.Values['to']);
      Pt2:=Proj(V2);
      Pt3.X:=Pt2.X-Pt1.X;
      Pt3.Y:=Pt2.Y-Pt1.Y;
      R:=Sqrt(Sqr(Pt3.X)+Sqr(Pt3.Y));
      if R<rien then Exit;
      S:=Q.Specifics.Values['arrow'];
      if S='' then
       J:=5
      else
       J:=Round(ReadNumValueEx(S));
      R:=J/R;
      Pt4.X:=Pt2.X-Round(R*(Pt3.X+Pt3.Y));
      Pt4.Y:=Pt2.Y-Round(R*(Pt3.Y-Pt3.X));
      Pt5.X:=Pt2.X-Round(R*(Pt3.X-Pt3.Y));
      Pt5.Y:=Pt2.Y-Round(R*(Pt3.Y+Pt3.X));
      Pen:=SelectPen;
      MoveToEx(g_DrawInfo.DC, Pt1.X, Pt1.Y, Nil);
      LineTo(g_DrawInfo.DC, Pt2.X, Pt2.Y);
      LineTo(g_DrawInfo.DC, Pt4.X, Pt4.Y);
      MoveToEx(g_DrawInfo.DC, Pt2.X, Pt2.Y, Nil);
      LineTo(g_DrawInfo.DC, Pt5.X, Pt5.Y);
      DeleteObject(SelectObject(g_DrawInfo.DC, Pen));
      Exit;
     end;
   except
    {rien}
   end;
  end;

var
 I: Integer;
 Brush: HBrush;
begin
 Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_brush));
 for I:=0 to Macros.Count-1 do
  MapMacros(Macros[I]);
 SelectObject(g_DrawInfo.DC, Brush);
end;

 {------------------------}

(*class function QMacro.TypeInfo: String;
begin
 TypeInfo:=':macro';
end;

 {------------------------}

initialization
  RegisterQObject(QMacro, 'a');*)
end.

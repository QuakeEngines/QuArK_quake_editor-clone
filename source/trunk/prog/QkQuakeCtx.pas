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

unit QkQuakeCtx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkFormVw;

type
 QQuakeCtx = class(QFormObject)
             protected
               function GetConfigStr1: String; override;
             public
               class function TypeInfo: String; override;
               procedure EtatObjet(var E: TEtatObjet); override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
             end;

 {------------------------}

function GetQuakeContext: TQList;
function BuildQuakeCtxObjects(nClass: QObjectClass; const nName: String) : TQList;
procedure ClearQuakeContext;

function OpacityFromFlags(Flags: Integer) : Integer;
function OpacityToFlags(Flags: Integer; Alpha: Integer) : Integer;

 {------------------------}

implementation

uses Setup, QkGroup, Quarkx;

 {------------------------}

type
 TTexOpacityInfo = record
                    Loaded: Boolean;
                    Count: Byte;
                    Reserved1, Reserved2: Byte;
                    Opacity: array[0..31] of Byte;
                   end;

var
 TInfo: TTexOpacityInfo;

procedure LoadTInfo;
var
 I, J, K, L, M: Integer;
 Li: TQList;
 Val32: array[0..63] of Single;
begin
 FillChar(TInfo.Opacity, SizeOf(TInfo.Opacity), 255);
 TInfo.Loaded:=True;
 Li:=GetQuakeContext;
 for J:=0 to Li.Count-1 do
  begin
   K:=Li[J].GetFloatsSpecPartial('TexFlagsTransparent', Val32);
   for I:=0 to K div 2 - 1 do
    begin
     M:=Round(Val32[I*2]);
     L:=0;
     while not Odd(M) and (M<>0) do
      begin
       Inc(L);
       M:=M shr 1;
      end;
     if M=1 then
      begin
       M:=Round((1-Val32[I*2+1])*255);
       if M<0 then M:=0 else if M>255 then M:=255;
       TInfo.Opacity[L]:=M;
      end;
    end;
  end;
end;

function OpacityFromFlags(Flags: Integer) : Integer;
var
 L: Integer;
begin
 Result:=255;
 if Flags=0 then Exit;

 if not TInfo.Loaded then
  LoadTInfo;

 L:=0;
 repeat
  if Odd(Flags) and (TInfo.Opacity[L]<Result) then
   Result:=TInfo.Opacity[L];
  Flags:=Flags shr 1;
  Inc(L);
 until Flags=0;
end;

function OpacityToFlags(Flags: Integer; Alpha: Integer) : Integer;
var
 L, Best, DistMin, Dist: Integer;
begin
 if not TInfo.Loaded then
  LoadTInfo;
 Best:=0;
 DistMin:=255-Alpha;
 for L:=Low(TInfo.Opacity) to High(TInfo.Opacity) do
  if TInfo.Opacity[L]<255 then
   begin
    Dist:=Abs(Alpha-Integer(TInfo.Opacity[L]));
    if Dist<DistMin then
     begin
      DistMin:=Dist;
      Best:=1 shl L;
     end;
    Flags:=Flags and not (1 shl L);
   end;
 Result:=Flags or Best;
end;

 {------------------------}

var
 QuakeContext: TQList = Nil;

procedure ClearQuakeContext;
begin
 QuakeContext.Free;
 QuakeContext:=Nil;
 TInfo.Loaded:=False;
end;

function GetQuakeContext: TQList;
var
 Addons: QFileObject;
 I: Integer;
 Q: QObject;
 S: String;
begin
 if QuakeContext=Nil then
  begin
   Addons:=MakeAddOnsList; try
   QuakeContext:=TQList.Create;
   Addons.FindAllSubObjects('', QQuakeCtx, Nil, QuakeContext);
   for I:=QuakeContext.Count-1 downto 0 do
    begin
     Q:=QuakeContext[I];
     Q.Acces;
     if not GameModeOk((Q as QQuakeCtx).ObjectGameCode) then
      begin
       while (Q<>Nil) and (Q.Flags and ofFileLink = 0) do
        Q:=Q.FParent;
       if (Q=Nil) or not (Q is QFileObject) then
        S:=LoadStr1(5552)
       else
        S:=QFileObject(Q).NomFichier;
       GlobalWarning(FmtLoadStr1(5582, [S, SetupGameSet.Name, QuakeContext[I].Specifics.Values['Game']]));
       QuakeContext.Delete(I);
      end;
    end;
   finally Addons.AddRef(-1); end;
  end;
 GetQuakeContext:=QuakeContext;
end;

function BuildQuakeCtxObjects(nClass: QObjectClass; const nName: String) : TQList;
var
 L: TQList;
 I, J: Integer;
 Q, Q1: QObject;
begin
 Result:=TQList.Create;
 try
  L:=GetQuakeContext;
  for I:=0 to L.Count-1 do
   begin
    Q:=L[I];
    for J:=0 to Q.SousElements.Count-1 do
     begin
      Q1:=Q.SousElements[J];
      if (Q1 is nClass)
      and ((nName='') or (CompareText(Q1.Name, nName) = 0)) then
       begin
        {Q1.Acces;}
        Result.Add(Q1);
       end;
     end;
   end;
 except
  Result.Free;
  Raise;
 end;
end;

 {------------------------}

class function QQuakeCtx.TypeInfo;
begin
 TypeInfo:='.qctx';
end;

function QQuakeCtx.GetConfigStr1: String;
begin
 Result:='QuakeCtx';
end;

procedure QQuakeCtx.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiQCtx;
 E.MarsColor:=clMaroon;
end;

class procedure QQuakeCtx.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5155);
{Info.FileExt:=779;
 Info.WndInfo:=[wiWindow];}
end;

 {------------------------}

initialization
  RegisterQObject(QQuakeCtx, 'a');
end.

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

 {------------------------}

implementation

uses Setup, QkGroup, Quarkx;

 {------------------------}

var
 QuakeContext: TQList = Nil;

procedure ClearQuakeContext;
begin
 QuakeContext.Free;
 QuakeContext:=Nil;
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

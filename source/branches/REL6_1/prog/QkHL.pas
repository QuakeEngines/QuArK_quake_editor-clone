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
Revision 1.3  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers


}


unit QkHL;

interface

uses Windows, SysUtils, Classes, Graphics, Dialogs, Controls,
     QkObjects, QkFileObjects, QkTextures, QkWad;

type
 QTextureHL = class(QTexture1)
        protected
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure ChargerFin(F: TStream; TailleRestante: Integer); override;
        public
          class function CustomParams : Integer; override;
          class function TypeInfo: String; override;
          function BaseGame : Char; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
 QTextureHL1 = class(QTextureHL)
        public
          class function TypeInfo: String; override;
        end;

const
 SignatureWad3 = $33444157;   { 'WAD3' }

 {------------------------}

implementation

uses Game, Setup, Quarkx;

 {------------------------}

class function QTextureHL.TypeInfo: String;
begin
 TypeInfo:='.wad3_C';
end;

class function QTextureHL1.TypeInfo: String;
begin
 TypeInfo:='.wad3_@';
end;

class procedure QTextureHL.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5164);
end;

class function QTextureHL.CustomParams : Integer;
begin
 Result:=4 or cpPalette;
end;

function QTextureHL.BaseGame : Char;
begin
 Result:=mjHalfLife;
end;

procedure QTextureHL.ChargerFin(F: TStream; TailleRestante: Integer);
const
 Spec2 = 'Pal=';
 MAXPAL = SizeOf(TPaletteLmp) div SizeOf(TPaletteLmp1);
var
 Data: String;
 P: PPaletteLmp;
 PalSize: SmallInt;
begin
  { reads the palette }
 Data:=Spec2;
 SetLength(Data, Length(Spec2)+SizeOf(TPaletteLmp));
 P:=PPaletteLmp(@Data[Length(Spec2)+1]);
 FillChar(P^, SizeOf(TPaletteLmp), 0);

 if TailleRestante>SizeOf(PalSize) then
  begin
   TailleRestante:=(TailleRestante-SizeOf(PalSize)) div SizeOf(TPaletteLmp1);
   F.ReadBuffer(PalSize, SizeOf(PalSize));
   if PalSize>MAXPAL then
     PalSize:=MAXPAL;
   if PalSize>TailleRestante then
     PalSize:=TailleRestante;
   if PalSize>0 then
     F.ReadBuffer(P^, PalSize*SizeOf(TPaletteLmp1));
  end;

 SpecificsAdd(Data);  { "Pal=xxxxx" }
end;

procedure QTextureHL.SaveFile(Info: TInfoEnreg1);
var
 S: String;
 PalSize: SmallInt;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      SaveAsHalfLife(F);
(*
      SaveAsQuake1(F);
       { writes the palette }
      S:=GetSpecArg('Pal');
      if S='' then
       PalSize:=0
      else
       PalSize:=(Length(S)-Length('Pal=')) div SizeOf(TPaletteLmp1);
      F.WriteBuffer(PalSize, SizeOf(PalSize));
      if PalSize>0 then
       F.WriteBuffer((PChar(S)+Length('Pal='))^, PalSize*SizeOf(TPaletteLmp1));
*)
     end;
 else
   inherited;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(QTextureHL, 'a');
  RegisterQObject(QTextureHL1, 'a');
end.

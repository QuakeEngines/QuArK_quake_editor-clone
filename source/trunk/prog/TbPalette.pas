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
Revision 1.6  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.4  2001/03/20 21:41:57  decker_dk
Updated copyright-header

Revision 1.3  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}


unit TbPalette;

interface

uses Windows, SysUtils, Classes, Graphics, Controls, TB97, Dialogs,
     QkObjects, Game;

 {------------------------}

type
 TTbPaletteClick = procedure(Sender: TToolbar97; Old, New: Integer) of object;

function GetPaletteToolbar(nOwner: TComponent) : TToolbar97;
function MakePaletteToolbar(nOwner: TWinControl) : TToolbar97;
procedure DynamicPaletteToolbar(Pal: TToolbar97; nLink: QObject; const nSpec: String);
procedure StaticPaletteToolbar(Pal: TToolbar97; const Lmp: TPaletteLmp);
procedure ColorSelPaletteToolbar(Pal: TToolbar97; const Lmp: TPaletteLmp; nColorSel: Integer; nOnColorSel: TTbPaletteClick);

function ChooseColor(nOwner: TComponent; var Color: TColor) : Boolean;

 {------------------------}

implementation

uses Undo, Travail, Quarkx, QkExceptions;

 {------------------------}

const
 tbName = '_TB97Pal';
 ccName = '_CCol';
 GroupeCouleurs = 8;

type
 TTbPal = class(TToolbar97)
          protected
            ColorSel: Integer;
            OnColorSel: TTbPaletteClick;
            Link: QObject;
            Spec: String;
            PaletteLmp: TPaletteLmp;
            destructor Destroy; override;
            procedure UpdatePalette;
            procedure BtnClick(Sender: TObject);
            procedure SetPaletteLmp(const Lmp: TPaletteLmp);
            procedure Editing;
            procedure Unlink;
          end;

 {------------------------}

function GetPaletteToolbar(nOwner: TComponent) : TToolbar97;
var
 C: TComponent;
begin
 C:=nOwner.FindComponent(tbName);
 if C<>Nil then
  Result:=C as TTbPal
 else
  Result:=Nil;
end;

function MakePaletteToolbar(nOwner: TWinControl) : TToolbar97;
var
 I, J: Integer;
 ClipRect: TRect;
 Btn: TToolbarButton97;
 Panel: TWinControl;
begin
 ProgressIndicatorStart(0,0); try
 with nOwner.ClientRect do
  begin
   ClipRect.TopLeft:=nOwner.ClientToScreen(TopLeft);
   ClipRect.BottomRight:=nOwner.ClientToScreen(BottomRight);
  end;
 Result:=GetPaletteToolbar(nOwner);
 if Result=Nil then
  begin
   with ClipRect.BottomRight do
    Result:=TTbPal.CustomCreate(nOwner, Bounds(X-16*17, Y-16*17-GetSystemMetrics(sm_CySmCaption),
     16*17, 16));
   Result.Name:=tbName;
   Result.Caption:=LoadStr1(5384);
   Result.CanDockLeftRight:=False;
   Result.CanDockTopBottom:=False;
   Inc(Result.DisableArrangeControls); try
   for I:=0 to 255 div GroupeCouleurs do
    begin
     Panel:=TWinControl.Create(nOwner);
     Panel.SetBounds(0,0,16*GroupeCouleurs,16);
     Panel.Parent:=Result;
     for J:=0 to GroupeCouleurs-1 do
      begin
       Btn:=TToolbarButton97.Create(nOwner);
       Btn.SetBounds(J*16,0,16,16);
       Btn.OnClick:=TTbPal(Result).BtnClick;
       Btn.Tag:=I*GroupeCouleurs+J;
       Btn.Parent:=Panel;
      end;
    end;
   finally Dec(Result.DisableArrangeControls); end;
   Result.AutoArrangeControls;
  end;
 with TTbPal(Result) do
  begin
   Parent:=nOwner;
   Unlink;
  end;
 finally ProgressIndicatorStop; end;
end;

procedure DynamicPaletteToolbar(Pal: TToolbar97; nLink: QObject; const nSpec: String);
var
 S: String;
 I: Integer;
begin
 with TTbPal(Pal) do
  begin
   Unlink;
   Link:=nLink;
   Link.AddRef(+1);
   Spec:=nSpec;
    { reads the palette from the object }
   FillChar(PaletteLmp, SizeOf(PaletteLmp), 0);
   S:=Link.Specifics.Values[Spec];
   I:=Length(S);
   if I>SizeOf(TPaletteLmp) then I:=SizeOf(TPaletteLmp);
   Move(PChar(S)^, PaletteLmp, I);
   UpdatePalette;
  end;
end;

procedure ColorSelPaletteToolbar(Pal: TToolbar97; const Lmp: TPaletteLmp; nColorSel: Integer; nOnColorSel: TTbPaletteClick);
begin
 with TTbPal(Pal) do
  begin
   Unlink;
   PaletteLmp:=Lmp;
   ColorSel:=nColorSel;
   UpdatePalette;
   OnColorSel:=nOnColorSel;
  end;
end;

procedure StaticPaletteToolbar(Pal: TToolbar97; const Lmp: TPaletteLmp);
begin
 with TTbPal(Pal) do
  begin
   Unlink;
   PaletteLmp:=Lmp;
   UpdatePalette;
  end;
end;

 {------------------------}

function ChooseColor(nOwner: TComponent; var Color: TColor) : Boolean;
var
 C, CustomR: Char;
 S: String;
 Ok: Boolean;
 ColorDialog1: TColorDialog;
 Cmp: TComponent;
begin
 Cmp:=nOwner.FindComponent(ccName);
 if Cmp<>Nil then
  begin
   ColorDialog1:=Cmp as TColorDialog;
   CustomR:=Chr(ColorDialog1.Tag);
  end
 else
  begin
   ColorDialog1:=TColorDialog.Create(nOwner);
   ColorDialog1.Name:=ccName;
   CustomR:='A';
  end;
 S:=IntToHex(Color,6);
 Ok:=False;
 for C:='A' to 'P' do
  if ColorDialog1.CustomColors.Values['Color'+C]=S then
   begin
    Ok:=True;
    Break;
   end;
 if not Ok then
  begin
   ColorDialog1.CustomColors.Values['Color'+CustomR]:=S;
   CustomR:=Succ(CustomR);
   if CustomR>'P' then
    CustomR:='A';
  end;
 ColorDialog1.Tag:=Ord(CustomR);
 ColorDialog1.Color:=Color;
 Result:=ColorDialog1.Execute and (Color<>ColorDialog1.Color);
 if Result then
  Color:=ColorDialog1.Color;
end;

 {------------------------}

destructor TTbPal.Destroy;
begin
 Unlink;
 inherited;
end;

procedure TTbPal.Unlink;
begin
 ColorSel:=-1;
 OnColorSel:=Nil;
 if Link<>Nil then
  begin
   Link.AddRef(-1);
   Link:=Nil;
  end;
end;

procedure TTbPal.Editing;
begin
 if Link=Nil then
  Raise EError(5386);
end;

procedure TTbPal.SetPaletteLmp;
var
 S: String;
begin
 Editing;
 SetString(S, PChar(@Lmp), SizeOf(Lmp));
 if S<>Link.Specifics.Values[Spec] then
  Undo.Action(Link, TSpecificUndo.Create(LoadStr1(595), Spec, S, sp_Auto, Link));
end;

procedure TTbPal.UpdatePalette;
var
 I, J, No: Integer;
 C: TColorRef;
 Panel: TWinControl;
 Btn: TControl;
begin
 C:=0;
 for I:=0 to 255 div GroupeCouleurs do
  begin
   Panel:=TWinControl(Controls[I]);
   for J:=0 to GroupeCouleurs-1 do
    begin
     Btn:=Panel.Controls[J];
     No:=Btn.Tag;
     Move(PaletteLmp[No], C, 3);
     with TToolbarButton97(Btn) do
      begin
       Color:=C;
       if No=ColorSel then
        begin
         if ColorIsLight(C) then
          Font.Color:=clBlack
         else
          Font.Color:=clWhite;
         Font.Style:=[fsBold];
         Caption:='X';
        end
       else
        Caption:='';
      end;
    end;
  end;
end;

procedure TTbPal.BtnClick(Sender: TObject);
var
 C: TColor;
 I: Integer;
 Lmp: TPaletteLmp;
begin
 with Sender as TToolbarButton97 do
  begin
   C:=Color;
   I:=Tag;
  end;
 if not Assigned(OnColorSel) then
  begin
   Editing;
   if ChooseColor(Owner, C) then
    begin
    {TToolbarButton97(Sender).Color:=C;}
     Lmp:=PaletteLmp;
     Move(C, Lmp[I], 3);
     SetPaletteLmp(Lmp);
    end;
  end
 else
  OnColorSel(Self, ColorSel, I);
end;

 {------------------------}

end.

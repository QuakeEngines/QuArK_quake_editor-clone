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

unit QkPixelSet;

interface

uses SysUtils, Windows, Game, Python, QkObjects, PyObjects,
     Setup, QkFileObjects;

type
 TPixelSetFormat = (psfDefault, psf8bpp, psf24bpp);
 TPixelSetPalette = (pspDefault, pspFixed, pspVariable);
 { pspFixed: PSDConvert will not try to write a custom palette to the target PSD.
   pspVariable: PSDConvert can write a custom palette to the target PSD. }
 TPixelSetAlpha = (psaDefault, psaNoAlpha, psaGlobalAlpha, psa8bpp);
 TPixelSetAllocated = set of (psaData, psaAlpha, psaPalette);
 TPixelSetDescription = object
                         Format: TPixelSetFormat;
                         Palette: TPixelSetPalette;
                         AlphaBits: TPixelSetAlpha;
                         Allocated: TPixelSetAllocated;
                         Size: TPoint;       { X and Y size, > 0 }
                         ScanLine: Integer;  { negative if bottom-up }
                         Data: Pointer;
                         AlphaScanLine: Integer;
                         AlphaData: Pointer;
                         GlobalAlphaValue: Byte;
                         FPaletteAllocated: Boolean;
                         ColorPalette: PPaletteLmp;
                         procedure Init;
                         procedure Done;
                         function StartPointer: PChar;
                         function AlphaStartPointer: PChar;
                         function GetColors(var Buffer: TBitmapInfoColors) : PBitmapInfoColors;
                         function GetBitmapInfo(var Buffer: TBitmapInfo256;
                                                Pal: HPalettePtr) : PBitmapInfo256;
                         procedure ReleasePalette(Pal: HPalette);
                         procedure Paint(DC: HDC; X, Y: Integer);
                         procedure AllocData;
                         procedure AllocAlpha;
                         procedure AllocPalette;
                         function IsGamePalette(Game: Char) : Boolean;
                        end;

const
 ccDefault      = 0;
 ccAuto         = 0;
 ccConfirm      = 1;   { ask user before doing information-loosing conversions }
 ccNoConversion = 2;   { do not allow any information lost (NOT IMPLEMENTED YET) }
 ccFixedResize  = 16;  { resize is normally allowed to shrink or expand the image;
                         if this flag is specified, it truncates or pads with black instead
                         (NOT IMPLEMENTED YET) }
 ccTemporary    = 32;  { don't allocate storage if unnecessary; returns temporary pointers }

type
 TSDConfirm = ccAuto..ccNoConversion;
 QPixelSetClass = class of QPixelSet;
 QPixelSet = class(QFileObject)  { base class for QImage and QTexture }
             public
               ReverseLink: QObject;  { actually a QTextureLnk object -- INTERNAL }
               function ConversionFrom(Source: QFileObject) : Boolean; override;
               function ConvertFrom(Source: QPixelSet; Flags: Integer) : Boolean; virtual;
               function GetSize : TPoint; virtual;
               procedure SetSize(const nSize: TPoint); virtual;
               function Description : TPixelSetDescription; virtual; abstract;
               function SetDescription(const PSD: TPixelSetDescription;
                                       Confirm: TSDConfirm) : Boolean; virtual; abstract;
               function LoadPixelSet : QPixelSet; virtual;
               procedure OpDansScene(Aj: TAjScene; PosRel: Integer); override;
               procedure Paint(DC: HDC; X, Y: Integer);
               function PyGetAttr(attr: PChar) : PyObject; override;
              end;

 {------------------------}

function PSDToDIB(const Source: TPixelSetDescription) : TPixelSetDescription;
function PSDConvert(var Target: TPixelSetDescription;
                    const Source: TPixelSetDescription;
                    Flags: Integer) : Boolean;  { ccXXX }

 {------------------------}

implementation

uses Controls, Dialogs, Quarkx, QkTextures, CCode, QkExplorer;

 {------------------------}

procedure TPixelSetDescription.Init;
begin
 FillChar(Self, SizeOf(TPixelSetDescription), 0);
end;

procedure TPixelSetDescription.Done;
begin
 if Allocated <> [] then
  begin
   if psaData in Allocated then begin FreeMem(Data); Data:=Nil; Exclude(Allocated, psaData); end;
   if psaAlpha in Allocated then begin FreeMem(AlphaData); AlphaData:=Nil; Exclude(Allocated, psaAlpha); end;
   if psaPalette in Allocated then begin Dispose(ColorPalette); ColorPalette:=Nil; Exclude(Allocated, psaPalette); end;
  end;
end;

function TPixelSetDescription.StartPointer : PChar;
begin
 if ScanLine>=0 then
  Result:=PChar(Data)
 else
  Result:=PChar(Data) - ScanLine*Pred(Size.Y);
end;

function TPixelSetDescription.AlphaStartPointer : PChar;
begin
 if AlphaScanLine>=0 then
  Result:=PChar(AlphaData)
 else
  Result:=PChar(AlphaData) - AlphaScanLine*Pred(Size.Y);
end;

procedure TPixelSetDescription.AllocData;
begin
 GetMem(Data, Abs(ScanLine)*Size.Y);
 Include(Allocated, psaData);
end;

procedure TPixelSetDescription.AllocAlpha;
begin
 GetMem(AlphaData, Abs(AlphaScanLine)*Size.Y);
 Include(Allocated, psaAlpha);
end;

procedure TPixelSetDescription.AllocPalette;
begin
 New(ColorPalette);
 Include(Allocated, psaPalette);
end;

function TPixelSetDescription.GetColors(var Buffer: TBitmapInfoColors) : PBitmapInfoColors;
begin
 if Format<>psf8bpp then
  Result:=Nil
 else
  begin
   if IsGamePalette(mjAny) then
      { fixed palette from the game }
    Result:=@GameBuffer(mjAny)^.BitmapInfo.bmiColors
   else
    begin  { custom palette }
     ColorsFromLmp(ColorPalette^, Buffer);
     Result:=@Buffer;
    end;
  end;
end;

function TPixelSetDescription.GetBitmapInfo(var Buffer: TBitmapInfo256;
                                            Pal: HPalettePtr) : PBitmapInfo256;
begin
 if IsGamePalette(mjAny) then
  begin
   FPaletteAllocated:=False;
   Result:=@GameBuffer(mjAny)^.BitmapInfo;
   if Assigned(Pal) then
    Pal^:=GameBuffer(mjAny)^.Palette;
  end
 else
  if Format<>psf8bpp then
   begin
    FPaletteAllocated:=False;
    ClearBmpInfo24(Buffer);
    Result:=@Buffer;
   end
  else
   begin
    FPaletteAllocated:=True;
    PaletteFromLmp(ColorPalette^, Buffer, Pal, Nil);
    Result:=@Buffer;
   end;
 Result^.bmiHeader.biWidth:=Size.X;
 Result^.bmiHeader.biHeight:=Size.Y;
end;

procedure TPixelSetDescription.ReleasePalette(Pal: HPalette);
begin
 if FPaletteAllocated then
  DeleteObject(Pal);
end;

 {---------------}

function TPixelSetDescription.IsGamePalette(Game: Char) : Boolean;
var
 GP: PPaletteLmp;
begin
 if Format<>psf8bpp then
  Result:=False
 else
  begin
   GP:=@GameBuffer(mjAny)^.PaletteLmp;
   Result:=(ColorPalette=GP) or CompareMem(ColorPalette, GP, SizeOf(TPaletteLmp));
  end;
end;

(* PSDConvert will perform any necessary conversion automatically.
   Any field left to its default (null) value in the target object will
   be set to the value that makes the conversion easier (e.g. leaving
   Format to psfDefault will let PSDConvert set the target format equal
   to the source format). Any field already set in the target object
   will force conversion. *)
   
function PSDConvert;
var
 Resizing: Boolean;
 I, J, J0, J1: Integer;
 Src, Dest: PChar;
 Confirm: String;
 CopyPalette: PPaletteLmp;
 TmpPalette: TPaletteLmp;
begin
 with Target do
  begin
   if Format=psfDefault then Format:=Source.Format;
   if Palette=pspDefault then Palette:=Source.Palette;
   if AlphaBits=psaDefault then AlphaBits:=Source.AlphaBits;
   if Size.X=0 then Size.X:=Source.Size.X;
   if Size.Y=0 then Size.Y:=Source.Size.Y;
   Resizing:=(Source.Format<>Format) or (Source.Size.X<>Size.X) or (Source.Size.Y<>Size.Y);
   if (Data=Nil) and not Resizing and (Flags and ccTemporary <> 0)
   and (Palette=Source.Palette) and ((ScanLine=0) or (ScanLine=Source.ScanLine)) then
    begin
     ScanLine:=Source.ScanLine;
     Data:=Source.Data;
    end
   else
    begin
     if ScanLine=0 then
      if Format=psf24bpp then
       ScanLine:=(Size.X*3+3) and not 3
      else
       ScanLine:=(Size.X+3) and not 3;
     if Data=Nil then
      AllocData;
    end;
   if (Format=psf8bpp) and ((Palette<>pspFixed) or (ColorPalette=Nil)) then
    begin
     if Source.Format=psf8bpp then
      begin
       CopyPalette:=Source.ColorPalette;
       if (ColorPalette=Nil) and (Flags and ccTemporary = 0) then
        AllocPalette;
      end
     else
      CopyPalette:=@GameBuffer(mjAny)^.PaletteLmp;
     if ColorPalette=Nil then
      ColorPalette:=CopyPalette
     else
      ColorPalette^:=CopyPalette^;
    end
   else
    CopyPalette:=Nil;

   if Flags and ccConfirm <> 0 then
    begin
     Confirm:='';
     if (Size.X<Source.Size.X) or (Size.Y<Source.Size.Y) then
      Confirm:=Confirm + LoadStr1(5684);
     if (Format=psf8bpp) and ((Source.Format<>psf8bpp) or
      (not Assigned(CopyPalette) and (ColorPalette<>Source.ColorPalette)
       and not CompareMem(ColorPalette, Source.ColorPalette, SizeOf(TPaletteLmp)))) then
        if IsGamePalette(mjAny) then
         Confirm:=Confirm + FmtLoadStr1(5687, [GameBuffer(mjAny)^.GameName])
        else
         Confirm:=Confirm + LoadStr1(5686);
     if (AlphaBits<>psa8bpp) and (Source.AlphaBits=psa8bpp) then
      Confirm:=Confirm + LoadStr1(5685);
     if Confirm<>'' then
      if MessageDlg(Confirm + LoadStr1(5683), mtWarning, mbOkCancel, 0) <> mrOk then
       begin
        Result:=False;
        Exit;
       end;
    end;
  
   if Resizing then
    Resample(Source.ColorPalette, Source.Data,
             ColorPalette, Data,
             Source.Size.X, Source.Size.Y, Source.ScanLine,
             Size.X, Size.Y, ScanLine)
   else   { not resizing, fast version }
    if Data<>Source.Data then
     if ScanLine=Source.ScanLine then
      Move(Source.Data^, Data^, Abs(ScanLine)*Size.Y)
     else
      begin
       Src:=Source.StartPointer;
       Dest:=StartPointer;
       J0:=Size.X;
       if Format=psf24bpp then
        J0:=J0*3;
       J1:=Abs(ScanLine)-1;
       for I:=0 to Size.Y-1 do
        begin
         Move(Src^, Dest^, J0);
         for J:=J0 to J1 do
          Dest[J]:=#0;   { pad with zeroes }
         Inc(Src, Source.ScanLine);
         Inc(Dest, ScanLine);
        end;
      end;

   case AlphaBits of
   psa8bpp:
    if (Flags and ccTemporary <> 0) and (AlphaData=Nil)
    and ((AlphaScanLine=0) or (AlphaScanLine=Source.AlphaScanLine))
    and (Source.AlphaBits=psa8bpp) and (Size.X=Source.Size.X) and (Size.Y=Source.Size.Y) then
     begin
      AlphaScanLine:=Source.AlphaScanLine;
      AlphaData:=Source.AlphaData;
     end
    else
     begin
      if AlphaScanLine=0 then AlphaScanLine:=Size.X;
      if AlphaData=Nil then AllocAlpha;
      if Source.AlphaBits<>psa8bpp then
       begin  { no source alpha mask }
        if Source.AlphaBits = psaGlobalAlpha then
         I:=Source.GlobalAlphaValue
        else
         I:=$FF;
        FillChar(AlphaData^, Abs(AlphaScanLine)*Size.Y, I);
       end
      else
       if (Size.X=Source.Size.X) and (Size.Y=Source.Size.Y) then
        begin   { not resizing, fast version }
         Src:=Source.AlphaStartPointer;
         Dest:=AlphaStartPointer;
         J0:=Size.X;
         J1:=Abs(AlphaScanLine)-1;
         for I:=0 to Size.Y-1 do
          begin
           Move(Src^, Dest^, J0);
           for J:=J0 to J1 do
            Dest[J]:=#0;   { pad with zeroes }
           Inc(Src, Source.AlphaScanLine);
           Inc(Dest, AlphaScanLine);
          end;
        end
       else
        begin   { resizing alpha mask : build a greyscale palette }
         for I:=0 to 255 do
          begin
           TmpPalette[I][0]:=I;
           TmpPalette[I][1]:=I;
           TmpPalette[I][2]:=I;
          end;
         Resample(@TmpPalette, Source.AlphaData, @TmpPalette, AlphaData,
                  Source.Size.X, Source.Size.Y, Source.AlphaScanLine,
                  Size.X, Size.Y, AlphaScanLine);
        end;
     end;
   psaGlobalAlpha:
    if Source.AlphaBits = psaGlobalAlpha then
     GlobalAlphaValue:=Source.GlobalAlphaValue
    else
     GlobalAlphaValue:=$FF;
   end;
  end;
 Result:=True;
end;

function PSDToDIB(const Source: TPixelSetDescription) : TPixelSetDescription;
begin
 Result.Init;
 case Source.Format of
  psf8bpp: Result.ScanLine:=-((Source.Size.X+3) and not 3);
  psf24bpp: Result.ScanLine:=-((Source.Size.X*3+3) and not 3);
 else
  raise InternalE('unknown PSD format');
 end;
 PSDConvert(Result, Source, ccTemporary);
end;

procedure TPixelSetDescription.Paint(DC: HDC; X, Y: Integer);
var
 BmpInfo: TBitmapInfo256;
 BitmapInfo: PBitmapInfo;
 NewPSD: TPixelSetDescription;
 Pal0, Pal1: HPalette;
begin
 Pal1:=0;
 Pal0:=0;
 NewPSD:=PSDToDIB(Self);
 try
  BitmapInfo:=PBitmapInfo(NewPSD.GetBitmapInfo(BmpInfo, @Pal0));
  if Pal0<>0 then
   begin
    Pal1:=SelectPalette(DC, Pal0, False);
    RealizePalette(DC);
   end;
  SetDIBitsToDevice(DC, X, Y, NewPSD.Size.X, NewPSD.Size.Y, 0, 0,
   0, NewPSD.Size.Y, NewPSD.Data, BitmapInfo^, dib_RGB_Colors);
 finally
  if Pal1<>0 then
   SelectPalette(DC, Pal1, False);
  NewPSD.ReleasePalette(Pal0);
  NewPSD.Done;
 end;
end;

 {------------------------}

function QPixelSet.GetSize : TPoint;
var
 V: array[1..2] of Single;
begin
 if not LoadPixelSet.GetFloatsSpec('Size', V) then
  Raise EErrorFmt(5534, ['Size']);
 Result.X:=Round(V[1]);
 Result.Y:=Round(V[2]);
end;

procedure QPixelSet.SetSize(const nSize: TPoint);
var
 V: array[1..2] of Single;
begin
 V[1]:=nSize.X;
 V[2]:=nSize.Y;
 SetFloatsSpec('Size', V);
end;

procedure QPixelSet.Paint(DC: HDC; X, Y: Integer);
var
 PSD: TPixelSetDescription;
begin
 PSD:=Description; try
 PSD.Paint(DC, X, Y);
 finally PSD.Done; end;
end;

function QPixelSet.LoadPixelSet : QPixelSet;
begin
 Acces;
 Result:=Self;
end;

procedure QPixelSet.OpDansScene(Aj: TAjScene; PosRel: Integer);
var
 Lnk: QTextureLnk;
 WE: TQkExplorer;
begin
 inherited;
 if (Aj=asModifie) and (PosRel=0) then
  begin
   Lnk:=QTextureLnk(ReverseLink);
   while Lnk<>Nil do
    begin
     WE:=WorkingExplorer; try
     OperationDansScene(Lnk, asModifie, Nil);
     finally WorkingExplorer:=WE; end;
     Lnk:=Lnk.Next;
    end;
  end;
end;

function QPixelSet.ConvertFrom(Source: QPixelSet; Flags: Integer) : Boolean;
var
 PSD: TPixelSetDescription;
begin
 PSD:=Source.Description; try
 Result:=SetDescription(PSD, Flags);
 finally PSD.Done; end;
end;

function QPixelSet.ConversionFrom(Source: QFileObject) : Boolean;
begin
 Result:=(Source is QPixelSet) and ConvertFrom(QPixelSet(Source), ccConfirm);
end;

function QPixelSet.PyGetAttr(attr: PChar) : PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'd': if StrComp(attr, 'disktexture')=0 then
        begin
         Result:=GetPyObj(LoadPixelSet);
         Exit;
        end;
 end;
end;

 {------------------------}

end.

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
Revision 1.25  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.24  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.23  2008/09/06 15:57:05  danielpharos
Moved exception code into separate file.

Revision 1.22  2008/03/29 15:15:47  danielpharos
Moved all the CompareMem stuff to ExtraFunctionality, where it belongs.

Revision 1.21  2008/02/06 00:12:55  danielpharos
The skinview now properly updates to reflect changes made to textures.

Revision 1.20  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

Revision 1.19  2007/11/21 00:06:22  danielpharos
BMP and PCX files are now also using DevIL and FreeImage to load and save. Also, fixed some memory-problems causing images to disappear.

Revision 1.18  2007/11/20 21:36:58  danielpharos
Fix paletted pictures not working correctly after prev. rev.

Revision 1.17  2007/11/20 18:28:06  danielpharos
Moved most of the DIB-calls to PixelSet, and added padding there. This should fix the few remaining image drawing issues.

Revision 1.16  2006/05/05 06:04:44  cdunde
To reverse Texture Memory changes. Cases problems with Quake 3 QkQ3.pas
handling of textures in the Texture Browser, hour glass icon jitters and memeor usage
increases causing prog crash, can not use scrole bar in TB.

Revision 1.15  2006/04/06 19:28:02  nerdiii
Texture memory wasn't freed because texture links had additional references to them.

Revision 1.14  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.12  2002/02/25 19:16:55  decker_dk
A possible memory leak solved? I'll wait for tiglari to hear what he finds out.
See also QuArK-Python forum date 2002.02.19 and 2002.02.25 subject "Leak hunt report".

Revision 1.11  2001/12/30 08:59:45  tiglari
add signature to decl of PSDConvert

Revision 1.10  2001/06/05 18:41:26  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.9  2001/03/20 21:44:37  decker_dk
Updated copyright-header

Revision 1.8  2000/07/18 19:38:00  decker_dk
Englishification - Big One This Time...

Revision 1.7  2000/04/24 09:54:54  arigo
Q3 shaders, once more

Revision 1.6  2000/04/22 08:56:48  arigo
Problems with texture sizes fixed

Revision 1.5  2000/04/18 18:47:57  arigo
Quake 3 : auto export shaders

Revision 1.4  2000/04/14 09:50:17  arigo
more TGA flips fix

Revision 1.3  2000/04/12 22:11:22  alexander
fixed: flipped exported TGA textures
}

unit QkPixelSet;

interface

uses SysUtils, Windows, Classes, Graphics, Game, Python, QkObjects, PyObjects,
     QkFileObjects;

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
                         procedure FlipBottomUp;
                         function IsGamePalette(Game: Char) : Boolean;
                         function GetNewDCImage : HDC;
                         (*function GetBitmapImage : TBitmap;*)
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
               procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
               procedure Paint(DC: HDC; X, Y: Integer);
               function PyGetAttr(attr: PChar) : PyObject; override;
               procedure ListDependencies(L: TStringList); virtual;
              end;

 {------------------------}

function PSDToDIB(const Source: TPixelSetDescription; BottomUp: Boolean) : TPixelSetDescription;
function PSDConvert(var Target: TPixelSetDescription;
                    const Source: TPixelSetDescription;
                    Flags: Integer) : Boolean;  { ccXXX }
function CreateToDC(DC: HDC; var BitmapInfo; Data: Pointer) : HBitmap;
procedure DrawToDC(DC: HDC; var BitmapInfo; Data: Pointer; Left, Top: Integer);

 {------------------------}

implementation

uses Controls, Dialogs, Quarkx, QkExceptions, QkTextures, CCode, QkExplorer,
     Setup, Logging, ExtraFunctionality;

 {------------------------}

procedure TPixelSetDescription.Init;
begin
 FillChar(Self, SizeOf(TPixelSetDescription), 0);

 {Decker 2002.02.25 - Can't use FillChar() on these member-variables.
  NIL is not equal to zero in Delphi!}
 Data := Nil;
 AlphaData := Nil;
 ColorPalette := Nil;

 {Decker 2002.02.25 - Don't know the value of an empty set, so I do the
  better thing: Initializing it properly.}
 Allocated := [];
end;

procedure TPixelSetDescription.Done;
begin
  if psaData in Allocated then
  begin
    FreeMem(Data);
    Data:=Nil;
    Exclude(Allocated, psaData);
  end;

  if psaAlpha in Allocated then
  begin
    FreeMem(AlphaData);
    AlphaData:=Nil;
    Exclude(Allocated, psaAlpha);
  end;

  if psaPalette in Allocated then
  begin
    Dispose(ColorPalette);
    ColorPalette:=Nil;
    Exclude(Allocated, psaPalette);
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
{$IFDEF DEBUG}
 if (psaData in Allocated) and (Data <> nil) then {Decker 2002.02.25 - Check for multiple function calls, which don't clean up memory}
   raise InternalE('TPixelSetDescription.AllocData(). Data is not NIL. Possible Memory Leak?');
{$ENDIF}
 GetMem(Data, Abs(ScanLine)*Size.Y);
 Include(Allocated, psaData);
end;

procedure TPixelSetDescription.AllocAlpha;
begin
{$IFDEF DEBUG}
 if (psaAlpha in Allocated) and (AlphaData <> nil) then {Decker 2002.02.25 - Check for multiple function calls, which don't clean up memory}
   raise InternalE('TPixelSetDescription.AllocAlpha(). AlphaData is not NIL. Possible Memory Leak?');
{$ENDIF}
 GetMem(AlphaData, Abs(AlphaScanLine)*Size.Y);
 Include(Allocated, psaAlpha);
end;

procedure TPixelSetDescription.AllocPalette;
begin
{$IFDEF DEBUG}
 if (psaPalette in Allocated) and (ColorPalette <> nil) then {Decker 2002.02.25 - Check for multiple function calls, which don't clean up memory}
   raise InternalE('TPixelSetDescription.AllocPalette(). ColorPalette is not NIL. Possible Memory Leak?');
{$ENDIF}
 New(ColorPalette);
 Include(Allocated, psaPalette);
end;

procedure TPixelSetDescription.FlipBottomUp;
begin
 ScanLine:=-ScanLine;
 AlphaScanLine:=-AlphaScanLine;
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

 {---------------}

(* PSDConvert will perform any necessary conversion automatically.
   Any field left to its default (null) value in the target object will
   be set to the value that makes the conversion easier (e.g. leaving
   Format to psfDefault will let PSDConvert set the target format equal
   to the source format). Any field already set in the target object
   will force conversion. *)

function PSDConvert(var Target: TPixelSetDescription;
                    const Source: TPixelSetDescription;
                    Flags: Integer) : Boolean;  { ccXXX }
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

function PSDToDIB(const Source: TPixelSetDescription; BottomUp: Boolean) : TPixelSetDescription;
begin
 Result.Init;
 case Source.Format of
  psf8bpp: Result.ScanLine:=((Source.Size.X+3) and not 3);
  psf24bpp: Result.ScanLine:=((Source.Size.X*3+3) and not 3);
 else
  raise InternalE('unknown PSD format');
 end;
 if BottomUp then
  Result.ScanLine:=-Result.ScanLine;
 PSDConvert(Result, Source, ccTemporary);
end;

function CreateToDC(DC: HDC; var BitmapInfo; Data: Pointer) : HBitmap;
var
  Width, Height: Integer;
begin
  Width:=TBitmapInfo(BitmapInfo).bmiHeader.biWidth;
  Height:=TBitmapInfo(BitmapInfo).bmiHeader.biHeight;
  Result:=CreateCompatibleBitmap(DC, Width, Height);
  if SetDiBits(DC, Result, 0, Height, Data, tagBITMAPINFO(BitmapInfo), DIB_RGB_COLORS) = 0 then
    Log(LOG_WARNING, 'SetDiBits: Returned zero!');
end;

procedure DrawToDC(DC: HDC; var BitmapInfo; Data: Pointer; Left, Top: Integer);
var
  Width, Height, BBP, ScanWidth: Integer;
  DIBSection: HGDIOBJ;
  Bits: Pointer;
begin
  DIBSection:=CreateDIBSection(DC, tagBITMAPINFO(BitmapInfo), DIB_RGB_COLORS, Bits, 0, 0);
  if DIBSection = 0 then
  begin
    LogWindowsError(GetLastError(), 'CreateDIBSection(DC, tagBITMAPINFO(BitmapInfo), DIB_RGB_COLORS, Bits, 0, 0)');
    LogAndRaiseError('CreateDIBSection failed!');
  end;
  try
    Width:=TBitmapInfo(BitmapInfo).bmiHeader.biWidth;
    Height:=TBitmapInfo(BitmapInfo).bmiHeader.biHeight;
    BBP:=TBitmapInfo(BitmapInfo).bmiHeader.biBitCount;
    ScanWidth:=(((Width * BBP) + 31) div 32) * 4;  //Scanline size in bytes, with padding
    CopyMemory(Bits, Data, ScanWidth * Height);
    if SetDIBitsToDevice(DC, Left, Top, Width, Height, 0, 0, 0, Height, Bits,
     tagBITMAPINFO(BitmapInfo), DIB_RGB_COLORS) = 0 then
      Log(LOG_WARNING, 'SetDIBitsToDevice: Returned zero!');
  finally
    DeleteObject(DIBSection);
  end;
end;

 {---------------}

procedure TPixelSetDescription.Paint(DC: HDC; X, Y: Integer);
var
 BmpInfo: TBitmapInfo256;
 BitmapInfo: PBitmapInfo;
 NewPSD: TPixelSetDescription;
 Pal0, Pal1: HPalette;
begin
 Pal1:=0;
 Pal0:=0;
 NewPSD:=PSDToDIB(Self, True);
 try
  BitmapInfo:=PBitmapInfo(NewPSD.GetBitmapInfo(BmpInfo, @Pal0));
  if Pal0<>0 then
   begin
    Pal1:=SelectPalette(DC, Pal0, False);
    RealizePalette(DC);
   end;
  DrawToDC(DC, BitmapInfo^, NewPSD.Data, X, Y);
 finally
  if Pal1<>0 then
   SelectPalette(DC, Pal1, False);
  NewPSD.ReleasePalette(Pal0);
  NewPSD.Done;
 end;
end;

function TPixelSetDescription.GetNewDCImage : HDC;
var
 BmpInfo: TBitmapInfo256;
 BitmapInfo: TBitmapInfo256;
 NewPSD: TPixelSetDescription;
 DesktopWindow: HWND;
 DC: HDC;
begin
 NewPSD:=PSDToDIB(Self, True);
 try
  DesktopWindow:=GetDesktopWindow;
  DC:=GetDC(DesktopWindow);
  try
   BitmapInfo:=NewPSD.GetBitmapInfo(BmpInfo, Nil)^;
   Result:=CreateToDC(DC, BitmapInfo, NewPSD.Data);
  finally
   ReleaseDC(DesktopWindow, DC);
  end;
 finally
  NewPSD.Done;
 end;
end;

(*function TPixelSetDescription.GetBitmapImage : TBitmap;
var
 BmpInfo: TBitmapInfo256;
 BitmapInfo: TBitmapInfo256;
 NewPSD: TPixelSetDescription;
 DC: HDC;
begin
 NewPSD:=PSDToDIB(Self, True);
 DC:=GetDC(GetDesktopWindow);
 try
  BitmapInfo:=NewPSD.GetBitmapInfo(BmpInfo, Nil)^;
  Result:=TBitmap.Create;
  Result.Handle:=CreateToDC(DC, BitmapInfo, NewPSD.Data);
 finally
  ReleaseDC(GetDesktopWindow, DC);
  NewPSD.Done;
 end;
end;*)

 {------------------------}

function QPixelSet.GetSize : TPoint;
var
 V: array[1..2] of Single;
begin
 if not LoadPixelSet.GetFloatsSpec('Size', V) then
  begin
   Acces;   { Maybe the object was not loaded ? Try again. }
   if not LoadPixelSet.GetFloatsSpec('Size', V) then
    Raise EErrorFmt(5534, ['Size']);
  end;
 Result.X:=Round(V[1]);
 Result.Y:=Round(V[2]);
end;

procedure QPixelSet.SetSize(const nSize: TPoint);
var
 V: array[1..2] of Single;
begin
 Acces;
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

procedure QPixelSet.OperationInScene(Aj: TAjScene; PosRel: Integer);
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
     WE:=g_WorkingExplorer; try
     OperationDansScene(Lnk, asModifie, Nil);
     finally g_WorkingExplorer:=WE; end;
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

procedure QPixelSet.ListDependencies(L: TStringList);
begin  { no dependency by default -- see QkTextures.pas }
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

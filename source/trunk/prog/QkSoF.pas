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
unit QkSoF;

interface

uses SysUtils, Classes, QkObjects, QkFileObjects, QkImages, Dialogs;

type
 QM32 = class(QImage)
        protected
          class function FormatName : String; override;
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
          procedure CheckTexName(const nName: String); //FIXME: Should probably inherit from QTexture...
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

implementation

uses StrUtils, Windows, Setup, Travail, Quarkx, QkExceptions,
     QkPixelSet, QkObjectClassList, ExtraFunctionality;

const
 MIP32_VERSION = 4;
 MIPLEVELS = 16;
 MAX_OSPATH = 128; //max length of a filesystem pathname

type
 TM32Header = packed record
                Id: LongInt;                                // id / version
                Name: array[0..MAX_OSPATH-1] of Char;       // texture name
                Altname: array[0..MAX_OSPATH-1] of Char;    // texture substitution
                Animname: array[0..MAX_OSPATH-1] of Char;   // next frame in animation chain
                Damagename: array[0..MAX_OSPATH-1] of Char; // image that should be shown when damaged
                Width, Height, Offsets: array[0..MIPLEVELS-1] of LongInt; //width, height, offsets of all miplevels
                Flags: LongInt;
                Contents: LongInt;
                Value: LongInt;
                Scale_x, Scale_y: Single;
                Mip_scale: LongInt;

                // detail texturing info
                dt_name: array[0..MAX_OSPATH-1] of Char; // detailed texture name
                dt_scale_x, dt_scale_y: Single;
                dt_u, dt_v: Single;
                dt_alpha: Single;
                dt_src_blend_mode, dt_dst_blend_mode: LongInt;

                flags2: LongInt;
                damage_health: Single;

                unused: array[0..17] of LongInt; // future expansion to maintain compatibility with h2
              end;

Procedure QM32.SaveFile(Info: TInfoEnreg1);
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
const
  spec1='Image1=';
  spec2='Alpha=';
var
  LineWidth, J, K: Integer;
  m32header: TM32Header;
  ScanLine, AlphaScanLine: PByte;
  PSD,OldPSD: TPixelSetDescription;
  PBaseLineBuffer,PLineBuffer: PByte;
  SourceRGB: PRGB;
  SourceAlpha: PByte;
begin
 with Info do case Format of
  rf_Default: begin  { as stand-alone file }
    FillChar(m32header, sizeOf(m32header), 0);
    PSD.Init;
    OldPSD:=Description;
    try
      PSD.Format:=psf24bpp;  { force to 24bpp }
      PSD.AlphaBits:=psa8bpp;  { force to 8bpp alpha }
      PSDConvert(PSD, OldPSD, ccTemporary);
     { use PSD here, it is guaranteed to be 24bpp + 8bpp alpha }

      m32header.Id:=MIP32_VERSION;
      StrPCopy(m32header.Name, Name); //@Need to verify string length!
      StrPCopy(m32header.Altname, Specifics.Values['Texture_Substitution_Path']);
      StrPCopy(m32header.Animname, Specifics.Values['Next_Frame_Path']);
      StrPCopy(m32header.Damagename, Specifics.Values['Damage_Texture_Path']);
      m32header.scale_x:=1.0;
      m32header.scale_y:=1.0;
      m32header.Contents:=StrToIntDef(Specifics.Values['Contents'], 0);
      m32header.Flags   :=StrToIntDef(Specifics.Values['Flags'], 0);
      m32header.Value   :=StrToIntDef(Specifics.Values['Value'], 0);

      with PSD.Size do
      begin
        m32header.Width[0]:=X;
        m32header.Height[0]:=Y;
        m32header.Offsets[0]:=sizeOf(m32header);
      end;
      F.WriteBuffer(m32header, sizeOf(m32header));

      LineWidth:= m32header.Width[0] * 4;  { 4 bytes per line (32 bit)}
      ScanLine:=PByte(PSD.StartPointer);
      AlphaScanLine:=PByte(PSD.AlphaStartPointer);
      GetMem(PBaseLineBuffer, LineWidth);
      try
        for J:=1 to m32header.Height[0] do {iterate lines}
        begin
          PLineBuffer:=PBaseLineBuffer;
          SourceRGB:=PRGB(ScanLine);
          SourceAlpha:=PByte(AlphaScanLine);
          for K:=1 to m32header.Width[0] do { mix color and alpha line-by-line }
          begin
            PLineBuffer^:=SourceRGB^[2];  {rgb -> bgr  }
            Inc(PLineBuffer, 1);
            PLineBuffer^:=SourceRGB^[1];  {rgb -> bgr  }
            Inc(PLineBuffer, 1);
            PLineBuffer^:=SourceRGB^[0];  {rgb -> bgr  }
            Inc(PLineBuffer, 1);
            Inc(SourceRGB);
            PLineBuffer^:=SourceAlpha^; {inject alpha after RGB}
            Inc(PLineBuffer, 1);
            Inc(SourceAlpha);
          end;
          F.WriteBuffer(PBaseLineBuffer^, LineWidth);
          Inc(ScanLine, PSD.ScanLine);
          Inc(AlphaScanLine, PSD.AlphaScanLine);
        end;
      finally
        FreeMem(PBaseLineBuffer);
      end;
    finally
      OldPSD.Done;
      PSD.Done;
    end;
  end;
 end;
end;

procedure QM32.CheckTexName;
var
  TexName: String;
begin
  //Copied and slightly modified from QkTextures!
  if SetupSubSet(ssFiles, 'Textures').Specifics.Values['TextureNameCheck']<>'' then
  begin
    TexName := Name;
    if ((nName = '') or (TexName = '')) and (SetupSubSet(ssFiles, 'Textures').Specifics.Values['TextureEmptyNameValid']<>'') then
      Exit;
    if CompareText(nName, TexName)<>0 then
      GlobalWarning(FmtLoadStr1(5569, [nName, TexName]));
  end;
end;

Procedure ReadRGBA(F: TStream; var rgb, a: string; width, height: integer);
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
const
  spec1='Image1=';
  spec2='Alpha=';
var
  RawData, Image_Buffer, Alpha_Buffer: String;
  ScanLine, Dest, Source, AlphaBuf: PChar;
  I, J, ScanW, sScanW: Integer;
begin
  {read into rawdata string}
  I:=Width*(32 div 8);    { bytes per line in the .m32 file }
  ScanW:=(I+3) and not 3; { the same but rounded up, for storing the data }
  RawData:=Spec1;
  J:=ScanW*Height;       { total byte count for storage }
  SetLength(RawData, Length(Spec1)+J);
  ScanLine:=PChar(RawData)+Length(RawData)-ScanW;
  sScanW:=-ScanW;
  for J:=1 to Height do
  begin
    F.ReadBuffer(ScanLine^, I);
    if I<ScanW then
      FillChar(ScanLine[I], ScanW-I, 0);  { pad with zeroes }
    Inc(ScanLine, sScanW);
  end;

  {prepare alpha buffer
   It is assumed to be one byte per pixel if available.
   It was loaded together with the image data into 'RawData',
   but 'RawData' must now be split into two buffers : one for the image colors
   and one for the alpha channel.}
  alpha_buffer:=Spec2;
  J:=Width*Height;       { pixel count }
  Setlength(alpha_buffer,Length(Spec2)+ J);

  {prepare image buffer}
  Image_Buffer:=Spec1;
  SetLength(Image_Buffer, Length(Spec1)+ 3*J);

  {split ABGR into RGB and Alpha}
  Source:=PChar(RawData)+Length(Spec1);
  Dest:=PChar(Image_Buffer)+Length(Spec1);
  AlphaBuf:=PChar(alpha_buffer)+Length(Spec2);
  for I:=1 to J do
  begin
    PRGB(Dest)^[2]:=PRGB(Source)^[0];  {bgr -> rgb  }
    PRGB(Dest)^[1]:=PRGB(Source)^[1];  {bgr -> rgb  }
    PRGB(Dest)^[0]:=PRGB(Source)^[2];  {bgr -> rgb  }
    AlphaBuf^:=Source[3];      { alpha }
    Inc(Dest, 3);
    Inc(Source, 4);
    Inc(AlphaBuf);
  end;
  a:=Alpha_Buffer;
  rgb:=Image_Buffer;
end;

Procedure QM32.LoadFile(F: TStream; FSize: Integer);
var
  m32header: TM32Header;
  org: Int64;
  S: string;
  I: Integer;
  rgb, a: string;
  V: array[1..2] of Single;
begin
 case ReadFormat of
  rf_Default: begin  { as stand-alone file }
     org:=F.Position;
     F.readbuffer(m32header, sizeof(m32header));
     if m32header.Id <> MIP32_VERSION then
       raise Exception.Create('Not a valid m32 file!');
     Specifics.Add(format('Texture_Path=%s',[m32header.Name]));

     //Verify if the texturename matches the filename
     S:=m32header.Name;
     for I:=Length(S) downto 1 do
     begin
       if S[I]='/' then
       begin
         Specifics.Add('Path='+Copy(S,1,I-1));
         S:=RightStr(S, Length(S)-I);
         Break;
       end;
     end;
     CheckTexName(S);

     Specifics.Add(format('Texture_Substitution_Path=%s',[m32header.Altname]));
     Specifics.Add(format('Next_Frame_Path=%s',[m32header.Animname]));
     Specifics.Add(format('Damage_Texture_Path=%s',[m32header.Damagename]));
     Specifics.Add(format('Contents=%d',[m32header.contents]));
     Specifics.Add(format('Flags=%d',[m32header.flags]));
     Specifics.Add(format('Value=%d',[m32header.value]));

     V[1]:=m32header.Width[0];
     V[2]:=m32header.Height[0];
     SetFloatsSpec('Size', V);

     F.Position:=org+m32header.Offsets[0];
     ReadRGBA(f, rgb, a, m32header.Width[0], m32header.Height[0]);

     specifics.add(rgb);
     specifics.add(a);
  end;
 else inherited;
 end;
end;

class function QM32.FormatName : String;
begin
 Result:='M32';
end;

class function QM32.Typeinfo: String;
begin
  Result:='.m32';
end;

class Procedure QM32.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5177);
  Info.FileExt:=806;
  Info.WndInfo:=[wiWindow];
end;

initialization
  RegisterQObject(QM32, 'l');
end.

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
Revision 1.8  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.7  2009/02/21 17:10:12  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.6  2007/09/12 15:39:51  danielpharos
Small file cleanup.

Revision 1.5  2007/02/06 13:08:47  danielpharos
Fixes for transparency. It should now work (more or less) correctly in all renderers that support it.

Revision 1.4  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.2  2003/01/01 13:49:56  decker_dk
added cvs headers
}

unit QkKingPin;

interface

uses Windows, SysUtils, Classes, Graphics, Dialogs, Controls,
     QkObjects, QkFileObjects, QkTextures, QkPak, qmath, QkImages, QkTGA;

type

QTextureKP = class(QTextureFile)
  private
{    source_image : QFileObject;}
{    static global_palette : QImage;}
  protected
    procedure Enregistrer(Info: TInfoEnreg1); override;
    procedure Charger(F: TStream; Taille: Integer); override;
  public
{    constructor Create(const nName: String; nParent: QObject);
    destructor Destroy; override;
}
    class function TypeInfo: String; override;
    class function CustomParams : Integer; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;

    function CheckAnim(Seq: Integer) : String; override;
    function GetTexOpacity(var Info: TTexOpacityInfo) : Integer; override;
    function BaseGame : Char; override;
    function GetTexName : String; override;
  end;

 {------------------------}


implementation

uses Game, Setup, Quarkx;


 {------------------------}
{
constructor QTextureKP.Create(const nName: String; nParent: QObject);
begin
  self.source_image := nil;
  inherited Create( nName,nParent);
end;
}
{
destructor QTextureKP.Destroy;
begin
  if  not (self.source_image = nil) then
    self.source_image.Destroy;
  inherited
end;
}
class function QTextureKP.TypeInfo: String;
begin
  TypeInfo:='.tga';
end;

class procedure QTextureKP.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.NomClasseEnClair:=LoadStr1(5168);
  Info.FileExt:=796;
end;

class function QTextureKP.CustomParams : Integer;
begin
  {this really sucks, but the texture display code
  requires multiple texure images, and we need to fake them}
  Result:=4 or cpAnyHeight;
end;

function QTextureKP.BaseGame : Char;
begin
  Result:=mjKingPin;
end;

function QTextureKP.CheckAnim(Seq: Integer) : String;
begin
  Result:=Specifics.Values['Anim'];
end;

function QTextureKP.GetTexName : String;
var
  S: String;
  J: Integer;
begin
  Result:=Name;
  S:=Specifics.Values['Path'];
  if S<>'' then
  begin
    J:=Length(Result);
    while (J>0) and (Result[J]<>'/') do
      Dec(J);
    if S[Length(S)]<>'/' then
      S:=S+'/';
    Result:=S+Copy(Result, J+1, MaxInt);
  end;
end;

function QTextureKP.GetTexOpacity(var Info: TTexOpacityInfo) : Integer;
var
  S: String;
begin
  S:=Specifics.Values['Contents'];
  if S='' then
    Result:=255
  else
    Result:=OpacityFromFlags(StrToIntDef(S,0), Info);
end;

procedure QTextureKP.Charger(F: TStream; Taille: Integer);
const
  Spec1 = 'Image#=';
  PosNb = 6;

  DataSpec = 'Data=';
  AlphaSpec = 'Alpha=';
var
  tex_size: TPoint;
  SourceStream: TQStream;
  source_image: QImage;
  V: array[1..2] of Single;
  S,tga_pixeldata,tga_alphadata: String;
  I, bytesize, Flags ,cnt,h,w,sourcewidth: Integer;


begin
  case ReadFormat of
    rf_Default:
    begin  { as stand-alone file }
      Flags:=CustomParams;

      {create appropiate image object}
      s:=GetSpecArg('Image1');
      {is the image there ?}
      if length(s) = Length(spec1) then
      begin
        source_image :=QTGA.Create(self.NomFichier,self);
        source_image.ReadFormat := ReadFormat;
        SourceStream:=AccesFichierQ(self.NomFichier, []);
        SourceStream.AddRef;
        try
          source_image.Ouvrir(SourceStream, SourceStream.Size);
          (source_image as QTGA).Acces;
        finally
          SourceStream.Release;
        end;

        {invoke fake charger}
        {size specific}
        tex_size := (source_image as QImage).getsize;
        V[1]:=tex_size.x;
        V[2]:=tex_size.y;
        SetFloatsSpec('Size', V);

        {path specific}
        S:=source_image.name;
        for I:=Length(S) downto 1 do
        begin
          if S[I]='/' then
          begin
            Specifics.Add('Path='+Copy(S,1,I-1));
            Break;
          end;
        end;

        CheckTexName(S);

        {image# specific}
        {0 : unscaled
         1 : halve
         2 : quarter
         3 : 1/8th}
        sourcewidth:=tex_size.x;
        for I:=0 to (Flags and cpIndexesMax)-1 do
        begin
          S:=Spec1;
          S[PosNb]:=ImgCodes[I];
          Bytesize:=tex_size.x*tex_size.y;
          SetLength(S, Length(Spec1) +
                       Bytesize  ); {}
          {read and convert the stuff}
          tga_pixeldata:=source_image.GetSpecArg('Image1');
          tga_alphadata:=source_image.GetSpecArg('Alpha');

          {for testing only - convert this to palette !}
          if I=0 then
          begin   {make non scaled image}
            for cnt:=0 to bytesize-1 do
            begin
              S[Length(Spec1)+1 + cnt] := char (
               ( ord (tga_pixeldata[Length(Spec1)+1 + cnt*3 + 0 ]) +  {r}
                 ord (tga_pixeldata[Length(Spec1)+1 + cnt*3 + 1 ]) +  {g}
                 ord (tga_pixeldata[Length(Spec1)+1 + cnt*3 + 2 ]))     {b} div 32 );
            end;
          end
          else
          begin   {make scaled image}
            cnt:=0;
            for h:=0 to tex_size.y-1 do
            begin
              for w:=0 to tex_size.x-1 do
              begin
                S[Length(Spec1)+1 + cnt] := char (
                  ( ord (tga_pixeldata[Length(Spec1)+1 + (h*(i+1)*sourcewidth +w*(i+1))*3 + 0 ]) +
                    ord (tga_pixeldata[Length(Spec1)+1 + (h*(i+1)*sourcewidth +w*(i+1))*3 + 1 ]) +
                    ord (tga_pixeldata[Length(Spec1)+1 + (h*(i+1)*sourcewidth +w*(i+1))*3 + 2 ]) )  div 32 );
                inc(cnt);
              end;
            end;
          end;

          Specifics.Add(S);

          if not ScaleDown(Integer(tex_size.x),Integer(tex_size.y)) then
            Break;
        end;

        {too bloody difficult to do this here , it needs the texture link:-)
         and is done in textures.pas
        Specifics.Add('Contents='+IntToStr(0));
        Specifics.Add('Flags='+IntToStr(0));
        Specifics.Add('Value='+IntToStr(0));}

        {free the temporary image object}
        source_image.destroy;
      end;{if image specific not exists}
     end; {read format = 1}
    else {case read format}
      inherited;
  end;
end;

{ tiglari:  Not sure what this is actually for, tho it seems to write
   info in the object-specific format to the header format.

  mac: dont know it, too :)}
procedure QTextureKP.Enregistrer;
var
  S: String;
  Header: TQ2Miptex;
  I: Integer;
begin
  with Info do
    case Format of
      rf_Default:
      begin  { as stand-alone file }
        Header:=BuildQ2Header;
        F.WriteBuffer(Header, SizeOf(Header));
        for I:=0 to 3 do
        begin
          S:=GetTexImage(I);
          F.WriteBuffer(S[1], Length(S));
        end;
      end;
    else
      inherited;
  end;{with info do}
end;

 {------------------------}

initialization
  RegisterQObject(QTextureKP, 'l');
end.

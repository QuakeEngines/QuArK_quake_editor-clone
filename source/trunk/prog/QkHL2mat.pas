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
Revision 1.7  2005/07/05 19:12:47  alexander
logging to file using loglevels

Revision 1.6  2005/07/04 18:53:20  alexander
changed steam acces to be a protocol steamaccess://

Revision 1.5  2005/07/03 20:20:41  alexander
better exception forwarding from load material to display of image
fixed problem with different material path lengths (decals folder)
accept $envmap  as image source

Revision 1.4  2005/06/24 00:06:32  alexander
tricky derive vtf base path from file path

Revision 1.3  2005/06/23 23:41:09  alexander
no image bug fixed

Revision 1.2  2005/06/22 17:26:29  alexander
parsing handles more cases, but not all - still alpha status

Revision 1.1  2005/06/22 01:19:40  alexander
added hl2 material source

}

unit QkHL2mat;

interface

uses
  SysUtils, Windows, Classes, QkFileObjects, Quarkx, QkObjects, QkText,
   QkTextures, Setup, QkWad, QkPixelSet,QkVTF,Logging;

type
  QHL2Mat = class(QPixelSet)
            protected
              DefaultImageCache : QPixelSet;
              ErrorText : String;
            public
              procedure LoadFile(F: TStream; FSize: Integer); override;
              class function TypeInfo: String; override;
              class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
              function DefaultImage : QPixelSet;
              function GetSize : TPoint; override;
              procedure SetSize(const nSize: TPoint); override;
              function Description : TPixelSetDescription; override;
              function SetDescription(const PSD: TPixelSetDescription;
                                      Confirm: TSDConfirm) : Boolean; override;
              procedure ListDependencies(L: TStringList); override;
            end;


implementation

uses Game, Travail, QkObjectClassList;


{------------------------}

class function QHL2Mat.TypeInfo;
begin
 Result:='.vmt';
end;

class procedure QHL2Mat.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5716);
 Info.FileExt:=815;
 Info.WndInfo:=[wiWindow];
end;

function QHL2Mat.DefaultImage : QPixelSet;
var
 Image: QPixelSet;
 i:integer;
 noimage:boolean;
begin
  Result :=nil;
  Acces;
  noimage:=true;
  for I:=0 to SubElements.Count-1 do
  begin
    if SubElements[I] is qpixelset then
    begin
      image := SubElements[I] as qpixelset;
      Result := image;
      noimage:=false;
      break;
    end;
  end;
  if noimage then
  begin
    if ErrorText = '' then
      Raise EErrorFmt(5695, [Name])
    else
      Raise Exception.Create(ErrorText);
  end;
{  else
  begin
     Size:=Result.GetSize;
     V[1]:=Size.X;
     V[2]:=Size.Y;
     SetFloatsSpec('Size',V);
   end;
 }
end;



function QHL2Mat.GetSize : TPoint;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5534, ['Size']);
 Image.Acces;
 Result:=Image.GetSize;
end;

function QHL2Mat.Description : TPixelSetDescription;
var
 Image: QPixelSet;
begin
  image:=defaultimage;
  if assigned(image ) then
    Result := image.description
  else
    Raise EErrorFmt(5695, [Name]);
end;

procedure QHL2Mat.SetSize;
begin
 Raise EError(5696);
end;

function QHL2Mat.SetDescription;
begin
 Raise EError(5696);
end;

procedure QHL2Mat.ListDependencies(L: TStringList);
var
 I: Integer;
 S, SpecialStage: String;
begin
  Acces;
  SpecialStage:=LoadStr1(5699);
  for I:=0 to SubElements.Count-1 do
  begin
    S:=SubElements[I].Name;
    { to do: check for animated stages }
    if (S<>'') and (S[1]<>'$') and (S<>SpecialStage) then
      L.Add(#255+S);   { #255 means it is not a texture name but directly a file name }
  end;
end;


procedure QHL2Mat.LoadFile(F: TStream; FSize: Integer);
var
  Data: String;
  Source: PChar;

const
 cSeperators = [' ', #13, #10, Chr(vk_Tab)];
 cExponentChars = ['E', 'e'];
 Granularite = 8192;
 FinDeLigne = False;

type
 TSymbols = (sEOF,
             sBracketLeft,
             sBracketRight,
             sCurlyBracketLeft,
             sCurlyBracketRight,
             sSquareBracketLeft,
             sSquareBracketRight,
             sStringToken,
             sStringQuotedToken,
             sNumValueToken,
             sTokenForcedToString);
var
 SymbolType: TSymbols;
 S: String;
 NumericValue: Double;

 LineNoBeingParsed: Integer;
 Juste13,  ReadSymbolForceToText: Boolean;


 procedure ReadSymbol(PrevSymbolMustBe: TSymbols);
{
ReadSymbol(PrevSymbolMustBe : TSymbols) reads the next token, and checks
whether the previous is what PrevSymbolMustBe says it should have been.
This can also be checked by examining SymbolType, if there are
several possibilities.

   "SymbolType" contains the kind of token just read :
   "sStringToken": a string token, whose value is in "S"
   "sStringQuotedToken": a quote-delimited string, whose value is in "S"
   "sNumValueToken": a floating-point value, found in "NumericValue"

Call the procedure "ReadSymbol()" to get the next token. The argument to the
procedure is the current token kind again; useful to read e.g. three
floating-point values :

   FirstValue:=NumericValue;
   ReadSymbol(sNumValueToken);
   SecondValue:=NumericValue;
   ReadSymbol(sNumValueToken);
   ThirdValue:=NumericValue;
   ReadSymbol(sNumValueToken);

This way, the procedure "ReadSymbol" checks that the kind of token was really the
expected one.
}
 var
   C: Char;
   Arret: Boolean;

   procedure ReadStringToken();
   begin
     S:='';
     repeat
       S:=S+C;
       C:=Source^;
       if C=#0 then
         Break;
       Inc(Source);
     until C in cSeperators;
     if (C=#13) or ((C=#10) {and not Juste13}) then
       Inc(LineNoBeingParsed);
     Juste13:=C=#13;
     SymbolType:=sStringToken;
   end;

 begin
   repeat
     if (SymbolType<>PrevSymbolMustBe) and (PrevSymbolMustBe<>sEOF) then
       Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(248)]);

     repeat
       C:=Source^;
       if C=#0 then
       begin
         SymbolType:=sEOF;
         Exit;
       end;

       Inc(Source);
       if (C=#13) or ((C=#10) and not Juste13) then
         Inc(LineNoBeingParsed);
       Juste13:=C=#13;
     until not (C in cSeperators);


     if ReadSymbolForceToText then
     begin
       ReadStringToken();
       SymbolType:=sTokenForcedToString;
       Exit;
     end;

     Arret:=True;

     case C of
     '(': SymbolType:=sBracketLeft;
     ')': SymbolType:=sBracketRight;
     '{': SymbolType:=sCurlyBracketLeft;
     '}': SymbolType:=sCurlyBracketRight;
     '[': SymbolType:=sSquareBracketLeft;
     ']': SymbolType:=sSquareBracketRight;

     '"':
       begin
         S:='';
         repeat
           C:=Source^;
           if C in [#0, #13, #10] then
           begin
             if FinDeLigne and (S<>'') and (S[Length(S)]='"') then
             begin
               SetLength(S, Length(S)-1);
               Break;
             end
             else
               Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(249)]);
           end;

           Inc(Source);
           if (C='"') and not FinDeLigne then
             Break;
           S:=S+C;
         until False;
         SymbolType:=sStringQuotedToken;
       end;

     '-', '0'..'9','.':
       begin
         if (C='-') and not (Source^ in ['0'..'9','.']) then
           ReadStringToken()
         else
         begin
           S:='';
           repeat
             S:=S+C;
             C:=Source^;
             if C=#0 then
               Break;
             Inc(Source);
           until not (C in ['0'..'9', '.']);

           { Did we encounter a exponent-value? Something like: "1.322e-12"
             Then continue to read the characters }
           if (C in cExponentChars) then
           begin
             repeat
               S:=S+C;
               C:=Source^;
               if C=#0 then
                 Break;
               Inc(Source);
             until not (C in ['0'..'9', '-', '+']);
           end;

           if (C=#0) or (C in cSeperators) then
           begin
             if (C=#13) or ((C=#10) {and not Juste13}) then
               Inc(LineNoBeingParsed);
             Juste13:=C=#13;
             NumericValue:=StrToFloat(S);
             SymbolType:=sNumValueToken;
           end
           else
             Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(251)]);
         end;
       end;

     '/', ';':
       begin
         if (C=';') or (Source^='/') then
         begin
           if C=';' then
             Dec(Source);

           Inc(Source);
           repeat
             C:=Source^;
             if C=#0 then
               Break;
             Inc(Source);
           until C in [#13, #10];

           if (C=#13) or ((C=#10) {and not Juste13}) then
             Inc(LineNoBeingParsed);

           Juste13:=C=#13;
           Arret:=False;
         end
         else
           Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(248)]);
       end;

     else
       ReadStringToken();
     end;
   until Arret;
 end;


  // we use this for all hierachic info that we
 // just want to read and skip. It allows
 // mixed quoted string pairs "a" "b"
 // and named objects   name { }
 // example
 // "a" { "b" "c" sub { "sa" "sb" } "d" "e" }

 procedure ReadHL2Sub;
 begin
   ReadSymbol(sCurlyBracketLeft);
   begin
     while SymbolType<>sCurlyBracketRight do
     begin
       if SymbolType=sStringQuotedToken then
         ReadSymbol(sStringQuotedToken)
       else
       if (SymbolType = sStringToken)  then
         ReadSymbol(sStringToken)
       else
       if (SymbolType = sNumValueToken)  then
         ReadSymbol(sNumValueToken)
       else
       if SymbolType=sCurlyBracketLeft then
         ReadHL2Sub
       else
         raise EErrorFmt(254, [LineNoBeingParsed, 'unknown thing']);
     end;
     ReadSymbol(sCurlyBracketRight);
   end;
 end;
 procedure ReadHL2GenericHierarchy;
 begin
   if SymbolType = sStringQuotedToken then
     ReadSymbol(sStringQuotedToken)
   else
     ReadSymbol(sStringToken);
   ReadHL2Sub;
 end;


 procedure ReadHL2Mat;
 var
   S1,path: String;
   VTFImage:QVTF;
   p:QObject;

 begin
   VTFImage:=nil;
   if SymbolType = sStringQuotedToken then
     ReadSymbol(sStringQuotedToken)
   else
     ReadSymbol(sStringToken);

   Log(LOG_VERBOSE,'material '+self.name);

   ReadSymbol(sCurlyBracketLeft);
   // read attributes of entity
   while SymbolType<>sCurlyBracketRight do
   begin

     if (SymbolType = sStringQuotedToken) or (SymbolType = sStringToken) then
     begin
       S1:=S;
       if SymbolType = sStringToken then
         ReadSymbol(sStringToken);
       if SymbolType = sStringQuotedToken then
         ReadSymbol(sStringQuotedToken);

       if (SymbolType = sNumValueToken)  then
       begin
         self.Specifics.Add(S1+'='+FloatToStr(NumericValue));
         Log(LOG_VERBOSE,'  attribute'+S1+' '+S);
         ReadSymbol(sNumValueToken);
       end
       else
         if SymbolType = sStringQuotedToken then
         begin
           self.Specifics.Add(S1+'='+S);
           Log(LOG_VERBOSE,'  attribute'+S1+' '+S);
           ReadSymbol(sStringQuotedToken);
         end
         else
           if SymbolType=sCurlyBracketLeft then
             ReadHL2Sub; //descend
     end
     else
       raise EErrorFmt(254, [LineNoBeingParsed, 'unknown thing']);

   end; //while SymbolType<>sCurlyBracketRight

   ReadSymbol(sCurlyBracketRight);


  p:=self;
  while p.FParent<>nil do
  begin
    if (p<>nil) and (p.fparent<>nil) and (p.fparent.fparent<>nil) and (p.fparent.fparent.fparent=nil)then
      path:=p.fparent.name+'/'+p.name;
    p:=p.FParent;
  end;

  S:=Specifics.Values['%tooltexture'];
  if (s<>'') then
    try
      Log(LOG_VERBOSE,'attempt %tooltexture '+S);
      VTFImage:=NeedGameFileBase(self.protocol+p.name, path + '/' + s + '.vtf') as QVTF;
    except
      VTFImage:=nil;
    end;

  S:=Specifics.Values['$basetexture'];
  if (VTFImage=nil) and (s<>'') then
    try
      Log(LOG_VERBOSE,'attempt $basetexture '+S);
      VTFImage:=NeedGameFileBase(self.protocol+p.name, path + '/' + s + '.vtf') as QVTF;
    except
      VTFImage:=nil;
    end;

  S:=Specifics.Values['$envmap'];
  if (VTFImage=nil) and (s<>'') then
    try
      Log(LOG_VERBOSE,'attempt $envmap '+S);
      VTFImage:=NeedGameFileBase(self.protocol+p.name, path + '/' + s + '.vtf') as QVTF;
    except
      VTFImage:=nil;
    end;

  S:=Specifics.Values['$modelmaterial'];
  if (VTFImage=nil) and (s<>'') then
    try
      Log(LOG_VERBOSE,'attempt $modelmaterial '+S);
      VTFImage:=NeedGameFileBase(self.protocol+p.name, path + '/' + s + '.vtf') as QVTF;
    except
      VTFImage:=nil;
    end;

  if vtfImage<>nil then
  begin
    Log(LOG_VERBOSE,'image found '+S);
    VTFImage.Acces;
    SubElements.Add(VTFImage);
    VTFImage.fparent:=self;
  end
  else
    Log(LOG_WARN,'no image found in material '+self.name);

 end;


begin
  case ReadFormat of
    1:
    begin  { as stand-alone file }
      try
        LogEx(LOG_VERBOSE,'load material %s',[self.name]);
        SetLength(Data, FSize);
        Source:=PChar(Data);
        F.ReadBuffer(Source^, FSize);  { read the whole file at once }

        ReadSymbolForceToText:=False;
        LineNoBeingParsed:=0;
        ReadSymbol(sEOF);

        while SymbolType<>sEOF do
          while (SymbolType=sStringQuotedToken) or (SymbolType=sStringToken )do
            ReadHL2Mat
      except
        on E: Exception do
        begin
          ErrorText:= E.Message;
          LogEx(LOG_WARN,'load material %s err %s',[self.Name,E.Message]);
        end;
      end;
    end;
  else
    inherited;
  end;
end;


 {------------------------}

initialization
  RegisterQObject(QHL2Mat, 'v');
end.


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
}

unit QkQ3;

interface

uses
  SysUtils, Windows, Classes, QkZip2, QkFileObjects, Quarkx, QkObjects, QkText,
  QkJpg, QkTextures, Setup, QkWad, QkPixelSet;

type
  Q_CFile = class(QCfgFile)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         procedure EtatObjet(var E: TEtatObjet); override;
        end;
  Q_HFile = class(QCfgFile)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         procedure EtatObjet(var E: TEtatObjet); override;
        end;
  Q3Pak = class(QZipPak)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
  QShader = class(QPixelSet)
            public
              class function TypeInfo: String; override;
              {procedure DataUpdate;}
              function DumpString : String;
              function DefaultImage : QPixelSet;
              {procedure OpDansScene(Aj: TAjScene; PosRel: Integer); override;}
              function GetSize : TPoint; override;
              procedure SetSize(const nSize: TPoint); override;
              function Description : TPixelSetDescription; override;
              function SetDescription(const PSD: TPixelSetDescription;
                                      Confirm: TSDConfirm) : Boolean; override;
              function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
              procedure ListDependencies(L: TStringList); override;
            end;
  QShaderStage = class(QPixelSet)
                 public
                   class function TypeInfo: String; override;
                   function ProvidesSomeImage : QPixelSet;
                   function LoadPixelSet : QPixelSet; override;
                   function Description : TPixelSetDescription; override;
                   function SetDescription(const PSD: TPixelSetDescription;
                                           Confirm: TSDConfirm) : Boolean; override;
                 end;
  QShaderFile = class(QWad)
                protected
                  procedure Enregistrer(Info: TInfoEnreg1); override;
                  procedure Charger(F: TStream; Taille: Integer); override;
                public
                  class function TypeInfo: String; override;
                  class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                  function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                end;

implementation

uses Game, Travail;

procedure Q_HFile.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiText;
// E.MarsColor:=clWhite;
end;

class function Q_HFile.TypeInfo;
begin
 Result:='.h';
end;

class procedure Q_HFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5174);
 Info.FileExt:=803;
end;

procedure Q_CFile.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiText;
// E.MarsColor:=clWhite;
end;

class function Q_CFile.TypeInfo;
begin
 Result:='.c';
end;

class procedure Q_CFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5173);
 Info.FileExt:=802;
end;

class function Q3Pak.TypeInfo;
begin
 Result:='.pk3';
end;

class procedure Q3Pak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5170);
 Info.FileExt:=798;
end;

 {------------------------}

class function QShader.TypeInfo;
begin
 Result:=':shader';
end;

function QShader.DefaultImage : QPixelSet;
var
 Q: QObject;
 I: Integer;
begin
  { returns the first valid stage image }
 Acces;
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QShaderStage then
    begin
     Result:=QShaderStage(Q).ProvidesSomeImage;
     if Result<>Nil then Exit;  { got it }
    end;
  end;
 Result:=Nil;  { nothing found }
end;

(*procedure QShader.DataUpdate;
var
 Image: QPixelSet;
begin
  { we only want to set the correct size of this shader,
    based on the first valid stage size }
 Image:=DefaultImage;
 if Image<>Nil then
  try
   Image.Acces;
   SetSize(Image.GetSize);
  except
   {nothing}
  end;
end;*)

function QShader.DumpString : String;
var
 I, J, K: Integer;
 Spec: String;
 Q: QObject;
begin
 Result:=Name + #13#10'{'#13#10;
 Acces;
 for I:=0 to Specifics.Count-1 do  { attributes }
  begin
   Spec:=Specifics[I];
   J:=Pos('=', Spec);
     { ignore specifics that cannot be written as text }
   if (J>0) and (Ord(Spec[1]) and chrFloatSpec = 0) then
    { dump the specific as a shader attribute }
    Result:=Result + chr(vk_Tab) + Copy(Spec,1,J-1) + ' ' + Copy(Spec,J+1,MaxInt) + #13#10;
  end;
 for K:=0 to SousElements.Count-1 do  { stages }
  begin
   Q:=SousElements[K];
   Q.Acces;
    { stage intro }
   Result:=Result + chr(vk_Tab) + '{'#13#10;
    { include 'map' attribute from the name of the stage }
   if Q.Name <> LoadStr1(5699) then
    Result:=Result + chr(vk_Tab) + chr(vk_Tab) + 'map ' + Q.Name + #13#10;
   for I:=0 to Specifics.Count-1 do  { stage attributes }
    begin
     Spec:=Specifics[I];
     J:=Pos('=', Spec);
       { ignore specifics that cannot be written as text }
     if (J>0) and (Ord(Spec[1]) and chrFloatSpec = 0) then
      { dump the specific as a stage attribute }
      Result:=Result + chr(vk_Tab) + chr(vk_Tab) + Copy(Spec,1,J-1) + ' ' + Copy(Spec,J+1,MaxInt) + #13#10;
    end;
    { stage end }
   Result:=Result + chr(vk_Tab) + '}'#13#10;
  end;
 { shader end }
 Result:=Result + '}'#13#10#13#10;
end;

(*procedure QShader.OpDansScene(Aj: TAjScene; PosRel: Integer);
begin
 inherited;
 if Aj=asModifie then
  DataUpdate;
end;*)

function QShader.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[Q is QShaderStage];
end;

function QShader.GetSize : TPoint;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5534, ['Size']);
 Image.Acces;
 Result:=Image.GetSize;
end;

function QShader.Description : TPixelSetDescription;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5695, [Name]);
 Result:=Image.Description;
end;

procedure QShader.SetSize;
begin
 Raise EError(5696);
end;

function QShader.SetDescription;
begin
 Raise EError(5696);
end;

procedure QShader.ListDependencies(L: TStringList);
var
 I: Integer;
 S, SpecialStage: String;
begin
 Acces;
 SpecialStage:=LoadStr1(5699);
 for I:=0 to SousElements.Count-1 do
  begin
   S:=SousElements[I].Name;
    { to do: check for animated stages }
   if (S<>'') and (S[1]<>'$') and (S<>SpecialStage) then
    L.Add(#255+S);   { #255 means it is not a texture name but directly a file name }
  end;
end;

 {------------------------}

class function QShaderStage.TypeInfo;
begin
 Result:=':shstg';
end;

function QShaderStage.ProvidesSomeImage : QPixelSet;
begin
 if (Name='') or (Name[1]='$') then
  Result:=Nil
 else
  if Name=LoadStr1(5699) then   { complex stage }
   Result:=Nil   { to do: check for animated stages }
  else
   Result:=NeedGameFile(Name) as QPixelSet;
end;

function QShaderStage.LoadPixelSet : QPixelSet;
begin
 Result:=ProvidesSomeImage;
 if Result=Nil then
  Raise EErrorFmt(5697, [Name]);
 Result.Acces;
end;

function QShaderStage.Description : TPixelSetDescription;
begin
 Result:=LoadPixelSet.Description;
end;

function QShaderStage.SetDescription(const PSD: TPixelSetDescription;
                                     Confirm: TSDConfirm) : Boolean;
begin
 Raise EError(5696);
end;

 {------------------------}

class function QShaderFile.TypeInfo;
begin
 Result:='.shader';
end;

class procedure QShaderFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5175);
 Info.FileExt{Count}:=804;
end;

function QShaderFile.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 if Q is QShader then
  Result:=ieResult[True] + [ieListView]
 else
  Result:=ieResult[False];
end;

procedure QShaderFile.Charger(F: TStream; Taille: Integer);
const
 ProgressStep = 4096;
var
 ComplexStage, S, Data: String;
 Source, NextStep: PChar;
 Shader: QShader;
 Stage: QShaderStage;
 I, LineNumber: Integer;
 Comment: Boolean;

  procedure SyntaxError;
  begin
   Raise EErrorFmt(5694, [LineNumber]);
  end;

  procedure SkipSpaces;
  begin
   repeat
    while Source^ in [' ', Chr(vk_Tab)] do
     Inc(Source);
    if Source^=#13 then
     begin
      Inc(LineNumber);
      Inc(Source);
      if Source^=#10 then Inc(source);
     end
    else
     if Source^=#10 then
      begin
       Inc(LineNumber);
       Inc(Source);
      end
     else
      Break;
   until False;
  end;

  function ReadLine : String;
  var
   P1, P2: PChar;
  begin
   P1:=Source;
   while not (Source^ in [#13, #10, #0]) do
    Inc(Source);
   P2:=Source;
   while (P2>P1) and (P2[-1] in [' ', Chr(vk_Tab)]) do
    Dec(P2);
   SetString(Result, P1, P2-P1);
  end;

  procedure ReadAttribute(Target: QObject);
  var
   P1: PChar;
   Spec: String;
  begin
   P1:=Source;
   while not (Source^ in [' ', Chr(vk_Tab), #13, #10, #0]) do
    Inc(Source);
   SetString(Spec, P1, Source-P1);
   while Source^ in [' ', Chr(vk_Tab)] do
    Inc(Source);
   if Source^ in [#13, #10, #0] then
    Target.Specifics.Values[Spec]:=' '   { no value explicitely set }
   else
    Target.Specifics.Values[Spec]:=ReadLine;
  end;

begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      DebutTravail(5453, Taille div ProgressStep); try
      SetLength(Data, Taille);
      Source:=PChar(Data);
      F.ReadBuffer(Source^, Taille);  { read the whole file at once }

       { preprocess comments }
      Comment:=False;
      for I:=0 to Taille-1 do
       begin
        if (Source[I]='/') and (Source[I+1]='/') then
         Comment:=True
        else
         if Source[I] in [#13,#10] then
          Comment:=False;
        if Comment then
         Source[I]:=' ';
       end;

      NextStep:=Source+ProgressStep;
      LineNumber:=1;
      ComplexStage:=LoadStr1(5699);
      repeat
        { read one shader definition per loop }
       SkipSpaces;
       if Source^=#0 then Break;    { end of file }
       Shader:=QShader.Create(ReadLine, Self);    { new shader object }
       SousElements.Add(Shader);
       SkipSpaces;
       if Source^<>'{' then SyntaxError;
       Inc(Source);
       repeat
         { read one shader attribute or stage per loop }
        SkipSpaces;
        if Source^='}' then Break;   { end of shader }
        if Source^='{' then
         begin   { shader stage }
          Inc(Source);
          Stage:=QShaderStage.Create(ComplexStage, Shader);
          Shader.SousElements.Add(Stage);
          repeat
            { read one stage attribute per loop }
           SkipSpaces;
           if Source^='}' then Break;   { end of stage }
           ReadAttribute(Stage);
          until False;
          Inc(Source);   { skip the closing brace }

           { remove the 'map' attribute and use it to set the name of the stage }
          S:=Stage.Specifics.Values['map'];
          if S<>'' then
           begin
            Stage.Specifics.Values['map']:='';
            Stage.Name:=S;
           end;
         end
        else   { shader attribute }
         ReadAttribute(Shader);
       until False;
       Inc(Source);   { skip the closing brace }
       { Shader.DataUpdate;   { shader ready }

        { progress bar stuff }
       while Source>=NextStep do
        begin
         ProgresTravail;
         Inc(NextStep, ProgressStep);
        end;
      until False;
      finally FinTravail; end;
     end;
 else inherited;
 end;
end;

procedure QShaderFile.Enregistrer(Info: TInfoEnreg1);
var
 I: Integer;
 Q: QObject;
 Data: String;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      for I:=0 to SousElements.Count-1 do
       begin
        Q:=SousElements[I];
        if Q is QShader then
         begin
           { write this shader definition into the string Data }
          Data:=QShader(Q).DumpString;
           { dump Data to the stream }
          F.WriteBuffer(PChar(Data)^, Length(Data));
         end;
       end;
     end;
 else inherited;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(Q3Pak, 's');
  RegisterQObject(Q_CFile, 's');
  RegisterQObject(Q_HFile, 's');
  RegisterQObject(QShader, 'a');
  RegisterQObject(QShaderStage, 'a');
  RegisterQObject(QShaderFile, 'p');
end.


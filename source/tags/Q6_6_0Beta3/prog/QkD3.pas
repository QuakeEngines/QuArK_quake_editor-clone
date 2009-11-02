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
Revision 1.13  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.12  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.11  2008/10/10 19:43:35  danielpharos
Fix .mtr files showing up as .shader.

Revision 1.10  2008/09/06 15:57:13  danielpharos
Moved exception code into separate file.

Revision 1.9  2008/08/26 16:21:52  danielpharos
Added filename of broken shader/material file to error-message.

Revision 1.8  2008/04/26 15:31:12  danielpharos
Added a missing Log-line.

Revision 1.7  2008/04/25 20:42:14  danielpharos
Added history, fixed two small material-file parsing bug, and added experimental 'e' shader keyword.
}

(*
 * stuff for Doom 3 - a lot (ok, most) of this was copied from QkQ3.pas as Quake 3
 * and Doom 3 have a lot in common, but it is the little differences that make for
 * potential problems if we use the classes defined in QkQ3.pas for Doom 3, starting
 * with shaders in Doom 3 are called "materials", and altho the syntax looks similar
 * there is no guarantee that they are syntactically identical, plus there are a few
 * work-arounds in QkQ3.pas for Q3 that might be ignored, or might produce undesirable
 * side-effects if applied to Doom 3.  So there :-P
 *)

unit QkD3;

interface

uses
  Classes, SysUtils, Windows,
  QkZip2, QkFileObjects, QkPixelSet, QkObjects, QkWad;

type
  D3Pak = class(QZipPak)
    public
      class function TypeInfo: String; override;
      class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
    end;

  D3Material = class(QPixelSet)
    protected
      DefaultImageCache : QPixelSet;
    public
      class function TypeInfo: String; override;
      {procedure DataUpdate;}
      function DumpString : String;
      function DefaultImage : QPixelSet;
      {procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;}
      function GetSize : TPoint; override;
      procedure SetSize(const nSize: TPoint); override;
      function Description : TPixelSetDescription; override;
      function SetDescription(const PSD: TPixelSetDescription; Confirm: TSDConfirm) : Boolean; override;
      function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
      procedure ListDependencies(L: TStringList); override;
    end;

  D3MaterialStage = class(QPixelSet)
    public
      class function TypeInfo: String; override;
      function ContainsImageReference : Boolean;
      function ProvidesSomeImage : QPixelSet;
      function LoadPixelSet : QPixelSet; override;
      function Description : TPixelSetDescription; override;
      function SetDescription(const PSD: TPixelSetDescription; Confirm: TSDConfirm) : Boolean; override;
    end;

  D3MaterialFile = class(QWad)
    protected
      procedure SaveFile(Info: TInfoEnreg1); override;
      procedure LoadFile(F: TStream; FSize: Integer); override;
    public
      class function TypeInfo: String; override;
      class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
      function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
    end;

implementation

uses Quarkx, QkExceptions, QkObjectClassList, Game, Setup, Travail, Logging;

{------------------------}

class function D3Pak.TypeInfo;
begin
 Result:='.pk4';
end;

class procedure D3Pak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5146);
 Info.FileExt:=813;
end;

{------------------------}

function D3Material.DefaultImage: QPixelSet;
var
 Q: QObject;
 I: Integer;
 ValidStage: QPixelSet;
 DefaultImageName: array[0..5] of String;
 DefaultImageIndex: Integer;
 ImageFileName: String;
 Size: TPoint;
 V: array [1..2] of Single;
 TexExt: String;
begin
 Acces;
 Result:=Nil;

 TexExt:=SetupGameSet.Specifics.Values['TextureFormat'];
 if ReverseLink<>nil then
   DefaultImagename[0]:=ReverseLink.Specifics.Values['e'];
 if DefaultImageName[0]<>'' then
 begin
    ImageFileName:=Specifics.Values[DefaultImageName[0]];
    Log(LOG_VERBOSE,'attempting to load '+ImageFileName);
    try
      Result:=NeedGameFile(ImageFileName, '') as QPixelSet
    except
      Result:=nil;
    end;
 end
 else
 begin
   DefaultImageIndex:=0;
   DefaultImageName[0]:=Specifics.Values['q']; // look at the q specific (QTextureLnk.LoadPixelSet)
   DefaultImageName[1]:=Specifics.Values['qer_editorimage'];
   DefaultImageName[2]:=Specifics.Values['map'];
   DefaultImageName[3]:=Specifics.Values['diffusemap'];
   DefaultImageName[4]:=Specifics.Values['specularmap'];
   DefaultImageName[5]:=Specifics.Values['bumpmap']; //FIXME: Doesn't work!

   while ((Result=nil) and (DefaultImageIndex<6)) do
   begin
      if (DefaultImageName[DefaultImageIndex]<>'') then
      begin
        ImageFileName:=DefaultImageName[DefaultImageIndex];
        Log(LOG_VERBOSE,'attempting to load '+ImageFileName);
        try
          if (ExtractFileExt(ImageFileName)='') then
            Result:=NeedGameFile(ImageFileName+TexExt, '') as QPixelSet
          else
            Result:=NeedGameFile(ImageFileName, '') as QPixelSet;
        except
          Result:=nil;
        end;
      end;
      if Result=nil then
        DefaultImageIndex:=DefaultImageIndex+1;
    end;
 end;

 { examines all shaderstages for existing images }
 if Result=Nil then
 begin
   for I:=0 to SubElements.Count-1 do
   begin
     Q:=SubElements[I];
     if Q is D3MaterialStage then
     begin
       { Skip over $lightmap and those not containing images }
       if D3MaterialStage(Q).ContainsImageReference then
       begin
         try
           ValidStage:=D3MaterialStage(Q).ProvidesSomeImage;
           { Missing a texture, shader invalid? Return NIL }
           if not (ValidStage=Nil) then
             { Set to first valid stage, so something is displayed in the texture-browser }
             Result:=ValidStage;
             break;
         except
           Result:=NIL;
         end;
       end;
     end;
   end;
 end;

  { If no image could be found yet, try the shader-name itself }
 if Result=Nil then
 begin
   Log(LOG_VERBOSE,'attempting to load '+Name+TexExt);
   try
     Result:=NeedGameFile(Name+TexExt, '') as QPixelSet;
   except
     Result:=nil;
   end;
 end;

 {tiglari: giving shaders a size.  a presumably
  horrible place to do it, but doesn't work when
  shaders are being loaded }
 if Result<>Nil then
 begin
   Size:=Result.GetSize;
   V[1]:=Size.X;
   V[2]:=Size.Y;
   SetFloatsSpec('Size',V);
 end
 {/tiglari}
end;

function D3Material.Description: TPixelSetDescription;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5695, [Name]);
 Result:=Image.Description;
end;

function D3Material.DumpString: String;
var
 I, K: Integer;
 Q: QObject;

  procedure DumpSpec(const Spec, Indent: String);
  var
   J: Integer;
  begin
   J:=Pos('=', Spec);
   { ignore specifics that cannot be written as text }
   if (J>0) and (Ord(Spec[1]) and chrFloatSpec = 0) then
    Result:=Result + Indent + Copy(Spec,1,J-1) + TrimRight(' ' + Copy(Spec,J+1,MaxInt)) + #13#10;
    { dump the specific as a shader or stage attribute }
  end;

begin
 Result:=Name + #13#10'{'#13#10;
 Acces;
 for I:=0 to Specifics.Count-1 do  { attributes }
  DumpSpec(Specifics[I], chr(vk_Tab));
 for K:=0 to SubElements.Count-1 do  { stages }
  begin
   Q:=SubElements[K];
   Q.Acces;
   { stage intro }
   Result:=Result + chr(vk_Tab) + '{'#13#10;
   for I:=0 to Q.Specifics.Count-1 do  { stage attributes }
    DumpSpec(Q.Specifics[I], chr(vk_Tab) + chr(vk_Tab));
   { stage end }
   Result:=Result + chr(vk_Tab) + '}'#13#10;
  end;
 { shader end }
 Result:=Result + '}'#13#10#13#10;
end;

function D3Material.GetSize: TPoint;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5534, ['Size']);
 Image.Acces;
 Result:=Image.GetSize;
end;

function D3Material.IsExplorerItem(Q: QObject): TIsExplorerItem;
begin
 Result:=ieResult[Q is D3MaterialStage];
end;

procedure D3Material.ListDependencies(L: TStringList);
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

function D3Material.SetDescription(const PSD: TPixelSetDescription; Confirm: TSDConfirm): Boolean;
begin
 Raise EError(5751);
end;

procedure D3Material.SetSize(const nSize: TPoint);
begin
 Raise EError(5751);
end;

class function D3Material.TypeInfo;
begin
 Result:=':material';
end;

{------------------------}

{ D3MaterialStage }

function D3MaterialStage.ContainsImageReference: Boolean;
begin
 if (Name='') or (Name[1]='$') then
  Result:=False
 else
  Result:=True;
end;

function D3MaterialStage.Description: TPixelSetDescription;
begin
 Result:=LoadPixelSet.Description;
end;

function D3MaterialStage.LoadPixelSet: QPixelSet;
begin
 Result:=ProvidesSomeImage;
 if Result=Nil then
  Raise EErrorFmt(5752, [Name]);
 Result.Acces;
end;

function D3MaterialStage.ProvidesSomeImage: QPixelSet;
begin
 Result:=Nil;
 if ContainsImageReference then
 begin
  if Name=LoadStr1(5699) then   { complex stage }
   Result:=Nil   { to do: check for animated stages }
  else
   Result:=NeedGameFile(Name, '') as QPixelSet;
 end;
end;

function D3MaterialStage.SetDescription(const PSD: TPixelSetDescription; Confirm: TSDConfirm): Boolean;
begin
 Raise EError(5751);
end;

class function D3MaterialStage.TypeInfo: String;
begin
 Result:=':matstg';
end;

{------------------------}

{ D3MaterialFile }

class procedure D3MaterialFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 // Rowdy: technically Doom 3 does not have material lists (equivalent to Q3's shader lists)
 Info.FileObjectDescriptionText:=LoadStr1(5753);
 Info.FileExt{Count}:=823;
end;

function D3MaterialFile.IsExplorerItem(Q: QObject): TIsExplorerItem;
begin
 if Q is D3Material then
  Result:=ieResult[True] + [ieListView]
 else
  Result:=ieResult[False];
end;

procedure D3MaterialFile.LoadFile(F: TStream; FSize: Integer);
const
 ProgressStep = 4096;
var
 ComplexStage, Data: String;
 Source, NextStep: PChar;
 Material: D3Material;
 MaterialName: String;
 Stage: D3MaterialStage;
 I, LineNumber, counter: Integer;
 SectionComment, Comment: Boolean;
 V: array[1..2] of Single;
 masked: boolean; { Mohaa (tiglari): I'm guessing that this means that some
                    of the surfaceparms should be loaded into the editor as
                    flags.  'surfaceparm' appears in various other contexts
                    where it doesn't seem to set checks in the Mohradiant
                    surf inspector }
 EditableSurfaceParms : boolean;
 Filename: String; //Used in SyntaxError for display purposes

  procedure SyntaxError;
  begin
   Raise EErrorFmt(5754, [Filename, LineNumber]);
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
   // stop reading at cr, lf, eof, sp or tab (sp and tab added to the original code, see QkQ3.pas)
   // we need this for Quake 4 support as some materials are defined as:
   //   materialName {
   // and the trailing '{' gets appended to the material name
   while not (Source^ in [#13, #10, #0, ' ', Chr(vk_Tab)]) do
    Inc(Source);
   P2:=Source;
   // if the string ends with sp or tab, remove them
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
   SetString(Spec, P1, Source-P1);  // Spec is the Keywords,like qer_editorimage map ..., one at a time.
   while Source^ in [' ', Chr(vk_Tab)] do
    Inc(Source);

    { decker:
      FIXME: we insert the attribute directly into the object's specifics/args list.
      It will create duplicated specifics and specifics with no corresponding argument.
      In any of these two situations, code that edit the object might mess things up.
      TO DO: when shaders editing is implemented, ensure all the way that we can edit
      a "raw" specifics/args list, without disturbing the order of the specifics,
      without removing empty ones, and supporting duplicated specifics.
    }

   {tiglari: this solution may seem horrible, but it involves less special-game
    coding and attendant mess than the others I can come up with.  It is a
    basic design defect of QuArK that the face-specifics used by QuArK
    specifically (v, tv etc) aren't systematically separated from the ones
    associated with particular games, so here we're using an '_esp_' prefix
    to achieve this effect.  Requires some corresponding footwork in mapmgr.py
    and mapmenus.py. }

   if EditableSurfaceParms then
 //   if CharModeJeu=mjMohaa then
   begin
     if (Spec='qer_keyword') and (ReadLine='masked')then
       masked:=true
     else
     if (masked and (Spec='surfaceparm')) then
       Target.Specifics.Add('_esp_'+Readline+'='+'1');
   end;
   Target.Specifics.Add(Spec+'='+ReadLine);
  end;

begin
 Filename:=Self.GetFullName;
 
 EditableSurfaceParms:=SetupGameSet.Specifics.Values['EditableSurfaceParms']<>'';

 case ReadFormat of
  1: begin  { as stand-alone file }
      ProgressIndicatorStart(5453, FSize div ProgressStep); try
      SetLength(Data, FSize);
      Source:=PChar(Data);
      F.ReadBuffer(Source^, FSize);  { read the whole file at once }
      // cdunde: This is where the material file is read in, line by line as 'Data'.
      // Rowdy: Doom 3 adds some extra things to material files (*.mtr), such as "table"
      // We will ignore these for now, unless and until it become apparent that they are
      // actually used/needed, and/or someone works out how to use them ;-)
      //
      // Example from base_wall.mtr:
      //
      // table senetable { { .9, .5, .8, .6, .6, .3, .8, .7} }
      //
      // We will ignore this by replacing "table" with "//ble" so that the line
      // is removed by the comment filtering code below
      for I:=0 to FSize-5 do
       if (Source[I]='t') and (Source[I+1]='a') and (Source[I+2]='b') and (Source[I+3]='l') and (Source[I+4]='e') then
        if (I=0) or (Source[I-1] in [#13, #10, #0, ' ', Chr(vk_Tab)]) then
         begin
          Source[I]:='/';
          Source[I+1]:='/';
         end;
      // cdunde: Do the same thing for "guide" that was causing "textures/" to get knocked out
      for I:=0 to FSize-5 do
       if (Source[I]='g') and (Source[I+1]='u') and (Source[I+2]='i') and (Source[I+3]='d') and (Source[I+4]='e') then
        if (I=0) or (Source[I-1] in [#13, #10, #0, ' ', Chr(vk_Tab)]) then
         begin
          Source[I]:='/';
          Source[I+1]:='/';
         end;

      { preprocess (remove by converting to spaces) comments }
      Comment:=False;
      SectionComment:=False;
      for I:=0 to FSize-2 do
      begin
        if (Source[I]='/') and (Source[I+1]='/') then
          begin
           Comment:=True;
           {Rowdy: handle comments starting with //*****... by replacing
                   both // in the start of comment marker with spaces,
                   so we do not see /* as the next character sequence}
           Source[I] := ' ';
           Source[I+1] := ' ';
           {/Rowdy}
         end
        else if (Source[I]='/') and (Source[I+1]='*') then
          SectionComment := TRUE
        else if (Source[I]='*') and (Source[I+1]='/') then
        begin
          SectionComment := FALSE;
          Source[I] := ' ';
          Source[I+1] := ' ';
        end
        else begin
          if Source[I] in [#13,#10]
          then Comment:=False;
        end;

        if (Comment or SectionComment) then
          Source[I]:=' ';
      end;

      NextStep:=Source+ProgressStep;
      LineNumber:=1;
      ComplexStage:=LoadStr1(5699);
      repeat
        { read one shader definition per loop }
        // cdunde: At this point 'Source' is all chopped up wrong and includes material link names
        // bunched in a group of data together. Handling of Source needs to be fixed, 'Data' still good.
       masked:= false;   // mohaa
       SkipSpaces;
       if Source^=#0 then Break;    { end of file }
       counter:=0;
       MaterialName:=ReadLine;
       if MaterialName='material' then
       begin
         //FIXME: Anybody any idea what this 'material' exactly means? 
         SkipSpaces;
         MaterialName:=ReadLine;
       end;
       Material:=D3Material.Create(MaterialName, Self);    { new material object }
       SubElements.Add(Material);
       counter:=counter+1;
       SkipSpaces;
       if Source^=#0 then Break;    { end of file }
    // cdunde: below line was causing some shaders to error because of no line break
    //   if Source^<>'{' then SyntaxError;
       Inc(Source);
       repeat
         { read one shader attribute or stage per loop }
         // Source^ is only the first letter of each specific read in but it does NOT
         // go inside sub-items that have their own open and close curly brackets.
         // But Source (without the ^) is one whole line of data imput.
        SkipSpaces;
        if (Source^='}') and (counter<>2) then
         counter:=counter-1;
        if (Source^='}') and (counter<3) then
          begin
            Break;   // end of shader
          end;
        if Source^='{' then
         begin   { shader stage }
          counter:=counter+1;
          Inc(Source);
          Stage:=D3MaterialStage.Create(ComplexStage, Material);
          Material.SubElements.Add(Stage);
          repeat
            { read one stage attribute per loop }
           SkipSpaces;
           if Source^='{' then
             counter:=counter+1;
           if (Source^='}') and (counter>2) then
             counter:=counter-1;
           if (Source^='}') and (counter<3) then
             begin
               counter:=0;
               Break;   // end of stage
             end;
           ReadAttribute(Stage);
          until False;
          Inc(Source);   { skip the closing brace }

          { remove the 'map' attribute and use it to set the name of the stage }
          if Stage.Specifics.Values['map']<>'' then
            Stage.Name:=Stage.Specifics.Values['map']
            // cdunde Stage.Specifics.Values['map']:='';
          else
            // cdunde - try 'diffusemap' instead
            if Stage.Specifics.Values['diffusemap']<>'' then
              Stage.Name:=Stage.Specifics.Values['diffusemap']
          else
            // cdunde - try 'qer_editorimage' instead
            if Stage.Specifics.Values['qer_editorimage']<>'' then
              Stage.Name:=Stage.Specifics.Values['qer_editorimage']
          else
            // cdunde - try 'specularmap' instead
            if Stage.Specifics.Values['specularmap']<>'' then
              Stage.Name:=Stage.Specifics.Values['specularmap']
          else
            // cdunde - try 'bumpmap' instead
            if Stage.Specifics.Values['bumpmap']<>'' then
              Stage.Name:=Stage.Specifics.Values['bumpmap']
          else
            // cdunde - try 'animmap' instead
            if Stage.Specifics.Values['animmap']<>'' then
              begin
                Stage.Name:=Stage.Specifics.Values['animmap'];
                // jump over the number and take the first filename in the 'animmap' list
                Stage.Name:=Copy(Stage.Name, Pos(' ', Stage.Name)+1, 999);
                SetLength(Stage.Name, Pos(' ', Stage.Name)-1);
              end;
         end
        else   { shader attribute }
         ReadAttribute(Material);
       until False;
       Inc(Source);   { skip the closing brace }
       { Shader.DataUpdate;   { shader ready }
       { tiglari:  tried to give it a real
         size here but failed.  Now in DefaultImage }

        V[1]:=128;
        V[2]:=128;
        SetFloatsSpec('Size',V);
        {/tiglari}

        { progress bar stuff }
       while Source>=NextStep do
        begin
         ProgressIndicatorIncrement;
         Inc(NextStep, ProgressStep);
        end;
      until False;
      finally ProgressIndicatorStop; end;
     end;
 else inherited;
 end;
end;

procedure D3MaterialFile.SaveFile(Info: TInfoEnreg1);
var
 I: Integer;
 Q: QObject;
 Data: String;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      for I:=0 to SubElements.Count-1 do
       begin
        Q:=SubElements[I];
        if Q is D3Material then
         begin
           { write this shader definition into the string Data }
          Data:=D3Material(Q).DumpString;
           { dump Data to the stream }
          F.WriteBuffer(PChar(Data)^, Length(Data));
         end;
       end;
     end;
 else inherited;
 end;
end;

class function D3MaterialFile.TypeInfo: String;
begin
 Result:='.mtr';
end;

initialization
  RegisterQObject(D3Pak, 's');
  RegisterQObject(D3Material, 'a');
  RegisterQObject(D3MaterialStage, 'a');
  RegisterQObject(D3MaterialFile, 'p');

end.


(**************************************************************************
Vmf map loader (c) by  alexander

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

}


unit QkVMF;

interface

uses
  Windows,  SysUtils, Classes,  Dialogs,
  QkFileObjects,  QkObjects,
  QkMapObjects, QkBsp,
  qmatrices,  QkMap;

type


 QVMFFile = class(QBaseMapFile)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
          procedure SaveFile(Info: TInfoEnreg1); override;
        end;


 {------------------------}

//function ReadEntityList(Racine: TTreeMapBrush; const SourceFile: String; BSP: QBsp) : Char;

 {------------------------}

implementation

uses Qk1, QkQme, QkMapPoly, qmath, Travail, Setup,
  Qk3D, QkBspHulls, Undo, Game, Quarkx,
  QkObjectClassList, MapError;


 {------------------------}


function ReadEntityList(Racine: TTreeMapBrush; const SourceFile: String; BSP: QBsp) : Char;
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
 t_vectarray= array[1..3] of TVect;
var
 SymbolType: TSymbols;
 S, S1, Classname: String;
 StringValue: String;
 NumericValue: Double;
 V: t_vectarray;
 P: TPolyhedron;
 Surface: TFace;
 I, J, K, NumericValue1, ContentsFlags: Integer;
 WorldSpawn: Boolean;
 Entite, EntitePoly: TTreeMapSpec;
 L: TStringList;
 LineNoBeingParsed: Integer;
 Juste13{, FinDeLigne}, Q2Tex, ReadSymbolForceToText: Boolean;
 HullNum, BrushNum, FaceNum: Integer;
 HullList: TList;
 Source, Prochain: PChar;
 Entities, MapStructure {Rowdy}, MapStructureB {/Rowdy}: TTreeMapGroup;
 Params: TFaceParams;
 InvPoly, InvFaces: Integer;
 TxCommand: Char;
 OriginBrush: TPolyhedron;
 Facteur: TDouble;
 Delta, Delta1: TVect;
 {Rowdy}
 V5: TVect5;
 EntiteBezier: TTreeMapSpec;
 pCP1: vec5_p;
 {/Rowdy}
 WC33map: Boolean; {Decker}
 SpecIndex: integer; {Decker}
 UAxis, VAxis : TVect;  {wc3.3 stuff/tiglari}
 UShift, VShift: Double;


 Flags, Contents : LongInt;
// Header : TQ2MipTex;

 MapVersion: double;

 function ReadInt(str : string) : LongInt;
 begin
   if str = '' then
     Result:=0
   else
     Result:=StrToInt(str)
 end;


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

     while Source>Prochain do
     begin
       ProgressIndicatorIncrement;
       Inc(Prochain, Granularite);
     end;

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

     '-', '0'..'9':
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

           if (Source[1]='T') and (Source[2]='X') then
             TxCommand:=Source[3];

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





(* The wc3.3 220 map format texture rep is quite close
  to the texinfo_t data structure used by qbsp.  This
  consists of 2 axes lying in one of the three planes
  normal to the axes, plus offsets, & the formula for
  computing the texture coordinate of a point xyz on
  a face is:
   u = x * u_axis.x + y * u_axis.y + z * u_axis.z + u_offset
   v = x * v_axis.x + y * v_axis.y + z * v_axis.z + v_offset
  (Max McGuire's Quake2 BSP file format tutorial on
   www.flipcode.com)

  However wc3.3 does *not* seem to require the texture-vectors
  to lie in an axis plane, and if you write with that assumption
  (projecting the points), things get distorted.

  U/V Axis/Shift are straight from the 4-vectors, param[3]
  is rot which is ignored (implicit from the axes), while
  param[4,5] are UV scales.  Diferent from the bsp-format
  is that the axes are normalized to length 1, and you
  divide by the scale to get the .bsp-version of the axis.
  (Zoner's HL tools source, textures.cpp) *)

procedure WC33Params;
 var
  PP0, PP1, PP2, NP0, NP1, NP2, PlanePoint, TexNorm : TVect;
 begin
   PP0:=VecSum(VecScale(-UShift*Params[4], UAxis),VecScale(-VShift*Params[5], VAxis));
   PP1:=VecSum(PP0,VecScale(Params[4]*128,UAxis));
    { note p.i.t.a sign-flip }
   PP2:=VecSum(PP0,VecScale(-Params[5]*128,VAxis));
   with Surface do
   begin
     TexNorm:=Cross(UAxis,VAxis);
     Normalise(TexNorm);
     PlanePoint:=VecScale(Dist, Normale);
     (* could perhaps be optimized by 'partial evaluation' *)
     try
       NP0:=ProjectPointToPlane(PP0, TexNorm, PlanePoint, Normale);
       NP1:=ProjectPointToPlane(PP1, TexNorm, PlanePoint, Normale);
       NP2:=ProjectPointToPlane(PP2, TexNorm, PlanePoint, Normale);
       SetThreePointsEx(NP0,NP1,NP2,Normale);
     except
       g_MapError.AddText('Problem with texture scale of face '+IntToStr(FaceNum)+ ' in brush '+IntToStr(BrushNum)+' in hull '+IntToStr(HullNum+1));
     end;
  end;
 end;

 procedure ReadHL2Entity;
 begin
   // tbd
 end;

 procedure ReadHL2Solid;
 // tbd , visgroups, entities, groups
 type
   t_vectarray = array [1..3] of TVect;
   t_txarray = array [1..5] of double;
 var
//   R1, R2, TexS, TexT, Tex0, P0, P1, P2, ZVect : TVect;
   V : t_vectarray ;
//   Denom : Double;
//   Matrix : TMatrixTransformation;
   u_txdata,v_txdata :  t_txarray;

  //"(-512 0 64) (0 0 64) (0 -512 64)"
  function readface(s:string): t_vectarray;
  var
    i,p,q: Integer;

  begin
    i:=1;
    p:=1;
    while p <= length(s) do
    begin
      if s[p]= '(' then
      begin
        for q:=p to length(s) do
          if s[q]=')' then
          begin
            result[i]:=ReadVector( copy( s,p+1,q-p-1 ) );
            i:=i+1;
            p:=q;
            break;
          end;
      end;
      p:=p+1;
    end;
  end;

  //"[1 0 0 0] 0.25"
  function readtxdata(s:string): t_txarray;
  var
    i: Integer;
  begin
    for i:=length(s) downto 1 do
      if s[i] in ['"','[',']'] then
        delete(s,i,1);
    ReadValues(s,result);
  end;

 begin



  ReadSymbol(sStringToken); // solid
  ReadSymbol(sCurlyBracketLeft);

  P:=TPolyhedron.Create(LoadStr1(138), EntitePoly);
  EntitePoly.SubElements.Add(P);
  ContentsFlags:=0;

  // read solid attributes
  while SymbolType=sStringQuotedToken do
  begin
    S1:=S;
    ReadSymbol(sStringQuotedToken);
    S1:=S;
    ReadSymbol(sStringQuotedToken);
  end;

//side
//{
//  "id" "1"
//  "plane" "(-512 0 64) (0 0 64) (0 -512 64)"
//			"material" "DEV/DEV_MEASUREHALL01"
//			"uaxis" "[1 0 0 0] 0.25"
//			"vaxis" "[0 -1 0 0] 0.25"
//			"rotation" "0"
//			"lightmapscale" "16"
//			"smoothing_groups" "0"
//}

  // read the side
  while (SymbolType=sStringToken) and (CompareText(S,'side')=0) do
  begin
    Inc(FaceNum);

    ReadSymbol(sStringToken); // side
    ReadSymbol(sCurlyBracketLeft);

    // read side attributes
    while SymbolType=sStringQuotedToken do
    begin
      S1:=S;
      ReadSymbol(sStringQuotedToken);
      if (S1='plane') then
      begin
        // read plane
        v:=ReadFace(s);
        Surface:=TFace.Create(LoadStr1(139), P);
        P.SubElements.Add(Surface);
        Surface.SetThreePoints(V[1], V[3], V[2]);
        if not Surface.LoadData then
          ShowMessage('LoadData failure');

      end
      else
        if (S1='material') then
        begin
          // read material
          Surface.NomTex:=S;
        end
        else
          if (S1='uaxis') then
          begin
           // read uaxis
           u_txdata:=readtxdata(s);

           UAxis.x:= u_txdata[1];
           UAxis.y:= u_txdata[2];
           UAxis.z:= u_txdata[3];
           UShift:=  u_txdata[4];
           Params[4]:=u_txdata[5];
          end
          else
            if (S1='vaxis') then
            begin
              // read vaxis
              v_txdata:=readtxdata(s);
              VAxis.x:= v_txdata[1];
              VAxis.y:= v_txdata[2];
              VAxis.z:= v_txdata[3];
              VShift:=  v_txdata[4];
              Params[5]:=v_txdata[5];
            end;

      ReadSymbol(sStringQuotedToken);
    end; // side attributes

    WC33Params;

    if (SymbolType=sStringToken) and (CompareText(S,'dispinfo')=0) then
    begin
      ReadSymbol(sStringToken); // dispinfo
      ReadSymbol(sCurlyBracketLeft);

      // read dispinfo attributes    ( tbd wat is dat)
      while SymbolType=sStringQuotedToken do
      begin
        S1:=S;
        ReadSymbol(sStringQuotedToken);
        S1:=S;
        ReadSymbol(sStringQuotedToken);
      end;

      // read dispinfo sub components
      while (SymbolType=sStringToken)  do
      begin
        ReadSymbol(sStringToken);
        ReadSymbol(sCurlyBracketLeft);
        // read dispinfo attributes
        while SymbolType=sStringQuotedToken do
        begin
          S1:=S;
          ReadSymbol(sStringQuotedToken);
          S1:=S;
          ReadSymbol(sStringQuotedToken);
        end;
        ReadSymbol(sCurlyBracketRight);
      end;
      ReadSymbol(sCurlyBracketRight);
    end;// dispinfo

    ReadSymbol(sCurlyBracketRight);
  end; // side loop

  if (SymbolType=sStringToken) and (CompareText(S,'editor')=0) then
  begin
    ReadSymbol(sStringToken); // editor
    ReadSymbol(sCurlyBracketLeft);

    // read editor attributes
    while SymbolType=sStringQuotedToken do
    begin
      S1:=S;
      ReadSymbol(sStringQuotedToken);
      S1:=S;
      ReadSymbol(sStringQuotedToken);
    end;
  end;
  ReadSymbol(sCurlyBracketRight);

  ReadSymbol(sCurlyBracketRight);

 end;


begin
  ProgressIndicatorStart(5451, Length(SourceFile) div Granularite);
  try
    Source:=PChar(SourceFile);
    Prochain:=Source+Granularite;   { point at which progress marker will be ticked}
    Result:=mjQuake;     { Into Result is but info about what game the map is for }
    Q2Tex:=False;
    WC33map:=False; {Decker}
    g_MapError.Clear;
    ReadSymbolForceToText:=False;    { ReadSymbol is not to expect text}
    LineNoBeingParsed:=1;
    InvPoly:=0;
    InvFaces:=0;
    Juste13:=False;
    {FinDeLigne:=False;}
    HullList:=Nil;
    L:=TStringList.Create;
    try   { L and HullList get freed by finally, regardless of exceptions }
     WorldSpawn:=False;  { we haven't seen the worldspawn entity yet }
     Entities:=TTreeMapGroup.Create(LoadStr1(136), Racine);
     Racine.SubElements.Add(Entities);
     MapStructure:=TTreeMapGroup.Create(LoadStr1(137), Racine);
     Racine.SubElements.Add(MapStructure);
     {Rowdy}
     MapStructureB:=Nil;
     (*** commented out by Armin : only create the group if actually needed
      *  MapStructureB:=TTreeMapGroup.Create(LoadStr1(264), Racine);
      *  Racine.SubElements.Add(MapStructureB);
      *)
     {/Rowdy}
     ReadSymbol(sEOF);
     while SymbolType<>sEOF do { when ReadSymbol's arg is sEOF, it's not really `expected'.
                   The first real char ought to be {.  If it is, it
                   will become C in ReadSymbol, and SymbolType will sCurlyBracketLeft }
     begin
       { if the thing just read wasn't {, the ReadSymbol call will bomb.
        Otherwise, it will pull in the next chunk (which ought to be
        a quoted string), and set SymbolType to the type of what it got. }

       // ... except for Doom 3, where we might have a "version 1" or "version 2" line BEFORE
       // the first "}" ...
       if (SymbolType=sStringToken) and (CompareText(S,'Version')=0) then
       begin
         ReadSymbol(sStringToken); // get the map version number // NumValueToken);
         if SymbolType<>sNumValueToken then
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(251)]); // invalid number
         MapVersion := NumericValue;
         if MapVersion <> 1 then
           raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(266)]); // can't read Doom 3 version 2 maps
         Result:=mjDoom3;
         ReadSymbol(sNumValueToken);
       end;

       // this is for hl2
       //found string versionsinfo
       if (SymbolType=sStringToken) and (CompareText(S,'versioninfo')=0) then
       begin
         ReadSymbol(sStringToken);
         ReadSymbol(sCurlyBracketLeft);
         // read attributes of versioninfo
         while SymbolType=sStringQuotedToken do
         begin
           S1:=S;
           ReadSymbol(sStringQuotedToken);

//tbd : what versions to allow ?            
//           if (S1='mapversion') and (S<>'1') then
//             raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(270)]);
//           if (S1='formatversion') and (S<>'100') then
//             raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(270)]);
           ReadSymbol(sStringQuotedToken);
         end;
         if (SymbolType=sCurlyBracketRight) then
           ReadSymbol(sCurlyBracketRight);

         //found string viewsettings
         if (SymbolType=sStringToken) and (CompareText(S,'viewsettings')=0) then
         begin
           ReadSymbol(sStringToken);
           ReadSymbol(sCurlyBracketLeft);
           // read attributes of viewsettings
           while SymbolType=sStringQuotedToken do
           begin
             S1:=S;
             ReadSymbol(sStringQuotedToken);
             S1:=S;
             ReadSymbol(sStringQuotedToken);
           end;
           if (SymbolType=sCurlyBracketRight) then
             ReadSymbol(sCurlyBracketRight);

           Result:=mjHl2;

           //found string world
           if (SymbolType=sStringToken) and (CompareText(S,'world')=0) then
           begin
             ReadSymbol(sStringToken);
             ReadSymbol(sCurlyBracketLeft);

             // read attributes of world
             while SymbolType=sStringQuotedToken do
             begin
               S1:=S;
               ReadSymbol(sStringQuotedToken);
               if (S1='classname') and (S='worldspawn') then
               begin
                 Entite:=Racine;
                 EntitePoly:=MapStructure;
                 EntiteBezier:=MapStructureB;
                 WorldSpawn:=True;
                 HullNum:=0;
                 Racine.Name:=ClassnameWorldspawn;
               end;
               ReadSymbol(sStringQuotedToken);


             end;
             if (SymbolType=sCurlyBracketRight) then
               ReadSymbol(sCurlyBracketRight);

             while SymbolType = sStringToken do  {read a brush}
               if LowerCase(s)='solid' then
                 ReadHL2Solid()
               else
                 if LowerCase(s)='entity' then
                   ReadHL2Entity()
                 else
                   raise EErrorFmt(254, [LineNoBeingParsed, 'unknown thing']);
           end;
         end;

         exit;
       end; // if versionsinfo



       if (OriginBrush<>Nil) and (EntitePoly<>MapStructure) then
       begin
         V[1].X:=MaxInt;
         V[1].Y:=MaxInt;
         V[1].Z:=MaxInt;
         V[2].X:=-MaxInt;
         V[2].Y:=-MaxInt;
         V[2].Z:=-MaxInt;
         OriginBrush.ChercheExtremites(V[1], V[2]);
         if V[1].X<V[2].X then
         begin
           Delta.X:=0.5*(V[1].X+V[2].X);
           Delta.Y:=0.5*(V[1].Y+V[2].Y);      { center of the 'origin brush' }
           Delta.Z:=0.5*(V[1].Z+V[2].Z);
           for I:=0 to EntitePoly.SubElements.Count-1 do
            with EntitePoly.SubElements[I] do
             for J:=0 to SubElements.Count-1 do
              with SubElements[J] as TFace do
               if GetThreePoints(V[1], V[2], V[3]) and LoadData then
               begin
                 Facteur:=Dot(Normale, Delta);
                 Delta1.X:=Delta.X - Normale.X*Facteur;
                 Delta1.Y:=Delta.Y - Normale.Y*Facteur;    { Delta1 is Delta forced in the plane of the face }
                 Delta1.Z:=Delta.Z - Normale.Z*Facteur;
                 for K:=1 to 3 do
                 begin
                   V[K].X:=V[K].X + Delta1.X;
                   V[K].Y:=V[K].Y + Delta1.Y;
                   V[K].Z:=V[K].Z + Delta1.Z;
                 end;
                 SetThreePoints(V[1], V[2], V[3]);
               end;
          end;
        end;

       if (WC33map) then
       begin
         { Remove the spec/arg "mapversion" from worldspawn,
           since QuArK will write it depending on whether the
           game config is set to write this format }
         SpecIndex := L.IndexOfName('mapversion');
         if (SpecIndex >= 0) then
           L.Delete(SpecIndex);
       end;

      {Entite.Item.Text:=Classname;}
       Entite.Specifics.Assign(L);
      {Entite.SpecificsChange;}
       ReadSymbol(sCurlyBracketRight);
      end;

     if HullList<>Nil then
      for I:=0 to HullList.Count-1 do
       begin
        EntitePoly:=TTreeMapSpec(HullList[I]);
        if EntitePoly<>Nil then
         EntitePoly.SubElements.Add(TBSPHull.CreateHull(BSP, I, EntitePoly as TTreeMapGroup));
       end;

    if not WorldSpawn then
       Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(255)]);

    finally
       L.Free;
       HullList.Free;
    end;

    Racine.FixupAllReferences;
  finally
    ProgressIndicatorStop;
  end;


  if InvFaces>0 then
    GlobalWarning(FmtLoadStr1(257, [InvFaces]));
  if InvPoly>0 then
    GlobalWarning(FmtLoadStr1(256, [InvPoly]));
end;

 {------------------------}


class function QVMFFile.TypeInfo;
begin
 Result:='.vmf';
end;

class procedure QVMFFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5715);
 Info.FileExt:=817;
end;

procedure QVMFFile.LoadFile(F: TStream; FSize: Integer);
var
 Racine: TTreeMapBrush;
 ModeJeu: Char;
 Source: String;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      SetLength(Source, FSize);
      F.ReadBuffer(Source[1], FSize);
      Racine:=TTreeMapBrush.Create('', Self);
      Racine.AddRef(+1);
      try
        ModeJeu:=ReadEntityList(Racine, Source, Nil);
        SubElements.Add(Racine);
        Specifics.Values['Root']:=Racine.Name+Racine.TypeInfo;
        ObjectGameCode:=ModeJeu;
      finally
        Racine.AddRef(-1);
      end;
     end;
 else
  inherited;
 end;
end;

procedure QVMFFile.SaveFile(Info: TInfoEnreg1);
var
 Dest, HxStrings: TStringList;
 Racine: QObject;
 List: TQList;
 saveflags : Integer;
 MapOptionSpecs : TStringList;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      Racine:=SubElements.FindName(Specifics.Values['Root']);
      if (Racine=Nil) or not (Racine is TTreeMapBrush) then
       Raise EError(5558);
      Racine.LoadAll;
      HxStrings:=Nil;
      List:=TQList.Create;
      Dest:=TStringList.Create;
      try
       if Specifics.IndexOfName('hxstrings')>=0 then
        begin
         HxStrings:=TStringList.Create;
         HxStrings.Text:=Specifics.Values['hxstrings'];
        end;

       { .MAP comment header, which explains that this .MAP has been written
         by QuArK, for this game, and then QuArK's webpage. }
       Dest.Add(CommentMapLine(FmtLoadStr1(176, [QuarkVersion])));
       Dest.Add(CommentMapLine(FmtLoadStr1(177, [SetupGameSet.Name])));
       Dest.Add(CommentMapLine(FmtLoadStr1(178, [])));
       Dest.Add('');
       Dest.Text:=Dest.Text;   { #13 -> #13#10 }

       saveflags:=0;
       MapOptionSpecs:=SetupSubSet(ssMap,'Options').Specifics;
       if MapOptionSpecs.Values['IgnoreToBuild']<>'' then
         saveflags:=saveflags or soIgnoreToBuild;
       if MapOptionSpecs.Values['DisableFPCoord']<>'' then
         saveflags:=saveflags or soDisableFPCoord;
        if MapOptionSpecs.Values['UseIntegralVertices']<>'' then
         saveflags:=saveflags or soUseIntegralVertices;
     saveflags:=saveflags or IntSpec['saveflags']; {merge in selonly}

       TTreeMap(Racine).SaveAsText(List, Dest, saveflags, HxStrings);
       Dest.SaveToStream(F);
       if HxStrings<>Nil then
        Specifics.Values['hxstrings']:=HxStrings.Text;
      finally
       Dest.Free;
       List.Free;
       HxStrings.Free;
      end;
     end;
 else
  inherited;
 end;
end;

 {------------------------}



initialization
  RegisterQObject(QVMFFile, 'x');
end.

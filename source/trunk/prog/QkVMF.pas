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
Revision 1.23  2008/10/13 19:12:48  danielpharos
Fixed a typo.

Revision 1.22  2008/10/12 11:31:32  danielpharos
Moved 6DX map format to separate file, and re-factored QkMap and QkQuakeMap.

Revision 1.21  2008/10/09 22:56:14  danielpharos
Fixed a .vmf importer bug.

Revision 1.20  2008/09/06 15:57:10  danielpharos
Moved exception code into separate file.

Revision 1.19  2007/07/05 10:19:44  danielpharos
Moved the Quake .map format code to a separate file.

Revision 1.18  2007/06/24 20:38:06  danielpharos
Comment out unused code.

Revision 1.17  2007/04/30 21:52:42  danielpharos
Small cleanup of code around VTFLib.

Revision 1.16  2007/04/12 15:04:42  danielpharos
BIG moving around of code. All the .map save routines should now be in QkMap. This will allow easy changes, and will simplify future map format support.

Revision 1.15  2007/03/25 13:52:24  danielpharos
Moved a few dictionnary words around.

Revision 1.14  2007/03/11 12:03:10  danielpharos
Big changes to Logging. Simplified the entire thing.

Revision 1.13  2007/02/01 23:13:53  danielpharos
Fixed a few copyright headers

Revision 1.12  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2005/07/30 23:04:44  alexander
introduced dispface class for faces with displacement mapping
vmf loader sets some displacement info

Revision 1.9  2005/07/05 19:12:48  alexander
logging to file using loglevels

Revision 1.8  2005/07/03 18:05:32  alexander
fixed bug that quark cant load its own exported vmf

Revision 1.7  2005/03/14 19:02:43  alexander
fixed duplicate classname

Revision 1.6  2005/02/06 21:39:26  alexander
dont break on hidden solids

Revision 1.5  2005/02/06 21:29:15  alexander
dont ignore hammers special groups but import them into quark under misc
import worldspawn attributes

Revision 1.4  2005/01/28 23:14:27  alexander
connections completed

Revision 1.3  2005/01/28 18:48:53  alexander
entities are processed, not yet the connections

Revision 1.2  2005/01/27 21:05:22  alexander
we can load a vmf map now, ignoring the entities

Revision 1.1  2005/01/27 00:16:13  alexander
added vmf file loading (brushes only)


}


unit QkVMF;

interface

uses
  Windows, SysUtils, Classes, Dialogs,
  QkFileObjects, QkObjects,
  QkMapObjects, QkBsp,
  qmatrices, QkMap;

type
 QVMFFile = class(QMapFile)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
          procedure SaveFile(Info: TInfoEnreg1); override;
        end;

 {------------------------}

implementation

uses Qk1, QkQme, QkMapPoly, qmath, Travail, Setup,
  Qk3D, QkBspHulls, Undo, Game, Quarkx, QkExceptions,
  QkObjectClassList, MapError,Logging,DispFace;


 {------------------------}


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

procedure WC33Params( BrushNum,Facenum:Integer; Params: TFaceParams;UAxis, VAxis : TVect; UShift, VShift: Double;Surface: TFace);

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
       g_MapError.AddText('Problem with texture scale of face '+IntToStr(FaceNum)+ ' in brush '+IntToStr(BrushNum));
     end;
  end;
 end;


function ReadEntityList(Root: TTreeMapBrush; const SourceFile: String; BSP: QBsp) : Char;
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
 S, S1,S2: String;
 NumericValue: Double;
 P: TPolyhedron;
 Surface: TDispFace;
 WorldSpawn: Boolean;
 Entities, MapStructure : TTreeMapGroup;

 LineNoBeingParsed: Integer;
 Juste13,  ReadSymbolForceToText: Boolean;
 BrushNum, FaceNum: Integer;
 Source, Prochain: PChar;
 Params: TFaceParams;
 InvPoly, InvFaces: Integer;
 TxCommand: Char;
 UAxis, VAxis : TVect;
 UShift, VShift: Double;






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






 procedure ReadHL2Solid(parentgroup: TTreeMapSpec);
 // tbd , visgroups, entities, groups
 type
   t_vectarray = array [1..3] of TVect;
   t_txarray = array [1..5] of double;
 var
   V : t_vectarray ;
   u_txdata,v_txdata :  t_txarray;
   aryn: array of double;
   aryd: array of double;
   row:integer;
   nrows:integer;

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
  Inc(BrushNum);

  P:=TPolyhedron.Create(LoadStr1(138), parentgroup);
  parentgroup.SubElements.Add(P);

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
        Surface:=TDispFace.Create(LoadStr1(139), P);
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

    WC33Params( FaceNum,BrushNum,Params,UAxis, VAxis , UShift, VShift,Surface);

    if (SymbolType=sStringToken) and (CompareText(S,'dispinfo')=0) then
    begin
      ReadSymbol(sStringToken); // dispinfo
      ReadSymbol(sCurlyBracketLeft);

      // read dispinfo attributes    ( tbd wat is dat)
      nrows:=0;
      while SymbolType=sStringQuotedToken do
      begin
        S1:=S;
        ReadSymbol(sStringQuotedToken);
        if lowercase(s1)='power' then
        begin
          surface.setpower(strtoint(s));
          nrows:=surface.meshsize;
        end;
        ReadSymbol(sStringQuotedToken);
      end;

      // read dispinfo sub components
      while (SymbolType=sStringToken)  do
      begin
        ReadSymbol(sStringToken);
        row:=0;
        S1:=S;
        //Log(S);
        ReadSymbol(sCurlyBracketLeft);
        // read dispinfo attributes
        while SymbolType=sStringQuotedToken do
        begin
          S2:=S;
          //Log(s);
          ReadSymbol(sStringQuotedToken);
          //Log(s);
          //Log('>'+S1+'_'+S2+'='+S);

          if lowercase(s1)='normals' then
          begin
            setlength(aryn,3*nrows);
            ReadValues(S,aryn);
            surface.addnormals(aryn);
          end;
          if lowercase(s1)='distances' then
          begin
            setlength(aryd,nrows);
            ReadValues(S,aryd);
            surface.adddists(row,aryd);
            inc(row);
          end;
          Surface.Specifics.Add(S1+'_'+S2+'='+S);
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
    ReadSymbol(sCurlyBracketRight);
  end;

  ReadSymbol(sCurlyBracketRight);

 end;


 procedure ReadHL2Entity(parentgroup: TTreeMapSpec);
 var

   Entity : TTreeMapSpec;
   SpecificList: TStringList;
   classname: String;


   procedure AddConnection(var List: TStringlist;outputname,value:string);
   var
     i,num{,lastfound} : integer;

   begin
     num:=0;
     //lastfound:=0;
     for i:=0 to list.count-1 do
     begin
       // count occurances
       if pos(outputname,list[i])<>0 then
       begin
         num:=num+1;
         //lastfound:=i;
       end;
     end;

     if num>1 then
       List.Add(outputname + '#'+IntToStr(num)+'=' + value)
     else
       List.Add(outputname + '=' + value)
   end;

 begin
   entity := nil;
   ReadSymbol(sStringToken);
   ReadSymbol(sCurlyBracketLeft);
   SpecificList:=TStringList.Create;
   SpecificList.Clear;


   // read attributes of entity
   while SymbolType<>sCurlyBracketRight do
   begin

     if SymbolType = sStringQuotedToken then
     begin
       S1:=S;
       ReadSymbol(sStringQuotedToken);
       if (S1='classname')  then
          classname:=S
       else
         SpecificList.Add(S1+'='+S);

       ReadSymbol(sStringQuotedToken);
     end
     else
       if (SymbolType = sStringToken) and (LowerCase(s)='connections') then
       begin
         ReadSymbol(sStringToken);
         ReadSymbol(sCurlyBracketLeft);
         while SymbolType=sStringQuotedToken do
         begin
           S1:=S;
           ReadSymbol(sStringQuotedToken);
           //check if connection for this output is already there and
           //adds with same name and #<n>
           AddConnection(SpecificList, 'output#'+S1, S);
           ReadSymbol(sStringQuotedToken);
         end;
         ReadSymbol(sCurlyBracketRight);
       end
       else
         if (SymbolType = sStringToken) and (LowerCase(s)='editor') then
         begin
           ReadSymbol(sStringToken);
           ReadSymbol(sCurlyBracketLeft);
           while SymbolType=sStringQuotedToken do
           begin
             S1:=S;
             ReadSymbol(sStringQuotedToken);
             S1:=S;
             ReadSymbol(sStringQuotedToken);
           end;
           ReadSymbol(sCurlyBracketRight);
         end
         else
           if (SymbolType = sStringToken) and (LowerCase(s)='solid') then
           begin
             Entity:=TTreeMapBrush.Create(classname, parentgroup);
             parentgroup.SubElements.Add(Entity);
             ReadHL2Solid(Entity);
           end
           else
             raise EErrorFmt(254, [LineNoBeingParsed, 'unknown thing']);

   end; //while SymbolType<>sCurlyBracketRight

   if entity = nil then
   begin
     Entity:=TTreeMapEntity.Create(classname, parentgroup);
     parentgroup.SubElements.Add(Entity);
   end;

   Entity.Specifics.Assign(SpecificList);
   ReadSymbol(sCurlyBracketRight);

 end;



 // we use this for all hierachic info that we
 // just want to read and skip. It allows
 // mixed quoted string pairs "a" "b"
 // and named objects   name { }
 // example
 // a { "b" "c" sub { "sa" "sb" } "d" "e" }
 procedure ReadHL2GenericHierarchy;
 begin
   ReadSymbol(sStringToken);
   ReadSymbol(sCurlyBracketLeft);
   // read attributes
   while SymbolType<>sCurlyBracketRight do
   begin
     if SymbolType=sStringQuotedToken then
     begin
       S1:=S;
       ReadSymbol(sStringQuotedToken);
       S1:=S;
       ReadSymbol(sStringQuotedToken);
     end
     else
       if SymbolType=sStringToken then
         ReadHL2GenericHierarchy; //descend
   end;
   ReadSymbol(sCurlyBracketRight);
 end;

 procedure ReadHL2Group(parentgroup: TTreeMapSpec);
 var
  group: TTreeMapSpec;
 begin
   group:=TTreeMapGroup.Create(S, parentgroup);
   parentgroup.SubElements.Add(group);

   ReadSymbol(sStringToken);
   ReadSymbol(sCurlyBracketLeft);
   // read attributes
   while SymbolType<>sCurlyBracketRight do
   begin
     if SymbolType=sStringQuotedToken then
     begin
       S1:=S;
       ReadSymbol(sStringQuotedToken);
       group.Specifics.Add(S1+'='+S);
       ReadSymbol(sStringQuotedToken);
     end
     else
       if SymbolType=sStringToken then
         if S='entity' then
           ReadHL2Entity(group)
         else
           if S='solid' then
             ReadHL2Solid(group)
           else
             ReadHL2Group(group); //descend
   end;
   ReadSymbol(sCurlyBracketRight);
 end;



 // read the world hierarchy
 procedure ReadWorld;
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
       WorldSpawn:=True;
       Root.Name:=ClassnameWorldspawn;
     end
     else
       Root.Specifics.Add(S1+'='+S);
     ReadSymbol(sStringQuotedToken);
   end;

   while SymbolType = sStringToken do  {read a brush}
     if LowerCase(s)='solid' then
       ReadHL2Solid(MapStructure)
     else
       if LowerCase(s)='group' then
         ReadHL2Group(MapStructure)
       else
         if LowerCase(s)='hidden' then
           ReadHL2Group(MapStructure)
         else
           raise EErrorFmt(254, [LineNoBeingParsed, 'unknown thing']);
   ReadSymbol(sCurlyBracketRight);
 end;

 procedure ReadVersionInfo;
 begin
   ReadSymbol(sStringToken);
   ReadSymbol(sCurlyBracketLeft);

   // read attributes of versioninfo
   while SymbolType=sStringQuotedToken do
   begin
     S1:=S;
     ReadSymbol(sStringQuotedToken);

     if (S1='formatversion') and (S<>'100') then
       raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(268)]);

     ReadSymbol(sStringQuotedToken);
   end;
   ReadSymbol(sCurlyBracketRight);
 end;

begin
  ProgressIndicatorStart(5451, Length(SourceFile) div Granularite);
  try
    Source:=PChar(SourceFile);
    Prochain:=Source+Granularite;   { point at which progress marker will be ticked}
    Result:=mjHl2;
    g_MapError.Clear;
    ReadSymbolForceToText:=False;    { ReadSymbol is not to expect text}
    LineNoBeingParsed:=1;
    InvPoly:=0;
    InvFaces:=0;
    Juste13:=False;

    try
     WorldSpawn:=False;  { we haven't seen the worldspawn entity yet }
     Entities:=TTreeMapGroup.Create(LoadStr1(136), Root);
     Root.SubElements.Add(Entities);
     MapStructure:=TTreeMapGroup.Create(LoadStr1(137), Root);
     Root.SubElements.Add(MapStructure);
     {Rowdy}
     ReadSymbol(sEOF);
     while SymbolType<>sEOF do { when ReadSymbol's arg is sEOF, it's not really `expected'.
                   The first real char ought to be {.  If it is, it
                   will become C in ReadSymbol, and SymbolType will sCurlyBracketLeft }
     begin
       { if the thing just read wasn't {, the ReadSymbol call will bomb.
        Otherwise, it will pull in the next chunk (which ought to be
        a quoted string), and set SymbolType to the type of what it got. }


       //found string versionsinfo
       while SymbolType=sStringToken do
       if CompareText(S,'versioninfo')=0 then
         ReadVersionInfo
       else
         //found string visgroups
         if CompareText(S,'visgroups')=0 then
           ReadHL2GenericHierarchy
         else
           //found string viewsettings
           if CompareText(S,'viewsettings')=0 then
               ReadHL2GenericHierarchy
           else
             //found string world
             if CompareText(S,'world')=0 then
             begin
               ReadWorld;
             end
             else
               //found string cameras
               if CompareText(S,'cameras')=0 then
                 ReadHL2GenericHierarchy
               else
                 //found string cordon
                 if CompareText(S,'cordon')=0 then
                   ReadHL2GenericHierarchy
                 else
                   //found string entity
                   if CompareText(S,'entity')=0 then
                     ReadHL2Entity(Entities)
                   else
                     //found string hidden
                     if CompareText(S,'hidden')=0 then
//                       ReadHL2GenericHierarchy
                       ReadHL2Group(root)
                     else
                       raise EErrorFmt(254, [LineNoBeingParsed, 'unknown thing']);

     end;

    if not WorldSpawn then
       Raise EErrorFmt(254, [LineNoBeingParsed, LoadStr1(255)]);

    finally
    end;

    Root.FixupAllReferences;
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
 Root: TTreeMapBrush;
 ModeJeu: Char;
 Source: String;
begin
 Log(LOG_VERBOSE,'load vmf file %s',[self.name]);
 case ReadFormat of
  1: begin  { as stand-alone file }
      SetLength(Source, FSize);
      F.ReadBuffer(Source[1], FSize);
      Root:=TTreeMapBrush.Create('', Self);
      Root.AddRef(+1);
      try
        ModeJeu:=ReadEntityList(Root, Source, Nil);
        SubElements.Add(Root);
        Specifics.Values['Root']:=Root.Name+Root.TypeInfo;
        ObjectGameCode:=ModeJeu;
      finally
        Root.AddRef(-1);
      end;
     end;
 else
  inherited;
 end;
end;

procedure QVMFFile.SaveFile(Info: TInfoEnreg1);
var
 Dest, HxStrings: TStringList;
 Root: QObject;
 List: TQList;
 saveflags : Integer;
 MapOptionSpecs : TStringList;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      Root:=SubElements.FindName(Specifics.Values['Root']);
      if (Root=Nil) or not (Root is TTreeMapBrush) then
       Raise EError(5558);
      Root.LoadAll;
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

       Dest.Add('versioninfo');
       Dest.Add('{');
       Dest.Add('  "formatversion" "100"');
       Dest.Add('}');
       SaveAsMapText(TTreeMap(Root), ObjectGameCode, -1, List, Dest, saveflags, HxStrings);
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

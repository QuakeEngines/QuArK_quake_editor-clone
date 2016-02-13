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
Revision 1.12  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.11  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.10  2008/09/06 15:57:00  danielpharos
Moved exception code into separate file.

Revision 1.9  2007/03/17 14:32:38  danielpharos
Moved some dictionary entries around, moved some error messages into the dictionary and added several new error messages to improve feedback to the user.

Revision 1.8  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.6  2001/03/20 21:42:44  decker_dk
Updated copyright-header

Revision 1.5  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.4  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QPAcc;

interface

uses Classes, SysUtils;

type
 TType = (
        tVoid
       ,tString
       ,tNumValue
       ,tVector
       ,tPointer
       ,tObjData
       ,tFrPointer
 );
 TSymbols = (
        symUnknown
       ,symType
       ,symIdent
       ,symVariable
       ,symObjVar
       ,symColon
       ,symSemiColon
       ,symPeriod
       ,symAssign
       ,symComma
       ,symAdd
       ,symSubtract
       ,symMultiply
       ,symDivide
       ,symAnd
       ,symOr
       ,symNot
       ,symLogicalOr
       ,symLogicalAnd
       ,symEqual
       ,symDifferent
       ,symLessThan
       ,symGreaterThan
       ,symLessOrEqualTo
       ,symGreaterOrEqualTo
       ,symCurlyBracketLeft
       ,symCurlyBracketRight
       ,symBracketLeft
       ,symBracketRight
       ,symLOCAL
       ,symBIND
       ,symAUTOEXEC
       ,symIF
       ,symELSE
       ,symWHILE
       ,symDO
       ,symRETURN
       ,symNumValue
       ,symChain
       ,symVector
       ,symEOF
       ,symDollar
       ,symSquareBracketLeft
       ,symSquareBracketRight
 );

const
 Impulse0Def = 166;
 VersionProgsDat = 6;
 MotsClefs: array[symLOCAL..symRETURN] of String =
  ('LOCAL', 'BIND', 'AUTOEXEC', 'IF', 'ELSE', 'WHILE', 'DO', 'RETURN');
 NomsTypes: array[tVoid..tPointer] of String =
  ('VOID', 'STRING', 'FLOAT', 'VECTOR', 'ENTITY');

type
 TTypeCode = (tcQuakeC, tcHexenC);
 ECompileError = class(Exception)
                  NoLigne: Integer;
                  MsgErreur: String;
                  constructor Create(const Erreur, nNomPatch: String; nNoLigne: Integer; const Extrait: String);
                 end;

procedure Compiler(Patch: TStrings; Source, Dest: TStream;
                    Touches: TStringList; Impulse0: Integer;
                    TypeCode: TTypeCode);

implementation

uses {Tableaux,} Quarkx, QkExceptions;

const
 FacteurT : ARRAY[1..6] OF LONGINT = (8, 8, 8, 36, 1, 4);

type
 TEntree = Packed RECORD
            Pos, Nb : LONGINT;
           END;
 TEntete = Packed RECORD
            Version : LONGINT;
            CodeSys : LONGINT;
            Entrees : ARRAY[1..6] OF TEntree;
            TailleLocData : LONGINT;
           END;
 TVarStruct = Packed RECORD
               case Integer of
                0: (InfoType : TType;
                    Sys      : BYTE;
                    Pos      : WORD;
                    Nom      : LONGINT);
                1: (AsObject : TObject);
              END;
 PTableauVar = ^TTableauVar;
 TTableauVar = array[0..0] of TVarStruct;
 PFrame = ^TFrame;
 TFrame = Packed RECORD
           Code, LocData, LocSize, Reserve : LONGINT;
           NomFrame, theFilename    : LONGINT;
           NbArguments          : LONGINT;
           Args                 : ARRAY[1..8] OF BYTE;
          END;

procedure Compiler(Patch: TStrings; Source, Dest: TStream;
                    Touches: TStringList; Impulse0: Integer;
                    TypeCode: TTypeCode);
type
 Vecteur = ARRAY[1..3] OF Single;
 TTypeEtendu = record
                T: TType;
                NbArg: Integer;
                Arg: array[1..8] of record
                                     T: TType;
                                     Nom: String;
                                    end;
               end;
const
 TailleType : ARRAY[TType] OF WORD = (1,1,1,3,1,1,1);
 tcExtension: array[TTypeCode] of String[3] = ('.qc', '.hc');
 tcImmediate: array[TTypeCode] of String = ('IMMEDIATE', 'I+');
var
 Zero: Vecteur;
 Header: TEntete;
 Origine: LongInt;
 Chaines: String;
{St: TVarStruct;
 I: Integer;}
 Vars: PTableauVar;
 Ligne, NomPatch: String;
 LigneCourante, LigneLocale: Integer;
 PL: PChar;
 SymboleMot: String;
 SymboleReel: Single;
 SymboleReel_AsLong: LongInt absolute SymboleReel;
 SymboleVecteur: Vecteur;
 VarLocales, VarGlobales, VarObjet: TStringList;
 SymbolType: TSymbols;
 SymboleVar: TVarStruct;
 SymboleType: TType;
 Commentaires, VariableEstLocale, VariableObjet: Boolean;
 Datas, ObjDataDef: TMemoryStream;
 NDatas: LongInt;
 NFrames, QCode, NDataDef: TMemoryStream;
 NoFrame, NQCode: LongInt;
 NoImpulse: Integer;
 Immediate: LongInt;
 MdlFrames: TStringList;

  procedure ErreurEx(const Erreur: String);
  var
   S: String;
   I, Min, Max: Integer;
  begin
   S:='';
   Min:=LigneCourante;
   while (Min>0) and (LigneCourante-Min<10)
   and (Copy(Patch[Min-1],1,1)<>#255) do
    Dec(Min);
   Max:=LigneCourante;
   if Max>=Patch.Count then Max:=Patch.Count-1;
   while (Max>=0) and (Copy(Patch[Max],1,1)=#255) do
    Dec(Max);
   for I:=Min to Max do
    S:=S+#$A+Patch[I];
   Raise ECompileError.Create(Erreur, NomPatch, LigneLocale-2, S);
  end;

  procedure Erreur(Numero: Integer);
  begin
   ErreurEx(LoadStr1(Numero));
  end;

  procedure ControleInt(I: LongInt);
  begin
   if (I>32767) or (I<-32768) then
    Erreur(4112);
  end;

  function ChercheVar(const Nom: String) : Boolean;
  var
   Min, Max, Test, Resultat: LongInt;
  {P: PTableauVar;}
  begin
  {P:=PTableauVar(NDataDef.Memory);
   for Test:=NDataDef.Size div SizeOf(TVarStruct) - 1 downto 0 do
    if StrIComp(PChar(@Chaines[P^[Test].Nom+1]), PChar(Nom)) = 0 then
     begin
      SymboleVar:=P^[Test];
      ChercheVar:=True;
      Exit;
     end;}
   Min:=0;
   Max:=Header.Entrees[2].Nb-1;
   while Min < Max do
    begin
     Test:=(Min+Max) shr 1;
     Resultat:=StrIComp(PChar(@Chaines[Vars^[Test].Nom+1]), PChar(Nom));
     if Resultat<0 then
      Min:=Test+1
     else
      if Resultat>0 then
       Max:=Test
      else
       begin
        while (Test>0) and (StrIComp(PChar(@Chaines[Vars^[Test-1].Nom+1]), PChar(Nom)) = 0) do
         Dec(Test);
        Max:=Header.Entrees[2].Nb-1;
        while ((Vars^[Test].InfoType=tObjData) xor VariableObjet)
        and (Test<Max) and (StrIComp(PChar(@Chaines[Vars^[Test+1].Nom+1]), PChar(Nom)) = 0) do
         Inc(Test);
        SymboleVar:=Vars^[Test];
        ChercheVar:=True;
        Exit;
       end;
    end;
   ChercheVar:=False;
  end;

  function EcrireChaine(const C: String) : LongInt;
  begin
   Result:=Pos(C+#0, Chaines);
   if Result=0 then
    begin
     Result:=Length(Chaines);
     Chaines:=Chaines+C+#0;
    end
   else
    Dec(Result);
  end;

  function EcrireReel(const R: Single) : LongInt;
  begin
   Result:=NDatas;
   Inc(NDatas);
   Datas.Write(R, 4);
  end;

  function EcrireVecteur(const V: Vecteur) : LongInt;
  begin
   Result:=NDatas;
   Inc(NDatas, 3);
   Datas.Write(V, 4*3);
  end;

  PROCEDURE ReadSymbol(Attendu : TSymbols);

    PROCEDURE Trim;
    var
     I: Integer;
    BEGIN
     if Commentaires then
      begin
       I:=Pos('*/', Ligne);
       if I=0 then
        PL:=StrEnd(PL)
       else
        begin
         PL:=PChar(@Ligne[I+2]);
         Commentaires:=False;
        end;
      end;
     if not Commentaires then
      begin
       WHILE PL^ IN [' ', #8, #9] DO
        INC(PL);
       IF PL^='/' THEN
        IF PL[1]='/' THEN
         PL:=StrEnd(PL)
        else
         if PL[1]='*' then
          begin
           Commentaires:=True;
           Trim;
          end;
      end;
    END;

    PROCEDURE Lu(nSymbole : TSymbols);

    BEGIN
     SymbolType:=nSymbole;
     INC(PL);
    END;

    PROCEDURE Lu2(Sym1, Sym2 : TSymbols);

    BEGIN
     IF PL[1]='=' THEN
      BEGIN
       SymbolType:=Sym2;
       INC(PL, 2);
      END
     ELSE
      Lu(Sym1);
    END;

    FUNCTION ReadNumValue : Single;

    VAR
     Echelle : Single;
     Neg             : BOOLEAN;

    BEGIN
     Neg:=PL^='-';
     IF Neg THEN
      INC(PL);
     IF NOT (PL^ IN ['0'..'9']) THEN
      Erreur(4096);
     Result:=0;
     REPEAT
      Result:=Result*10 + (ORD(PL^)-48);
      INC(PL);
     UNTIL NOT (PL^ IN ['0'..'9']);
     IF PL^='.' THEN
      BEGIN
       Echelle:=1;
       INC(PL);
       WHILE PL^ IN ['0'..'9'] DO
        BEGIN
         Echelle:=Echelle*0.1;
         Result:=Result + Echelle*(ORD(PL^)-48);
         INC(PL);
        END;
      END;
     IF Neg THEN
      Result:=-Result;
    END;

  var
   I: Integer;
   Normal, Surbrillance: Boolean;
   VarL: TStringList;

  BEGIN
   IF (SymbolType<>Attendu) AND (Attendu<>symUnknown) THEN
    ErreurEx(FmtLoadStr1(4097, [
     LoadStr1(4160+Ord(Attendu)),
     LoadStr1(4160+Ord(SymbolType))]));
   WHILE (PL^=#0) AND (LigneCourante < Patch.Count) DO
    BEGIN
     Ligne:=Patch[LigneCourante];
     Inc(LigneCourante);
     PL:=PChar(Ligne);
     if PL^ = #255 then
      begin
       Inc(PL);
       NomPatch:=PL;
       PL:=StrEnd(PL);
       MdlFrames.Clear;
       LigneLocale:=0;
      end
     else
      Trim;
     Inc(LigneLocale);
    END;
   CASE PL^ OF
    'a'..'z','A'..'Z' : BEGIN
                         SymboleMot:='';
                         REPEAT
                          SymboleMot:=SymboleMot+PL^;
                          INC(PL);
                         UNTIL NOT (PL^ IN ['a'..'z','A'..'Z','0'..'9','_']);
                         SymbolType:=Low(MotsClefs);
                         while (SymbolType<=High(MotsClefs)) and (CompareText(SymboleMot, MotsClefs[SymbolType])<>0) do
                          Inc(SymbolType);
                         if SymbolType>High(MotsClefs) then
                          begin
                           SymboleType:=Low(NomsTypes);
                           while (SymboleType<=High(NomsTypes)) and (CompareText(SymboleMot, NomsTypes[SymboleType])<>0) do
                            Inc(SymboleType);
                           if SymboleType<=High(NomsTypes) then
                            SymbolType:=symType
                           else
                            begin
                             SymbolType:=symVariable;
                             VariableEstLocale:=(VarLocales<>Nil) and VarLocales.Find(SymboleMot, I);
                             if VariableEstLocale then
                              SymboleVar.AsObject:=VarLocales.Objects[I]
                             else
                              begin
                               if VariableObjet then
                                VarL:=VarObjet
                               else
                                VarL:=VarGlobales;
                               if VarL.Find(SymboleMot, I) then
                                SymboleVar.AsObject:=VarL.Objects[I]
                               else
                                if not ChercheVar(SymboleMot) then
                                 begin
                                  if VariableObjet then
                                   VarL:=VarGlobales
                                  else
                                   VarL:=VarObjet;
                                  if VarL.Find(SymboleMot, I) then
                                   SymboleVar.AsObject:=VarL.Objects[I]
                                  else
                                   begin
                                    SymbolType:=symIdent;
                                    SymboleVar.InfoType:=tVoid;
                                   end;
                                 end;
                               if SymboleVar.InfoType=tObjData then
                                SymbolType:=symObjVar;
                              end;
                            end;
                          end;
                        END;
    '0'..'9' : BEGIN
                SymbolType:=symNumValue;
                SymboleReel:=ReadNumValue;
               END;
    ';' : Lu(symSemiColon);
    '{' : Lu(symCurlyBracketLeft);
    '}' : Lu(symCurlyBracketRight);
    ':' : Lu(symColon);
    '=' : Lu2(symAssign, symEqual);
    '+' : Lu(symAdd);
    '-' : IF PL[1] IN ['0'..'9'] THEN
           BEGIN
            SymbolType:=symNumValue;
            SymboleReel:=ReadNumValue;
           END
          ELSE
           Lu(symSubtract);
    '*' : Lu(symMultiply);
    '/' : Lu(symDivide);
    '&' : BEGIN
           Lu(symAnd);
           IF PL^='&' THEN
            Lu(symLogicalAnd);
          END;
    '|' : BEGIN
           Lu(symOr);
           IF PL^='|' THEN
            Lu(symLogicalOr);
          END;
    '!' : Lu2(symNot, symDifferent);
    '<' : Lu2(symLessThan, symLessOrEqualTo);
    '>' : Lu2(symGreaterThan, symGreaterOrEqualTo);
    '(' : Lu(symBracketLeft);
    ')' : Lu(symBracketRight);
    '.' : Lu(symPeriod);
    ',' : Lu(symComma);
    '''': BEGIN
           INC(PL);
           Trim;
           FOR I:=1 TO 3 DO
            BEGIN
             SymboleVecteur[I]:=ReadNumValue;
             Trim;
            END;
           IF PL^<>'''' THEN
            Erreur(4098);
           Lu(symVector);
          END;
    '"' : BEGIN
           INC(PL);
           SymboleMot:='';
           Surbrillance:=False;
           WHILE PL^<>'"' DO
            BEGIN
             Normal:=True;
             CASE PL^ OF
              #0  : Erreur(4099);
              '\' : begin
                     Inc(PL);
                     case Upcase(PL^) of
                      'N': begin
                            SymboleMot:=SymboleMot+#10;
                            Normal:=False;
                           end;
                      'Q': begin
                            SymboleMot:=SymboleMot+'"';
                            Normal:=False;
                           end;
                      '0'..'9','A'..'F':
                       if Upcase((PL+1)^) in ['0'..'9','A'..'F'] then
                        begin
                         if PL^<='9' then
                          I:=Ord(PL^)-Ord('0')
                         else
                          I:=Ord(Upcase(PL^))-(Ord('A')-10);
                         Inc(PL);
                         I:=I*16;
                         if PL^<='9' then
                          Inc(I, Ord(PL^)-Ord('0'))
                         else
                          Inc(I, Ord(Upcase(PL^))-(Ord('A')-10));
                         SymboleMot:=SymboleMot+Chr(I);
                         Normal:=False;
                        end;
                      'H': begin
                            Surbrillance:=True;
                            Normal:=False;
                           end;
                      'L': begin
                            Surbrillance:=False;
                            Normal:=False;
                           end;
                     end;
                    end;
             END;
             if Normal then
              if Surbrillance then
               SymboleMot:=SymboleMot+Chr(Ord(PL^) or 128)
              else
               SymboleMot:=SymboleMot+PL^;
             INC(PL);
            END;
           Lu(symChain);
          END;
    '$': Lu(symDollar);
    '[': Lu(symSquareBracketLeft);
    ']': Lu(symSquareBracketRight);
    #0 : begin
          SymbolType:=symEOF;
          Exit;
         end;
    ELSE Erreur(4100);
   END;
   Trim;
  END;

  function TypeEtendu: TTypeEtendu;
  begin
   Result.T:=SymboleType;
   Result.NbArg:=-1;
   ReadSymbol(symType);
   if SymbolType=symBracketLeft then
    begin
     ReadSymbol(symBracketLeft);
     Result.NbArg:=0;
     if SymbolType<>symBracketRight then
      repeat
       if Result.NbArg=8 then
        Erreur(4117);
       Inc(Result.NbArg);
       with Result.Arg[Result.NbArg] do
        begin
         T:=SymboleType;
         ReadSymbol(symType);
         if (T=tVoid) and (SymbolType=symBracketLeft) then
          begin
           ReadSymbol(symBracketLeft);
           ReadSymbol(symBracketRight);
           T:=tFrPointer;
          end;
         Nom:=SymboleMot;
        end;
       if SymbolType in [symVariable, symObjVar] then
        ReadSymbol(symUnknown)
       else
        ReadSymbol(symIdent);
       if SymbolType=symBracketRight then Break;
       ReadSymbol(symComma);
      until False;
     ReadSymbol(symBracketRight);
    end;
  end;

  function ChargerFrame(Pos: LongInt; var F: TFrame) : LongInt;
  begin
   Datas.Position:=4*Pos;
   Datas.ReadBuffer(Result, 4);
   Datas.Position:=LongInt(NDatas)*4;
   if Result>0 then
    begin
     NFrames.Position:=Result*SizeOf(TFrame);
     NFrames.ReadBuffer(F, SizeOf(TFrame));
    end;
   {if L>=Header.Entrees[4].Nb then
     begin
      NFrames.Position:=(L-Header.Entrees[4].Nb)*SizeOf(TFrame);
      NFrames.ReadBuffer(F, SizeOf(TFrame));
     end
    else
     begin
      Source.Position:=Origine+Header.Entrees[4].Pos + L*SizeOf(TFrame);
      Source.ReadBuffer(F, SizeOf(TFrame));
     end;}
  end;

  function DefVar(L: TStringList; const T: TTypeEtendu) : Integer;
  forward;

TYPE
 TExprFlags = (fInstr, fPeutAffecter);
 PExpr = ^TExpr;
 TExpr = RECORD
          P, P2, Instr : WORD;
          T            : TType;
          Flags        : SET OF TExprFlags;
         END;

VAR
 ResultatAppel : PExpr;

  PROCEDURE Coder(Instr, Arg1, Arg2, Arg3 : WORD);

  BEGIN
   QCode.Write(Instr, 2);
   QCode.Write(Arg1, 2);
   QCode.Write(Arg2, 2);
   QCode.Write(Arg3, 2);
   INC(NQCode);
  END;

  PROCEDURE Transm(const De : TExpr; A : PExpr);

  BEGIN
   IF @De=ResultatAppel THEN
    ResultatAppel:=A;
  END;

  FUNCTION Immediat(const E : TExpr) : WORD;

  BEGIN
   IF fInstr IN E.Flags THEN
    BEGIN
     Coder(E.Instr, E.P, E.P2, NDatas);
     Immediat:=NDatas;
     Datas.Write(Zero, 4*TailleType[E.T]);
     INC(NDatas, TailleType[E.T]);
    END
   ELSE
    Immediat:=E.P;
  END;

  PROCEDURE Expression(var Result : TExpr); FORWARD;

  PROCEDURE CoderAppel(PosAppel : WORD);

  VAR
   NbArg,I : INTEGER;
   Args    : ARRAY[1..8] OF TExpr;
   F       : TFrame;

  BEGIN
   ReadSymbol(symBracketLeft);
   NbArg:=0;
   IF SymbolType<>symBracketRight THEN
    REPEAT
     if NbArg=8 then
      Erreur(4117);
     INC(NbArg);
     IF SymbolType=symObjVar THEN
      WITH Args[NbArg] DO
       BEGIN
        P:=SymboleVar.Pos;
        Flags:=[];
        T:=tObjData;
        ReadSymbol(symObjVar);
       END
     ELSE
      Expression(Args[NbArg]);
     IF SymbolType=symBracketRight THEN Break;
     ReadSymbol(symComma);
    UNTIL False;
   if ChargerFrame(PosAppel, F)>0 then
    begin
     if NbArg <> F.NbArguments then
      Erreur(4115);
     for I:=1 to NbArg do
      if TailleType[Args[I].T] <> F.Args[I] then
       Erreur(4116);
    end;
   FOR I:=1 TO NbArg DO
    BEGIN
     WITH Args[I] DO
      if (TypeCode<>tcHexenC) or (I>2) or (fInstr in Flags) then
       begin   { codage normal }
        IF fInstr IN Flags THEN
         Coder(Instr, P, P2, I*3+1)
        ELSE
         Coder($20, P, I*3+1, 0);
        if TypeCode=tcHexenC then
         P:=I*3+1
        else
         P:=0;
       end;
     Transm(Args[I], Nil);
    END;
   for I:=NbArg+1 to 2 do
    Args[I].P:=0;
   Coder($33+NbArg, PosAppel, Args[1].P, Args[2].P);
   ReadSymbol(symBracketRight);
  END;

  PROCEDURE chk(const E : TExpr; T : TType);

  BEGIN
   IF (E.T<>tVoid) and (T<>tVoid) and (E.T<>T) THEN
    Erreur(4101);
  END;

  CONST
   Affecte : ARRAY[Boolean, TType] OF WORD =
    (($1F, $21, $1F, $20, $22, $23, $24),
     ($25, $27, $25, $26, $28, $29, $2A));

  PROCEDURE Expression(var Result : TExpr);

   PROCEDURE ExprEtOu(var Result : TExpr);

    PROCEDURE ExprComparaison(var Result : TExpr);

     PROCEDURE ExprPlusMoins(var Result : TExpr);

      PROCEDURE ExprFoisDivise(var Result : TExpr);

       PROCEDURE ExprNot(var Result : TExpr);

        PROCEDURE ExprFinal(var Result : TExpr);

         PROCEDURE Appel;

         VAR
          F : TVarStruct;
         {R : TResApp;}

         BEGIN
          F:=SymboleVar;
          ReadSymbol(symVariable);
          IF SymbolType=symBracketLeft THEN
           BEGIN
            IF Assigned(ResultatAppel) AND
             ((ResultatAppel^.P=1)
              OR ((fInstr IN ResultatAppel^.Flags) AND (ResultatAppel^.P2=1))) THEN
             BEGIN
              Coder(Affecte[False, ResultatAppel^.T], 1, NDatas, 0);
              IF ResultatAppel^.P=1 THEN
               ResultatAppel^.P:=NDatas
              ELSE
               ResultatAppel^.P2:=NDatas;
              Datas.Write(Zero, 4*TailleType[ResultatAppel^.T]);
              INC(NDatas, TailleType[ResultatAppel^.T]);
             END;
            CoderAppel(F.Pos);
            ResultatAppel:=@Result;
            Result.P:=1;
            Result.T:=tVoid;
           {Result.Flags:=[];
            Coder(Affecte[False, F^.TypeExpr], 1, NDatas, 0);
            Result.P:=NDatas;
            Result.T:=F^.TypeExpr;
            Datas.Write(Zero, 4*TailleType[F^.TypeExpr]);
            INC(NDatas, TailleType[F^.TypeExpr]);}
           {Result.P:=ResultatAppels.Count;
            IF Result.P>0 THEN
             BEGIN
              R.P:=ResultatAppels.At(Result.P-1);
              IF R.Pos=1 THEN
               BEGIN
                Coder(Affecte[False, R.TypeRetour], 1, NDatas, 0);
                R.Pos:=NDatas;
                Datas.Write(Zero, 4*TailleType[R.TypeRetour]);
                INC(NDatas, TailleType[R.TypeRetour]);
                ResultatAppels.AtPut(Result.P-1, R.P);
               END;
             END;
            Result.T:=F^.TypeExpr;
            Result.Flags:=[fResultatAppel];
            R.Pos:=1;
            R.TypeRetour:=Result.T;
            ResultatAppels.Insert(R.P);}
           END
          ELSE
           BEGIN
            Result.P:=F.Pos;
            Result.T:=tFrPointer;
           END;
          Result.Flags:=[];
         END;

        VAR
         St : TVarStruct;

        BEGIN
         CASE SymbolType OF
          symVariable    : if SymboleVar.InfoType=tFrPointer then
                            Appel
                           else
                            BEGIN
                             Result.P:=SymboleVar.Pos;
                             Result.T:=SymboleVar.InfoType;
                             Result.Flags:=[fPeutAffecter];
                             ReadSymbol(symVariable);
                            END;
          symNumValue        : BEGIN
                            St.InfoType:=tNumValue;
                            St.Sys:=0;
                            St.Pos:=EcrireReel(SymboleReel);
                            St.Nom:=Immediate;
                            NDataDef.Write(St, SizeOf(St));
                            Result.P:=St.Pos;
                            Result.T:=tNumValue;
                            Result.Flags:=[];
                            ReadSymbol(symNumValue);
                           END;
          symChain      : BEGIN
                            SymboleReel_AsLong:=EcrireChaine(SymboleMot);
                           {IF ObjChaine^.Immediat=0 THEN
                             BEGIN
                              ObjChaine^.Immediat:=NDatas;
                              Datas.Write(SymboleReel, 4);
                              INC(NDatas);
                             END;}
                            St.InfoType:=tString;
                            St.Sys:=0;
                            St.Pos:={ObjChaine^.Immediat} EcrireReel(SymboleReel);
                            St.Nom:=Immediate;
                            NDataDef.Write(St, SizeOf(St));
                            Result.P:=St.Pos;
                            Result.T:=tString;
                            Result.Flags:=[];
                            ReadSymbol(symChain);
                           END;
          symVector     : BEGIN
                            St.InfoType:=tVector;
                            St.Sys:=0;
                            St.Pos:=EcrireVecteur(SymboleVecteur);
                            St.Nom:=Immediate;
                            NDataDef.Write(St, SizeOf(St));
                            Result.P:=St.Pos;
                            Result.T:=tVector;
                            Result.Flags:=[];
                            ReadSymbol(symVector);
                           END;
          symBracketLeft : BEGIN
                            ReadSymbol(symBracketLeft);
                            Expression(Result);
                            ReadSymbol(symBracketRight);
                           END;
          symDollar : BEGIN
                       ReadSymbol(symDollar);
                       if not (SymbolType in [symIdent, symVariable, symObjVar]) then
                        Erreur(4113);
                       SymboleReel:=MdlFrames.IndexOf(SymboleMot);
                       if SymboleReel<0 then
                        Erreur(4113);
                       SymbolType:=symNumValue;
                       ExprFinal(Result);
                      END;
          symIdent : ErreurEx(FmtLoadStr1(4114, [SymboleMot]));
          ELSE
           Erreur(4102);
         END;
        END;

       var
        St: TVarStruct;

       BEGIN
        ExprFinal(Result);
        WHILE SymbolType=symPeriod DO
         BEGIN
          IF Result.T<>tPointer THEN
           Erreur(4103);
          Result.P:=Immediat(Result);
          VariableObjet:=True;
          ReadSymbol(symPeriod);
          VariableObjet:=False;
          Result.P2:=SymboleVar.Pos;
         {Datas.Position:=4*LongInt(Result.P2);
          Datas.Read(St.Nom, 4);
          Datas.Position:=4*LongInt(NDatas);
          ObjDataDef.Position:=0;
          Cherche:=L;
          while (ObjDataDef.Read(L, 4) = 4) and ((L shr 16) <> Cherche) do
           begin
            ObjDataDef.Read(L, 4);
            L:=0;
           end;
          ReadSymbol(symObjVar);
          Result.T:=TType(Lo(L));}
          ObjDataDef.Position:=SizeOf(St);
          while (ObjDataDef.Read(St, SizeOf(St)) = SizeOf(St))
            and (StrIComp(@Chaines[St.Nom+1], PChar(SymboleMot))<>0) do
           St.InfoType:=tVoid;
          ReadSymbol(symObjVar);
          Result.T:=St.InfoType;
          CASE Result.T OF
           tVector : Result.Instr:=$19;
           tString     : Result.Instr:=$1A;
           tPointer     : Result.Instr:=$1B;
           tFrPointer   : Result.Instr:=$1D;
           ELSE       Result.Instr:=$18;
          END;
          Result.Flags:=[fInstr, fPeutAffecter];
         END;
       END;

      BEGIN
       IF SymbolType=symNot THEN
        BEGIN
         ReadSymbol(symNot);
         ExprNot(Result);
         Result.P:=Immediat(Result);
         CASE Result.T OF
          tString   : Result.Instr:=$2E;
          tPointer   : Result.Instr:=$2F;
          tFrPointer : Result.Instr:=$30;
          ELSE     Result.Instr:=$2C;
         END;
         Result.P2:=0;
         Result.T:=tNumValue;
         Result.Flags:=[fInstr];
        END
       ELSE
        ExprNot(Result);
      END;

     VAR
      E : TExpr;
      Divise : BOOLEAN;

     BEGIN
      ExprFoisDivise(Result);
      WHILE SymbolType IN [symMultiply, symDivide] DO
       BEGIN
        Result.P:=Immediat(Result);
        Divise:=SymbolType=symDivide;
        ReadSymbol(symUnknown);
        ExprFoisDivise(E);
        IF Divise THEN
         BEGIN
          chk(E,tNumValue);
          chk(Result,tNumValue);
          Result.Instr:=$05;
         END
        ELSE
         IF Result.T=tVector THEN
          IF E.T=tVector THEN
           BEGIN
            Result.Instr:=$02;
            Result.T:=tNumValue;
           END
          ELSE
           BEGIN
            chk(E,tNumValue);
            Result.Instr:=$04;
           END
         ELSE
          BEGIN
           chk(Result,tNumValue);
           IF E.T=tVector THEN
            BEGIN
             Result.Instr:=$03;
             Result.T:=tVector;
            END
           ELSE
            BEGIN
             chk(E,tNumValue);
             Result.Instr:=$01;
             Result.T:=tNumValue;
            END;
          END;
        Result.P2:=Immediat(E);
        Transm(E, @Result);
        Result.Flags:=[fInstr];
       END;
     END;

    VAR
     E : TExpr;

    BEGIN
     ExprPlusMoins(Result);
     WHILE (SymbolType IN [symAdd, symSubtract])
      or ((SymbolType = symNumValue) and (SymboleReel<0)) DO
      BEGIN
       Result.P:=Immediat(Result);
       if SymbolType <> symSubtract then
        IF Result.T=tVector THEN
         Result.Instr:=$07
        ELSE
         BEGIN
          chk(Result,tNumValue);
          Result.Instr:=$06;
         END
       else
        IF Result.T=tVector THEN
         Result.Instr:=$09
        ELSE
         BEGIN
          chk(Result,tNumValue);
          Result.Instr:=$08;
         END;
       if SymbolType<>symNumValue then
        ReadSymbol(symUnknown);
       ExprPlusMoins(E);
       chk(E, Result.T);
       Result.P2:=Immediat(E);
       Transm(E, @Result);
       Result.Flags:=[fInstr];
      END;
    END;

   VAR
    E : TExpr;

   BEGIN
    ExprComparaison(Result);
    WHILE SymbolType IN [symEqual, symDifferent, symLessThan, symLessOrEqualTo,
    symGreaterThan, symGreaterOrEqualTo] DO
     BEGIN
      Result.P:=Immediat(Result);
      WITH Result DO
       CASE SymbolType OF
        symEqual : CASE T OF
                   tVector : Instr:=$0B;
                   tString     : Instr:=$0C;
                   tPointer     : Instr:=$0D;
                   tFrPointer   : Instr:=$0E;
                   ELSE Instr:=$0A;
                  END;
        symDifferent : CASE T OF
                        tVector : Instr:=$10;
                        tString     : Instr:=$11;
                        tPointer     : Instr:=$12;
                        tFrPointer   : Instr:=$13;
                        ELSE Instr:=$0F;
                       END;
        symLessThan : Instr:=$16;
        symGreaterThan : Instr:=$17;
        symLessOrEqualTo : Instr:=$14;
        symGreaterOrEqualTo : Instr:=$15;
       END;
      IF Result.Instr>=$14 THEN
       chk(Result,tNumValue);
      ReadSymbol(symUnknown);
      ExprComparaison(E);
      chk(E, Result.T);
      Result.P2:=Immediat(E);
      Transm(E, @Result);
      Result.Flags:=[fInstr];
      Result.T:=tNumValue;
     END;
   END;

  VAR
   E, Cible : TExpr;

  BEGIN
   ExprEtOu(Cible);
   WHILE SymbolType IN [symOr, symAnd, symLogicalOr, symLogicalAnd] DO
    BEGIN
    {chk(Cible,tNumValue);}
     Cible.P:=Immediat(Cible);
     WITH Cible DO
      CASE SymbolType OF
       symOr     : Instr:=$41;
       symAnd    : Instr:=$40;
       symLogicalOr  : Instr:=$3F;
       symLogicalAnd : Instr:=$3E;
      END;
     ReadSymbol(symUnknown);
     ExprEtOu(E);
    {chk(E,tNumValue);}
     Cible.P2:=Immediat(E);
     Transm(E, @Cible);
     Cible.Flags:=[fInstr];
    END;
   IF SymbolType=symAssign THEN
    BEGIN
     IF NOT (fPeutAffecter IN Cible.Flags) THEN
      Erreur(4104);
     ReadSymbol(symAssign);
     Expression(E);
     chk(E, Cible.T);
     IF fInstr IN Cible.Flags THEN
      BEGIN
       Coder($1E, Cible.P, Cible.P2, NDatas);
       Cible.P:=NDatas;
       Datas.Write(Zero, 4);
       INC(NDatas);
      END;
     IF (fInstr IN E.Flags) AND NOT (fInstr IN Cible.Flags) THEN
      BEGIN
       Coder(E.Instr, E.P, E.P2, Cible.P);
       Transm(E, Nil);
       Result.P:=Cible.P;
      END
     ELSE
      BEGIN
       Result.P:=Immediat(E);
       Transm(E, @Result);
       Coder(Affecte[fInstr IN Cible.Flags, Cible.T], Result.P, Cible.P, 0);
      END;
     Result.T:=Cible.T;
     Result.Flags:=[];
    END
   ELSE
    BEGIN
     Result:=Cible;
     Transm(Cible, @Result);
    END;
  END;

  procedure QCodeSeek(Pos1: LongInt; Delta: LongInt);
  begin
   QCode.Position:=(Pos1-Header.Entrees[1].Nb)*8+Delta;
  end;

  function CompilerBloc : Integer;  { result = taille var. locales }

  VAR
   E    : TExpr;
   Pos1 : LONGINT;
   Pos2 : LONGINT;
   VL0  : TStringList;
   Ouvert, PVirg:Boolean;

  BEGIN
   Result:=0;
   VL0:=Nil; try
   Ouvert:=SymbolType=symCurlyBracketLeft;
   if Ouvert then
    ReadSymbol(symCurlyBracketLeft);
   REPEAT
    PVirg:=True;
    CASE SymbolType OF
     symSemiColon : ;
     symCurlyBracketRight : if Ouvert then Break;
    {symDiese : BEGIN
                 ReadSymbol(symDiese);
                 R:=SymboleReel;
                 ReadSymbol(symNumValue);
                 F:=SymboleFunc;
                 ReadSymbol(symAppel);
                 St.InfoType:=tNumValue;
                 St.Sys:=0;
                 St.Pos:=EcrireReel(R);
                 St.Nom:=Immediate;
                 DSDesc.Write(St, SizeOf(St));
                 Coder($3C, St.Pos, F^.Pos, 0);
                END;}
     symIF    : BEGIN
                 ReadSymbol(symIF);
                 ReadSymbol(symBracketLeft);
                 Expression(E);
                 ReadSymbol(symBracketRight);
                 E.P:=Immediat(E);
                 ResultatAppel:=Nil;
                 Pos1:=NQCode;
                 Coder($32, E.P, 0, 0);
                 Inc(Result, CompilerBloc);
                 QCodeSeek(Pos1, +4);
                 Pos1:=NQCode-Pos1;
                 if SymbolType=symSemiColon then
                  ReadSymbol(symSemiColon);
                 IF SymbolType=symELSE THEN
                  INC(Pos1);
                 ControleInt(Pos1);
                 QCode.Write(Pos1, 2);
                 QCodeSeek(NQCode, 0);
                 IF SymbolType=symELSE THEN
                  BEGIN
                   Pos1:=NQCode;
                   Coder($3D, 0, 0, 0);
                   ReadSymbol(symELSE);
                   Inc(Result, CompilerBloc);
                   QCodeSeek(Pos1, +2);
                   Pos1:=NQCode-Pos1;
                   ControleInt(Pos1);
                   QCode.Write(Pos1, 2);
                   QCodeSeek(NQCode, 0);
                  END;
                 PVirg:=False;
                END;
     symWHILE : BEGIN
                 ReadSymbol(symWHILE);
                 Pos1:=NQCode;
                 ReadSymbol(symBracketLeft);
                 Expression(E);
                 ReadSymbol(symBracketRight);
                 E.P:=Immediat(E);
                 ResultatAppel:=Nil;
                 Pos2:=NQCode;
                 Coder($32, E.P, 0, 0);
                 Inc(Result, CompilerBloc);
                 Dec(Pos1, NQCode);
                 ControleInt(Pos1);
                 Coder($3D, Pos1, 0, 0);
                 QCodeSeek(Pos2, +4);
                 Pos1:=NQCode-Pos2;
                 ControleInt(Pos1);
                 QCode.Write(Pos1, 2);
                 QCodeSeek(NQCode, 0);
                 PVirg:=False;
                END;
     symDO    : BEGIN
                 ReadSymbol(symDO);
                 Pos1:=NQCode;
                 Inc(Result, CompilerBloc);
                 ReadSymbol(symWHILE);
                 ReadSymbol(symBracketLeft);
                 Expression(E);
                 ReadSymbol(symBracketRight);
                 E.P:=Immediat(E);
                 ResultatAppel:=Nil;
                 Dec(Pos1, NQCode);
                 ControleInt(Pos1);
                 Coder($31, E.P, Pos1, 0);
                END;
     symRETURN: BEGIN
                 ReadSymbol(symRETURN);
                 if SymbolType<>symSemiColon then
                  begin
                   Expression(E);
                   Coder($2B, Immediat(E), 0, 0);
                   ResultatAppel:=Nil;
                  end
                 else
                  Coder($2B, 0, 0, 0);
                END;
     symCurlyBracketLeft: begin
                    Inc(Result, CompilerBloc);
                    ResultatAppel:=Nil;
                    PVirg:=False;
                   end;
     symLOCAL: begin
                if VL0=Nil then
                 begin
                  VL0:=VarLocales;
                  VarLocales:=TStringList.Create;
                  VarLocales.Assign(VL0);
                  VarLocales.Sorted:=True;
                 end;
                ReadSymbol(symLocal);
                Inc(Result, DefVar(VarLocales, TypeEtendu));
               end;
    ELSE BEGIN
          Expression(E);
          E.P:=Immediat(E);
          IF (SymbolType=symBracketLeft) AND (E.T=tFrPointer) THEN
           CoderAppel(E.P);
          ResultatAppel:=Nil;
         END;
    END;
    if not Ouvert then Exit;
    if PVirg then
     ReadSymbol(symSemiColon);
   {ResultatAppels.DeleteAll;}
   UNTIL False;
   finally
    if VL0<>Nil then
     begin
      VarLocales.Free;
      VarLocales:=VL0;
     end;
   end;
   ReadSymbol(symCurlyBracketRight);
  END;

  function DefVar(L: TStringList; const T: TTypeEtendu) : Integer;
  var
   Taille, I: Integer;
   Variable: TVarStruct;
   F: TFrame;
   V: Vecteur;
   V1_AsLong: LongInt absolute V;
   S: String;

    procedure Definir;
    begin
     Variable.Pos:=NDatas;
     Variable.Sys:=0;
     Variable.Nom:=EcrireChaine(SymboleMot);
     L.AddObject(SymboleMot, Variable.AsObject);
     NDataDef.Write(Variable, SizeOf(Variable));
    end;

  begin
   if T.NbArg<0 then
    begin
     Variable.InfoType:=T.T;
     Taille:=TailleType[T.T];
    end
   else
    begin
     Variable.InfoType:=tFrPointer;
     Taille:=1;
    end;
   if (SymbolType=symVariable)
   and (VariableEstLocale xor (L=VarGlobales)) then
    begin
     if Variable.InfoType <> SymboleVar.InfoType then
      Erreur(4105);
     if (Variable.InfoType=tFrPointer) and (ChargerFrame(SymboleVar.Pos, F)>0) then
      begin
      {if F.Code<0 then
        Erreur(4106);}
       if F.NbArguments <> T.NbArg then
        Erreur(4107);
       for I:=1 to T.NbArg do
        if TailleType[T.Arg[I].T] <> F.Args[I] then
         Erreur(4108);
      end;
     Variable:=SymboleVar;
     ReadSymbol(symVariable);
     Variable.Nom:=0;
    end
   else
    begin
     Definir;
     if SymbolType in [symVariable, symObjVar] then
      ReadSymbol(symUnknown)
     else
      ReadSymbol(symIdent);
     Datas.Position:=4*LongInt(NDatas);
     Datas.Write(Zero, 4*Taille);
     if Variable.InfoType = tVector then
      begin
       Variable.InfoType:=tNumValue;
       S:=SymboleMot;
       SymboleMot:=S + '_x';
       Definir;
       Inc(NDatas);
       SymboleMot:=S + '_y';
       Definir;
       Inc(NDatas);
       SymboleMot:=S + '_z';
       Definir;
       Inc(NDatas);
      end
     else
      Inc(NDatas, Taille);
    end;
   Result:=Taille;
   if SymbolType = symAssign then
    begin
     ReadSymbol(symAssign);
     if T.NbArg<0 then
      case T.T of
       tString: begin
              LongInt(Pointer(@V[1])^):=EcrireChaine(SymboleMot);
              ReadSymbol(symChain);
             end;
       tNumValue: begin
               V[1]:=SymboleReel;
               ReadSymbol(symNumValue);
              end;
       tVector: begin
                  V:=SymboleVecteur;
                  ReadSymbol(symVector);
                 end;
       else Erreur(4109);
      end
     else
      begin
       VarLocales:=TStringList.Create; try
       I:=ChargerFrame(Variable.Pos, F);
       if I>0 then
        begin
         SymboleVar.Pos:=NDatas;
         Inc(NDatas);
         Datas.Write(I, 4);
         SymboleVar.InfoType:=tFrPointer;
         VarLocales.AddObject('INHERITED', SymboleVar.AsObject);
         SymboleVar.Nom:=EcrireChaine('inherited_' + SymboleMot);
         NDataDef.Write(SymboleVar, SizeOf(SymboleVar));
         F.NomFrame:=SymboleVar.Nom;
         NFrames.Position:=I*SizeOf(TFrame);
         NFrames.WriteBuffer(F, SizeOf(TFrame));
        end;
       FillChar(F, SizeOf(F), 0);
       F.Code:=NQCode;
       F.LocData:=NDatas;
       F.LocSize:=0;
       F.NbArguments:=T.NbArg;
       if Variable.Nom=0 then
        Variable.Nom:=EcrireChaine(SymboleMot);
       F.NomFrame:=Variable.Nom;
       F.theFilename:=EcrireChaine(NomPatch+tcExtension[TypeCode]);
       for I:=1 to T.NbArg do
        begin
         Taille:=TailleType[T.Arg[I].T];
         F.Args[I]:=Taille;
         Inc(F.LocSize, Taille);
         SymboleVar.Pos:=NDatas;
         Inc(NDatas, Taille);
         Datas.Write(Zero, 4*Taille);
         SymboleVar.InfoType:=T.Arg[I].T;
         SymboleVar.Sys:=0;
         VarLocales.AddObject(T.Arg[I].Nom, SymboleVar.AsObject);
         SymboleVar.Nom:=EcrireChaine(T.Arg[I].Nom);
         NDataDef.Write(SymboleVar, SizeOf(SymboleVar));
         if SymboleVar.InfoType = tVector then
          begin
           SymboleVar.InfoType:=tNumValue;
           VarLocales.AddObject(T.Arg[I].Nom+'_x', SymboleVar.AsObject);
           SymboleVar.Nom:=EcrireChaine(T.Arg[I].Nom+'_x');
           NDataDef.Write(SymboleVar, SizeOf(SymboleVar));
           Inc(SymboleVar.Pos);
           VarLocales.AddObject(T.Arg[I].Nom+'_y', SymboleVar.AsObject);
           SymboleVar.Nom:=EcrireChaine(T.Arg[I].Nom+'_y');
           NDataDef.Write(SymboleVar, SizeOf(SymboleVar));
           Inc(SymboleVar.Pos);
           VarLocales.AddObject(T.Arg[I].Nom+'_z', SymboleVar.AsObject);
           SymboleVar.Nom:=EcrireChaine(T.Arg[I].Nom+'_z');
           NDataDef.Write(SymboleVar, SizeOf(SymboleVar));
          end;
        end;
       VarLocales.Sorted:=True;

       if (SymbolType=symSquareBracketLeft) and (F.NbArguments=0) then
        begin
         ReadSymbol(symSquareBracketLeft);
         if SymbolType=symNumValue then
          begin
           V[2]:=SymboleReel;
           ReadSymbol(symNumValue);
          end
         else
          begin
           ReadSymbol(symDollar);
           if not (SymbolType in [symIdent, symVariable, symObjVar]) then
            Erreur(4113);
           I:=MdlFrames.IndexOf(SymboleMot);
           if I<0 then
            Erreur(4113);
           V[2]:=I;
           ReadSymbol(symUnknown);
          end;
         I:=NDatas;
         Inc(NDatas);
         Datas.Write(V[2], 4);
         ReadSymbol(symComma);
         if SymbolType=symVariable then
          begin
           if SymboleVar.InfoType<>tFrPointer then
            Erreur(4111);
           Coder($3C, I, SymboleVar.Pos, 0);
           ReadSymbol(symVariable);
          end
         else
          begin
           SymboleVar.Pos:=Variable.Pos;
           Definir;
           Inc(NDatas);
           Datas.Write(Zero, 4);
           Coder($3C, I, Variable.Pos, 0);
           Variable.Pos:=SymboleVar.Pos;
           if SymbolType=symObjVar then
            ReadSymbol(symObjVar)
           else
            ReadSymbol(symIdent);
          end;
         ReadSymbol(symSquareBracketRight);
        end;

       ResultatAppel:=Nil;
       Inc(F.LocSize, CompilerBloc);
       Coder(0, 0, 0, 0);

       finally VarLocales.Free; end;
       VarLocales:=Nil;
       Taille:=1;

       V1_AsLong:=NoFrame;
       NFrames.Position:=NFrames.Size;
       NFrames.Write(F, SizeOf(F));
       Inc(NoFrame);
      end;
     Datas.Position:=4*LongInt(Variable.Pos);
     Datas.Write(V, 4*Taille);
    end
   else
    if SymbolType=symComma then
     begin
      ReadSymbol(symComma);
      Inc(Result, DefVar(L, T));
     end;
  end;

  procedure DefObjVar;
  var
   T: TType;
   L: LongInt;
   C: Char;
  begin
   ReadSymbol(symPeriod);
   T:=SymboleType;
   ReadSymbol(symType);
   if SymbolType=symBracketLeft then
    begin
     ReadSymbol(symBracketLeft);
     if SymbolType<>symBracketRight then
      repeat
       ReadSymbol(symType);
       ReadSymbol(symUnknown);
       if SymbolType=symBracketRight then Break;
       ReadSymbol(symComma);
      until False;
     ReadSymbol(symBracketRight);
     T:=tFrPointer;
    end;
   repeat
    SymboleVar.InfoType:=T;
    SymboleVar.Sys:=0;
    SymboleVar.Pos:=NDatas;
    Datas.Position:=4*LongInt(NDatas);
   {ObjDataDef.Position:=ObjDataDef.Size-8;
    ObjDataDef.Read(SymboleVar, 8);
    Inc(SymboleVar.Pos, TailleType[SymboleVar.InfoType]);
    L:=SymboleVar.Pos;}
    ObjDataDef.Position:=ObjDataDef.Size;
    L:=Header.TailleLocData;
    SymboleVar.Pos:=L;
    SymboleVar.InfoType:=T;
    SymboleVar.Nom:=EcrireChaine(SymboleMot);
    ObjDataDef.Write(SymboleVar, 8);
    Header.TailleLocData:=L+TailleType[T];
    SymboleVar.InfoType:=tObjData;
    SymboleVar.Pos:=NDatas;
    NDataDef.Write(SymboleVar, 8);
    VarObjet.AddObject(SymboleMot, SymboleVar.AsObject);
    Inc(NDatas);
    Datas.Write(L, 4);
    if T=tVector then
     for C:='x' to 'z' do
      begin
       SymboleVar.InfoType:=tNumValue;
       SymboleVar.Pos:=L;
       SymboleVar.Nom:=EcrireChaine(SymboleMot+'_'+C);
       ObjDataDef.Write(SymboleVar, 8);
       SymboleVar.InfoType:=tObjData;
       SymboleVar.Pos:=NDatas;
       NDataDef.Write(SymboleVar, 8);
       VarObjet.AddObject(SymboleMot+'_'+C, SymboleVar.AsObject);
       Inc(NDatas);
       Datas.Write(L, 4);
       Inc(L);
      end;
    ReadSymbol(symUnknown);
    if SymbolType<>symComma then Break;
    ReadSymbol(symComma);
   until False;
  end;

  procedure DefCodeImpulse;
  var
   F: TFrame;
  begin
   ReadSymbol(symBIND);
   Touches.Add(#255 + SymboleMot);
   ReadSymbol(symChain);
   ReadSymbol(symComma);
   Touches.Add(SymboleMot);
   ReadSymbol(symChain);
   ReadSymbol(symComma);
   if SymbolType<>symVariable then
    begin
     Touches.Add(SymboleMot);
     ReadSymbol(symChain);
    end
   else
    begin
     if SymboleVar.InfoType<>tFrPointer then
      Erreur(4111);
     if ChargerFrame(SymboleVar.Pos, F)>0 then
      begin
       if (F.Code<0) or (F.NbArguments>1) then
        Erreur(4111);
       if (F.NbArguments=1) and (F.Args[1]<>1) then
        Erreur(4111);
      end
     else
      F.NbArguments:=0;
     if F.NbArguments=0 then
      Touches.Add('impulse ' + IntToStr(NoImpulse))
     else
      begin
       Touches.Add('+impulse' + IntToStr(NoImpulse));
       Touches.Add('alias +impulse' + IntToStr(NoImpulse) + ' "impulse ' + IntToStr(NoImpulse) + '"');
       Touches.Add('alias -impulse' + IntToStr(NoImpulse) + ' "impulse ' + IntToStr(NoImpulse+1) + '"');
      end;
     if NoImpulse=Impulse0 then
      begin
       Patch.Add(#255'QuArK');
       Patch.Add('Void() W_WeaponFrame = {');
       Patch.Add('');
      end;
     if F.NbArguments=0 then
      Patch[Patch.Count-1]:='if (self.impulse == ' + IntToStr(NoImpulse)
       + ') { ' + SymboleMot + '(); self.impulse=0; };'
     else
      begin
       Patch[Patch.Count-1]:='if (self.impulse == ' + IntToStr(NoImpulse)
        + ') { ' + SymboleMot + '(1); self.impulse=0; } else if (self.impulse == '
        + IntToStr(NoImpulse+1) + ') { ' + SymboleMot + '(0); self.impulse=0; };';
       Inc(NoImpulse);
      end;
     Patch.Add('inherited(); };');
     ReadSymbol(symVariable);
     Inc(NoImpulse);
    end;
  end;

  procedure DefCodeAutoexec;
  var
   F: TFrame;
   Chaine: String;
  begin
   ReadSymbol(symAUTOEXEC);
   Chaine:='';
   repeat
    if SymbolType<>symVariable then
     begin
      Chaine:=Chaine+SymboleMot;
      ReadSymbol(symChain);
     end
    else
     begin
      if SymboleVar.InfoType<>tFrPointer then
       Erreur(4111);
      if ChargerFrame(SymboleVar.Pos, F)>0 then
       begin
        if (F.Code<0) or (F.NbArguments>0) then
         Erreur(4111);
       end;
      if NoImpulse=Impulse0 then
       begin
        Patch.Add(#255'QuArK');
        Patch.Add('Void() W_WeaponFrame = {');
        Patch.Add('');
       end;
      Patch[Patch.Count-1]:='if (self.impulse == ' + IntToStr(NoImpulse)
       + ') { ' + SymboleMot + '(); self.impulse=0; };';
      Patch.Add('inherited(); };');
      Chaine:=Chaine+'impulse '+IntToStr(NoImpulse);
      ReadSymbol(symVariable);
      Inc(NoImpulse);
     end;
    if SymbolType<>symComma then Break;
    ReadSymbol(symComma);
   until False;
   Touches.Add(Chaine);
  end;

  procedure DefMdlFrames;
  var
   S, Nouveau: String;
   Espace: Integer;
  begin
   ReadSymbol(symDollar);
   S:=PL;
   PL:=StrEnd(PL);
   if (SymbolType in [symIdent, symVariable, symObjVar])
   and (CompareText(SymboleMot, 'frame')=0) then
    repeat
     S:=Trim(S);
     if S='' then Break;
     Espace:=Pos(' ', S);
     if Espace=0 then
      Espace:=Length(S)+1;
     Nouveau:=Copy(S, 1, Espace-1);
     Delete(S, 1, Espace);
    {if MdlFrames.IndexOf(Nouveau)>=0 then
      Erreur(....);}
     MdlFrames.Add(Nouveau);
    until False;
   SymbolType:=symSemiColon;
  end;

  function ReadEntry(Numero: Integer) : String;
  var
   T: Integer;
  begin
   T:=Header.Entrees[Numero].Nb * FacteurT[Numero];
   SetLength(Result, T);
   Source.Position:=Origine+Header.Entrees[Numero].Pos;
   Source.ReadBuffer(PChar(Result)^, T);
  end;

  procedure EcrireResultat;
  var
   nEntete: TEntete;
  begin
   Chaines:=Chaines + Copy(#0#0#0, (Length(Chaines)-1) and 3 + 1, 3);
   nEntete:=Header;
   nEntete.Entrees[1].Nb:=NQCode;
   Inc(nEntete.Entrees[2].Nb, NDataDef.Size div 8);
   nEntete.Entrees[3].Nb:=ObjDataDef.Size div 8;
   nEntete.Entrees[4].Nb:=NoFrame;
   nEntete.Entrees[5].Nb:=Length(Chaines);
   nEntete.Entrees[6].Nb:=Datas.Size div 4;
   if nEntete.Entrees[6].Nb > 65535 then
    Erreur(4112);
   Dest.WriteBuffer(nEntete, SizeOf(nEntete));
   nEntete.Entrees[5].Pos:=Dest.Position;
   Dest.WriteBuffer(PChar(Chaines)^, Length(Chaines));
   nEntete.Entrees[1].Pos:=Dest.Position;
   Source.Position:=Origine+Header.Entrees[1].Pos;
   if Header.Entrees[1].Nb>0 then
    Dest.CopyFrom(Source, Header.Entrees[1].Nb*8);
   QCode.Position:=0;
   if NQCode>Header.Entrees[1].Nb then
    Dest.CopyFrom(QCode, (NQCode-Header.Entrees[1].Nb)*8);
   nEntete.Entrees[4].Pos:=Dest.Position;
  {Source.Position:=Origine+Header.Entrees[4].Pos;
   Dest.CopyFrom(Source, Header.Entrees[4].Nb*SizeOf(TFrame));}
   NFrames.Position:=0;
   if NoFrame>0 then
    Dest.CopyFrom(NFrames, (NoFrame{-Header.Entrees[4].Nb})*SizeOf(TFrame));
   nEntete.Entrees[2].Pos:=Dest.Position;
   Source.Position:=Origine+Header.Entrees[2].Pos;
   if Header.Entrees[2].Nb>0 then
    Dest.CopyFrom(Source, Header.Entrees[2].Nb*8);
   Dest.CopyFrom(NDataDef, 0);
   nEntete.Entrees[3].Pos:=Dest.Position;
   Dest.CopyFrom(ObjDataDef, 0);
   nEntete.Entrees[6].Pos:=Dest.Position;
   Dest.CopyFrom(Datas, 0);
   Dest.Position:=0;
   Dest.WriteBuffer(nEntete, SizeOf(nEntete));
  end;

  procedure TrieVariables(L, R: LongInt);
  var
    I, J, Resultat: LongInt;
    P, T: TVarStruct;
    NomP: PChar;
  begin
    repeat
      I := L;
      J := R;
      P := Vars^[(L + R) shr 1];
      NomP:=PChar(@Chaines[P.Nom+1]);
      repeat
        repeat
         Resultat:=StrIComp(PChar(@Chaines[Vars^[I].Nom+1]), NomP);
         if (Resultat > 0)
         or ((Resultat = 0) and (Vars^[I].Pos <= P.Pos)) then
          Break;
         Inc(I);
        until False;
        repeat
         Resultat:=StrIComp(PChar(@Chaines[Vars^[J].Nom+1]), NomP);
         if (Resultat < 0)
         or ((Resultat = 0) and (Vars^[J].Pos >= P.Pos)) then
          Break;
         Dec(J);
        until False;
        if I <= J then
        begin
          T := Vars^[I];
          Vars^[I] := Vars^[J];
          Vars^[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then TrieVariables(L, J);
      L := I;
    until I >= R;
  end;

begin
 FillChar(Zero, SizeOf(Zero), 0);
(*if CompareText(sTypeCode, 'HexenC')=0 then
  TypeCode:=tcHexenC
 else
  TypeCode:=tcQuakeC;*)
 NomPatch:='NoName';
 NoImpulse:=Impulse0;
  { lecture des informations principales du Progs.dat original }
 Origine:=Source.Position;
 Source.ReadBuffer(Header, SizeOf(Header));
 if Header.Version <> VersionProgsDat then
  Raise EError(4095);
 Chaines:=ReadEntry(5);
 GetMem(Vars, Pred(Header.Entrees[2].Nb)*SizeOf(TVarStruct));
 Datas:=TMemoryStream.Create;
 ObjDataDef:=TMemoryStream.Create;
 NDataDef:=TMemoryStream.Create;
 try
  Source.Position:=Origine+Header.Entrees[2].Pos + SizeOf(TVarStruct);
  Source.ReadBuffer(Vars^, Pred(Header.Entrees[2].Nb)*SizeOf(TVarStruct));
  TrieVariables(0, Header.Entrees[2].Nb-2);
  Source.Position:=Origine+Header.Entrees[6].Pos;
  if Header.Entrees[6].Nb>0 then
   Datas.CopyFrom(Source, Header.Entrees[6].Nb*4);
  Source.Position:=Origine+Header.Entrees[3].Pos;
  if Header.Entrees[3].Nb>0 then
   ObjDataDef.CopyFrom(Source, Header.Entrees[3].Nb*8);
  NFrames:=TMemoryStream.Create;
  Source.Position:=Origine+Header.Entrees[4].Pos;
  if Header.Entrees[4].Nb>0 then
   NFrames.CopyFrom(Source, Header.Entrees[4].Nb*SizeOf(TFrame));
   { compilation du patch }
  QCode:=TMemoryStream.Create;
  VarGlobales:=TStringList.Create;
  VarObjet:=TStringList.Create;
  MdlFrames:=TStringList.Create;
  try
   VariableObjet:=False;
   NDatas:=Header.Entrees[6].Nb;
   NoFrame:=Header.Entrees[4].Nb;
   NQCode:=Header.Entrees[1].Nb;
   Commentaires:=False;
   LigneCourante:=0;
   LigneLocale:=0;
   Ligne:='';
   PL:=PChar(Ligne);
   VarLocales:=Nil;
   VarGlobales.Sorted:=True;
   VarObjet.Sorted:=True;
   Immediate:=EcrireChaine(tcImmediate[TypeCode]);
   ReadSymbol(symUnknown);
   while SymbolType<>symEOF do
    begin
     case SymbolType of
      symType: DefVar(VarGlobales, TypeEtendu);
      symPeriod: DefObjVar;
      symBIND: DefCodeImpulse;
      symAUTOEXEC: DefCodeAutoexec;
      symDollar: DefMdlFrames;
      else Erreur(4110);
     end;
     ReadSymbol(symSemiColon);
    end;
   EcrireResultat;
  finally
   MdlFrames.Free;
   VarObjet.Free;
   VarGlobales.Free;
   QCode.Free;
   NFrames.Free;
  end;
 finally
  NDataDef.Free;
  ObjDataDef.Free;
  Datas.Free;
  FreeMem(Vars);
 end;
end;

constructor ECompileError.Create;
begin
 inherited Create(FmtLoadStr1(4159, [Erreur, nNomPatch, nNoLigne+1, Extrait]));
 MsgErreur:=Erreur;
 NoLigne:=nNoLigne;
end;

end.

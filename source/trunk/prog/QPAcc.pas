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

unit QPAcc;

interface

uses Classes, SysUtils;

type
 TType = (
        tInconnu
       ,tStr
       ,tReel
       ,tVecteur
       ,tPtr
       ,tObjData
       ,tFrPtr
 );
 TSymbole = (
        symInconnu
       ,symType
       ,symIdent
       ,symVariable
       ,symObjVar
       ,symDeuxPoints
       ,symPointVirgule
       ,symPoint
       ,symAffecte
       ,symVirgule
       ,symPlus
       ,symMoins
       ,symFois
       ,symDivise
       ,symAnd
       ,symOr
       ,symNot
       ,symLogOr
       ,symLogAnd
       ,symEgal
       ,symDifferent
       ,symPlusPetit
       ,symPlusGrand
       ,symPlusPetitOuEgal
       ,symPlusGrandOuEgal
       ,symAccolade1
       ,symAccolade2
       ,symParenthese1
       ,symParenthese2
       ,symLOCAL
       ,symBIND
       ,symAUTOEXEC
       ,symIF
       ,symELSE
       ,symWHILE
       ,symDO
       ,symRETURN
       ,symReel
       ,symChaine
       ,symVecteur
       ,symEOF
       ,symDollar
       ,symCrochet1
       ,symCrochet2
 );

const
 Impulse0Def = 166;
 VersionProgsDat = 6;
 MotsClefs: array[symLOCAL..symRETURN] of String =
  ('LOCAL', 'BIND', 'AUTOEXEC', 'IF', 'ELSE', 'WHILE', 'DO', 'RETURN');
 NomsTypes: array[tInconnu..tPtr] of String =
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

uses {Tableaux,} Quarkx;

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
 Entete: TEntete;
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
 Symbole: TSymbole;
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

  function ChercheVar(Nom: String) : Boolean;
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
   Max:=Entete.Entrees[2].Nb-1;
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
        Max:=Entete.Entrees[2].Nb-1;
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

  PROCEDURE Lire(Attendu : TSymbole);

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

    PROCEDURE Lu(nSymbole : TSymbole);

    BEGIN
     Symbole:=nSymbole;
     INC(PL);
    END;

    PROCEDURE Lu2(Sym1, Sym2 : TSymbole);

    BEGIN
     IF PL[1]='=' THEN
      BEGIN
       Symbole:=Sym2;
       INC(PL, 2);
      END
     ELSE
      Lu(Sym1);
    END;

    FUNCTION LireReel : Single;

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
   IF (Symbole<>Attendu) AND (Attendu<>symInconnu) THEN
    ErreurEx(FmtLoadStr1(4097, [
     LoadStr1(4160+Ord(Attendu)),
     LoadStr1(4160+Ord(Symbole))]));
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
                         Symbole:=Low(MotsClefs);
                         while (Symbole<=High(MotsClefs)) and (CompareText(SymboleMot, MotsClefs[Symbole])<>0) do
                          Inc(Symbole);
                         if Symbole>High(MotsClefs) then
                          begin
                           SymboleType:=Low(NomsTypes);
                           while (SymboleType<=High(NomsTypes)) and (CompareText(SymboleMot, NomsTypes[SymboleType])<>0) do
                            Inc(SymboleType);
                           if SymboleType<=High(NomsTypes) then
                            Symbole:=symType
                           else
                            begin
                             Symbole:=symVariable;
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
                                    Symbole:=symIdent;
                                    SymboleVar.InfoType:=tInconnu;
                                   end;
                                 end;
                               if SymboleVar.InfoType=tObjData then
                                Symbole:=symObjVar;
                              end;
                            end;
                          end;
                        END;
    '0'..'9' : BEGIN
                Symbole:=symReel;
                SymboleReel:=LireReel;
               END;
    ';' : Lu(symPointVirgule);
    '{' : Lu(symAccolade1);
    '}' : Lu(symAccolade2);
    ':' : Lu(symDeuxPoints);
    '=' : Lu2(symAffecte, symEgal);
    '+' : Lu(symPlus);
    '-' : IF PL[1] IN ['0'..'9'] THEN
           BEGIN
            Symbole:=symReel;
            SymboleReel:=LireReel;
           END
          ELSE
           Lu(symMoins);
    '*' : Lu(symFois);
    '/' : Lu(symDivise);
    '&' : BEGIN
           Lu(symAnd);
           IF PL^='&' THEN
            Lu(symLogAnd);
          END;
    '|' : BEGIN
           Lu(symOr);
           IF PL^='|' THEN
            Lu(symLogOr);
          END;
    '!' : Lu2(symNot, symDifferent);
    '<' : Lu2(symPlusPetit, symPlusPetitOuEgal);
    '>' : Lu2(symPlusGrand, symPlusGrandOuEgal);
    '(' : Lu(symParenthese1);
    ')' : Lu(symParenthese2);
    '.' : Lu(symPoint);
    ',' : Lu(symVirgule);
    '''': BEGIN
           INC(PL);
           Trim;
           FOR I:=1 TO 3 DO
            BEGIN
             SymboleVecteur[I]:=LireReel;
             Trim;
            END;
           IF PL^<>'''' THEN
            Erreur(4098);
           Lu(symVecteur);
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
           Lu(symChaine);
          END;
    '$': Lu(symDollar);
    '[': Lu(symCrochet1);
    ']': Lu(symCrochet2);
    #0 : begin
          Symbole:=symEOF;
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
   Lire(symType);
   if Symbole=symParenthese1 then
    begin
     Lire(symParenthese1);
     Result.NbArg:=0;
     if Symbole<>symParenthese2 then
      repeat
       if Result.NbArg=8 then
        Erreur(4117);
       Inc(Result.NbArg);
       with Result.Arg[Result.NbArg] do
        begin
         T:=SymboleType;
         Lire(symType);
         if (T=tInconnu) and (Symbole=symParenthese1) then
          begin
           Lire(symParenthese1);
           Lire(symParenthese2);
           T:=tFrPtr;
          end;
         Nom:=SymboleMot;
        end;
       if Symbole in [symVariable, symObjVar] then
        Lire(symInconnu)
       else
        Lire(symIdent);
       if Symbole=symParenthese2 then Break;
       Lire(symVirgule);
      until False;
     Lire(symParenthese2);
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
   {if L>=Entete.Entrees[4].Nb then
     begin
      NFrames.Position:=(L-Entete.Entrees[4].Nb)*SizeOf(TFrame);
      NFrames.ReadBuffer(F, SizeOf(TFrame));
     end
    else
     begin
      Source.Position:=Origine+Entete.Entrees[4].Pos + L*SizeOf(TFrame);
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
   Lire(symParenthese1);
   NbArg:=0;
   IF Symbole<>symParenthese2 THEN
    REPEAT
     if NbArg=8 then
      Erreur(4117);
     INC(NbArg);
     IF Symbole=symObjVar THEN
      WITH Args[NbArg] DO
       BEGIN
        P:=SymboleVar.Pos;
        Flags:=[];
        T:=tObjData;
        Lire(symObjVar);
       END
     ELSE
      Expression(Args[NbArg]);
     IF Symbole=symParenthese2 THEN Break;
     Lire(symVirgule);
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
   Lire(symParenthese2);
  END;

  PROCEDURE chk(const E : TExpr; T : TType);

  BEGIN
   IF (E.T<>tInconnu) and (T<>tInconnu) and (E.T<>T) THEN
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
          Lire(symVariable);
          IF Symbole=symParenthese1 THEN
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
            Result.T:=tInconnu;
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
            Result.T:=tFrPtr;
           END;
          Result.Flags:=[];
         END;

        VAR
         St : TVarStruct;

        BEGIN
         CASE Symbole OF
          symVariable    : if SymboleVar.InfoType=tFrPtr then
                            Appel
                           else
                            BEGIN
                             Result.P:=SymboleVar.Pos;
                             Result.T:=SymboleVar.InfoType;
                             Result.Flags:=[fPeutAffecter];
                             Lire(symVariable);
                            END;
          symReel        : BEGIN
                            St.InfoType:=tReel;
                            St.Sys:=0;
                            St.Pos:=EcrireReel(SymboleReel);
                            St.Nom:=Immediate;
                            NDataDef.Write(St, SizeOf(St));
                            Result.P:=St.Pos;
                            Result.T:=tReel;
                            Result.Flags:=[];
                            Lire(symReel);
                           END;
          symChaine      : BEGIN
                            SymboleReel_AsLong:=EcrireChaine(SymboleMot);
                           {IF ObjChaine^.Immediat=0 THEN
                             BEGIN
                              ObjChaine^.Immediat:=NDatas;
                              Datas.Write(SymboleReel, 4);
                              INC(NDatas);
                             END;}
                            St.InfoType:=tStr;
                            St.Sys:=0;
                            St.Pos:={ObjChaine^.Immediat} EcrireReel(SymboleReel);
                            St.Nom:=Immediate;
                            NDataDef.Write(St, SizeOf(St));
                            Result.P:=St.Pos;
                            Result.T:=tStr;
                            Result.Flags:=[];
                            Lire(symChaine);
                           END;
          symVecteur     : BEGIN
                            St.InfoType:=tVecteur;
                            St.Sys:=0;
                            St.Pos:=EcrireVecteur(SymboleVecteur);
                            St.Nom:=Immediate;
                            NDataDef.Write(St, SizeOf(St));
                            Result.P:=St.Pos;
                            Result.T:=tVecteur;
                            Result.Flags:=[];
                            Lire(symVecteur);
                           END;
          symParenthese1 : BEGIN
                            Lire(symParenthese1);
                            Expression(Result);
                            Lire(symParenthese2);
                           END;
          symDollar : BEGIN
                       Lire(symDollar);
                       if not (Symbole in [symIdent, symVariable, symObjVar]) then
                        Erreur(4113);
                       SymboleReel:=MdlFrames.IndexOf(SymboleMot);
                       if SymboleReel<0 then
                        Erreur(4113);
                       Symbole:=symReel;
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
        WHILE Symbole=symPoint DO
         BEGIN
          IF Result.T<>tPtr THEN
           Erreur(4103);
          Result.P:=Immediat(Result);
          VariableObjet:=True;
          Lire(symPoint);
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
          Lire(symObjVar);
          Result.T:=TType(Lo(L));}
          ObjDataDef.Position:=SizeOf(St);
          while (ObjDataDef.Read(St, SizeOf(St)) = SizeOf(St))
            and (StrIComp(@Chaines[St.Nom+1], PChar(SymboleMot))<>0) do
           St.InfoType:=tInconnu;
          Lire(symObjVar);
          Result.T:=St.InfoType;
          CASE Result.T OF
           tVecteur : Result.Instr:=$19;
           tStr     : Result.Instr:=$1A;
           tPtr     : Result.Instr:=$1B;
           tFrPtr   : Result.Instr:=$1D;
           ELSE       Result.Instr:=$18;
          END;
          Result.Flags:=[fInstr, fPeutAffecter];
         END;
       END;

      BEGIN
       IF Symbole=symNot THEN
        BEGIN
         Lire(symNot);
         ExprNot(Result);
         Result.P:=Immediat(Result);
         CASE Result.T OF
          tStr   : Result.Instr:=$2E;
          tPtr   : Result.Instr:=$2F;
          tFrPtr : Result.Instr:=$30;
          ELSE     Result.Instr:=$2C;
         END;
         Result.P2:=0;
         Result.T:=tReel;
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
      WHILE Symbole IN [symFois, symDivise] DO
       BEGIN
        Result.P:=Immediat(Result);
        Divise:=Symbole=symDivise;
        Lire(symInconnu);
        ExprFoisDivise(E);
        IF Divise THEN
         BEGIN
          chk(E,tReel);
          chk(Result,tReel);
          Result.Instr:=$05;
         END
        ELSE
         IF Result.T=tVecteur THEN
          IF E.T=tVecteur THEN
           BEGIN
            Result.Instr:=$02;
            Result.T:=tReel;
           END
          ELSE
           BEGIN
            chk(E,tReel);
            Result.Instr:=$04;
           END
         ELSE
          BEGIN
           chk(Result,tReel);
           IF E.T=tVecteur THEN
            BEGIN
             Result.Instr:=$03;
             Result.T:=tVecteur;
            END
           ELSE
            BEGIN
             chk(E,tReel);
             Result.Instr:=$01;
             Result.T:=tReel;
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
     WHILE (Symbole IN [symPlus, symMoins])
      or ((Symbole = symReel) and (SymboleReel<0)) DO
      BEGIN
       Result.P:=Immediat(Result);
       if Symbole <> symMoins then
        IF Result.T=tVecteur THEN
         Result.Instr:=$07
        ELSE
         BEGIN
          chk(Result,tReel);
          Result.Instr:=$06;
         END
       else
        IF Result.T=tVecteur THEN
         Result.Instr:=$09
        ELSE
         BEGIN
          chk(Result,tReel);
          Result.Instr:=$08;
         END;
       if Symbole<>symReel then
        Lire(symInconnu);
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
    WHILE Symbole IN [symEgal, symDifferent, symPlusPetit, symPlusPetitOuEgal,
    symPlusGrand, symPlusGrandOuEgal] DO
     BEGIN
      Result.P:=Immediat(Result);
      WITH Result DO
       CASE Symbole OF
        symEgal : CASE T OF
                   tVecteur : Instr:=$0B;
                   tStr     : Instr:=$0C;
                   tPtr     : Instr:=$0D;
                   tFrPtr   : Instr:=$0E;
                   ELSE Instr:=$0A;
                  END;
        symDifferent : CASE T OF
                        tVecteur : Instr:=$10;
                        tStr     : Instr:=$11;
                        tPtr     : Instr:=$12;
                        tFrPtr   : Instr:=$13;
                        ELSE Instr:=$0F;
                       END;
        symPlusPetit : Instr:=$16;
        symPlusGrand : Instr:=$17;
        symPlusPetitOuEgal : Instr:=$14;
        symPlusGrandOuEgal : Instr:=$15;
       END;
      IF Result.Instr>=$14 THEN
       chk(Result,tReel);
      Lire(symInconnu);
      ExprComparaison(E);
      chk(E, Result.T);
      Result.P2:=Immediat(E);
      Transm(E, @Result);
      Result.Flags:=[fInstr];
      Result.T:=tReel;
     END;
   END;

  VAR
   E, Cible : TExpr;

  BEGIN
   ExprEtOu(Cible);
   WHILE Symbole IN [symOr, symAnd, symLogOr, symLogAnd] DO
    BEGIN
    {chk(Cible,tReel);}
     Cible.P:=Immediat(Cible);
     WITH Cible DO
      CASE Symbole OF
       symOr     : Instr:=$41;
       symAnd    : Instr:=$40;
       symLogOr  : Instr:=$3F;
       symLogAnd : Instr:=$3E;
      END;
     Lire(symInconnu);
     ExprEtOu(E);
    {chk(E,tReel);}
     Cible.P2:=Immediat(E);
     Transm(E, @Cible);
     Cible.Flags:=[fInstr];
    END;
   IF Symbole=symAffecte THEN
    BEGIN
     IF NOT (fPeutAffecter IN Cible.Flags) THEN
      Erreur(4104);
     Lire(symAffecte);
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
   QCode.Position:=(Pos1-Entete.Entrees[1].Nb)*8+Delta;
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
   Ouvert:=Symbole=symAccolade1;
   if Ouvert then
    Lire(symAccolade1);
   REPEAT
    PVirg:=True;
    CASE Symbole OF
     symPointVirgule : ;
     symAccolade2 : if Ouvert then Break;
    {symDiese : BEGIN
                 Lire(symDiese);
                 R:=SymboleReel;
                 Lire(symReel);
                 F:=SymboleFunc;
                 Lire(symAppel);
                 St.InfoType:=tReel;
                 St.Sys:=0;
                 St.Pos:=EcrireReel(R);
                 St.Nom:=Immediate;
                 DSDesc.Write(St, SizeOf(St));
                 Coder($3C, St.Pos, F^.Pos, 0);
                END;}
     symIF    : BEGIN
                 Lire(symIF);
                 Lire(symParenthese1);
                 Expression(E);
                 Lire(symParenthese2);
                 E.P:=Immediat(E);
                 ResultatAppel:=Nil;
                 Pos1:=NQCode;
                 Coder($32, E.P, 0, 0);
                 Inc(Result, CompilerBloc);
                 QCodeSeek(Pos1, +4);
                 Pos1:=NQCode-Pos1;
                 if Symbole=symPointVirgule then
                  Lire(symPointVirgule);
                 IF Symbole=symELSE THEN
                  INC(Pos1);
                 ControleInt(Pos1);
                 QCode.Write(Pos1, 2);
                 QCodeSeek(NQCode, 0);
                 IF Symbole=symELSE THEN
                  BEGIN
                   Pos1:=NQCode;
                   Coder($3D, 0, 0, 0);
                   Lire(symELSE);
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
                 Lire(symWHILE);
                 Pos1:=NQCode;
                 Lire(symParenthese1);
                 Expression(E);
                 Lire(symParenthese2);
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
                 Lire(symDO);
                 Pos1:=NQCode;
                 Inc(Result, CompilerBloc);
                 Lire(symWHILE);
                 Lire(symParenthese1);
                 Expression(E);
                 Lire(symParenthese2);
                 E.P:=Immediat(E);
                 ResultatAppel:=Nil;
                 Dec(Pos1, NQCode);
                 ControleInt(Pos1);
                 Coder($31, E.P, Pos1, 0);
                END;
     symRETURN: BEGIN
                 Lire(symRETURN);
                 if Symbole<>symPointVirgule then
                  begin
                   Expression(E);
                   Coder($2B, Immediat(E), 0, 0);
                   ResultatAppel:=Nil;
                  end
                 else
                  Coder($2B, 0, 0, 0);
                END;
     symAccolade1: begin
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
                Lire(symLocal);
                Inc(Result, DefVar(VarLocales, TypeEtendu));
               end;
    ELSE BEGIN
          Expression(E);
          E.P:=Immediat(E);
          IF (Symbole=symParenthese1) AND (E.T=tFrPtr) THEN
           CoderAppel(E.P);
          ResultatAppel:=Nil;
         END;
    END;
    if not Ouvert then Exit;
    if PVirg then
     Lire(symPointVirgule);
   {ResultatAppels.DeleteAll;}
   UNTIL False;
   finally
    if VL0<>Nil then
     begin
      VarLocales.Free;
      VarLocales:=VL0;
     end;
   end;
   Lire(symAccolade2);
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
     Variable.InfoType:=tFrPtr;
     Taille:=1;
    end;
   if (Symbole=symVariable)
   and (VariableEstLocale xor (L=VarGlobales)) then
    begin
     if Variable.InfoType <> SymboleVar.InfoType then
      Erreur(4105);
     if (Variable.InfoType=tFrPtr) and (ChargerFrame(SymboleVar.Pos, F)>0) then
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
     Lire(symVariable);
     Variable.Nom:=0;
    end
   else
    begin
     Definir;
     if Symbole in [symVariable, symObjVar] then
      Lire(symInconnu)
     else
      Lire(symIdent);
     Datas.Position:=4*LongInt(NDatas);
     Datas.Write(Zero, 4*Taille);
     if Variable.InfoType = tVecteur then
      begin
       Variable.InfoType:=tReel;
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
   if Symbole = symAffecte then
    begin
     Lire(symAffecte);
     if T.NbArg<0 then
      case T.T of
       tStr: begin
              LongInt(Pointer(@V[1])^):=EcrireChaine(SymboleMot);
              Lire(symChaine);
             end;
       tReel: begin
               V[1]:=SymboleReel;
               Lire(symReel);
              end;
       tVecteur: begin
                  V:=SymboleVecteur;
                  Lire(symVecteur);
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
         SymboleVar.InfoType:=tFrPtr;
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
         if SymboleVar.InfoType = tVecteur then
          begin
           SymboleVar.InfoType:=tReel;
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

       if (Symbole=symCrochet1) and (F.NbArguments=0) then
        begin
         Lire(symCrochet1);
         if Symbole=symReel then
          begin
           V[2]:=SymboleReel;
           Lire(symReel);
          end
         else
          begin
           Lire(symDollar);
           if not (Symbole in [symIdent, symVariable, symObjVar]) then
            Erreur(4113);
           I:=MdlFrames.IndexOf(SymboleMot);
           if I<0 then
            Erreur(4113);
           V[2]:=I;
           Lire(symInconnu);
          end;
         I:=NDatas;
         Inc(NDatas);
         Datas.Write(V[2], 4);
         Lire(symVirgule);
         if Symbole=symVariable then
          begin
           if SymboleVar.InfoType<>tFrPtr then
            Erreur(4111);
           Coder($3C, I, SymboleVar.Pos, 0);
           Lire(symVariable);
          end
         else
          begin
           SymboleVar.Pos:=Variable.Pos;
           Definir;
           Inc(NDatas);
           Datas.Write(Zero, 4);
           Coder($3C, I, Variable.Pos, 0);
           Variable.Pos:=SymboleVar.Pos;
           if Symbole=symObjVar then
            Lire(symObjVar)
           else
            Lire(symIdent);
          end;
         Lire(symCrochet2);
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
    if Symbole=symVirgule then
     begin
      Lire(symVirgule);
      Inc(Result, DefVar(L, T));
     end;
  end;

  procedure DefObjVar;
  var
   T: TType;
   L: LongInt;
   C: Char;
  begin
   Lire(symPoint);
   T:=SymboleType;
   Lire(symType);
   if Symbole=symParenthese1 then
    begin
     Lire(symParenthese1);
     if Symbole<>symParenthese2 then
      repeat
       Lire(symType);
       Lire(symInconnu);
       if Symbole=symParenthese2 then Break;
       Lire(symVirgule);
      until False;
     Lire(symParenthese2);
     T:=tFrPtr;
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
    L:=Entete.TailleLocData;
    SymboleVar.Pos:=L;
    SymboleVar.InfoType:=T;
    SymboleVar.Nom:=EcrireChaine(SymboleMot);
    ObjDataDef.Write(SymboleVar, 8);
    Entete.TailleLocData:=L+TailleType[T];
    SymboleVar.InfoType:=tObjData;
    SymboleVar.Pos:=NDatas;
    NDataDef.Write(SymboleVar, 8);
    VarObjet.AddObject(SymboleMot, SymboleVar.AsObject);
    Inc(NDatas);
    Datas.Write(L, 4);
    if T=tVecteur then
     for C:='x' to 'z' do
      begin
       SymboleVar.InfoType:=tReel;
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
    Lire(symInconnu);
    if Symbole<>symVirgule then Break;
    Lire(symVirgule);
   until False;
  end;

  procedure DefCodeImpulse;
  var
   F: TFrame;
  begin
   Lire(symBIND);
   Touches.Add(#255 + SymboleMot);
   Lire(symChaine);
   Lire(symVirgule);
   Touches.Add(SymboleMot);
   Lire(symChaine);
   Lire(symVirgule);
   if Symbole<>symVariable then
    begin
     Touches.Add(SymboleMot);
     Lire(symChaine);
    end
   else
    begin
     if SymboleVar.InfoType<>tFrPtr then
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
     Lire(symVariable);
     Inc(NoImpulse);
    end;
  end;

  procedure DefCodeAutoexec;
  var
   F: TFrame;
   Chaine: String;
  begin
   Lire(symAUTOEXEC);
   Chaine:='';
   repeat
    if Symbole<>symVariable then
     begin
      Chaine:=Chaine+SymboleMot;
      Lire(symChaine);
     end
    else
     begin
      if SymboleVar.InfoType<>tFrPtr then
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
      Lire(symVariable);
      Inc(NoImpulse);
     end;
    if Symbole<>symVirgule then Break;
    Lire(symVirgule);
   until False;
   Touches.Add(Chaine);
  end;

  procedure DefMdlFrames;
  var
   S, Nouveau: String;
   Espace: Integer;
  begin
   Lire(symDollar);
   S:=PL;
   PL:=StrEnd(PL);
   if (Symbole in [symIdent, symVariable, symObjVar])
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
   Symbole:=symPointVirgule;
  end;

  function LireEntree(Numero: Integer) : String;
  var
   T: Integer;
  begin
   T:=Entete.Entrees[Numero].Nb * FacteurT[Numero];
   SetLength(Result, T);
   Source.Position:=Origine+Entete.Entrees[Numero].Pos;
   Source.ReadBuffer(PChar(Result)^, T);
  end;

  procedure EcrireResultat;
  var
   nEntete: TEntete;
  begin
   Chaines:=Chaines + Copy(#0#0#0, (Length(Chaines)-1) and 3 + 1, 3);
   nEntete:=Entete;
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
   Source.Position:=Origine+Entete.Entrees[1].Pos;
   if Entete.Entrees[1].Nb>0 then
    Dest.CopyFrom(Source, Entete.Entrees[1].Nb*8);
   QCode.Position:=0;
   if NQCode>Entete.Entrees[1].Nb then
    Dest.CopyFrom(QCode, (NQCode-Entete.Entrees[1].Nb)*8);
   nEntete.Entrees[4].Pos:=Dest.Position;
  {Source.Position:=Origine+Entete.Entrees[4].Pos;
   Dest.CopyFrom(Source, Entete.Entrees[4].Nb*SizeOf(TFrame));}
   NFrames.Position:=0;
   if NoFrame>0 then
    Dest.CopyFrom(NFrames, (NoFrame{-Entete.Entrees[4].Nb})*SizeOf(TFrame));
   nEntete.Entrees[2].Pos:=Dest.Position;
   Source.Position:=Origine+Entete.Entrees[2].Pos;
   if Entete.Entrees[2].Nb>0 then
    Dest.CopyFrom(Source, Entete.Entrees[2].Nb*8);
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
 Source.ReadBuffer(Entete, SizeOf(Entete));
 if Entete.Version <> VersionProgsDat then
  Raise EError(809);
 Chaines:=LireEntree(5);
 GetMem(Vars, Pred(Entete.Entrees[2].Nb)*SizeOf(TVarStruct));
 Datas:=TMemoryStream.Create;
 ObjDataDef:=TMemoryStream.Create;
 NDataDef:=TMemoryStream.Create;
 try
  Source.Position:=Origine+Entete.Entrees[2].Pos + SizeOf(TVarStruct);
  Source.ReadBuffer(Vars^, Pred(Entete.Entrees[2].Nb)*SizeOf(TVarStruct));
  TrieVariables(0, Entete.Entrees[2].Nb-2);
  Source.Position:=Origine+Entete.Entrees[6].Pos;
  if Entete.Entrees[6].Nb>0 then
   Datas.CopyFrom(Source, Entete.Entrees[6].Nb*4);
  Source.Position:=Origine+Entete.Entrees[3].Pos;
  if Entete.Entrees[3].Nb>0 then
   ObjDataDef.CopyFrom(Source, Entete.Entrees[3].Nb*8);
  NFrames:=TMemoryStream.Create;
  Source.Position:=Origine+Entete.Entrees[4].Pos;
  if Entete.Entrees[4].Nb>0 then
   NFrames.CopyFrom(Source, Entete.Entrees[4].Nb*SizeOf(TFrame));
   { compilation du patch }
  QCode:=TMemoryStream.Create;
  VarGlobales:=TStringList.Create;
  VarObjet:=TStringList.Create;
  MdlFrames:=TStringList.Create;
  try
   VariableObjet:=False;
   NDatas:=Entete.Entrees[6].Nb;
   NoFrame:=Entete.Entrees[4].Nb;
   NQCode:=Entete.Entrees[1].Nb;
   Commentaires:=False;
   LigneCourante:=0;
   LigneLocale:=0;
   Ligne:='';
   PL:=PChar(Ligne);
   VarLocales:=Nil;
   VarGlobales.Sorted:=True;
   VarObjet.Sorted:=True;
   Immediate:=EcrireChaine(tcImmediate[TypeCode]);
   Lire(symInconnu);
   while Symbole<>symEOF do
    begin
     case Symbole of
      symType: DefVar(VarGlobales, TypeEtendu);
      symPoint: DefObjVar;
      symBIND: DefCodeImpulse;
      symAUTOEXEC: DefCodeAutoexec;
      symDollar: DefMdlFrames;
      else Erreur(4110);
     end;
     Lire(symPointVirgule);
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

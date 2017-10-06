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
Revision 1.20  2016/01/02 20:01:10  danielpharos
Generate proper error if something went wrong trying to save files, instead of always displaying a message about "save" not being supported.

Revision 1.19  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.18  2009/02/21 17:10:12  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.17  2008/11/06 20:18:22  danielpharos
Removed old stuff in preparation for new specifics code.

Revision 1.16  2008/10/12 11:31:32  danielpharos
Moved 6DX map format to separate file, and re-factored QkMap and QkQuakeMap.

Revision 1.15  2008/09/06 15:57:15  danielpharos
Moved exception code into separate file.

Revision 1.14  2007/10/30 20:43:35  danielpharos
Fixed two weird-looking spaces

Revision 1.13  2007/07/05 10:19:45  danielpharos
Moved the Quake .map format code to a separate file.

Revision 1.12  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2001/03/20 21:44:19  decker_dk
Updated copyright-header

Revision 1.9  2001/01/21 15:49:48  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.8  2001/01/15 19:21:27  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.7  2000/11/26 19:08:32  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.6  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.4  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkQme;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, QkForm, TB97, QkMap, QkQuakeC, QkWad, QkPak,
  QkMdl, QkGroup, Python;

type
 QQme = class(QFileObject)
        private
        protected
          function OpenWindow(nOwner: TComponent) : TQForm1; override;
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
          function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
          procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
        end;

type
  TFQQme = class(TQForm1)
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
  end;

 {------------------------}

type
 QQme0  = class(QFileObject) public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme1  = class(QMap       ) public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme2  = class(QQuakeC    ) public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme3  = class(QFileObject) public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme4  = class(QTextureList)public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme5  = class(QImport)     public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme6  = class(QFileObject) public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme7  = class(QExplorerGroup)public class function TypeInfo: String;override;protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;
 QQme8  = class(QMdlFile)    public class function TypeInfo: String; override; protected procedure LoadFile(F: TStream; FSize: Integer); override; procedure SaveFile(Info: TInfoEnreg1); override; end;

 {------------------------}

implementation

uses QkMapObjects, QkMapPoly, qmath, Setup, Duplicator, Quarkx, QkExceptions,
     Travail, QkObjectClassList;

{$R *.DFM}

const
 SignatureQM = $50414D51;
 qmDescription = 0;
 qmCarte       = 1;
 qmPatchQC     = 2;
 qmBSP0        = 3;
 qmTextureDef  = 4;
 qmFileDef     = 5;
 qmQContext    = 6;
 qmFileLnk     = 7;
 qmModel       = 8;
 qmInfoTypeMax = 9;

 mapEntity     = 0;
 mapBrush      = 1;
 mapPoly       = 2;
 mapGroup      = 3;
 mapDuplicator = 4;
 mapDigger     = 5;

 MaxNomEntreeQM = 52;

 VersionNXF    = 3;

 nxfBinaire    = 0;
 nxfTexte      = 1;
 nxfNombre     = 2;
 nxfDate       = 3;

type
 TIntroQM = record
             Signature : LongInt;
             PositionRep, TailleRep: LongInt;
            end;
 PEnteteNXF = ^TEnteteNXF;
 TEnteteNXF = record
               Taille: LongInt;
               TailleId: Word;
               Reserve: Byte;
               Format: Byte;
              end;
 TEntreeRepQM = record
                 Nom: array[0..MaxNomEntreeQM-1] of Byte;
                 InfoType, Version: Word;
                 Position: LongInt;
                 Taille: LongInt;
                end;

 {------------------------}

class function QQme0 .TypeInfo; begin TypeInfo:='.qme0';  end;
class function QQme1 .TypeInfo; begin TypeInfo:='.qme1';  end;
class function QQme2 .TypeInfo; begin TypeInfo:='.qme2';  end;
class function QQme3 .TypeInfo; begin TypeInfo:='.qme3';  end;
class function QQme4 .TypeInfo; begin TypeInfo:='.qme4';  end;
class function QQme5 .TypeInfo; begin TypeInfo:='.qme5';  end;
class function QQme6 .TypeInfo; begin TypeInfo:='.qme6';  end;
class function QQme7 .TypeInfo; begin TypeInfo:='.qme7';  end;
class function QQme8 .TypeInfo; begin TypeInfo:='.qme8';  end;

function ReadAsQmeEntry(Q: QFileObject; SourceFile: TStream; Taille: Integer) : Boolean;
var
 I, Compte, TailleIds: Integer;
 Entetes, P: PEnteteNXF;
 Ids, S: String;
begin
 Result:=Q.ReadFormat=rf_Default;
 if Result then
  begin    { reads all QuArK 3.x - style data from the entry into Q.Specifics }
   SourceFile.ReadBuffer(Compte, SizeOf(Compte));
   I:=SizeOf(TEnteteNXF)*Compte;
   GetMem(Entetes, I); try
   SourceFile.ReadBuffer(Entetes^, I);
   TailleIds:=0;
   P:=Entetes;
   for I:=1 to Compte do
    begin
     Inc(TailleIds, P^.TailleId);
     Inc(P);
    end;
   SetLength(Ids, TailleIds);
   if TailleIds>0 then
    SourceFile.ReadBuffer(Ids[1], TailleIds);
   TailleIds:=1;
   P:=Entetes;
   for I:=1 to Compte do
    begin
     S:=Copy(Ids, TailleIds, P^.TailleId)+'=';
     SetLength(S, Length(S)+P^.Taille);
     SourceFile.ReadBuffer(S[P^.TailleId+2], P^.Taille);
     Q.Specifics.Add(S);
     Inc(TailleIds, P^.TailleId);
     Inc(P);
    end;
   finally FreeMem(Entetes); end;
  end;
end;

function SaveAsQmeEntry(Q: QFileObject; Format: Integer; F: TStream) : Boolean;
begin
 if Format=rf_Default then
  Raise EError(5554);
 Result:=False;
end;

function SpecAsMemStream(const S: String) : TMemoryStream;
begin
 Result:=TMemoryStream.Create;
 Result.Write(Pointer(S)^, Length(S));
 Result.Position:=0;
end;

 {------------------------}

procedure QQme0 .LoadFile;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
 else
  inherited;
end;

procedure QQme0. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

 {------------------------}

    { QuArK 4.x maps types and constants }
const
 TailleNomTex = 32;

type
 PSurface = ^TSurface;
 TSurface = record
            { définition de la face }
             Normale: TVect;
             Dist: TDouble;
             Params: TFaceParams;
             case Integer of
              0: (Q2Contents, Q2Flags, Q2Value: Integer;
                  NomTex: String[TailleNomTex-1]);
              1: (Q2Data: Byte);
            end;

const
 TailleSurfDef1 = SizeOf(TVect)+6*SizeOf(TDouble);     {Normale..Params}
 TailleSurfQ2   = 3*SizeOf(Integer);                {Q2Contents..Q2Value}
 TailleSurfDef  = TailleSurfDef1+TailleSurfQ2;      {Normale..Q2Value}
 TailleSurfPlan = SizeOf(TVect)+SizeOf(TDouble);       {Normale..Dist}
 TailleSurfParm1= TailleSurfDef1 - TailleSurfPlan;  {Params}
 TailleSurfParm = TailleSurfDef  - TailleSurfPlan;  {Params..Q2Value}
 TailleSurfVis  = TailleSurfParm + TailleNomTex;    {Params..NomTex}
 tntQuake2 = $80;

type
 PTransfertTreeMap = ^TTransfertTreeMap;
 TTransfertTreeMap = record
                      Tampon: array[0..31, 0..TailleSurfVis-1] of Byte;
                      PositionTampon: Integer;
                     end;

function TTreeMapEntityCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap; forward;
function TTreeMapBrushCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap; forward;
function TPolyedreCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap; forward;
function TTreeMapGroupCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap; forward;
function TDuplicatorCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap; forward;
function TDiggerCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap; forward;

const
 TreeMapObj : array[mapEntity..mapDigger] of function (S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap
  = (TTreeMapEntityCharger, TTreeMapBrushCharger, TPolyedreCharger,
     TTreeMapGroupCharger, TDuplicatorCharger, TDiggerCharger);

procedure TTreeMapSpecCharger(Q: TTreeMap; S: TStream);
var
 Taille: Word;
 Tampon: String;
begin
 S.ReadBuffer(Taille, SizeOf(Taille));
 SetLength(Tampon, Taille);
 S.ReadBuffer(Pointer(Tampon)^, Taille);
 Q.Specifics.Text:=Tampon;
 Q.Name:=Q.Specifics.Values['classname'];
 Q.Specifics.Values['classname']:='';
end;

function TTreeMapEntityCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap;
begin
 Result:=TTreeMapEntity.Create('', nParent);
 TTreeMapSpecCharger(Result, S);
end;

function TPolyedreCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap;
var
 NbFaces: Word;
 I: Integer;
 F: TFace;
 Codes: String;
 Abr: Byte;
 OldF, OldFArray: ^TSurface;
begin
 Result:=TPolyedre.Create(LoadStr1(138), nParent);
 S.ReadBuffer(NbFaces, SizeOf(NbFaces));
 Result.SubElements.Capacity:=NbFaces;
 SetLength(Codes, NbFaces);
 S.ReadBuffer(Pointer(Codes)^, NbFaces);
 GetMem(OldFArray, SizeOf(TSurface)*NbFaces); try
 OldF:=OldFArray;
 for I:=1 to NbFaces do
  begin
   Abr:=Ord(Codes[I]);
   if Abr<8 then
    begin
     FillChar(OldF^.NomTex, TailleNomTex, 0);
     S.ReadBuffer(OldF^.Params, TailleSurfParm1+1);
     SetLength(OldF^.NomTex, OldF^.Q2Data and not tntQuake2);
     S.ReadBuffer(OldF^.NomTex[1], Length(OldF^.NomTex));
     if OldF^.Q2Data and tntQuake2 = tntQuake2 then
      S.ReadBuffer(OldF^.Q2Data, TailleSurfQ2)
     else
      FillChar(OldF^.Q2Data, TailleSurfQ2, 0);
     Move(OldF^.Params, T.Tampon[T.PositionTampon], TailleSurfVis);
     T.PositionTampon:=(T.PositionTampon+1) and 31;
    end
   else
    Move(T.Tampon[Abr and 31], OldF^.Params, TailleSurfVis);
   Inc(OldF);
  end;
 OldF:=OldFArray;
 for I:=1 to NbFaces do
  begin
   Abr:=Ord(Codes[I]);
   if Abr>=8 then
    Abr:=Abr shr 5;
   if Abr=7 then
    S.ReadBuffer(OldF^.Normale, SizeOf(TVect)+SizeOf(TDouble))
   else
    begin
     OldF^.Normale:={Origine}OriginVectorZero;
     case Abr of
      1 : OldF^.Normale.X:=-1;
      2 : OldF^.Normale.X:=1;
      3 : OldF^.Normale.Y:=-1;
      4 : OldF^.Normale.Y:=1;
      5 : OldF^.Normale.Z:=-1;
      6 : OldF^.Normale.Z:=1;
     end;
     S.ReadBuffer(OldF^.Dist, SizeOf(TDouble));
    end;
   F:=TFace.Create(LoadStr1(139), Result);
   Result.SubElements.Add(F);
   F.NomTex:=OldF^.NomTex;
   F.SetFaceFromParams(OldF^.Normale, OldF^.Dist, OldF^.Params);
   if (OldF^.Q2Contents<>0)
   or (OldF^.Q2Flags<>0)
   or (OldF^.Q2Value<>0) then
    begin
     F.Specifics.Values['Contents']:=IntToStr(OldF^.Q2Contents);
     F.Specifics.Values['Flags']:=IntToStr(OldF^.Q2Flags);
     F.Specifics.Values['Value']:=IntToStr(OldF^.Q2Value);
    end;
   Inc(OldF);
  end;
 finally FreeMem(OldFArray); end;
end;

procedure LoadGroup(Result: TTreeMap; S: TStream; T: PTransfertTreeMap);
var
 I: Integer;
 Nombre: Word;
 InfoTypes: String;
begin
 TTreeMapSpecCharger(Result, S);
 S.ReadBuffer(Nombre, SizeOf(Nombre));
 if Nombre>0 then
  begin
   SetLength(InfoTypes, Nombre);
   S.ReadBuffer(Pointer(InfoTypes)^, Nombre);
   for I:=1 to Nombre do
    begin
     if Ord(InfoTypes[I]) > High(TreeMapObj) then
      Raise EErrorFmt(5509, [9000+Ord(InfoTypes[I])]);
     Result.SubElements.Add(TreeMapObj[Ord(InfoTypes[I])](S, T, Result));
    end;
  end;
end;

function TTreeMapGroupCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap;
begin
 Result:=TTreeMapGroup.Create('', nParent);
 LoadGroup(Result, S, T);
end;

function TTreeMapBrushCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap;
begin
 Result:=TTreeMapBrush.Create('', nParent);
 LoadGroup(Result, S, T);
end;

function TDuplicatorCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap;
var
 Sym, Macro: String;
 I: Integer;
begin
 Result:=TDuplicator.Create('', nParent);
 TTreeMapSpecCharger(Result, S);
 Result.Specifics.Values['out']:='1';
 Macro:='';
 if Result.Specifics.Values['linear']<>'' then
  Macro:='dup lin'
 else
  begin
   Sym:=Result.Specifics.Values['sym'];
   if Sym='' then
    Macro:='dup basic'
   else
    begin
     for I:=1 to Length(Sym) do
      Sym[I]:=Upcase(Sym[I]);
     if Sym='X' then Macro:='symx' else
     if Sym='Y' then Macro:='symy' else
     if Sym='Z' then Macro:='symz' else
     if (Sym='XY') or (Sym='YX') then Macro:='symxy' else
      GlobalWarning(FmtLoadStr1(5624, [Sym]));
    end;
  end;
 Result.Specifics.Values['macro']:=Macro;
end;

function TDiggerCharger(S: TStream; T: PTransfertTreeMap; nParent: QObject) : TTreeMap;
var
 Macro, Depth: String;
begin
 Result:=TDuplicator.Create('', nParent);
 LoadGroup(Result, S, T);
 Depth:=Result.Specifics.Values['hollow'];
 if Depth='' then
  Macro:='digger'
 else
  begin
   Macro:='hollow maker';
   Result.Specifics.Values['hollow']:='';
   Result.Specifics.Values['depth']:=Depth;
  end;
 Result.Specifics.Values['macro']:=Macro;
end;

procedure QQme1.LoadFile;
var
 T: TTransfertTreeMap;
 Q: QObject;
 S: String;
 M: TMemoryStream;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
  begin  { turns this data into a map }
   FillChar(T, SizeOf(T), 0);
   S:=Specifics.Values['Map'];
   if S='' then Raise EErrorFmt(5509, [811]);
   M:=SpecAsMemStream(S); try
   Q:=TTreeMapBrushCharger(M, @T, Self);
   SubElements.Add(Q);
   finally M.Free; end;
   Specifics.Values['Root']:=Q.Name+Q.TypeInfo;
   Specifics.Values['Map']:='';
   S:=Specifics.Values['GameCfg'];
   Specifics.Values['GameCfg']:='';
   if (S='') or (CompareText(S, 'Quake')=0) then
    Specifics.Values['Game']:=GetGameName(mjQuake)
   else
    Specifics.Values['Game']:=S;
   FixupAllReferences;
  end
 else
  inherited;
end;

procedure QQme1.SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

 {------------------------}

procedure QQme2 .LoadFile;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
 else
  inherited;
end;

procedure QQme2. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

procedure QQme3 .LoadFile;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
 else
  inherited;
end;

procedure QQme3. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

procedure QQme4 .LoadFile;
var
 M: TMemoryStream;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
  begin
   M:=SpecAsMemStream(Specifics.Values['Data']); try
   Specifics.Values['Data']:='';
   inherited LoadFile(M, M.Size);
   finally M.Free; end;
  end
 else
  inherited;
end;

procedure QQme4. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

procedure QQme5 .LoadFile;
var
 S: String;
 Folder: QPakFolder;
 nFile: QFileObject;
 I, J: Integer;
 M: TMemoryStream;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
  begin
   Folder:=GetFolder(Specifics.Values['Path']);
   for I:=Specifics.Count-1 downto 0 do
    begin
     S:=Specifics[I];
     if S[1]=':' then
      begin
       J:=Pos('=',S);
       nFile:=BuildFileRoot(Copy(S, 2, J-2), Folder);
       Folder.SubElements.Insert(0, nFile);
       nFile.Flags:=nFile.Flags and not ofFileLink;
       M:=TMemoryStream.Create; try
       M.Write(PChar(S)[J], Length(S)-J);
       M.Position:=0;
       nFile.LoadFromStream(M);
       finally M.Free; end;
       Specifics.Delete(I);
      end;
    end;
  end
 else
  inherited;
end;

procedure QQme5. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

procedure QQme6 .LoadFile;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
 else
  inherited;
end;

procedure QQme6. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

procedure QQme7 .LoadFile;
var
 S: String;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
  begin
   S:=Specifics.Values['FileName'];
   if S<>'' then
    LoadedFileLink(S+'.qme', 0);
  end
 else
  inherited;
end;

procedure QQme7. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

procedure QQme8 .LoadFile;
var
 M: TMemoryStream;
begin
 if ReadAsQmeEntry(Self, F, FSize) then
  begin
   M:=SpecAsMemStream(Specifics.Values['Mdl']); try
   Specifics.Values['Mdl']:='';
   inherited LoadFile(M, M.Size);
   finally M.Free; end;
  end
 else
  inherited;
end;

procedure QQme8. SaveFile;
begin
 with Info do if not SaveAsQmeEntry(Self, Format, F) then
  inherited;
end;

 {------------------------}

class function QQme.TypeInfo;
begin
 Result:='.qme';
end;

function QQme.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=TFQQme.Create(nOwner);
end;

procedure QQme.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiQme;
 E.MarsColor:=clWhite;
end;

class procedure QQme.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5141);
 Info.FileExt:=783;
 Info.WndInfo:=[wiOwnExplorer];
end;

function QQme.IsExplorerItem(Q: QObject) : TIsExplorerItem;
var
 S: String;
begin
 S:=Q.Name+Q.TypeInfo;
 Result:=ieResult[
  { any ".qme0" to ".qme8" }
    (CompareText(Copy(S, Length(S)-4, 4), '.qme') = 0) and (S[Length(S)] in ['0'..'8'])];
end;

procedure QQme.LoadFile(F: TStream; FSize: Integer);
var
 Intro: TIntroQM;
 Entree: TEntreeRepQM;
 Origine: LongInt;
 Q: QObject;
 I: Integer;
begin
 case ReadFormat of
  rf_Default: begin  { as stand-alone file }
      if FSize<SizeOf(Intro) then
       Raise EError(5519);
      Origine:=F.Position;
      F.ReadBuffer(Intro, SizeOf(Intro));
      if Intro.Signature<>SignatureQM then
       Raise EErrorFmt(5555, [LoadName, Intro.Signature, SignatureQM]);
      for I:=0 to Intro.TailleRep div SizeOf(TEntreeRepQM)-1 do
       begin
        F.Position:=Origine+Intro.PositionRep+I*SizeOf(TEntreeRepQM);
        F.ReadBuffer(Entree, SizeOf(TEntreeRepQM));
        if (Entree.Version<>VersionNXF) or (Entree.InfoType>=qmInfoTypeMax) then
         Raise EErrorFmt(5556, [LoadName, Entree.Version, Entree.InfoType]);
        F.Position:=Origine + Entree.Position;
        Q:=OpenFileObjectData(F, CharToPas(Entree.Nom)+'.qme'+IntToStr(Entree.InfoType),
            Entree.Taille, Self);
        SubElements.Add(Q);
        LoadedItem(rf_Default, F, Q, Entree.Taille);
       end;
     end;
 else inherited;
 end;
end;

procedure QQme.SaveFile(Info: TInfoEnreg1);
begin
 with Info do case Format of
  rf_Default: begin  { as stand-alone file }
      Raise EQObjectSavingNotSupported.Create(LoadStr1(5554));   { not implemented }
     end;
 else inherited;
 end;
end;

procedure QQme.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
var
 I: Integer;
begin
 Acces;
 ProgressIndicatorStart(175, SubElements.Count); try
 for I:=0 to SubElements.Count-1 do
  begin
   if SubElements[I] is QFileObject then
    QFileObject(SubElements[I]).Go1(maplist, extracted, FirstMap, QCList);
   ProgressIndicatorIncrement;
  end;
 finally ProgressIndicatorStop; end;
end;

 {------------------------}

procedure TFQQme.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_AfficherObjet:
    begin

    end;
 end;
 inherited;
end;

function TFQQme.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QQme) and inherited AssignObject(Q, State);
end;

initialization
  RegisterQObject(QQme, 'b');
  RegisterQObject(QQme0,  'a');
  RegisterQObject(QQme1,  'a');
  RegisterQObject(QQme2,  'a');
  RegisterQObject(QQme3,  'a');
  RegisterQObject(QQme4,  'a');
  RegisterQObject(QQme5,  'a');
  RegisterQObject(QQme6,  'a');
  RegisterQObject(QQme7,  'a');
  RegisterQObject(QQme8,  'a');
end.

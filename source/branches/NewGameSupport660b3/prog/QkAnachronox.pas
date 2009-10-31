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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
}

//Based on: DatExtract by John Rittenhouse

unit QkAnachronox;

interface

uses
  Classes, SysUtils, QkObjects, QkFileObjects, QkPak;

type
 QPakAnachronox = class(QPakFolder)
        protected
         procedure EcrireEntreesPak(Info: TInfoEnreg1; Origine: LongInt; const Chemin: String; TailleNom: Integer; Repertoire: TStream);
         {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); override;}
         procedure SaveFile(Info: TInfoEnreg1); override;
         procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses
  QuarkX, QkObjectClassList, QkExceptions, UNZIP;

const
 SignatureDAT = $54414441; { Anachronox }

type
 TDATHeader = record
//               ID: array[0..3] of Char;//Should be ADAT
               Signature: Longint;

               FInfoPos: Longword;
               FLength: Longword;
               Unknown: Longword;//Version Number?  Always 9
 end;

 TFileInfo = record
	            FileName: array[0..127] of Char;

	            StartPos: Longword;
              Length: Longword;//Size of extracted file(decompressed size)

              Compressed: Longword;//If not 0 this is the compressed size
              Unknown: Longword;//Probably a checksum of some sort
 end;

 {------------------------}

class function QPakAnachronox.TypeInfo;
begin
 Result:='.dat';
end;

procedure QPakAnachronox.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPak;
end;

class procedure QPakAnachronox.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5150);
 Info.FileExt:=825;
 Info.WndInfo:=[wiOwnExplorer];
end;

//function DecompressFile(fs: TStream; {<-- INPUT} ms: TStream {<-- OUTPUT}; offset: longint {Of LocalHeader in Zip File}): integer;
//begin
// //@
// Result:=1;
//end;

Function DatAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
var
  mem: TMemoryStream;
  err: integer;
begin
  Ref^.Self.Position:=Ref^.Position;
  mem:=TMemoryStream.Create;
//  err:=DecompressFile(Ref^.Self, mem, Ref^.Position);
  err:=UnzipFile(Ref^.Self, mem, Ref^.Position);
  if err<>0 then
    raise Exception.CreateFmt('Error decompressing file (%d)', [err]);
  Result:=mem.Size;
  mem.Position:=0;
  S:=mem;
end;

procedure QPakAnachronox.LoadFile(F: TStream; FSize: Integer);
var
 Header: TDATHeader;
 Entry: TFileInfo;
 Origine, Size, nEnd: LongInt;
 NumberOfFiles: Cardinal;
 I: Integer;
 Dossier: QObject;
 Chemin: String;
 Q: QObject;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if FSize<SizeOf(TDATHeader) then
       Raise EError(5519);
      Origine:=F.Position;
      F.ReadBuffer(Header, SizeOf(TDATHeader));
      if Header.Signature <> SignatureDAT then
       raise EErrorFmt(5507, [LoadName, Header.Signature, SignatureDAT]);
      F.Position:=Origine + Integer(Header.FInfoPos);
      Dossier:=Self;
      NumberOfFiles:=Header.FLength div SizeOf(TFileInfo);
      for I:=0 to NumberOfFiles-1 do
      begin
       F.ReadBuffer(Entry, SizeOf(TFileInfo));
       SetString(Chemin, Entry.FileName, 128);
       SetLength(Chemin, StrLen(PChar(Chemin)));
       //@
       if Entry.Compressed=0 then
       begin
        Size:=Entry.Length;
        Q:=OpenFileObjectData(F, Chemin, Size, Dossier);
        Dossier.SubElements.Add(Q);
        LoadedItem(rf_Default, F, Q, Size);
       end
       else
       begin
        Size:=Entry.Compressed;
        Q:=OpenFileObjectData(nil, Chemin, Size, Dossier);
        Dossier.SubElements.Add(Q);
        {Copied From LoadedItem & Modified}
        nEnd:=F.Position;
        Q.Open(TQStream(F), Size);
        F.Position:=nEnd;
        {/Copied From LoadedItem & Modified}
        Q.FNode^.OnAccess:=DatAddRef;
       end;
        //@
      end;
      //@
     end;
 else
  inherited;
 end;
end;

procedure QPakAnachronox.SaveFile(Info: TInfoEnreg1);
begin
  //@
end;

procedure QPakAnachronox.EcrireEntreesPak(Info: TInfoEnreg1; Origine: LongInt; const Chemin: String; TailleNom: Integer; Repertoire: TStream);
begin
  //@
end;

initialization
  RegisterQObject(QPakAnachronox, 't');
end.

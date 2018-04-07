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
unit QkVMT;

interface

uses Windows, Classes, QkWad, QkPixelSet, QkObjects, QkFileObjects, QkVTFLib;

type
  QVMTStage = class(QObject)
         private
           function DumpData: String;
         public
           class function TypeInfo: String; override;
         end;
  QVMTFile = class(QPixelSet)
         protected
           DefaultImageCache : QPixelSet;
         public
           procedure SaveFile(Info: TInfoEnreg1); override;
           procedure LoadFile(F: TStream; FSize: Integer); override;
           class function TypeInfo: String; override;
           class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
           function DefaultImage : QPixelSet;
           function GetSize : TPoint; override;
           procedure SetSize(const nSize: TPoint); override;
           function Description : TPixelSetDescription; override;
           function SetDescription(const PSD: TPixelSetDescription;
                                   Confirm: TSDConfirm) : Boolean; override;
         end;

{-------------------}

implementation

uses SysUtils, Setup, Quarkx, QkExceptions, QkObjectClassList, Game, Logging,
     QkVTF, StrUtils, ExtraFunctionality, QkApplPaths, QkTextures;

var
  VMTLoaded: Boolean;

class function QVMTStage.TypeInfo: String;
begin
 TypeInfo:='.vmtstage';
end;

function QVMTStage.DumpData : String;
const
  NumberChars: string = '0123456789-';
type
  vlDataType = (vlDTString, vlDTInteger, vlDTSingle);
var
  I, J, K: Integer;
  Q: QObject;
  Spec: String;
  SpecName: String;
  SpecDataType: vlDataType;
  CharFound: Boolean;
begin
  for I:=0 to Specifics.Count-1 do
  begin
    Spec:=Specifics[I];
    J:=Pos('=', Spec);
    SpecName:=LeftStr(Spec,J-1);
    Spec:=RightStr(Spec,Length(Spec)-J);

    //DanielPharos: Ugly, slow and inaccurate way of determining the type...
    SpecDataType:=vlDTInteger;
    for J:=0 to Length(Spec) do
    begin
      CharFound:=false;
      for K:=0 to Length(NumberChars)-1 do
      begin
        if NumberChars[K]=Spec[J] then
        begin
          CharFound:=true;
          break;
        end;
      end;
      if CharFound=false then
      begin
        SpecDataType:=vlDTString;
        break;
      end;
    end;
    if SpecDataType=vlDTInteger then
    begin
      K:=Pos(Spec,'.');
      if K>0 then
        SpecDataType:=vlDTSingle;
    end;

    case SpecDataType of
    vlDTString: vlMaterialAddNodeString(PvlChar(SpecName),PvlChar(Spec));
    vlDTInteger: vlMaterialAddNodeInteger(PvlChar(SpecName),StrToInt(Spec));
    vlDTSingle: vlMaterialAddNodeSingle(PvlChar(SpecName),StrToFloat(Spec));
    end;
  end;

  for I:=0 to SubElements.Count-1 do
  begin
    Q:=SubElements[I];
    if Q is QVMTStage then
    begin
      vlMaterialAddNodeGroup(PvlChar(Q.name));
      if vlMaterialGetChildNode(PvlChar(Q.name))=vlFalse then
        raise Exception.Create('Unable to parse VMT file. Call to vlMaterialGetChildNode failed.');
      QVMTStage(Q).DumpData;
      if vlMaterialGetParentNode=vlFalse then
        raise Exception.Create('Unable to parse VMT file. Call to vlMaterialGetParentNode failed.');
    end;
  end;
end;

class function QVMTFile.TypeInfo: String;
begin
 TypeInfo:='.vmt';
end;

class procedure QVMTFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5716);
  Info.FileExt:=815;
  Info.WndInfo:=[wiWindow];
end;

function QVMTFile.DefaultImage : QPixelSet;
var
 PakFilename: String;
 FullTextureFile: String;
 DefaultImageName: array[0..9] of String;
 DefaultImageIndex: Integer;
 ImageFileName: String;
 Size: TPoint;
 V: array [1..2] of Single;
 TexExt: String;
begin
  Acces;
  Result:=nil;
  {if DefaultImageCache<>Nil then
  begin
    result:=DefaultImageCache
  end
  else
  begin}

  if ReverseLink<>nil then
  begin
    PakFilename:=ReverseLink.Specifics.Values['TexturePakFile'];
    if PakFilename='' then
      PakFilename:=ReverseLink.Specifics.Values['PakFile'];
  end
  else
  begin
    PakFilename:='';
  end;

  //FIXME: Horribly horriby workaround...! We need to get into the first
  //element to find an image
  if SubElements.Count>0 then
  begin

  //TexExt:=SetupGameSet.Specifics.Values['TextureFormat'];
  TexExt:='.vtf';
  if ReverseLink<>nil then
    DefaultImageName[0]:=ReverseLink.Specifics.Values['e'];
  if DefaultImageName[0]<>'' then
  begin
    FullTextureFile:=SubElements[0].Specifics.Values[DefaultImageName[0]];
    Log(LOG_VERBOSE,'attempting to load '+FullTextureFile);
    try
      Result:=NeedGameFile(FullTextureFile, PakFilename) as QPixelSet
    except
      Result:=nil;
    end;
  end
  else
  begin
    DefaultImageIndex:=0;
    DefaultImageName[0]:=SubElements[0].Specifics.Values['%tooltexture'];
    DefaultImageName[1]:=SubElements[0].Specifics.Values['$basetexture'];
    DefaultImageName[2]:=SubElements[0].Specifics.Values['$basetexture2'];
    DefaultImageName[3]:=SubElements[0].Specifics.Values['$2basetexture'];
    DefaultImageName[4]:=SubElements[0].Specifics.Values['$material'];
    DefaultImageName[5]:=SubElements[0].Specifics.Values['$bumpmap'];
    DefaultImageName[6]:=SubElements[0].Specifics.Values['$normalmap'];
    DefaultImageName[7]:=SubElements[0].Specifics.Values['$dudvmap'];
    DefaultImageName[8]:=SubElements[0].Specifics.Values['$envmap'];
    DefaultImageName[9]:=SubElements[0].Specifics.Values['$parallaxmap'];
    while ((Result=nil) and (DefaultImageIndex<10)) do
    begin
      if (DefaultImageName[DefaultImageIndex]<>'') then
      begin
        ImageFileName:=DefaultImageName[DefaultImageIndex]+TexExt;
        FullTextureFile:=ConcatPaths([GameTexturesPath, ImageFileName]);
        Log(LOG_VERBOSE,'attempting to load '+FullTextureFile);
        try
          Result:=NeedGameFile(FullTextureFile, PakFilename) as QPixelSet
        except
          Result:=nil;
        end;
      end;
      if Result=nil then
        DefaultImageIndex:=DefaultImageIndex+1;
    end;
  end;

  end;

  if (Result=nil) then
  begin
    if ReverseLink<>nil then
      ImageFileName:=ReverseLink.name + TexExt
    else
      ImageFileName:=self.name + TexExt;
    FullTextureFile:=ConcatPaths([GameTexturesPath, ImageFileName]);
    Log(LOG_VERBOSE,'attempting to load '+FullTextureFile);
    try
      Result:=NeedGameFile(FullTextureFile, PakFilename) as QPixelSet;
    except
      Result:=nil;
    end;
  end;
  //DefaultImageCache:=Result;

  {tiglari: giving shaders a size.  a presumably
  horrible place to do it, but doesn't work when
  shaders are being loaded }
  if Result<>Nil then
  begin
    Log(LOG_VERBOSE, LoadStr1(5708), [ImageFileName]);
    Size:=Result.GetSize;
    V[1]:=Size.X;
    V[2]:=Size.Y;
    SetFloatsSpec('Size',V);
  end
  else
  begin
    Log(LOG_WARNING, LoadStr1(5695), [self.name]);
    Raise EErrorFmt(5695, [self.name]);
  end;
end;

function QVMTFile.GetSize : TPoint;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5534, ['Size']);
 Image.Acces;
 Result:=Image.GetSize;
end;

function QVMTFile.Description : TPixelSetDescription;
var
 Image: QPixelSet;
begin
 Image:=DefaultImage;
 if Image=Nil then Raise EErrorFmt(5695, [Name]);
 Result:=Image.Description;
end;

procedure QVMTFile.SetSize;
begin
 Raise EError(5696);
end;

function QVMTFile.SetDescription;
begin
 Raise EError(5696);
end;

procedure QVMTFile.LoadFile(F: TStream; FSize: Integer);
var
  Setup: QObject;

  RawBuffer: String;
  VMTMaterial: vlUInt;
  Stage: QVMTStage;
  StageList: array of QObject;
  GroupEndWorkaround: Boolean;
  GroupEndWorkaroundName: PvlChar;

  NodeLevel: Cardinal;
  NodeType: VMTNodeType;
  NodeName: PvlChar;
  NodeValueString: PvlChar;
  NodeValueInteger: vlUInt;
  NodeValueSingle: vlFloat;
begin
  Log(LOG_VERBOSE,'load vmt %s',[self.name]);;
  case ReadFormat of
    rf_Default: begin  { as stand-alone file }
      if not VMTLoaded then
      begin
        if not LoadVTFLib then
          Raise EErrorFmt(5718, [GetLastError]);
        VMTLoaded:=true;
      end;

      SetLength(RawBuffer, FSize);
      F.ReadBuffer(Pointer(RawBuffer)^, FSize);

      if vlCreateMaterial(@VMTMaterial)=vlFalse then
        LogAndRaiseError('Unable to load VMT file. Call to vlCreateMaterial failed.');

      try
        if vlBindMaterial(VMTMaterial)=vlFalse then
          LogAndRaiseError('Unable to load VMT file. Call to vlBindMaterial failed.');

        Setup:=SetupSubSet(ssFiles, 'VMT');
        try
          case StrToInt(Setup.Specifics.Values['ParseMode']) of
          0: vlSetInteger(VTFLIB_VMT_PARSE_MODE, PARSE_MODE_STRICT);
          1: vlSetInteger(VTFLIB_VMT_PARSE_MODE, PARSE_MODE_LOOSE);
          else
            vlSetInteger(VTFLIB_VMT_PARSE_MODE, PARSE_MODE_LOOSE);
          end;
        except
          vlSetInteger(VTFLIB_VMT_PARSE_MODE, PARSE_MODE_LOOSE);
        end;

        if vlMaterialLoadLump(Pointer(RawBuffer), Length(RawBuffer))=vlFalse then
          LogAndRaiseError('Unable to load VMT file. Call to vlMaterialLoadLump failed. Please make sure the file is a valid VMT file, and not damaged or corrupt.');

        if vlMaterialGetFirstNode=vlFalse then
          LogAndRaiseError('Unable to load VMT file. Call to vlMaterialGetFirstNode failed.');

        NodeLevel:=0;
        SetLength(StageList, NodeLevel+1);
        StageList[NodeLevel]:=Self;
        GroupEndWorkaround:=false;
        { DanielPharos:
          We need a workaround for the fact that VTFLib reports a GROUP with
          exactly the same name AFTER each GROUPEND (unless it's the last one
          of the file). So we will simply ignore the first GROUP after any
          GROUPEND if it has the same name as the GROUPEND.}

        repeat
          NodeName:=vlMaterialGetNodeName;
          NodeType:=vlMaterialGetNodeType;
          case NodeType of
          NODE_TYPE_GROUP:
            begin
              if (GroupEndWorkaround=false) or (NodeName<>GroupEndWorkaroundName) then
              begin
                NodeLevel:=NodeLevel+1;
                Stage:=QVMTStage.Create(PChar(NodeName), StageList[NodeLevel-1]);
                StageList[NodeLevel-1].SubElements.Add(Stage);
                SetLength(StageList, NodeLevel+1);
                StageList[NodeLevel]:=Stage;
              end;
            end;
          NODE_TYPE_GROUP_END:
            begin
              if (NodeLevel = 0) then raise Exception.Create('ERROR: QkVMT: NodeLevel < 0!');
              NodeLevel:=NodeLevel-1;
              SetLength(StageList, NodeLevel+1);
            end;
          NODE_TYPE_STRING:
            begin
              NodeValueString:=vlMaterialGetNodeString;
              StageList[NodeLevel].Specifics.Add(PChar(NodeName)+'='+PChar(NodeValueString));
            end;
          NODE_TYPE_INTEGER:
            begin
              NodeValueInteger:=vlMaterialGetNodeInteger;
              StageList[NodeLevel].Specifics.Add(PChar(NodeName)+'='+IntToStr(NodeValueInteger));
            end;
          NODE_TYPE_SINGLE:
            begin
              NodeValueSingle:=vlMaterialGetNodeSingle;
              StageList[NodeLevel].Specifics.Add(PChar(NodeName)+'='+FloatToStr(NodeValueSingle));
            end;
          end;

          if NodeType=NODE_TYPE_GROUP_END then
          begin
            GroupEndWorkaround:=true;
            GroupEndWorkaroundName:=NodeName;
          end
          else
            GroupEndWorkaround:=false;
        until vlMaterialGetNextNode=vlFalse;
        if (NodeLevel <> 0) then raise Exception.Create('ERROR: QkVMT: NodeLevel != 0!');
      finally
        vlDeleteMaterial(VMTMaterial);
      end;
    end;
    else
      inherited;
  end;
end;

procedure QVMTFile.SaveFile(Info: TInfoEnreg1);
var
  I: Integer;
  Q: QObject;
  RawBuffer: String;
  VMTMaterial, OutputSize: Cardinal;
begin
 Log(LOG_VERBOSE,'save vmt %s',[self.name]);
 with Info do case Format of
  rf_Default:
  begin  { as stand-alone file }

    if not VMTLoaded then
    begin
      if not LoadVTFLib then
        Raise EErrorFmt(5718, [GetLastError]);
      VMTLoaded:=true;
    end;

    if vlCreateMaterial(@VMTMaterial)=vlFalse then
      LogAndRaiseError('Unable to save VMT file. Call to vlCreateMaterial failed.');

    try
      if vlBindMaterial(VMTMaterial)=vlFalse then
        LogAndRaiseError('Unable to save VMT file. Call to vlBindMaterial failed.');

      for I:=0 to SubElements.Count-1 do
      begin
        Q:=SubElements[I];
        if Q is QVMTStage then
        begin
          //DanielPharos: There should only one subelement: the root
          if vlMaterialCreate(PvlChar(Q.name))=vlFalse then
            LogAndRaiseError('Unable to save VMT file. Call to vlMaterialCreate failed.');
          if vlMaterialGetFirstNode=vlFalse then
            LogAndRaiseError('Unable to save VMT file. Call to vlMaterialGetFirstNode failed.');

          QVMTStage(Q).DumpData;
          break;
        end;
      end;

      SetLength(RawBuffer, 2048);     //FIXME: 2048 is just a number. We need a better way!
      if vlMaterialSaveLump(Pointer(RawBuffer), Length(RawBuffer), @OutputSize)=vlFalse then
        LogAndRaiseError('Unable to save VMT file. Call to vlMaterialSaveLump failed.');

      F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);
    finally
      vlDeleteMaterial(VMTMaterial);
    end;
  end
 else inherited;
 end;
end;

{-------------------}

initialization
begin
  RegisterQObject(QVMTFile, 'v');
end;

finalization
  UnloadVTFLib(true);
end.

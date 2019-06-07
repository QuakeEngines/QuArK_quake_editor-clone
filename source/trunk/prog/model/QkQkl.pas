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
unit QkQkl;

interface

uses
  Windows, Graphics, SysUtils, QkModel, QkModelRoot, QkComponent, QkFrame,
  QkModelBone, QkImages, QkObjects, QkFileObjects;

type
  QQkl = class(QModel)
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  protected
    function Loaded_Root : QModelRoot;
    function Saving_Root : QModelRoot;
    function Loaded_Skin(Component: QComponent; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer) : QImage;
    function Loaded_Frame(Component: QComponent; const Name: String) : QFrame;
    function Loaded_SkinFile(Component: QComponent; const Name: String; warnifnotfound: Boolean) : QImage;
    function Loaded_Component(Root: QModelRoot; const cname: string): QComponent;
    function Loaded_Bone(Root: QModelRoot; Parent: QModelBone; const Name: String): QModelBone;
    function CantFindTexture(Component: QComponent; const name: string; SZ: TPoint): QImage;
  end;

implementation

uses QuarkX, QkObjectClassList, qkskindrawobject, Python, Game, QkExceptions,
  qkskingroup, qkframegroup, qkbonegroup, qkpcx, qktextures, qkmiscgroup;

class function QQkl.TypeInfo;
begin
  Result:='.qkl';
end;

class procedure QQkl.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5143);
  Info.FileExt:=785;
  Info.QuArKFileObject:=True;
end;

function QQkl.Loaded_Component(Root: QModelRoot; const cname: string): QComponent;
var
  L: TQList;
begin
  L:=TQList.Create;
  try
    Root.FindAllSubObjects('', QComponent, Nil, L);
    if cname<>'' then
      Result:=QComponent.Create(cname, Root)
    else
      Result:=QComponent.Create(format('Component %d',[L.Count+1]), Root);
    with result do begin
      IntSpec['show']:=1;
      CreateSkinGroup;
      CreateFrameGroup;
      CreateSDO;
    end;
    Root.SubElements.Add(Result);
  finally
    L.Free;
  end;
end;

function QQkl.Loaded_Skin(Component: QComponent; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer) : QImage;
const
  Spec1 = 'Image1=';
  Spec2 = 'Pal=';
var
  S: String;
  Skins: QSkinGroup;
begin
  if component=nil then
    Skins:=nil
  else
    Skins:=Component.SkinGroup;
  Result:=QPcx.Create(Name, Skins);
  if component<>nil then
    Skins.SubElements.Add(Result);
  Result.SetFloatsSpec('Size', Size);
  S:=Spec2;
  SetLength(S, Length(Spec2) + SizeOf(TPaletteLmp));
  Move(GameBuffer(ObjectGameCode)^.PaletteLmp, S[Length(Spec2)+1], SizeOf(TPaletteLmp));
  Result.Specifics.Add(S);
  S:=Spec1;
  DeltaW:=-((Round(Size[0])+3) and not 3);
  SetLength(S, Length(Spec1) - DeltaW*Round(Size[1]));
  P:=PChar(S)+Length(S)+DeltaW;
  Result.Specifics.Add(S);
end;

function QQkl.Loaded_Frame(Component: QComponent; const Name: String) : QFrame;
var
  Frames: QFrameGroup;
begin
  Frames:=Component.FrameGroup;
  Result:=QFrame.Create(Name, Frames);
  Result.ParentComponent:=Component;
  Frames.SubElements.Add(Result);
end;

function QQkl.Loaded_Bone(Root: QModelRoot; Parent: QModelBone; const Name: String): QModelBone;
var
  Bones: QBoneGroup;
begin
  Bones:=Root.BoneGroup;
  Result:=QModelBone.Create(Name, Bones);
  Bones.SubElements.Add(Result);
end;

function QQkl.Loaded_SkinFile(Component: QComponent; const Name: String; warnifnotfound: Boolean) : QImage;
var
  Path: String;
  J: Integer;
  nImage: QObject;
  Skins: QSkinGroup;
begin
  Skins:=Component.SkinGroup;
  Path:=Name;
  repeat
    nImage:=LoadSibling(Path);
    if nImage<>Nil then begin
      try
        if nImage is QTextureFile then begin
          Result:=QPcx.Create('', Skins);
          try
            Result.ConversionFrom(QTextureFile(nImage));
          except
            Result.Free;
            Raise;
          end;
        end else begin
          Result:=nImage as QImage;
          Result:=Result.Clone(Skins, False) as QImage;
        end;
        Skins.SubElements.Add(Result);
        Result.Name:=Copy(Name, 1, Length(Name)-Length(nImage.TypeInfo));
        {Result.Flags:=Result.Flags or ofFileLink;}
        Exit;
      finally
        nImage.AddRef(-1);
      end;
    end;
    J:=Pos('/',Path);
    if J=0 then
    begin
      J:=Pos('\',Path);
      if J = 0 then
        Break;
    end;
    System.Delete(Path, 1, J);
  until False;
  if warnifnotfound then
    GlobalWarning(FmtLoadStr1(5575, [Name, LoadName]));
  Result:=Nil;
end;

function QQkl.Loaded_Root : QModelRoot;
var
  misc: QMiscGroup;
  BoneGroup: QBoneGroup;
begin
  Specifics.Values['FileName']:=ExtractFileName(LoadName);
  Result:=QModelRoot.Create(LoadStr1(2371), Self);
  SubElements.Add(Result);
  Misc:=QMiscGroup.Create('Misc', Result);
  Misc.IntSpec['type']:=MDL_GROUP_MISC;
  Result.SubElements.Add(Misc);
  BoneGroup:=QBoneGroup.Create('Skeleton', Result);
  BoneGroup.IntSpec['type']:=MDL_GROUP_BONE;
  Result.SubElements.Add(BoneGroup);
  Specifics.Values['Root']:=Result.Name+Result.TypeInfo;
end;

function QQkl.Saving_Root;
begin
  LoadAll;
  Result:=GetRoot;
end;

function xoxox: TBitmap;
begin
  result:=TBitmap.Create;
  result.width:=10;
  result.height:=10;
  result.canvas.brush.color:=clWhite;
  result.canvas.FloodFill(result.Width div 2,result.Height div 2, clBlack, fsBorder);
  result.canvas.brush.color:=clBlack;
  result.canvas.pen.color:=clBlack;
  result.Canvas.Rectangle(0,0, 4,4);
  result.Canvas.Rectangle(5,5, 9,9);
end;

// Create a QImage object of size 'SZ' containing black and white squares,
// if a texture can't be found...
function QQkl.CantFindTexture(Component: QComponent; const name: string; SZ: TPoint): QImage;
var
  bmp: TBitmap;
  Skins: QSkinGroup;
begin
  Skins:=Component.SkinGroup;
  Result:=QPCX.Create(name, skins);
  bmp:=TBitmap.Create;
  try
    if SZ.X<>0 then bmp.Width:=SZ.X else bmp.Width:=50;
    if SZ.Y<>0 then bmp.Height:=SZ.Y else bmp.Height:=50;
    bmp.canvas.brush.color:=clWhite;
    bmp.canvas.FloodFill(bmp.Width div 2,bmp.Height div 2, clBlack, fsBorder);
    bmp.Canvas.brush.color:=clBlack;
    bmp.canvas.brush.bitmap:=xoxox;
    bmp.canvas.FloodFill(bmp.Width div 2,bmp.Height div 2, clBlack, fsBorder);
    result.PasteBitmap(GameBuffer(ObjectGameCode), bmp);
    Skins.SubElements.Add(Result);
  finally
    bmp.free;
  end;
end;

initialization
  RegisterQObject(QQkl, 'w');
end.

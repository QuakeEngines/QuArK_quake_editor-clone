{
$Header$
----------- REVISION HISTORY ------------
$Log$
}

unit QkModelFile;

interface

uses
  windows, sysutils, QkObjects, QkFileObjects, QkForm, QkImages, Python, Game, QkModel, QkModelRoot,
  qkcomponent, qkframe, qkskingroup, qkframegroup, qkbonegroup, qkpcx, qktextures, qkmiscgroup,
  graphics;

type
  QModelFile = class(QModel)
  protected
    function Loaded_Root : QModelRoot;
    function Saving_Root : QModelRoot;
    function Loaded_Skin(Component: QComponent; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer) : QImages;
    function Loaded_Frame(Component: QComponent; const Name: String) : QFrame;
    function Loaded_SkinFile(Component: QComponent; const Name: String; warnifnotfound: Boolean) : QImages;
    function Loaded_Component(Root: QModelRoot; cname: string): QComponent;
    function CantFindTexture(Component: QComponent; name: string; SZ: TPoint): QImages;
  end;

implementation

uses quarkx, qkskindrawobject;

function QModelFile.Loaded_Component(Root: QModelRoot; cname: string): QComponent;
var
  L: TQList;
begin
  L:=TQList.Create;
  try
    Root.FindAllSubObjects('', QComponent, Nil, L);
    if cname='' then
      cname:=format('Component %d',[L.Count+1]);
    Result:=QComponent.Create(cname, Root);
    with result do begin
      IntSpec['show']:=1;
      CreateSkinGroup;
      CreateFrameGroup;
      CreateBoneGroup;
      CreateSDO;
    end;
    Root.SubElements.Add(Result);
  finally
    L.Free;
  end;
end;

function QModelFile.Loaded_Skin(Component: QComponent; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer) : QImages;
const
  Spec1 = 'Pal=';
  Spec2 = 'Image1=';
var
  S: String;
  Skins: QSkinGroup;
begin
  Skins:=Component.SkinGroup;
  Result:=QPcx.Create(Name, Skins);
  Skins.SubElements.Add(Result);
  Result.SetFloatsSpec('Size', Size);
  S:=Spec1;
  SetLength(S, Length(Spec1) + SizeOf(TPaletteLmp));
  Move(GameBuffer(ObjectGameCode)^.PaletteLmp, S[Length(Spec1)+1], SizeOf(TPaletteLmp));
  Result.SpecificsAdd(S);
  S:=Spec2;
  DeltaW:=-((Round(Size[0])+3) and not 3);
  SetLength(S, Length(Spec2) - DeltaW*Round(Size[1]));
  P:=PChar(S)+Length(S)+DeltaW;
  Result.Specifics.Add(S);
end;

function QModelFile.Loaded_Frame(Component: QComponent; const Name: String) : QFrame;
var
  Frames: QFrameGroup;
begin
  Frames:=Component.FrameGroup;
  Result:=QFrame.Create(Name, Frames);
  Frames.SubElements.Add(Result);
end;

function QModelFile.Loaded_SkinFile(Component: QComponent; const Name: String; warnifnotfound: Boolean) : QImages;
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
          Result:=nImage as QImages;
          Result:=Result.Clone(Skins, False) as QImages;
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
    if J=0 then Break;
    System.Delete(Path, 1, J);
  until False;
  if warnifnotfound then
    GlobalWarning(FmtLoadStr1(5575, [Name, LoadName]));
  Result:=Nil;
end;

function QModelFile.Loaded_Root : QModelRoot;
var
  misc: QMiscGroup;
begin
  Specifics.Values['FileName']:=ExtractFileName(LoadName);
  Result:=QModelRoot.Create(LoadStr1(2371), Self);
  Misc:=QMiscGroup.Create('Misc', Result);
  Misc.IntSpec['type']:=6;
  SubElements.Add(Result);
  Result.SubElements.Add(Misc);
  Specifics.Values['Root']:=Result.Name+Result.TypeInfo;
end;

function QModelFile.Saving_Root;
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

// Create a QImages object of size 'SZ' containing black and white squares,
// if a texture can't be found...
function QModelFile.CantFindTexture(Component: QComponent; name: string; SZ: TPoint): QImages;
var
  bmp: TBitmap;
  Skins: QSkinGroup;
begin
  Skins:=Component.SkinGroup;
  Result:=QPCX.Create(name, skins);
  bmp:=TBitmap.Create;
  if SZ.X<>0 then bmp.Width:=SZ.X else bmp.Width:=50;
  if SZ.Y<>0 then bmp.Height:=SZ.Y else bmp.Height:=50;
  bmp.canvas.brush.color:=clWhite;
  bmp.canvas.FloodFill(bmp.Width div 2,bmp.Height div 2, clBlack, fsBorder);
  bmp.Canvas.brush.color:=clBlack;
  bmp.canvas.brush.bitmap:=xoxox;
  bmp.canvas.FloodFill(bmp.Width div 2,bmp.Height div 2, clBlack, fsBorder);
  result.PasteBitmap(GameBuffer(ObjectGameCode), bmp);
  Skins.SubElements.Add(Result);
  bmp.free;
end;

end.

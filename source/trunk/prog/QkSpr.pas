unit QkSpr;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkForm, QkFileObjects, QSplitter, StdCtrls, ComCtrls, ExtCtrls, TB97,
  QkObjects, Quarkx, Game, Setup, Menus, QkImages;

type
  dsprite_t = packed record // Half Life Sprite
	ident           :  Longint;
	version         :  Longint;
	stype           :  Longint;
	texFormat       :  Longint;
	boundingradius  :  Single;
	width           :  Longint;
	height          :  Longint;
	numframes       :  Longint;
	beamlength      :  Single;
	synctype        :  Single;
  end;
  dspr_t = packed record  // Quake 1 Sprite
	ident           :  Longint;
	version         :  Longint;
	stype           :  Longint;
	boundingradius  :  Single;
	width           :  Longint;
	height          :  Longint;
	numframes       :  Longint;
	beamlength      :  Single;
	synctype        :  Single;
  end;
  dspr2_t = packed record  // Quake 2 Sprite
        ident: Longint;
        version: Longint;
        noFrames: Longint;
  end;

  QSprFile = class(QFileObject)
        public
         function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         procedure EtatObjet(var E: TEtatObjet); override;
         function OuvrirFenetre(nOwner: TComponent) : TQForm1; override;
         procedure Charger(F: TStream; Taille: Integer); override;
         procedure DoQ1Spr(fs:TStream; PPPalette:PGameBuffer);
         procedure DoQ2Spr(Fs: TStream);
         procedure DoHLSpr(Fs:TStream);
         Procedure WriteQ1Spr(F:TStream);
         Procedure WriteQ2Spr(F:TStream);
         Procedure WriteHLSpr(F:TStream);
         procedure Enregistrer(Info: TInfoEnreg1); override;
         function Loaded_Frame(Root: QSprFile; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer; pal:TPaletteLmp; var palout:String) : QImages;
         function Loaded_FrameFile(Root: QSprFile; const Name: String) : QImages;
         procedure GetWidthHeight(var size:TPoint);
        end;
  QSp2File = class(QSprFile)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
  TQSprForm = class(TQForm1)
    Panel1: TPanel;
    ListView1: TListView;
    QSplitter1: TQSplitter;
    Panel2: TPanel;
    OpenDialog1: TOpenDialog;
    ComboBox1: TComboBox;
    Label13: TLabel;
    Label1: TLabel;
    ComboBox2: TComboBox;
    Label2: TLabel;
    ComboBox3: TComboBox;
    Panel3: TPanel;
    ffw: TToolbarButton97;
    play: TToolbarButton97;
    back: TToolbarButton97;
    rew: TToolbarButton97;
    Bevel1: TBevel;
    Label3: TLabel;
    procedure QSplitter1Resized(Sender: TObject; nPosition: Integer);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1DropDown(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure playClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
  private
    OldIndex, Index:Longint;
    procedure UpdateListView;
    { Private declarations }
  public
    ImageDisplayer: TImageDisplayer;
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    procedure wmMessageInterne(var Msg: TMessage); override;
    { Public declarations }
  end;

var
  QSprForm: TQSprForm;

Function MyFloatSpec(s:QSprFile; ident:String):Single;
Function MyStrSpec(s:QSprFile; ident:String):String;
Function MyIntSpec(s:QSprFile; ident:String):INteger;

implementation

uses QkPcx, QkTextures;

{$R *.DFM}

function QSprFile.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
if (Q is QPcx) then
  Result:=ieResult[True]
 else
   Result:=[];
end;

procedure QSprFile.Charger(F: TStream; Taille: Integer);
var
ID_SPRHEADER,ID_SP2Header,head,ver,org:Longint;
pgb:PGameBuffer;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
        ID_SPRHEADER:=(ord('P') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
        ID_SP2Header:=(ord('2') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
        org:=f.Position;
        f.ReadBuffer(head,4);
        if (head<>ID_SPRHEADER) and (head<>ID_SP2HEADER) then
          raise Exception.CreateFmt('Sprite Signiture = %d, Should be: %d or %d',[head,ID_SPRHEADER,ID_SP2HEADER]);
        f.ReadBuffer(ver,4);
        f.seek(org,soFromBeginning);
        if (ver=1 ) and (head=ID_SPRHEADER) then begin
           pgb:=GameBuffer(mjQuake);
           f.seek(org,soFromBeginning); // something happens to f after gamebuffer is called, so i reset the position to org.
           DoQ1Spr(f, pgb);
           end
        else if (ver=2) and (head=ID_SP2HEADER) then DoQ2Spr(f)
        else if ver=2 then DoHLSpr(f)
        else
          raise Exception.CreateFmt('Sprite Version %d UnSupported, Versions Supported: 1 (Quake 1), and 2 (Quake 2 / Half-Life)',[ver]);
       end;
    else inherited;
  end;
end;

procedure QSprFile.DoQ1Spr(fs:TStream; PPPalette:PGameBuffer);
var dst:DSpr_T;
group,ID_SPRHeader,i,j,pos,nopics{,noframes}:longint;
xoffset,yoffset,width,height:Longint;
aPalette: TPaletteLmp;
p: PChar;
DeltaW:Integer;
pout:String;
times: array[1..1024] of Single;
begin
  aPalette:=PPPalette^.PaletteLmp;
  fs.ReadBuffer(dst,sizeof(dst));
  ID_SPRHeader:=(ord('P') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
  if dst.ident<>ID_SPRHEADER then
    raise Exception.CreateFmt('Quake 1 Sprite Signiture = %d, Should be: %d',[dst.ident,ID_SPRHEADER]);
  if dst.version<>1 then
    raise Exception.CreateFmt('Quake 1 Sprite Version = %d, Should be: %d',[dst.version,2]);
  ObjectGameCode:=mjQuake;
  Self.SpecificsAdd(format('SPR_STYPE=%d',[dst.sType]));
  Self.SpecificsAdd(format('SPR_TXTYPE=%d',[-1]));
  Self.SpecificsAdd(format('SPR_RADIUS=%f',[dst.boundingradius]));
  Self.SpecificsAdd(format('SPR_WIDTH=%d',[dst.width]));
  Self.SpecificsAdd(format('SPR_HEIGHT=%d',[dst.height]));
  for i:=1 to dst.numframes do begin
    fs.ReadBuffer(group,4);
    if group=0 then begin
      fs.ReadBuffer(xoffset,4);
      fs.ReadBuffer(yoffset,4);
      fs.ReadBuffer(width,4);
      fs.ReadBuffer(height,4);
      J:=Fs.Position;
      Loaded_Frame(Self, format('Frame %d',[i]), [width,height], p, DeltaW, apalette,pout);
      Fs.Position:=J;
      for J:=1 to height do
        begin
         Fs.ReadBuffer(P^, width);
         Inc(P, DeltaW);
        end;
    end else begin
      fs.readbuffer(nopics,4);
      for j:=1 to nopics do
        fs.readbuffer(times[j],4);
      for j:=1 to nopics do begin
        fs.ReadBuffer(xoffset,4);
        fs.ReadBuffer(yoffset,4);
        fs.ReadBuffer(width,4);
        fs.ReadBuffer(height,4);
        Pos:=Fs.Position;
        Loaded_Frame(Self, format('Frame %d',[i]), [width,height], p, DeltaW, apalette,pout);
        Fs.Position:=Pos;
        for Pos:=1 to height do begin
          Fs.ReadBuffer(P^, width);
          Inc(P, DeltaW);
        end;
      end;
    end;
  end;
end;

procedure QSprFile.DoHLSpr(Fs:TStream);
var
dst:dsprite_t;
ID_SPRHEADER, i, w, h, lint, lint2, J:Longint;
DeltaW:Integer;
fshort:SmallInt;
aPalette: TPaletteLmp;
P:PChar;
pout:String;
begin
  ID_SPRHEADER:=(ord('P') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
  fs.ReadBuffer(dst,sizeof(dst));
  if dst.ident<>ID_SPRHEADER then
    raise Exception.CreateFmt('Half Life Sprite Signiture = %d, Should be: %d',[dst.ident,ID_SPRHEADER]);
  if dst.version<>2 then
    raise Exception.CreateFmt('Half Life Sprite Version = %d, Should be: %d',[dst.version,2]);
  ObjectGameCode:=mjHalfLife;
  Self.SpecificsAdd(format('SPR_STYPE=%d',[dst.sType]));
  Self.SpecificsAdd(format('SPR_TXTYPE=%d',[dst.texformat]));
  Self.SpecificsAdd(format('SPR_RADIUS=%f',[dst.boundingradius]));
  Self.SpecificsAdd(format('SPR_WIDTH=%d',[dst.width]));
  Self.SpecificsAdd(format('SPR_HEIGHT=%d',[dst.height]));
  Self.SpecificsAdd(format('SPR_NOFRAMES=%d',[dst.numframes]));

  fs.ReadBuffer(FShort,2);
  for i:=0 to FShort-1 do begin
    fs.ReadBuffer(lint2,1);
    aPalette[i,0]:=lint2;
    fs.ReadBuffer(lint2,1);
    aPalette[i,1]:=lint2;
    fs.ReadBuffer(lint2,1);
    aPalette[i,2]:=lint2;
    end;
  for i:=1 to dst.numframes do begin
    fs.ReadBuffer(lint,4);
    fs.ReadBuffer(lint,4);
    fs.ReadBuffer(lint,4);
    fs.ReadBuffer(W,4);
    fs.ReadBuffer(H,4);
    J:=Fs.Position;
    Loaded_Frame(Self, format('Frame %d',[i]), [w,h], p, DeltaW, apalette,pout);
    Fs.Position:=J;
    for J:=1 to h do
      begin
       Fs.ReadBuffer(P^, w);
       Inc(P, DeltaW);
      end;
    end;
end;

procedure QSprFile.DoQ2Spr(Fs:TStream);
var
dst:DSpr2_t;
ID_SP2HEADER,i,x,y,w,h:Longint;
s:array[1..64] of char;
str{,fstr}:String;
begin
  ID_SP2Header:=(ord('2') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
  fs.ReadBuffer(Dst,sizeof(dst));
  if dst.ident<>ID_SP2HEADER then
    raise Exception.CreateFmt('Quake 2 Sprite Signiture = %d, Should be: %d',[dst.ident,ID_SP2HEADER]);
  if dst.version<>2 then
    raise Exception.CreateFmt('Quake 2 Sprite Version = %d, Should be: %d',[dst.version,2]);
  ObjectGameCode:=mjQuake2;
  Self.SpecificsAdd(format('SPR_STYPE=%d',[-1]));
  Self.SpecificsAdd(format('SPR_TXTYPE=%d',[-1]));
  Self.SpecificsAdd(format('SPR_RADIUS=%s',['N / A']));
  Self.SpecificsAdd(format('SPR_WIDTH=%s',['N / A']));
  Self.SpecificsAdd(format('SPR_HEIGHT=%s',['N / A']));
  Self.SpecificsAdd(format('SPR_NOFRAMES=%d',[Dst.noframes]));
  fillchar(s,sizeof(s),#0);
  For i:=1 to dst.Noframes do begin
    fs.Readbuffer(x,4);
    fs.Readbuffer(y,4);
    fs.Readbuffer(w,4);
    fs.Readbuffer(h,4);
    fs.Readbuffer(s,64);
    str:=s;
    setlength(str,pos(#0,s));
    Self.SpecificsAdd(Format('SPR_FRAME%d_CAPTION=%d',[i,i]));
    Self.SpecificsAdd(Format('SPR_FRAME%d_FTYPE=%s',[i,str]));
    Self.SpecificsAdd(Format('SPR_FRAME%d_XORG=%d',[i,x]));
    Self.SpecificsAdd(Format('SPR_FRAME%d_YORG=%d',[i,y]));
    Self.SpecificsAdd(Format('SPR_FRAME%d_WIDTH=%d',[i,w]));
    Self.SpecificsAdd(Format('SPR_FRAME%d_HEIGHT=%d',[i,h]));
    Self.SpecificsAdd(Format('SPR_FRAME%d_IDATASIZE=%s',[i,'N / A']));
//    fstr:=copy(str,pos('/',str),length(str)-pos('/',str));
//    Loaded_FrameFile(self, fStr);// Doesn't Work. Wonder Why??
  end;
end;

Procedure QSprFile.WriteQ1Spr(F:TStream);
var
ID_SPRHEADER,Ver,cnt,typ,i,j,delta:Longint;
rad:Single;
w,h,z:Longint;
P:PChar;
SkinObj : QImage;
pt:TPoint;
begin
z:= 0;
ID_SPRHeader:=(ord('P') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
f.WriteBuffer(ID_SPRHEADER,4);
ver:=1;
f.WriteBuffer(ver,4);
typ:=myIntSpec(self,'SPR_STYPE');
f.Writebuffer(typ,4);

GetWidthHeight(pt);
w:=pt.x;
h:=pt.y;
rad:=Sqrt(Sqr(w/2)+Sqr(h/2));
f.Writebuffer(rad,4);
f.WriteBuffer(w,4);
f.WriteBuffer(h,4);
cnt:=Souselements.count;
f.Writebuffer(cnt,4);
f.WriteBuffer(z,4);
f.WriteBuffer(z,4);
for i:=1 to cnt do begin
 SkinObj:=QImage(SousElements[i-1]);
 SkinObj.NotTrueColor;
 pt:=SkinObj.GetSize;
 f.WriteBuffer(z,4);
 f.WriteBuffer(z,4);
 f.WriteBuffer(z,4);
 f.WriteBuffer(pt.x,4);
 f.WriteBuffer(pt.y,4);
 P:=SkinObj.GetImagePtr1;
 Delta:=(w + 3) and not 3;
 Inc(P, Delta*h);   { FIXME: check palette }
 for J:=1 to h do
   begin
   Dec(P, Delta);
   F.WriteBuffer(P^, w);
   end;
 end;
end;

function Max(a,b:Longint):Longint;
begin
if a>b then result:=a else result:=b;
end;

procedure QSprFile.GetWidthHeight(var size:TPoint);
var
 SizeTemp:TPoint;
 WTemp,HTemp,i:Longint;
begin
 WTemp:=0;HTemp:=0;
 for i:=0 to  SousElements.Count-1 do
  begin
    SizeTemp:=QPcx(SousElements.Items[i]).GetSize;
    WTemp:=Max(SizeTemp.X,WTemp);
    HTemp:=Max(SizeTemp.X,HTemp);
  end;
  Size.X:=WTemp;
  Size.Y:=HTemp;
end;

Procedure QSprFile.WriteHLSpr(F:TStream);
var
ID_SPRHEADER,Ver,cnt,typ,i,j,delta:Longint;
rad:Single;
w,h,z:Longint;
P:PChar;
SkinObj : QImage;
bt:Byte;
pt:TPoint;
Pal:TPaletteLmp;
begin
z:= 0;
ID_SPRHeader:=(ord('P') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
f.WriteBuffer(ID_SPRHEADER,4);
ver:=2;
f.WriteBuffer(ver,4);
typ:=myIntSpec(self,'SPR_STYPE');
f.Writebuffer(typ,4);
typ:=myIntSpec(self,'SPR_TXTYPE');
f.Writebuffer(typ,4);

GetWidthHeight(pt);
w:=pt.x;
h:=pt.y;

rad:=Sqrt(Sqr(w/2)+Sqr(h/2));
f.Writebuffer(rad,4);
f.WriteBuffer(w,4);
f.WriteBuffer(h,4);
cnt:=souselements.count;
f.Writebuffer(cnt,4);
f.WriteBuffer(z,4);
f.WriteBuffer(z,4);
typ:=256;
f.WriteBuffer(typ,2);
SkinObj:=QImage(SousElements[0]); // use palette of first image for sprite.
SkinObj.NotTrueColor;
skinobj.GetPalette1(pal);
for i:=0 to 255 do begin
  for j:=0 to 2 do begin
    bt:=ord(pal[i,j]);
    f.WriteBuffer(bt,1);
    end;
  end;

for i:=1 to cnt do begin
 SkinObj:=QImage(SousElements[i-1]);
 SkinObj.NotTrueColor;
 pt:=SkinObj.GetSize;
 f.WriteBuffer(z,4);
 f.WriteBuffer(z,4);
 f.WriteBuffer(z,4);
 f.WriteBuffer(pt.x,4);
 f.WriteBuffer(pt.y,4);
 P:=SkinObj.GetImagePtr1;
 Delta:=(w + 3) and not 3;
 Inc(P, Delta*h);   { FIXME: check palette }
 for J:=1 to h do
   begin
   Dec(P, Delta);
   F.WriteBuffer(P^, w);
   end;
 end;
end;

Procedure QSprFile.WriteQ2Spr(F:TStream);
var
ID_SP2HEADER,Ver,cnt,i:Longint;
x,y,w,h:Longint;
data:String;
begin
ID_SP2Header:=(ord('2') shl 24)+(ord('S')shl 16)+(ord('D') shl 8)+ord('I');
f.WriteBuffer(ID_SP2HEADER,4);
ver:=2;
f.WriteBuffer(ver,4);
cnt:=IntSpec['SPR_NOFRAMES'];
f.Writebuffer(cnt,4);
for i:=1 to cnt do begin
 x:=myIntSpec(self,format('SPR_FRAME%d_XORG',[i]));
 y:=myIntSpec(self,format('SPR_FRAME%d_YORG',[i]));
 w:=myIntSpec(self,format('SPR_FRAME%d_WIDTH',[i]));
 h:=myIntSpec(self,format('SPR_FRAME%d_HEIGHT',[i]));
 Data:=myStrSpec(self,format('SPR_FRAME%d_FTYPE',[i]));
 f.WriteBuffer(x,4);
 f.WriteBuffer(y,4);
 f.WriteBuffer(w,4);
 f.WriteBuffer(h,4);
 f.WriteBuffer(PChar(Data)^,64);
 end;
end;

procedure QSprFile.Enregistrer(Info: TInfoEnreg1);
var
fg:Char;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      fg:=ObjectGameCode;
      if fg=mjQuake then
        WriteQ1Spr(Info.F)
      else if fg=mjHalfLife then
        WriteHLSpr(Info.F)
      else if fg=mjQuake2 then
        WriteQ2Spr(Info.F);
     end;
  else inherited;
 end;
end;

function QSprFile.OuvrirFenetre;
begin
 Result:=TQSprForm.Create(nOwner);
end;

class function QSprFile.TypeInfo;
begin
 Result:='.spr';
end;

procedure QSprFile.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiSpriteFile;
end;

class procedure QSprFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5171);
 Info.FileExt:=799;
 Info.WndInfo:=[wiOwnExplorer];
end;

function QSprFile.Loaded_Frame(Root: QSprFile; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer; pal:TPaletteLmp; var palout:String) : QImages;
const
 Spec1 = 'Pal=';
 Spec2 = 'Image1=';
var
 S: String;
begin
 Result:=QPcx.Create(Name, Root);
 Root.SousElements.Add(Result);
 Result.SetFloatsSpec('Size', Size);
 S:=Spec1;
 SetLength(S, Length(Spec1) + SizeOf(TPaletteLmp));
 if ObjectGameCode=mjQuake then
   Move(GameBuffer(ObjectGameCode)^.PaletteLmp, S[Length(Spec1)+1], SizeOf(TPaletteLmp))
 else if ObjectGameCode=mjHalfLife then
   Move(pal,S[Length(Spec1)+1],sizeof(TPaletteLmp));
 palout:=s;
 Result.SpecificsAdd(S);
 S:=Spec2;
 DeltaW:=-((Round(Size[0])+3) and not 3);
 SetLength(S, Length(Spec2) - DeltaW*Round(Size[1]));
 P:=PChar(S)+Length(S)+DeltaW;
 Result.Specifics.Add(S);
end;

function QSprFile.Loaded_FrameFile(Root: QSprFile; const Name: String) : QImages;
var
 Path: String;
 J: Integer;
 nImage: QObject;
begin
 GameBuffer(ObjectGameCode);
 Path:=Name;
 repeat
  nImage:=LoadSibling(Path);
  if nImage<>Nil then
   try
    if nImage is QTextureFile then
     begin
      Result:=QPcx.Create('', Root);
      try
       Result.ConversionFrom(QTextureFile(nImage));
      except
       Result.Free;
       Raise;
      end;
     end
    else
     begin
      Result:=nImage as QImages;
      Result:=Result.Clone(Root, False) as QImages;
     end;
    Root.SousElements.Add(Result);
    Result.Name:=Copy(Name, 1, Length(Name)-Length(nImage.TypeInfo));
    {Result.Flags:=Result.Flags or ofFileLink;}
    Exit;
   finally
    nImage.AddRef(-1);
   end;
  J:=Pos('/',Path);
  if J=0 then Break;
  System.Delete(Path, 1, J);
 until False;
 GlobalWarning(FmtLoadStr1(5575, [Name, LoadName]));
 Result:=Nil;
end;

class function QSp2File.TypeInfo;
begin
 Result:='.sp2';
end;

class procedure QSp2File.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5171);
 Info.FileExt:=800;
 Info.WndInfo:=[wiWindow,wiSameExplorer];
end;

function TQSprForm.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QSprFile) and inherited AssignObject(Q, State);
end;

procedure TQSprForm.QSplitter1Resized(Sender: TObject; nPosition: Integer);
begin
  Panel2.Height:=nPosition;
end;

procedure TQSprForm.wmMessageInterne(var Msg: TMessage);
var
s:QSprFile;
fg:char;
begin
  case Msg.wParam of
    wp_AfficherObjet:
      if FileObject<>Nil then begin
        ListView1.Items.Clear;
        s:=QSprFile(FileObject);
        if FileObject is QSp2File then begin
          s.ObjectGameCode:=mjQuake2;
          ListView1.Visible:=true;
          panel3.visible:=false;
          end
        else begin
          ListView1.Visible:=false;
          panel3.visible:=true;
          rew.click;
          end;
        fg:=s.ObjectGameCode;
        if fg=mjQuake then
           begin
             Combobox1.itemindex:=0;//'Quake 1'
             Combobox2.Enabled:=true;
             Combobox3.Enabled:=false;
             Combobox2.ItemIndex:=myintspec(s,'SPR_STYPE');
             Combobox3.ItemIndex:=-1;
           end
        else if FG=mjQuake2 then
           begin
             Combobox1.itemindex:=1;//'Quake 2'
             Combobox2.Enabled:=false;
             Combobox3.Enabled:=false;
             Combobox2.ItemIndex:=-1;
             Combobox3.ItemIndex:=-1;
           end
        else if FG=mjHalfLife then begin
             Combobox1.itemindex:=2;//'Half-Life';
             Combobox2.Enabled:=true;
             Combobox3.Enabled:=true;
             Combobox2.ItemIndex:=myintspec(s,'SPR_STYPE');
             Combobox3.ItemIndex:=myintspec(s,'SPR_TXTYPE');
           end;
        UpdateListView;
      end;
    end;
  inherited;
end;

Procedure MySetIntSpec(s:QSprFile; ident:String; value:Integer);
begin
  s.Specifics.Values[ident]:=inttostr(value);
end;

Function MyIntSpec(s:QSprFile; ident:String):INteger;
begin
if s.Specifics.IndexOfName(ident)=-1 then
  result:=0
else
  try
    result:=strtoint(s.Specifics.Values[ident]);
  except
    on EConvertError do result:=0;
  end;
end;

Function MyStrSpec(s:QSprFile; ident:String):String;
begin
if s.Specifics.IndexOfName(ident)=-1 then
  result:=''
else
  result:=s.Specifics.Values[ident];
end;

Function MyFloatSpec(s:QSprFile; ident:String):Single;
begin
if s.Specifics.IndexOfName(ident)=-1 then
  result:=0.0
else
  try
    result:=strtofloat(s.Specifics.Values[ident]);
  except
    on EConvertError do result:=0.0;
  end;
end;

procedure TQSprForm.UpdateListView;
var
li:TListItem;
cnt,i:Integer;
s:QSprFile;
begin
s:=QSprFile(FileObject);
listview1.items.clear;
cnt:=myintspec(s,'SPR_NOFRAMES');
for i:=1 to cnt do begin
  li:=ListView1.Items.add;
  li.Caption:=s.Specifics.Values[format('SPR_FRAME%d_CAPTION',[i])];
  li.SubItems.add(s.Specifics.Values[format('SPR_FRAME%d_FTYPE',[i])]);
  li.SubItems.add(s.Specifics.Values[format('SPR_FRAME%d_XORG',[i])]);
  li.SubItems.add(s.Specifics.Values[format('SPR_FRAME%d_YORG',[i])]);
  li.SubItems.add(s.Specifics.Values[format('SPR_FRAME%d_WIDTH',[i])]);
  li.SubItems.add(s.Specifics.Values[format('SPR_FRAME%d_HEIGHT',[i])]);
  li.SubItems.add(s.Specifics.Values[format('SPR_FRAME%d_IDATASIZE',[i])]);
  end;
end;

procedure TQSprForm.ComboBox1Change(Sender: TObject);
begin
if OldIndex=1 then begin
  Combobox1.ItemIndex:=1;
  raise Exception.Create('Cannnot Convert From a Quake 2 Sprite!');
  end;
if Combobox1.itemindex=0 then begin
  QSprFile(FileObject).ObjectGameCode:=mjQuake;
  Combobox2.Enabled:=true;
  Combobox2.ItemIndex:=0;
  Combobox3.Enabled:=false;
  Combobox3.ItemIndex:=-1;
  end
else if Combobox1.itemindex=1 then begin
  Combobox1.itemindex:=oldindex;
  raise Exception.Create('Cannnot Convert To a Quake 2 Sprite!');
  end
else if Combobox1.itemindex=2 then begin
  QSprFile(FileObject).ObjectGameCode:=mjHalfLife;
  Combobox2.Enabled:=true;
  Combobox2.ItemIndex:=0;
  Combobox3.Enabled:=true;
  Combobox3.ItemIndex:=0;
  end;
end;

procedure TQSprForm.ComboBox1DropDown(Sender: TObject);
begin
  OldIndex:=Combobox1.itemindex;
end;

Function SetRowSelect(h: THandle):Boolean;
var
lvflags: DWord;
begin
  lvflags:=SendMessage(h, $1000 + 55, 0, 0);
  lvflags:=lvflags or $00000020;
  result:=bool(SendMessage(h, $1000 + 54, 0, lvflags));
end;

procedure TQSprForm.FormCreate(Sender: TObject);
begin
  inherited;
  ImageDisplayer:=TImageDisplayer.Create(Self);
  ImageDisplayer.Parent:=Panel1;
  ImageDisplayer.Align:=alClient;
  SetRowSelect(ListView1.Handle);
end;

procedure TQSprForm.playClick(Sender: TObject);
var
 s:QSprFile;
 ps:QPcx;
begin
s:=QSprFile(FileObject);
case TComponent(Sender).Tag of
  -100: index:=0;
   100: index:=s.SousElements.count-1;
     1: if index+1>s.souselements.count-1 then index:=s.SousElements.count-1 else index:=index+1;
    -1: if index-1<0 then index:=0 else index:=index-1;
  else raise Exception.Create('Invalid Tag!');
end;

if index>s.souselements.count-1 then index:=s.Souselements.count-1;

if s.souselements[index]<>nil then begin
   ps:=QPcx(s.Souselements[index]);
   ImageDisplayer.Source:=ps;
   ImageDisplayer.AutoSize;
   end;

label3.caption:='Frame '+inttostr(index+1);
end;

procedure TQSprForm.ComboBox2Change(Sender: TObject);
begin
  MySetIntSpec(QSprFile(FileObject), 'SPR_STYPE', Combobox2.ItemIndex);
end;

procedure TQSprForm.ComboBox3Change(Sender: TObject);
begin
  MySetIntSpec(QSprFile(FileObject), 'SPR_TXTYPE', Combobox3.ItemIndex);
end;

initialization
  RegisterQObject(QSprFile, 'p');
  RegisterQObject(QSp2File, 'p');
end.


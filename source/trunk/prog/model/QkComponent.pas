unit QkComponent;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, QkForm, Graphics,
     QkImages, qmath, QkTextures, PyMath, Python, dialogs, QkMdlObject,
     QkFrame, QkFrameGroup, QkSkinGroup, QkBoneGroup;

const
  MDL_GROUP_FRAME = 1;
  MDL_GROUP_SKIN  = 2;
  MDL_GROUP_BONE  = 5;

type
  QComponent = class(QMdlObject)
  private
    FCurrentFrameObj: QFrame;
    FCurrentSkin: QImages;
    FSelTris: PyObject;    { List of integers }
    FInfo: PyObject;
    FSkinCounter: Integer;
    procedure SetCurrentSkin(nSkin: QImages);
    procedure SetCurrentFrame(nFrame: QFrame);
  protected
    procedure CouleurDessin(var C: TColor);
  public
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
    destructor Destroy; override;
    function Triangles(var P: PComponentTris) : Integer;
    function GetSkinDescr(Static: Boolean) : String;
    property CurrentSkin : QImages read FCurrentSkin write SetCurrentSkin;
    property CurrentFrame : QFrame read FCurrentFrameObj write SetCurrentFrame;
    procedure AddTo3DScene; override;
    procedure BuildRefList(L: TQList); override;
    function GetFrameFromIndex(N: Integer) : QFrame;
    function GetFrameFromName(const nName: String) : QFrame;
    function GetSkinFromIndex(N: Integer): QImages;
    function GetSkinFromName(const nName: String) : QImages;
    function BuildFrameList : TQList;
    function BuildSkinList : TQList;
    function QuickSetSkin(nSkin: QImages; const StaticBase: String) : QComponent;
    procedure ChercheExtremites(var Min, Max: TVect); override;
    function MergeVertices(Frames: TQList) : Boolean;
    procedure Dessiner; override;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    procedure AnalyseClic(Liste: PyObject); override;
    Function FrameGroup: QFrameGroup;
    Function SkinGroup: QSkinGroup;
    Function BoneGroup: QBoneGroup;
    Function CreateBoneGroup: QBoneGroup;
    Function CreateSkinGroup: QSkinGroup;
    Function CreateFrameGroup: QFrameGroup;
    procedure SetParentFrames(nFrame: QFrame);
    Function FindRoot: QObject;
  end;

implementation

uses Ed3dfx, PyMapView, quarkx, travail, pyobjects, QkModelRoot;

var
  GlobalSkinCounter: Integer;

function qAddFrame(self, args: PyObject) : PyObject; cdecl;
var
  fg: QFrameGroup;
  f: QFrame;
begin
  try
    with QkObjFromPyObj(self) as QComponent do begin
      fg:=FrameGroup;
      if CurrentFrame<>nil then
        f:=QFrame(CurrentFrame.Clone(fg, true))
      else begin
        f:=QFrame.Create('new frame', fg);
        f.specificsadd(FloatSpecNameOf('Vertices='));
      end;
      fg.subelements.add(f);
      CurrentFrame.SelUnique:=False;
      CurrentFrame:=f;
      CurrentFrame.SelUnique:=True;
      Result:=GetPyObj(CurrentFrame);
    end;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qRemoveTriangle(self, args: PyObject) : PyObject; cdecl;
const
  Spec1 = 'Tris';
  BaseSize = Length('Tris=');
var
  index, cnt, i, j: Integer;
  tris, tris2, dest: PComponentTris;
  S: String;
  f1, f2, f3: boolean;
  v1,v2,v3: Integer;
begin
  try
    result:=nil;
    if not PyArg_ParseTupleX(args, 'iii', [@v1, @v2,@v3]) then
      exit;
    with QkObjFromPyObj(self) as QComponent do begin
      cnt:=Triangles(tris2);
      tris:=tris2;
      // Find number of triangles not containing vertex 'index'
      index:=-1;
      for i:=1 to cnt do begin
        f1:=(tris^[0].vertexno=v1)or(tris^[0].vertexno=v2)or(tris^[0].vertexno=v3);
        f2:=(tris^[1].vertexno=v2)or(tris^[1].vertexno=v2)or(tris^[1].vertexno=v3);
        f3:=(tris^[2].vertexno=v2)or(tris^[2].vertexno=v2)or(tris^[2].vertexno=v3);
        if f1 and f2 and f3 then begin
          index:=i;
          break;
        end;
      end;
      if index=-1 then
        exit;
      S:=Spec1+'=';
      // Recompute size of specific.
      SetLength(S, BaseSize+SizeOf(TComponentTris)*(cnt-1));
      PChar(Dest):=PChar(S)+BaseSize;

      // Recreate triangles array specific.
      tris:=tris2;
      for i:=1 to cnt do begin
        if i=index then
          inc(tris)
        else begin
          for j:=0 to 2 do begin
            if not (tris^[j].vertexno = index) then begin
              with Dest^[j] do begin
                VertexNo:= tris^[j].VertexNo;
                S       := tris^[j].S;
                T       := tris^[j].T;
              end;
            end;
          end;
          Inc(Dest);
        end;
        inc(tris);
      end;
      Specifics.Delete(Specifics.IndexofName(Spec1));
      Specifics.Add(S);

    end;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qRemoveVertex(self, args: PyObject) : PyObject; cdecl;
const
  Spec1 = 'Tris';
  BaseSize = Length('Tris=');
var
  index, cnt, i, j, realno: Integer;
  tris, tris2, dest: PComponentTris;
  S: String;
  found: boolean;
  L: TQList;
begin
  try
    result:=nil;
    if not PyArg_ParseTupleX(args, 'i', [@Index]) then
      exit;
    with QkObjFromPyObj(self) as QComponent do begin
      ////////////////
      // The hard bit: find all triangle associated with the vertex
      // and delete them.
      ////////////////
      cnt:=Triangles(tris2);
      tris:=tris2;
      realno:=cnt;
      // Find number of triangles not containing vertex 'index'
      for i:=1 to cnt do begin
        found:=false;
        for j:=0 to 2 do
          if tris^[j].vertexno = index then begin
            found:=true;
//            break;  // it shouldn't be here again so exit this loop.
          end;
        if found then
          dec(realno);
        inc(tris);
      end;
      S:=Spec1+'=';
      // Recompute size of specific.
      SetLength(S, BaseSize+SizeOf(TComponentTris)*realno);
      PChar(Dest):=PChar(S)+BaseSize;

      // Recreate triangles array specific.
      tris:=tris2;
      for i:=1 to cnt do begin
        found:=false;
        for j:=0 to 2 do begin
          if tris^[j].vertexno = index then begin
            found:=true;
            break;
          end else begin
            Dest^[j].VertexNo:= tris^[j].VertexNo;
            if Dest^[j].VertexNo>index then
              Dest^[j].VertexNo:= Dest^[j].VertexNo-1;
            Dest^[j].S:= tris^[j].S;
            Dest^[j].T:= tris^[j].T;
          end;
        end;
        inc(tris);
        if not found then
          Inc(Dest);
      end;
      // add triangles spec to component.
      Specifics.Delete(Specifics.IndexofName(Spec1));
      Specifics.Add(S);
      ////////////////
      // The Easy bit: go through all frames and delete the vertex.
      ////////////////
      L:=BuildFrameList;
      for i:=0 to l.count-1 do
        QFrame(L.Items1[i]).RemoveVertex(index);
      L.free;
    end;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qShowHideComp(self, args: PyObject) : PyObject; cdecl;
var
  index: Integer;
begin
  try
    result:=nil;
    if not PyArg_ParseTupleX(args, 'i', [@Index]) then
      exit;
    with QkObjFromPyObj(self) as QComponent do begin
      IntSpec['show']:=index;
    end;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qSetFrame(self, args: PyObject) : PyObject; cdecl;
var
  u: PyObject;
  Q: QObject;
begin
  try
    Result:=Nil;
    if not PyArg_ParseTupleX(args, 'O', [@u]) then
      Exit;
    Q:=QkObjFromPyObj(u);
    if not (Q is QFrame) then
      Q:=Nil;
    with QkObjFromPyObj(self) as QComponent do
      SetCurrentFrame(QFrame(Q));
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qMergeVertices(self, args: PyObject) : PyObject; cdecl;
var
  lst: PyObject;
  Q: TQList;
begin
  try
    Result:=Nil;
    if not PyArg_ParseTupleX(args, 'O!', [PyList_Type, @lst]) then
      Exit;
    Q:=TQList.Create;
    try
      PyListToQList(lst, Q, QFrame);
      with QkObjFromPyObj(self) as QComponent do begin
        LoadAll;
        Result:=PyInt_FromLong(Ord(MergeVertices(Q)));
      end;
    finally
      Q.Free;
    end;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qSetParentFrames(self, args: PyObject) : PyObject; cdecl;
var
  u: PyObject;
  Q: QObject;
begin
  try
    Result:=nil;
    if not PyArg_ParseTupleX(args, 'O', [@u]) then
      Exit;
    Q:=QkObjFromPyObj(u);
    with QkObjFromPyObj(self) as QComponent do begin
      SetParentFrames(QFrame(Q));
    end;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

const
  MethodTable: array[0..6] of TyMethodDef =
   ((ml_name: 'setframe';      ml_meth: qSetFrame;      ml_flags: METH_VARARGS),
    (ml_name: 'mergevertices'; ml_meth: qMergeVertices; ml_flags: METH_VARARGS),
    (ml_name: 'showhide';      ml_meth: qShowHideComp;  ml_flags: METH_VARARGS),
    (ml_name: 'removevertex';  ml_meth: qRemoveVertex;  ml_flags: METH_VARARGS),
    (ml_name: 'removetriangle';ml_meth: qRemoveTriangle;ml_flags: METH_VARARGS),
    (ml_name: 'addframe';      ml_meth: qAddFrame;      ml_flags: METH_VARARGS),
    (ml_name: 'setparentframes';ml_meth: qSetParentFrames; ml_flags: METH_VARARGS));

function QComponent.FindRoot: QObject;
var
  p: QObject;
begin
  p:=Self.FParent;
  while (p<>nil) and (p.typeinfo<>':mr') do
    p:=p.FParent;
  result:=p;
end;

procedure QComponent.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiComponent;
end;

function QComponent.GetSkinDescr(Static: Boolean) : String;
begin
  if Static then
    Result:=':'+Specifics.Values['ssd']
  else
    Result:=':'+IntToHex(FSkinCounter, 8);
end;

procedure QComponent.SetCurrentFrame(nFrame: QFrame);
begin
  FCurrentFrameObj.AddRef(-1);
  FCurrentFrameObj:=nFrame;
  FCurrentFrameObj.AddRef(+1);
end;

procedure QComponent.SetParentFrames(nFrame: QFrame);
var
  index: Integer;
begin
  index:=FrameGroup.SubElements.IndexOf(nframe);
  QModelRoot(FindRoot).SetFrames(index);
end;

procedure QComponent.SetCurrentSkin(nSkin: QImages);
begin
  FCurrentSkin.AddRef(-1);
  FCurrentSkin:=nSkin;
  if nSkin<>Nil then begin
    nSkin.AddRef(+1);
    FSkinCounter:=GlobalSkinCounter;
    Inc(GlobalSkinCounter);
  end;
end;

destructor QComponent.Destroy;
begin
  Py_XDECREF(FInfo);
  FCurrentSkin.AddRef(-1);
  FCurrentFrameObj.AddRef(-1);
  {FreeMem(FCurrentFrame);}
  Py_XDECREF(FSelTris);
  {Py_XDECREF(FColor);}
  inherited;
end;

function QComponent.Triangles(var P: PComponentTris) : Integer;
const
  Spec1 = 'Tris';
var
  S: String;
begin
  if IntSpec['show']=0 then begin
    result:=0;
    exit;
  end;
  S:=GetSpecArg(Spec1);
  PChar(P):=PChar(S)+(Length(Spec1)+1);
  Result:=(Length(S)-(Length(Spec1)+1)) div SizeOf(TComponentTris);
end;

procedure QComponent.AddTo3DScene;
var
  Info: PModel3DInfo;
begin
  if CurrentFrame=Nil then begin
    SetCurrentFrame(GetFrameFromIndex(0));
    if CurrentFrame=Nil then
      Exit;
  end;
  if CurrentSkin=Nil then
    CurrentSkin:=GetSkinFromIndex(0);
  New(Info);
  FillChar(Info^, SizeOf(TModel3DInfo), 0);
  Info^.Base:=Self;
  Info^.ModelAlpha:=255;
  Info^.VertexCount:=FCurrentFrameObj.GetVertices(Info^.Vertices);
  AddRef(+1);
  CurrentMapView.Scene.ModelInfo.Add(Info);
end;

procedure QComponent.BuildRefList(L: TQList);
begin
  L.Add(Self);
end;

class function QComponent.TypeInfo;
begin
  TypeInfo:=':mc';
end;

procedure QComponent.ChercheExtremites(var Min, Max: TVect);
begin
  if FCurrentFrameObj=Nil then
    inherited
  else
    FCurrentFrameObj.ChercheExtremites(Min, Max);
end;

function QComponent.QuickSetSkin(nSkin: QImages; const StaticBase: String) : QComponent;
begin
  if nSkin = FCurrentSkin then
    Result:=Self
  else
    if FCurrentSkin = Nil then begin
      CurrentSkin:=nSkin;
      Result:=Self;
    end else begin
      Result:=QComponent.Create('', Nil);
      Result.Specifics.Add(GetSpecArg('Tris'));
      Result.CurrentSkin:=nSkin;
    end;
  Result.Specifics.Values['ssd']:=StaticBase;
  Result.AddRef(+1);
end;

function QComponent.BuildSkinList : TQList;
begin
  Result:=TQList.Create;
  try
    FindAllSubObjects('', QImages, Nil, Result);
  except
    Result.Free;
    Raise;
  end;
end;

function QComponent.BuildFrameList : TQList;
begin
  Result:=TQList.Create;
  try
    FindAllSubObjects('', QFrame, Nil, Result);
  except
    Result.Free;
    Raise;
  end;
end;

function QComponent.GetFrameFromName(const nName: String) : QFrame;
begin
  Result:=FindSubObject(nName, QFrame, Nil) as QFrame;
end;

function QComponent.GetFrameFromIndex(N: Integer) : QFrame;
var
  L: TQList;
begin
  if N<0 then begin
    Result:=Nil;
    Exit;
  end;
  L:=TQList.Create; try
  FindAllSubObjects('', QFrame, Nil, L);
  if N>=L.Count then
    Result:=Nil
  else
    Result:=L[N] as QFrame;
  finally
    L.Free;
  end;
end;

function QComponent.GetSkinFromName(const nName: String) : QImages;
begin
  Result:=QImages(FindSubObject(nName, QImages, Nil));
end;

function QComponent.GetSkinFromIndex(N: Integer) : QImages;
var
  L: TQList;
begin
  if N<0 then begin
    Result:=Nil;
    Exit;
  end;
  L:=TQList.Create; try
  FindAllSubObjects('', QImages, Nil, L);
  if N>=L.Count then
    Result:=Nil
  else
    Result:=L[N] as QImages;
  finally
    L.Free;
  end;
end;

function QComponent.MergeVertices(Frames: TQList) : Boolean;
const
  Spec1 = 'Tris';
  Spec2 = 'Vertices';
type
  TVertexMap = array[0..99] of Integer;
var
  Bits: TBits;
  I, J, B: Integer;
  CVert, CVertJ, CVertK: vec3_p;
  VertexCount, nVertexCount, Target: Integer;
  VertexMap: ^TVertexMap;
  S: String;
  CTris: PComponentTris;
  FrSourcePts: vec3_p;
  FrameObj: QFrame;
begin
  Result:=False;
  VertexCount:=-1;
  for I:=0 to Frames.Count-1 do begin
    J:=(Frames[I] as QFrame).GetVertices(CVert);
    if VertexCount=-1 then
      VertexCount:=J
    else
      if VertexCount<>J then
        Raise EErrorFmt(2433, ['VertexCount']);
  end;
  if VertexCount<=0 then
    Exit;  { no frames or no vertices }
  ProgressIndicatorStart(503, Frames.Count+3);
  try
    Bits:=TBits.Create;
    try
      Bits.Size:=VertexCount*(VertexCount-1) div 2;
      for I:=0 to Frames.Count-1 do begin
        B:=0;
        QFrame(Frames[I]).GetVertices(CVert);
        CVertJ:=CVert;
        for J:=2 to VertexCount do begin
          CVertK:=CVert;
          Inc(CVertJ);
          repeat
            if not Bits[B] then
              if (Abs(CVertJ^[0] - CVertK^[0]) > rien) or (Abs(CVertJ^[1] - CVertK^[1]) > rien)
                or (Abs(CVertJ^[2] - CVertK^[2]) > rien) then
                Bits[B]:=True;
                Inc(B);
                Inc(CVertK);
          until CVertK=CVertJ;
        end;
        ProgressIndicatorIncrement;
      end;
      GetMem(VertexMap, SizeOf(Integer)*VertexCount);
      try
        B:=0;
        nVertexCount:=0;
        for I:=0 to VertexCount-1 do begin
          Target:=-1;
          for J:=0 to I-1 do begin
            if not Bits[B] then begin
              VertexMap^[I]:=VertexMap^[J];
              Inc(B, I-J);
              Target:=J;
              Break;
            end;
            Inc(B);
          end;
          if Target<0 then begin
            VertexMap^[I]:=nVertexCount;
            Inc(nVertexCount);
          end;
        end;
        if nVertexCount = VertexCount then
          Exit;  { no changes }
        Bits.Size:=0;
        ProgressIndicatorIncrement;
        S:=GetSpecArg(Spec1);
        UniqueString(S);
        Specifics.Values[Spec1]:='';
        Specifics.Add(S);
        for I:=1 to Triangles(CTris) do begin
          for J:=0 to 2 do begin
            if CTris^[J].VertexNo >= VertexCount then
              Raise EError(5667);
            CTris^[J].VertexNo:=VertexMap^[CTris^[J].VertexNo];
          end;
          Inc(CTris);
        end;
        for I:=0 to VertexCount-1 do
          VertexMap^[VertexMap^[I]]:=I;
        ProgressIndicatorIncrement;
        for I:=0 to Frames.Count-1 do begin
          FrameObj:=QFrame(Frames[I]);
          FrameObj.GetVertices(CVert);
          S:=FloatSpecNameOf(Spec2)+'=';
          SetLength(S, Length(Spec2+'=') + nVertexCount*SizeOf(vec3_t));
          PChar(FrSourcePts):=PChar(S) + Length(Spec2+'=');
          for J:=0 to nVertexCount-1 do begin
            CVertJ:=CVert;
            Inc(CVertJ, VertexMap^[J]);
            FrSourcePts^:=CVertJ^;
            Inc(FrSourcePts);
          end;
          FrameObj.Specifics.Values[FloatSpecNameOf(Spec2)]:='';
          FrameObj.Specifics.Add(S);
        end;
        ProgressIndicatorIncrement;
      finally
        FreeMem(VertexMap);
      end;
    finally
      Bits.Free;
    end;
  finally
    ProgressIndicatorStop;
  end;
  Result:=True;
end;

procedure QComponent.CouleurDessin;
var
  S: String;
begin
  S:=Specifics.Values['_color'];
  if S<>'' then begin
    C:=clNone;
    try
      C:=vtocol(ReadVector(S));
    except
      {rien}
    end;
  end;
end;


type
  PTriangleInfo = ^TTriangleInfo;
  TTriangleInfo = record
    Vertices: array[0..2] of PPointProj;
    SourceCTris: PComponentTris;
    OowMin: Single;
  end;

function ByOow(Item1, Item2: Pointer) : Integer;
begin
  if PTriangleInfo(Item1)^.OowMin < PTriangleInfo(Item2)^.OowMin then
    Result:=+1
  else
    if PTriangleInfo(Item1)^.OowMin > PTriangleInfo(Item2)^.OowMin then
      Result:=-1
    else
      Result:=0;
end;

Function QComponent.CreateBoneGroup: QBoneGroup;
begin
  Result:=QBoneGroup.Create('Skeleton', Self);
  Result.IntSpec['type']:=MDL_GROUP_BONE;
  SubElements.Add(Result);
end;

Function QComponent.CreateSkinGroup: QSkinGroup;
begin
  Result:=QSkinGroup.Create('Skins', Self);
  Result.IntSpec['type']:=MDL_GROUP_SKIN;
  SubElements.Add(Result);
end;

Function QComponent.CreateFrameGroup: QFrameGroup;
begin
  Result:=QFrameGroup.Create('Frames', Self);
  Result.IntSpec['type']:=MDL_GROUP_FRAME;
  SubElements.Add(Result);
end;

Function QComponent.BoneGroup: QBoneGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QBoneGroup then begin
      result:=QBoneGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateBoneGroup;
end;

Function QComponent.SkinGroup: QSkinGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QSkinGroup then begin
      result:=QSkinGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateSkinGroup;
end;

Function QComponent.FrameGroup: QFrameGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QFrameGroup then begin
      result:=QFrameGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateFrameGroup;
end;

procedure QComponent.Dessiner;
type
  TProjArray = array[0..99] of TPointProj;
var
  I, J, K, TrisCount, FillTrisCount: Integer;
  L: TList;
  CTris: PComponentTris;
  ProjPts: ^TProjArray;
  SourceTris, Tris: PTriangleInfo;
  v3p: array[0..2] of vec3_p;
  Pts: array[0..2] of TPointProj;
  NewPen, DeletePen, OldPen: HPen;
  Hollow, Back: Boolean;
  CurPenMode, NewPenMode, ScrAnd, ScrAnd0: Integer;
  C1, C2: TColor;
  V1, V2, Normale: TVect;
  obj: PyObject;
  patterns: array[Boolean] of PyObject;
  CDC: TCDC;
  FCurrentFrame: vec3_p;
  FCurrentFrameCount: Integer;
  S: String;
  test, total: Single;
  Mode3D: Boolean;
begin
  if CurrentFrame=Nil then begin
    CurrentFrame:=GetFrameFromIndex(0);
    if CurrentFrame=Nil then
      Exit;
  end;
  FCurrentFrameCount:=FCurrentFrameObj.GetVertices(FCurrentFrame);
  GetMem(ProjPts, FCurrentFrameCount * SizeOf(TPointProj));
  try
    v3p[0]:=FCurrentFrame;
    for I:=0 to FCurrentFrameCount-1 do begin
      V1.X:=v3p[0]^[0];
      V1.Y:=v3p[0]^[1];
      V1.Z:=v3p[0]^[2];
      ProjPts^[I]:=CCoord.Proj(V1);
      CCoord.CheckVisible(ProjPts^[I]);
      Inc(v3p[0]);
    end;
    Mode3D:=not CCoord.FlatDisplay;
    TrisCount:=Triangles(CTris);
    GetMem(SourceTris, TrisCount * SizeOf(TTriangleInfo));
    try
      Tris:=SourceTris;
      for I:=1 to TrisCount do begin
        with Tris^ do begin
          OowMin:=-MaxInt;
          total:=0;
          SourceCTris:=CTris;
          for K:=0 to 2 do begin
            J:=CTris^[K].VertexNo;
            if J > FCurrentFrameCount then begin    { ignore the invalid triangle }
              Dec(TrisCount);
              Dec(Tris);
              Break;
            end;
            Vertices[K]:=@ProjPts^[J];
            test:=Vertices[K]^.oow;
            if Mode3D then
              test:=-test;
            total:=total+test;
            if test > OowMin then
              OowMin:=test;
          end;
          OowMin:=OowMin + total*0.01;
        end;
        Inc(Tris);
        Inc(CTris);
      end;
      L:=TList.Create;
      try
        L.Capacity:=TrisCount;
        Tris:=SourceTris;
        for I:=1 to TrisCount do begin
          L.Add(Tris);
          Inc(Tris);
        end;
        L.Sort(ByOow);
        NewPen:=0;
        DeletePen:=0;
        if Info.GreyBrush <> 0 then begin    { if color changes must be made now }
          if not Odd(SelMult) then begin
            C1:=clDefault;
            CouleurDessin(C1);
            if C1<>clDefault then
              if C1=clNone then
                NewPen:=GetStockObject(Null_pen)
              else begin
                DeletePen:=CreatePen(ps_Solid, 0, C1);
                NewPen:=DeletePen;
              end;
          end;
        end;
        if NewPen<>0 then begin
          OldPen:=Info.BlackBrush;
          Info.BlackBrush:=NewPen;
        end else
          OldPen:=0;
        SetupComponentDC(CDC);
        if Info.SelectedBrush<>0 then begin
          SelectObject(Info.DC, Info.SelectedBrush);
          SetROP2(Info.DC, R2_CopyPen);
          CurPenMode:=0;
          ScrAnd0:=0;
        end else begin
          CurPenMode:=-1;
          if Info.ModeAff=0 then
            ScrAnd0:=0
          else
            ScrAnd0:=os_Back or os_Far;
        end;
        try
          if FSelTris=Nil then
            FillTrisCount:=0
          else
            FillTrisCount:=PyObject_Length(FSelTris);
          Back:=False;
          Hollow:=True;
          for I:=0 to TrisCount-1 do begin
            Tris:=PTriangleInfo(L[I]);
            if I<FillTrisCount then begin
              obj:=PyList_GetItem(FSelTris, I);
              if obj=Nil then
                Exit;
              if obj^.ob_type=PyTuple_Type then begin
                if not PyArg_ParseTupleX(obj, 'OO;filltris format error', [@patterns[False], @patterns[True]]) then
                  Exit;
                with Tris^ do
                  for K:=0 to 2 do begin
                    v3p[K]:=FCurrentFrame;
                    Inc(v3p[K], SourceCTris^[K].VertexNo);
                  end;
                V1.X:=v3p[1]^[0] - v3p[0]^[0];
                V1.Y:=v3p[1]^[1] - v3p[0]^[1];
                V1.Z:=v3p[1]^[2] - v3p[0]^[2];
                V2.X:=v3p[2]^[0] - v3p[0]^[0];
                V2.Y:=v3p[2]^[1] - v3p[0]^[1];
                V2.Z:=v3p[2]^[2] - v3p[0]^[2];
                Normale:=Cross(V1, V2);
                Back:=CCoord.PositiveHalf(Normale.X, Normale.Y, Normale.Z,
                Normale.X * v3p[0]^[0] + Normale.Y * v3p[0]^[1] + Normale.Z * v3p[0]^[2]);
                obj:=patterns[Back];
                if obj <> Py_None then begin
                  Hollow:=False;
                  if obj^.ob_type <> PyTuple_Type then begin
                    C1:=PyInt_AsLong(obj);
                    C2:=C1;
                  end else
                    if not PyArg_ParseTupleX(obj, 'ii;filltris format error', [@C1, @C2]) then
                      Exit;
                  SetTextColor(Info.DC, C1);
                  SetBkColor(Info.DC, C2);
                end;
              end;
            end;
          ScrAnd:=ScrAnd0;
          with Tris^ do
            for K:=0 to 2 do begin
              Pts[K]:=Vertices[K]^;
              ScrAnd:=ScrAnd and Pts[K].OffScreen;
            end;
            if ScrAnd<>0 then begin
              NewPenMode:=1;
              if (Info.ModeAff=2) or (ScrAnd and CCoord.HiddenRegions <> 0) then
                Continue;
            end else
              NewPenMode:=0;
            if NewPenMode<>CurPenMode then begin
              if NewPenMode=0 then begin
                SelectObject(Info.DC, Info.BlackBrush);
                SetROP2(Info.DC, R2_CopyPen);
              end else begin
                SelectObject(Info.DC, Info.GreyBrush);
                SetROP2(Info.DC, Info.MaskR2);
              end;
              CurPenMode:=NewPenMode;
            end;
            if Hollow then
              CCoord.Polyline95f(Pts, 3)
            else begin
              CCoord.Polygon95f(Pts, 3, not Back);
              Hollow:=True;
            end;
          end;  { note: "Continue" used in the loop }
        finally
          CloseComponentDC(CDC);
          if OldPen<>0 then begin
            SelectObject(Info.DC, OldPen);
            Info.BlackBrush:=OldPen;
            if DeletePen<>0 then
              DeleteObject(DeletePen);
          end;
        end;
      finally
        L.Free;
      end;
    finally
      FreeMem(SourceTris);
    end;
  finally
    FreeMem(ProjPts);
  end;
end;

procedure QComponent.AnalyseClic(Liste: PyObject);
type
  vec3_array_t = array[0..99] of record v3: vec3_t end;
var
  I, Count, L, PrevL: Integer;
  CTris: PComponentTris;
  CVertArray: ^vec3_array_t;
  V: array[0..2] of TVect;
  W1, W2: TVect;
  Normale: TVect;
  obj: PyObject;
  f, d0,dv,d1: TDouble;
begin
  if CurrentFrame=Nil then
    Exit;
  Count:=CurrentFrame.GetVertices(vec3_p(CVertArray));
  W2.X:=Info.Clic2.X - Info.Clic.X;
  W2.Y:=Info.Clic2.Y - Info.Clic.Y;
  W2.Z:=Info.Clic2.Z - Info.Clic.Z;
  for I:=0 to Triangles(CTris)-1 do begin
    if (CTris^[0].VertexNo < Count) and (CTris^[0].VertexNo < Count) and (CTris^[0].VertexNo < Count) then begin
      for L:=0 to 2 do
        with V[L], CVertArray^[CTris^[L].VertexNo] do begin
          X:=v3[0];
          Y:=v3[1];
          Z:=v3[2];
        end;
       PrevL:=2;
       L:=0;
      repeat
        W1.X:=V[L].X-V[PrevL].X;
        W1.Y:=V[L].Y-V[PrevL].Y;
        W1.Z:=V[L].Z-V[PrevL].Z;
        Normale:=Cross(W1, W2);
        if Dot(V[L], Normale) <= Dot(Info.Clic, Normale) then
          Break;
        PrevL:=L;
        Inc(L);
      until L=3;
      if L=3 then begin
        d0:=Dot(V[0], Normale);
        d1:=Dot(V[1], Normale);
        if Abs(d1-d0)>rien then begin
          dv:=Dot(Info.Clic, Normale);
          f:=(d1-dv) / (d1-d0);
          W1:=W2;
          Normalise(W1);
          f:=Dot(V[1],W1) * (1-f) + Dot(V[0],W1) * f - Dot(Info.Clic,W1);
          W1.X:=Info.Clic.X + W1.X*f;
          W1.Y:=Info.Clic.Y + W1.Y*f;
          W1.Z:=Info.Clic.Z + W1.Z*f;
          obj:=PyInt_FromLong(I);
          try
            ResultatAnalyseClic(Liste, CCoord.Proj(W1), obj);
          finally
            Py_DECREF(obj);
          end;
        end;
      end;
    end;
    Inc(CTris);
  end;
end;

function QComponent.PyGetAttr(attr: PChar) : PyObject;
var
  I, L, Count: Integer;
  CTris: PComponentTris;
  tri: PyObject;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  for I:=Low(MethodTable) to High(MethodTable) do
    if StrComp(attr, MethodTable[I].ml_name) = 0 then begin
      Result:=PyCFunction_New(MethodTable[I], @PythonObj);
      Exit;
    end;
  case attr[0] of
    'c': if StrComp(attr, 'currentframe')=0 then begin
      Result:=GetPyObj(CurrentFrame);
      Exit;
    end else if StrComp(attr, 'currentskin')=0 then begin
      Result:=GetPyObj(CurrentSkin);
      Exit;
    end;
    'f': if StrComp(attr, 'filltris')=0 then begin
      if FSelTris=Nil then
        FSelTris:=PyList_New(0);
      Result:=FSelTris;
      Py_INCREF(Result);
      Exit;
    end;
    'g': if StrComp(attr, 'group_frame')=0 then begin
      Result:=GetPyObj(FrameGroup);
      Exit;
    end else if StrComp(attr, 'group_skin')=0 then begin
      Result:=GetPyObj(SkinGroup);
      Exit;
    end else if StrComp(attr, 'group_bone')=0 then begin
      Result:=GetPyObj(BoneGroup);
      Exit;
    end;
    'i': if StrComp(attr, 'info')=0 then begin
      if FInfo=Nil then
        Result:=Py_None
      else
        Result:=FInfo;
      Py_INCREF(Result);
      Exit;
    end;
    't': if StrComp(attr, 'triangles')=0 then begin
      Count:=Triangles(CTris);
      Result:=PyList_New(Count);
      for I:=0 to Count-1 do begin
        tri:=PyTuple_New(3);
        for L:=0 to 2 do
          with CTris^[L] do
            PyTuple_SetItem(tri, L, Py_BuildValueX('iii', [VertexNo, S, T]));
          PyList_SetItem(Result, I, tri);
          Inc(CTris);
      end;
      Exit;
    end;
  end;
end;

function QComponent.PySetAttr(attr: PChar; value: PyObject) : Boolean;
const
  Spec1 = 'Tris';
  BaseSize = Length('Tris=');
var
  Q: QObject;
  S: String;
  Count, I, L: Integer;
  Dest: PComponentTris;
  tri: PyObject;
  pt: array[0..2] of PyObject;
  VN, SS, TT: Integer;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then
    case attr[0] of
    'c': if StrComp(attr, 'currentframe')=0 then begin
      Q:=QkObjFromPyObj(value);
      if not (Q is QFrame) then
        Q:=Nil;
      CurrentFrame:=QFrame(Q);
      Result:=True;
      Exit;
    end else if StrComp(attr, 'currentskin') = 0 then begin
      Q:=QkObjFromPyObj(value);
      if not (Q is QImages) then
        Q:=Nil;
      CurrentSkin:=QImages(Q);
      Result:=True;
      Exit;
    end else if StrComp(attr, 'currentframeindex') = 0 then begin
      I:=0;
      PyArg_ParseTupleX(value,'i',[@i]);
      CurrentFrame:=GetFrameFromIndex(I);
      Result:=True;
      Exit;
    end;
    'f': if StrComp(attr, 'filltris')=0 then begin
      Py_XDECREF(FSelTris);
      if value^.ob_type = PyList_Type then begin
        FSelTris:=value;
        Py_INCREF(value);
      end else
        FSelTris:=Nil;
      Result:=True;
      Exit;
    end;
    'i': if StrComp(attr, 'info')=0 then begin
      Py_XDECREF(FInfo);
      FInfo:=value;
      Py_INCREF(value);
      Result:=True;
      Exit;
    end;
    't': if StrComp(attr, 'triangles')=0 then begin
      Count:=PyObject_Length(value);
      if Count<0 then
        Exit;
      S:=Spec1+'=';
      SetLength(S, BaseSize+SizeOf(TComponentTris)*Count);
      PChar(Dest):=PChar(S)+BaseSize;
      for I:=0 to Count-1 do begin
        tri:=PyList_GetItem(value, I);
        if tri=Nil then
          Exit;
        if not PyArg_ParseTupleX(tri, 'OOO;a triangle needs three points', [@pt[0], @pt[1], @pt[2]]) then
          Exit;
        for L:=0 to 2 do begin
          if not PyArg_ParseTupleX(pt[L], 'iii;bad tripoint format', [@VN, @SS, @TT]) then
            Exit;
          with Dest^[L] do begin
            VertexNo:=VN;
            S:=SS;
            T:=TT;
          end;
        end;
        Inc(Dest);
      end;
      Specifics.Delete(Specifics.IndexofName(Spec1));
      Specifics.Add(S);
      Result:=True;
      Exit;
    end;
  end;
end;

initialization
  RegisterQObject(QComponent, 'a');
end.

{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.6  2001/01/15 19:23:18  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.5  2001/01/08 23:04:59  aiv
Possible fix again...?

Revision 1.4  2001/01/07 18:44:59  aiv
Fixed game code prob.

Revision 1.3  2000/10/11 19:01:08  aiv
Small updates

}

unit QkMdl;

interface

uses
  SysUtils, Classes, QkObjects, QkFileObjects, QkImages, Python, Game, QkModelFile, QMath,
  Graphics, Windows, QkModelRoot, QkMdlObject, QkFrame, QkComponent;

type
  QMdlFile = class(QModelFile)
  protected
    procedure LoadFile(F: TStream; FSize: Integer); override;
    procedure SaveFile(Info: TInfoEnreg1); override;
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;

(***********  Quake 1 and Hexen II .mdl format  ***********)

const
  SignatureMdl   = $4F504449;  { 'IDPO' }
  VersionMdl     = 6;
  SignatureMdlRa = $4F504152;  { 'RAPO'  for PoP }
  VersionMdlRa   = $32;

  SignatureHLMdl = ((Ord('T') shl 24)or(ord('S') shl 16)or(ord('D') shl 8)or ord('I'));
  SignatureHLMdlS= ((Ord('Q') shl 24)or(ord('S') shl 16)or(ord('D') shl 8)or ord('I'));
  VersionHLMdl   = 10;

  VecteursNormaux : array[0..161, 0..2] of Single =
    ({$I anorms.inc});

type
  mdl_t = record
    id, version: LongInt;
    scale, origin: vec3_t;
    radius: scalar_t;
    offsets: vec3_t;
    numskins, skinwidth, skinheight: LongInt;
    numverts, numtris, numframes: LongInt;
    synctype, flags: LongInt;
    size: scalar_t;
  end;
  mdl_ra_t = record
    numstverts: LongInt;
  end;
  stvert_t = record
    onseam, ss, tt: LongInt;
  end;
  trivertx_t = record
    X,Y,Z,N: Byte;
  end;
  itriangle_t = record
    facesfront: LongInt;
    index_xyz: array[0..2] of LongInt;
  end;
  itriangle_ra_t = record
    facesfront: LongInt;
    index_xyz, index_st: array[0..2] of Word;
  end;
  skingroup_t = record
    count: Integer;
  end;
  framegroup_t = record
    count: Integer;
    min, max: trivertx_t;
  end;
  frame_t = record
    min, max: trivertx_t;
    Nom: array[0..15] of Byte;
  end;

implementation

uses QuarkX, Setup, Travail, QkObjectClassList;

class function QMdlFile.TypeInfo;
begin
  Result:='.mdl';
end;

class procedure QMdlFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5144);
  Info.FileExt:=786;
end;

procedure QMdlFile.LoadFile(F: TStream; FSize: Integer);
const
  Spec1 = 'Tris=';
  Spec2 = 'Vertices=';
type
  PVertxArray = ^TVertxArray;
  TVertxArray = array[0..99] of stvert_t;
var
  mdl: mdl_t;
  Root: QModelRoot;
  Size: array[1..2] of Single;
  I, J, K, Taille1, Delta, SkinCounter, DeltaW: Integer;
  SkinGroup: skingroup_t;
  P: PChar;
  S: String;
  SkinObj: QImages;
  STData: PVertxArray;
  Triangles, Tris: ^itriangle_t;
  CTris: PComponentTris;
  CVert: vec3_p;
  Derriere: Boolean;
  Frame: frame_t;
  FrameGroup: framegroup_t;
  FrameObj: QFrame;
  FrSourcePts, FrSource: ^trivertx_t;
  Times: String;
  PreviousTime: Single;
  NextTime: ^Single;
  mdl_ra: mdl_ra_t;
  RA: Boolean;
  TrisRA: ^itriangle_ra_t absolute Tris;
  //----
  C: QComponent;

  procedure Read1(var Buf; Count: Integer);
  begin
    if Count>FSize then
      Raise EErrorFmt(5186, [LoadName]);
    Dec(FSize, Count);
    F.ReadBuffer(Buf, Count);
  end;

begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      if FSize<SizeOf(mdl) then
        Raise EError(5519);
      F.ReadBuffer(mdl, SizeOf(mdl));
      Dec(FSize, SizeOf(mdl));
      RA:=(mdl.id=SignatureMdlRa) and (mdl.version=VersionMdlRa);
      if RA then
        Read1(mdl_ra, SizeOf(mdl_ra))
      else begin
        if (mdl.id<>SignatureMdl) or (mdl.version<>VersionMdl) then
          if ((mdl.id=SignatureHLMdl) or (mdl.id=SignatureHLMdlS))and(mdl.version=VersionHLMdl) then
            Raise EErrorFmt(5503, [LoadName]) {FIXME: Load HL Model Here}
          else
            Raise EErrorFmt(5593, [LoadName, mdl.id, mdl.version, SignatureMdl, VersionMdl]);
        mdl_ra.numstverts:=mdl.numverts;
      end;

        { setup Root }
      Root:=Loaded_Root;
      C:=Loaded_Component(Root, '');
{      if RA then
        ObjectGameCode:=mjHexen
      else
        ObjectGameCode:=mjQuake;}
      Root.Specifics.Values['seamtrick']:='1';
      Size[1]:=mdl.synctype;
      Size[2]:=mdl.flags;
      Root.SetFloatsSpec('flags', Size);

        { load skins }
      Size[1]:=mdl.skinwidth;
      Size[2]:=mdl.skinheight;
      {Taille1:=mdl.skinwidth * mdl.skinheight;}
      SkinCounter:=0;
      for I:=1 to mdl.numskins do begin
        Read1(J, SizeOf(LongInt));
        if J=0 then begin
          SkinGroup.count:=1;
          NextTime:=Nil;
        end else begin
          Read1(SkinGroup, SizeOf(SkinGroup));
          SetLength(Times, SkinGroup.count*SizeOf(Single));
          PChar(NextTime):=PChar(Times);
          Read1(NextTime^, SkinGroup.count*SizeOf(Single));
        end;
        PreviousTime:=0;
        for K:=1 to SkinGroup.count do begin
          J:=F.Position;
          SkinObj:=Loaded_Skin(C, FmtLoadStr1(2372, [SkinCounter]), Size, P, DeltaW);
          F.Position:=J;
          Inc(SkinCounter);
          if NextTime<>Nil then begin
            if K=1 then
              SkinObj.Specifics.Values['group']:='1';
            SkinObj.SetFloatSpec('duration', NextTime^-PreviousTime);
            PreviousTime:=NextTime^;
            Inc(NextTime);
          end;
          for J:=1 to mdl.skinheight do begin
            Read1(P^, mdl.skinwidth);
            Inc(P, DeltaW);
          end;
        end;
      end;
       { load Skin Vertices and Triangles }
      Taille1:=SizeOf(stvert_t)*mdl_ra.numstverts;
      GetMem(STData, Taille1);
      try
        Read1(STData^, Taille1);

        Taille1:=SizeOf(itriangle_t)*mdl.numtris;
        GetMem(Triangles, Taille1);
        try
          Read1(Triangles^, Taille1);

          J:=mdl.numtris*SizeOf(TComponentTris);
          S:=Spec1;
          SetLength(S, Length(Spec1)+J);

          Delta:=mdl.skinwidth div 2;
          Tris:=Triangles;
          PChar(CTris):=PChar(S)+Length(Spec1);
          if RA then begin
            for I:=1 to mdl.numtris do begin  { PoP Models }
              Derriere:=TrisRA^.facesfront=0;
              for J:=0 to 2 do begin
                with CTris^[J] do begin
                  VertexNo:=TrisRA^.index_xyz[J];
                  with STData^[TrisRA^.index_st[J]] do begin
                    S:=ss;
                    T:=tt;
                    if Derriere and (onseam and $20 <> 0) then
                      Inc(S, Delta);
                  end;
                end;
              end;
              Inc(TrisRA);
              Inc(CTris);
            end;
          end else  { default Q1 and H2 Models }
            for I:=1 to mdl.numtris do begin
              Derriere:=Tris^.facesfront=0;
              for J:=0 to 2 do
                with CTris^[J] do begin
                  VertexNo:=Tris^.index_xyz[J];
                  with STData^[Tris^.index_xyz[J]] do begin
                    S:=ss;
                    T:=tt;
                    if Derriere and (onseam and $20 <> 0) then
                      Inc(S, Delta);
                  end;
                end;
              Inc(Tris);
              Inc(CTris);
            end;
          C.Specifics.Add(S);   { Tris= }
        finally
          FreeMem(Triangles);
        end;
      finally
        FreeMem(STData);
      end;
      { load frames }
      Taille1:=SizeOf(trivertx_t)*mdl.numverts;
      GetMem(FrSourcePts, Taille1);
      try
        for I:=1 to mdl.numframes do begin
          Read1(J, SizeOf(LongInt));
          if J=0 then begin
            FrameGroup.count:=1;
            NextTime:=Nil;
          end else begin
            Read1(FrameGroup, SizeOf(FrameGroup));
            SetLength(Times, FrameGroup.count*SizeOf(Single));
            PChar(NextTime):=PChar(Times);
            Read1(NextTime^, FrameGroup.count*SizeOf(Single));
          end;
          PreviousTime:=0;
          for K:=1 to FrameGroup.count do begin
            Read1(Frame, SizeOf(Frame));
            Read1(FrSourcePts^, Taille1);
            FrameObj:=Loaded_Frame(C, CharToPas(Frame.Nom));
            if NextTime<>Nil then begin
              if K=1 then
                FrameObj.Specifics.Values['group']:='1';
              FrameObj.SetFloatSpec('duration', NextTime^-PreviousTime);
              PreviousTime:=NextTime^;
              Inc(NextTime);
            end;
            S:=FloatSpecNameOf(Spec2);
            SetLength(S, Length(Spec2)+mdl.numverts*SizeOf(vec3_t));
            PChar(CVert):=PChar(S)+Length(Spec2);
            FrSource:=FrSourcePts;
            for J:=0 to mdl.numverts-1 do begin
              with FrSource^ do begin
                CVert^[0]:=mdl.scale[0] * X + mdl.origin[0];
                CVert^[1]:=mdl.scale[1] * Y + mdl.origin[1];
                CVert^[2]:=mdl.scale[2] * Z + mdl.origin[2];
              end;
              Inc(FrSource);
              Inc(CVert);
            end;
            FrameObj.Specifics.Add(S);
          end;
        end;
      finally
        FreeMem(FrSourcePts);
      end;
    end;
    else begin
      inherited;
    end;
  end;
end;

procedure QMdlFile.SaveFile(Info: TInfoEnreg1);
type
  PVertxArray = ^TVertxArray;
  TVertxArray = array[0..99] of stvert_t;

  PVertexNode = ^TVertexNode;
  TVertexNode = record
    Next: PVertexNode;
    OutputIndex: Integer;
    case Integer of
      0: (S, T: SmallInt);
      1: (st: dstvert_t);
      2: (longst: LongInt);
    end;
  TVertexMap = array[0..99] of PVertexNode;
  trivertx_array_t = array[0..99] of trivertx_t;
  vec3_array_t = array[0..99] of vec3_t;
  TVect_array = array[0..99] of TVect;
var
  Root: QModelRoot;
  TheComp: QComponent;
  FrameList, SkinList: TQList;
  mdl: mdl_t;
  Size: array[1..2] of Single;
  Position0, I, J, K, K1, Taille1, Delta, InputVertexCount: Integer;
  SkinGroup: skingroup_t;
  FrameGroup: framegroup_t;
  P: PChar;
  S: String;
  SkinObj: QImages;
  SkinSize: TPoint;
  STData: PVertxArray;
  Triangles, Tris: ^itriangle_t;
  CTriangles, CTris: PComponentTris;
  CVertArray: ^vec3_array_t;
  CVert: vec3_p;
  Frame: frame_t;
  FrameObj: QFrame;
  FrSourcePts: ^trivertx_array_t;
  Times: String;
  PreviousTime, Time1: Single;
  NextTime: ^Single;
  VertexMap: ^TVertexMap;
  Node: PVertexNode;
  Min, Max, EchelleCompacter, Centre, Vec1, Vec2, Vec3: TVect;
  Vert1, Vert2, Vert3: vec3_t;
  tvx: trivertx_t;
  NormalesSommets: ^TVect_array;
  Aire, AireTotale, Maximum: TDouble;

  function Compacter(const T: vec3_t) : trivertx_t;
  begin
    Result.X:=Round((T[0] - mdl.origin[0]) * EchelleCompacter.x);
    Result.Y:=Round((T[1] - mdl.origin[1]) * EchelleCompacter.y);
    Result.Z:=Round((T[2] - mdl.origin[2]) * EchelleCompacter.z);
    Result.N:=0;
  end;

begin
  with Info do begin
    case Format of
      1: begin
        Root:=Self.GetRoot;
        if Root.CurrentComponent=nil then
          Root.CurrentComponent:=Root.GetComponentFromIndex(0);
        TheComp:=Root.CurrentComponent;
        if TheComp=nil then
          Raise Exception.Create('Nothing to save! (Root.CurrentComponent = nil [QMDLFILE.ENREGISTRER])');

        FrameList:=TheComp.BuildFrameList;
        SkinList:=TheComp.BuildSkinList;

        ProgressIndicatorStart(502, FrameList.Count+SkinList.Count);
        try
          Position0:=F.Position;
          FillChar(mdl, SizeOf(mdl), 0);
          F.WriteBuffer(mdl, SizeOf(mdl));

          mdl.id:=SignatureMdl;
          mdl.version:=VersionMdl;
          if Root.GetFloatsSpec('flags', Size) then begin
            mdl.synctype:=Round(Size[1]);
            mdl.flags:=Round(Size[2]);
          end;
          for I:=0 to SkinList.Count-1 do begin
            if SkinList.Items1[I] is QImage then begin
              SkinObj:=QImage(SkinList.Items1[I]);
              SkinSize:=SkinObj.GetSize;
              if mdl.skinwidth=0 then begin
                mdl.skinwidth:=SkinSize.X;
                mdl.skinheight:=SkinSize.Y;
              end else
                if (mdl.skinwidth<>SkinSize.X) or (mdl.skinheight<>SkinSize.Y) then
                  Raise EErrorFmt(2433, ['SkinSize']);
            end;
          end;
          Delta:=(mdl.skinwidth+3) and not 3;
          I:=0;
          while I<SkinList.Count do begin
            SkinGroup.count:=1;
            if QImage(SkinList.Items1[I]).GetFloatSpec('duration', 0)<=0 then begin  { not in a skin group }
              J:=0;
              F.WriteBuffer(J, SizeOf(LongInt));
            end else begin
              while (I+SkinGroup.count < SkinList.Count)
               and (QImage(SkinList.Items1[I+SkinGroup.count]).GetFloatSpec('duration', 0) > 0)
                and (QImage(SkinList.Items1[I+SkinGroup.count]).Specifics.Values['group']='') do
                Inc(SkinGroup.count);
              J:=1;
              F.WriteBuffer(J, SizeOf(LongInt));
              F.WriteBuffer(SkinGroup, SizeOf(SkinGroup));
              SetLength(Times, SkinGroup.count*SizeOf(Single));
              PChar(NextTime):=PChar(Times);
              PreviousTime:=0;
              for J:=0 to SkinGroup.count-1 do begin
                PreviousTime:=PreviousTime + QImage(SkinList.Items1[I+J]).GetFloatSpec('duration', 0);
                NextTime^:=PreviousTime;
                Inc(NextTime);
              end;
              F.WriteBuffer(Times[1], SkinGroup.count*SizeOf(Single));
            end;
          for J:=0 to SkinGroup.count-1 do begin
            SkinObj:=QImage(SkinList.Items1[I]);
            SkinObj.NotTrueColor;
            P:=SkinObj.GetImagePtr1;
            Inc(P, Delta*mdl.skinheight);   { FIXME: check palette }
            for K:=1 to mdl.skinheight do begin
              Dec(P, Delta);
              F.WriteBuffer(P^, mdl.skinwidth);
            end;
            Inc(I);
            ProgressIndicatorIncrement;
          end;
          Inc(mdl.numskins);
        end;
               { parse the frames and compute model size }

        Min.X:=MaxInt;
        Min.Y:=MaxInt;
        Min.Z:=MaxInt;
        Max.X:=-MaxInt;
        Max.Y:=-MaxInt;
        Max.Z:=-MaxInt;
        InputVertexCount:=0;
        for I:=0 to FrameList.Count-1 do begin
          if FrameList.Items1[I] is QFrame then begin
            FrameObj:=QFrame(FrameList.Items1[I]);
            J:=FrameObj.GetVertices(CVert);
            if J>0 then begin
              if InputVertexCount=0 then
                InputVertexCount:=J
              else
                if InputVertexCount<>J then
                  Raise EErrorFmt(2433, ['VertexCount']);
              FrameObj.ChercheExtremites(Min, Max);
            end;
          end;
        end;
        mdl.origin[0]:=Min.X;
        mdl.origin[1]:=Min.Y;
        mdl.origin[2]:=Min.Z;
        mdl.scale[0]:=(Max.x-Min.x) * (1/255);
        mdl.scale[1]:=(Max.y-Min.y) * (1/255);
        mdl.scale[2]:=(Max.z-Min.z) * (1/255);
        if mdl.scale[0] < rien then EchelleCompacter.x:=0 else EchelleCompacter.x:=1/mdl.scale[0];
        if mdl.scale[1] < rien then EchelleCompacter.y:=0 else EchelleCompacter.y:=1/mdl.scale[1];
        if mdl.scale[2] < rien then EchelleCompacter.z:=0 else EchelleCompacter.z:=1/mdl.scale[2];
        Centre.x:=(Min.x+Max.x) * (1/2);
        Centre.y:=(Min.y+Max.y) * (1/2);
        Centre.z:=(Min.z+Max.z) * (1/2);
        mdl.offsets[0]:=Centre.x - Min.x;   { à défaut d'autre chose }
        mdl.offsets[1]:=Centre.y - Min.y;
        mdl.offsets[2]:=Centre.z - Min.z;
        if Abs(Min.x)>Abs(Max.x) then Vec1.x:=Min.x else Vec1.x:=Max.x;
        if Abs(Min.y)>Abs(Max.y) then Vec1.y:=Min.y else Vec1.y:=Max.y;
        if Abs(Min.z)>Abs(Max.z) then Vec1.z:=Min.z else Vec1.z:=Max.z;
        mdl.radius:=Sqrt(Sqr(Vec1.x)+Sqr(Vec1.y)+Sqr(Vec1.z));

       { save Skin Vertices and Triangles }
       { note: There is a trick with Quake 1 models. We never save "on-seam" vertices.
               Instead, we save several copies of some 3D vertices. This is done so because
               it would be very hard and often impossible to convert the generic QuArK
               models (inspired by Quake 2's) to the more restricted Quake 1 format. }

        Taille1:=InputVertexCount*SizeOf(PVertexNode);
        GetMem(VertexMap, Taille1);
        FillChar(VertexMap^, Taille1, 0);
        try
          mdl.numtris:=TheComp.Triangles(CTriangles);
          CTris:=CTriangles;
          for I:=1 to mdl.numtris do begin
            for K:=0 to 2 do begin
              with CTris^[K] do begin
                if VertexNo > InputVertexCount then
                  Raise EErrorFmt(2433, ['VertexNo']);
                Node:=VertexMap^[VertexNo];
                while (Node<>Nil) and (Node^.longst<>longst) do
                  Node:=Node^.Next;
                if Node=Nil then begin
                  New(Node);
                  Node^.OutputIndex:=mdl.numverts;
                  Inc(mdl.numverts);
                  Node^.st:=st;
                  Node^.Next:=VertexMap^[VertexNo];
                  VertexMap^[VertexNo]:=Node;
                end;
              end;
            end;
            Inc(CTris);
          end;
          Taille1:=SizeOf(stvert_t)*mdl.numverts;
          GetMem(STData, Taille1);
          try
            for I:=0 to InputVertexCount-1 do begin
              Node:=VertexMap^[I];
              while Node<>Nil do begin
                with STData^[Node^.OutputIndex], Node^ do begin
                  onseam:=0;
                  ss:=S;
                  tt:=T;
                end;
              Node:=Node^.Next;
              end;
            end;
            F.WriteBuffer(STData^, Taille1);
          finally
            FreeMem(STData);
          end;

          Taille1:=SizeOf(itriangle_t)*mdl.numtris;
          GetMem(Triangles, Taille1);
          try
            Tris:=Triangles;
            CTris:=CTriangles;
            for I:=1 to mdl.numtris do begin
              Tris^.facesfront:=1;
              for K:=0 to 2 do begin
                with CTris^[K] do begin
                  Node:=VertexMap^[VertexNo];
                  while Node^.longst<>longst do
                    Node:=Node^.Next;
                end;
                Tris^.index_xyz[K]:=Node^.OutputIndex;
              end;
              Inc(CTris);
              Inc(Tris);
            end;
            F.WriteBuffer(Triangles^, Taille1);
          finally
            FreeMem(Triangles);
          end;
          { save Frames }
          AireTotale:=0;
          Taille1:=SizeOf(trivertx_t)*mdl.numverts;
          GetMem(FrSourcePts, Taille1);
          GetMem(NormalesSommets, SizeOf(TVect)*InputVertexCount);
          try
          I:=0;
          while I<FrameList.Count do begin
            FrameGroup.count:=1;
            FrameObj:=QFrame(FrameList.Items1[I]);
            if FrameObj.GetFloatSpec('duration', 0)<=0 then begin  { not in a frame group }
              J:=0;
              F.WriteBuffer(J, SizeOf(LongInt));
            end else begin
              Min.X:=MaxInt;
              Min.Y:=MaxInt;
              Min.Z:=MaxInt;
              Max.X:=-MaxInt;
              Max.Y:=-MaxInt;
              Max.Z:=-MaxInt;
              FrameObj.ChercheExtremites(Min, Max);
              while (I+FrameGroup.count < FrameList.Count)
               and (QFrame(FrameList.Items1[I+FrameGroup.count]).GetFloatSpec('duration', 0) > 0)
                and (QFrame(FrameList.Items1[I+FrameGroup.count]).Specifics.Values['group']='') do begin
                QFrame(FrameList.Items1[I+FrameGroup.count]).ChercheExtremites(Min, Max);
                Inc(FrameGroup.count);
              end;
              J:=1;
              F.WriteBuffer(J, SizeOf(LongInt));
              Vert1[0]:=Min.X;
              Vert1[1]:=Min.Y;
              Vert1[2]:=Min.Z;
              FrameGroup.min:=Compacter(Vert1);
              Vert1[0]:=Max.X;
              Vert1[1]:=Max.Y;
              Vert1[2]:=Max.Z;
              FrameGroup.max:=Compacter(Vert1);
              F.WriteBuffer(FrameGroup, SizeOf(FrameGroup));
              SetLength(Times, FrameGroup.count*SizeOf(Single));
              PChar(NextTime):=PChar(Times);
              PreviousTime:=0;
              for J:=0 to FrameGroup.count-1 do begin
                PreviousTime:=PreviousTime + QFrame(FrameList.Items1[I+J]).GetFloatSpec('duration', 0);
                NextTime^:=PreviousTime;
                Inc(NextTime);
              end;
              F.WriteBuffer(Times[1], FrameGroup.count*SizeOf(Single));
            end;
            for J:=0 to FrameGroup.count-1 do begin
              FrameObj:=QFrame(FrameList.Items1[I]);
              FrameObj.GetVertices(CVert);
              vec3_p(CVertArray):=CVert;
              { computes the normal vectors }
              FillChar(NormalesSommets^, SizeOf(TVect)*InputVertexCount, 0);
              CTris:=CTriangles;
              for K:=1 to mdl.numtris do begin
                if (CTris^[0].VertexNo >= InputVertexCount)
                 or (CTris^[1].VertexNo >= InputVertexCount)
                  or (CTris^[2].VertexNo >= InputVertexCount) then
                  Raise EError(5667);
                Vert1:=CVertArray^[CTris^[0].VertexNo];
                Vert2:=CVertArray^[CTris^[1].VertexNo];
                Vert3:=CVertArray^[CTris^[2].VertexNo];
                Vec1.X:=Vert1[0]-Vert2[0];
                Vec1.Y:=Vert1[1]-Vert2[1];
                Vec1.Z:=Vert1[2]-Vert2[2];
                Vec2.X:=Vert3[0]-Vert2[0];
                Vec2.Y:=Vert3[1]-Vert2[1];
                Vec2.Z:=Vert3[2]-Vert2[2];
                Vec3:=Cross(Vec1, Vec2);
                Aire:=Sqrt(Sqr(Vec3.X)+Sqr(Vec3.Y)+Sqr(Vec3.Z));
                AireTotale:=AireTotale + Aire;
                if Aire > rien then begin
                  Aire:=1/Aire;
                  Vec3.X:=Vec3.X*Aire;
                  Vec3.Y:=Vec3.Y*Aire;
                  Vec3.Z:=Vec3.Z*Aire;
                  for K1:=0 to 2 do begin
                    with NormalesSommets^[CTris^[K1].VertexNo] do begin
                      X:=X+Vec3.X;
                      Y:=Y+Vec3.Y;
                      Z:=Z+Vec3.Z;
                    end;
                  end;
                end;
                Inc(CTris);
              end;
              Frame.min.x:=255;
              Frame.min.y:=255;
              Frame.min.z:=255;
              Frame.min.n:=0;
              Frame.max.x:=0;
              Frame.max.y:=0;
              Frame.max.z:=0;
              Frame.max.n:=0;
              for K:=0 to InputVertexCount-1 do begin
                tvx:=Compacter(CVert^);
                if tvx.x < Frame.min.x then Frame.min.x:=tvx.x;
                if tvx.y < Frame.min.y then Frame.min.y:=tvx.y;
                if tvx.z < Frame.min.z then Frame.min.z:=tvx.z;
                if tvx.x > Frame.max.x then Frame.max.x:=tvx.x;
                if tvx.y > Frame.max.y then Frame.max.y:=tvx.y;
                if tvx.z > Frame.max.z then Frame.max.z:=tvx.z;
                with NormalesSommets^[K] do begin
                  Maximum:=-MaxInt;
                  for K1:=Low(VecteursNormaux) to High(VecteursNormaux) do begin
                    Aire:=X*VecteursNormaux[K1,0] + Y*VecteursNormaux[K1,1] + Z*VecteursNormaux[K1,2];
                    if Aire > Maximum then begin
                      Maximum:=Aire;
                      tvx.N:=K1;   { trouvé une meilleure approximation }
                    end;
                  end;
                end;
                Node:=VertexMap^[K];
                while Node<>Nil do begin
                  FrSourcePts^[Node^.OutputIndex]:=tvx;
                  Node:=Node^.Next;
                end;
                Inc(CVert);
              end;
              PasToChar(Frame.Nom, FrameObj.Name);
              F.WriteBuffer(Frame, SizeOf(Frame));
              F.WriteBuffer(FrSourcePts^, Taille1);
              Inc(I);
              ProgressIndicatorIncrement;
            end;
            Inc(mdl.numframes);
          end;
        finally
          FreeMem(NormalesSommets);
          FreeMem(FrSourcePts);
        end;
        if mdl.numframes=0 then
          mdl.size:=0
        else
          mdl.size:=AireTotale / (2*mdl.numframes*mdl.numtris);
        J:=F.Position;
        F.Position:=Position0;
        F.WriteBuffer(mdl, SizeOf(mdl));
        F.Position:=J;
      finally
        for I:=0 to InputVertexCount-1 do begin
          while VertexMap^[I]<>Nil do begin
            Node:=VertexMap^[I];
            VertexMap^[I]:=Node^.Next;
            FreeMem(Node);
          end;
        end;
        FreeMem(VertexMap);
      end;
      finally
        ProgressIndicatorStop;
        FrameList.Free;
        SkinList.Free;
      end;
   end;
    else
      inherited;
    end;
  end;
end;

initialization
  RegisterQObject(QMdlFile, 'u');
end.

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

{

$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.3  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers


}


unit QkBspHulls;

interface

uses Windows, SysUtils, Classes, QkObjects, QkMapObjects, QkBsp,
     qmath, QkFileObjects;

type
 TBoundBox = record
              Min, Max: vec3_t;
             end;
 PHull = ^THull;
 THull = record
          Bound: TBoundBox;
          Origin: vec3_t;
          Node_id0, Node_id1, Node_id2, Node_id3: LongInt;
          NumLeafs, Face_id, Face_num: LongInt;
         end;
 PHullH2 = ^THullH2;
 THullH2 = record
            Bound: TBoundBox;
            Origin: vec3_t;
            Node_id0, Node_id1, Node_id2, Node_id3: LongInt;
            Bidon0, Bidon1, Bidon2, Bidon3: LongInt;
            NumLeafs, Face_id, Face_num: LongInt;
           end;
 PHullQ2 = ^THullQ2;
 THullQ2 = record
            Bound: TBoundBox;
            Origin: vec3_t;
            HeadNode, Face_id, Face_num: LongInt;
           end;

 PbPlane = ^TbPlane;
 TbPlane = record
            normal: vec3_t;
            dist: scalar_t;
            flags: Integer;
           end;
 PbSurface = ^TbSurface;
 TbSurface = record
              Plane_id, Side: Word;
              LEdge_id: LongInt;
              LEdge_num, TexInfo_id: Word;
              LightStyles: array[0..3] of Char;
              LightMap: LongInt;
             end;
 PLEdge = ^TLEdge;
 TLEdge = LongInt;
 PEdge = ^TEdge;
 TEdge = record
          Vertex0, Vertex1: Word;
         end;

 PTexInfoVecs = ^TTexInfoVecs;
 TTexInfoVecs = array[0..1, 0..3] of scalar_t;
 PTexInfo = ^TTexInfo;
 TTexInfo = record
             vecs: TTexInfoVecs;     // [s/t][xyz offset]
             miptex: Integer;
             flags: Integer;
            end;
 PTexInfoQ2 = ^TTexInfoQ2;
 TTexInfoQ2 = record
               vecs: TTexInfoVecs;             // [s/t][xyz offset]
               flags: Integer;                 // miptex flags + overrides
               value: Integer;                 // light emission, etc
               texture: array[0..31] of Byte;  // texture name (textures/*.wal)
               nexttexinfo: Integer;           // for animations, -1 = end of chain
              end;

type
 TBSPHull = class(TTreeMapGroup)
            private
              FBsp: QBSP;
              HullNum, UsedVertex: Integer;
              NbFaces, FirstFace: Integer;
              SurfaceList: PChar;
            public
              constructor CreateHull(nBsp: QBSP; Index: Integer; nParent: QObject);
              destructor Destroy; override;
              class function TypeInfo: String; override;
              procedure EtatObjet(var E: TEtatObjet); override;
              function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
              procedure Dessiner; override;
             {function SingleLevel: Boolean; override;}
             {procedure AjouterRef(Liste: TList; Niveau: Integer) : Integer; override;}
            end;

 {------------------------}

function CheckQ1Hulls(Hulls: PHull; Size, FaceCount: Integer) : Boolean;
function CheckH2Hulls(Hulls: PHullH2; Size, FaceCount: Integer) : Boolean;

 {------------------------}

implementation

uses QkMapPoly, Setup, qmatrices, QkWad, Quarkx, PyMath, Qk3D;

 {------------------------}

function CheckQ1Hulls(Hulls: PHull; Size, FaceCount: Integer) : Boolean;
var
 P, Q: PHull;
 I, J, HullCount: Integer;
begin
 Result:=False;
 HullCount:=Size div SizeOf(THull);
 if HullCount*SizeOf(THull)<>Size then Exit;
 P:=Hulls;
 for I:=1 to HullCount do
  begin
   with P^ do
    begin
     if (Face_id<0) or (Face_num<=0) or (Face_id>=FaceCount) or (Face_id+Face_num>FaceCount) then
      Exit;    { invalid Face_id and Face_num }
     Q:=Hulls;
     for J:=2 to I do
      begin
       if (Face_id+Face_num > Q^.Face_id) and (Face_id < Q^.Face_id+Q^.Face_num) then
        Exit;   { overlapping Face_id and Face_num }
       Inc(Q);
      end;
    end;
   Inc(P);
  end;
 Result:=True;
end;

function CheckH2Hulls(Hulls: PHullH2; Size, FaceCount: Integer) : Boolean;
var
 P, Q: PHullH2;
 I, J, HullCount: Integer;
begin
 Result:=False;
 HullCount:=Size div SizeOf(THullH2);
 if HullCount*SizeOf(THullH2)<>Size then Exit;
 P:=Hulls;
 for I:=1 to HullCount do
  begin
   with P^ do
    begin
     if (Face_id<0) or (Face_num<=0) or (Face_id>=FaceCount) or (Face_id+Face_num>FaceCount) then
      Exit;    { invalid Face_id and Face_num }
     Q:=Hulls;
     for J:=2 to I do
      begin
       if (Face_id+Face_num > Q^.Face_id) and (Face_id < Q^.Face_id+Q^.Face_num) then
        Exit;   { overlapping Face_id and Face_num }
       Inc(Q);
      end;
    end;
   Inc(P);
  end;
 Result:=True;
end;

 {------------------------}

constructor TBSPHull.CreateHull(nBsp: QBSP; Index: Integer; nParent: QObject);
var
 HullType: Char;
 Delta, Size1: Integer;
 S: String;
 I, J, NoVert, NoVert2{, TexInfo_id}: Integer;
 Faces, Faces2: PbSurface;
 LEdges, Edges, Vertices, TexInfo, Planes, P: PChar;
 cLEdges, cEdges, cVertices, cTexInfo, cPlanes: Integer;
 LEdge: PLEdge;
 NoEdge: LongInt;
 Face: TFace;
 Surface1: PSurface;
 Dest: ^PSommet;
 BspVecs: PTexInfoVecs;
 InvFaces: Integer;
 LastError: String;
 P1, P2, P3, NN: TVect;
 PlaneDist: TDouble;
 TextureList: QTextureList;
begin
 inherited Create(FmtLoadStr1(5406, [Index]), nParent);
 HullNum:=Index;
 FBsp:=nBsp;
 FBsp.AddRef(+1);
 FBsp.VerticesAddRef(+1);

 try
  InvFaces:=0;
  cTexInfo:=0;
  HullType:=FBsp.NeedObjectGameCode;
  case HullType of
   mjQuake, mjHalfLife:  Size1:=SizeOf(THull);
   mjHexen:              Size1:=SizeOf(THullH2);
   mjQuake2, mjHeretic2: Size1:=SizeOf(THullQ2);
  else Exit;
  end;
  I:=FBsp.GetBspEntryData(eHulls, lump_models, P);
  Delta:=Size1*Succ(Index);
  if I<Delta then
   Raise EErrorFmt(5635, [1]);
  Inc(P, Delta-Size1);
  case HullType of
   mjQuake, mjHalfLife: with PHull(P)^ do
             begin
              NbFaces:=Face_num;
              FirstFace:=Face_id;
              cTexInfo:=SizeOf(TTexInfo);
             end;
   mjHexen: with PHullH2(P)^ do
             begin
              NbFaces:=Face_num;
              FirstFace:=Face_id;
              cTexInfo:=SizeOf(TTexInfo);
             end;
   mjQuake2, mjHeretic2: with PHullQ2(P)^ do
              begin
               NbFaces:=Face_num;
               FirstFace:=Face_id;
               cTexInfo:=SizeOf(TTexInfoQ2);
              end;
  end;
  if HullType>=mjQuake2 then
   TextureList:=Nil
  else
   begin
    TextureList:=FBsp.BspEntry[eMipTex, NoBsp2] as QTextureList;
    TextureList.Acces;
   end;
  if FBsp.GetBspEntryData(eSurfaces, lump_faces, PChar(Faces)) < (FirstFace+NbFaces)*SizeOf(TbSurface) then
   Raise EErrorFmt(5635, [2]);
  Inc(PChar(Faces), Pred(FirstFace) * SizeOf(TbSurface));
  cLEdges:=FBsp.GetBspEntryData(eListEdges, lump_surfedges, LEdges) div SizeOf(TLEdge);
  cEdges:=FBsp.GetBspEntryData(eEdges, lump_edges, Edges) div SizeOf(TEdge);
  cTexInfo:=FBsp.GetBspEntryData(eTexInfo, lump_texinfo, TexInfo) div cTexInfo;
  cPlanes:=FBsp.GetBspEntryData(ePlanes, lump_planes, Planes) div SizeOf(TbPlane);
  cVertices:=FBsp.GetBspEntryData(eVertices, lump_vertexes, Vertices) div SizeOf(vec3_t);
  Vertices:=PChar(FBsp.FVertices);

  Faces2:=Faces;
  Size1:=0;
  for I:=1 to NbFaces do
   begin
    Inc(PChar(Faces2), SizeOf(TbSurface));
    if Faces2^.ledge_id + Faces2^.ledge_num > cLEdges then
     Raise EErrorFmt(5635, [3]);
    Inc(Size1, TailleBaseSurface+Faces2^.ledge_num*SizeOf(PSommet));
   end;
  GetMem(SurfaceList, Size1);
  PChar(Surface1):=SurfaceList;

  SubElements.Capacity:=NbFaces;

  for I:=1 to NbFaces do
   begin
    Inc(PChar(Faces), SizeOf(TbSurface));
    PChar(LEdge):=LEdges + Faces^.ledge_id * SizeOf(TLEdge);
    Surface1^.Source:=Self;
    Surface1^.NextF:=Nil;
    Surface1^.prvNbS:=Faces^.ledge_num;

    if Faces^.Plane_id >= cPlanes then
     begin
      Inc(InvFaces); LastError:='Err Plane_id'; Continue;
     end;
    with PbPlane(Planes + Faces^.Plane_id * SizeOf(TbPlane))^ do
     begin
      NN.X:=normal[0];
      NN.Y:=normal[1];
      NN.Z:=normal[2];
      PlaneDist:=dist;
     end;
    {TexInfo_id:=Faces^.TexInfo_id;}

    PChar(Dest):=PChar(Surface1)+TailleBaseSurface;
    for J:=1 to Faces^.ledge_num do
     begin
      NoEdge:=LEdge^;
      Inc(PChar(LEdge), SizeOf(TLEdge));
      if NoEdge < 0 then
       begin
        if -NoEdge>=cEdges then
         Raise EErrorFmt(5635, [4]);
        with PEdge(Edges - NoEdge * SizeOf(TEdge))^ do
         begin
          NoVert:=Vertex0;
          NoVert2:=Vertex1;
         end;
       end
      else
       begin
        if NoEdge>=cEdges then
         Raise EErrorFmt(5635, [5]);
        with PEdge(Edges + NoEdge * SizeOf(TEdge))^ do
         begin
          NoVert:=Vertex1;
          NoVert2:=Vertex0;
         end;
       end;
      if NoVert2>=UsedVertex then
       UsedVertex:=NoVert2+1;
      if NoVert>=UsedVertex then
       UsedVertex:=NoVert+1;
      Dest^:=PSommet(Vertices + NoVert * SizeOf(TVect));
     {if Abs(Dot(Dest^^.P, NN) - Planedist) > rien then
       begin
        TexInfo_id:=MaxInt;
        Break;
       end;}
      Inc(Dest);
     end;
    if UsedVertex>cVertices then
     Raise EErrorFmt(5635, [6]);

     { load texture infos }
    if Faces^.TexInfo_id >= cTexInfo then
     begin
      Inc(InvFaces);
     {if TexInfo_id = MaxInt then
       LastError:='Err Point Off Plane'
      else}
       LastError:='Err TexInfo_id';
      Continue;
     end;
    if HullType>=mjQuake2 then
     with PTexInfoQ2(TexInfo + Faces^.TexInfo_id * SizeOf(TTexInfoQ2))^ do
      begin
       S:=CharToPas(texture);
       BspVecs:=@vecs;
      end
    else
     with PTexInfo(TexInfo + Faces^.TexInfo_id * SizeOf(TTexInfo))^ do
      begin
       BspVecs:=@vecs;
       if miptex>=TextureList.SubElements.Count then
        begin
         Inc(InvFaces); LastError:=FmtLoadStr1(5639,[miptex]); Continue;
        end;
       S:=TextureList.SubElements[miptex].Name;
      end;

        (** Equations to solve :     with (s,s0)=vecs[0] and (t,t0)=vecs[1]

              s*P1 + s0 = 0       s*P2 + s0 = 128     s*P3 + s0 = 0
              t*P1 + t0 = 0       t*P2 + t0 = 0       t*P3 + t0 = -128

            with P1, P2, P3 in the plane PlaneInfo = (n,d).
            We must solve (s*p,t*p) = (s1,t1).

              s.x*p.x + s.y*p.y + s.z*p.z = s1
              t.x*p.x + t.y*p.y + t.z*p.z = t1
              n.x*p.x + n.y*p.y + n.z*p.z = d
        **)
    Info.Matrice[1,1]:=bspvecs^[0,0];
    Info.Matrice[1,2]:=bspvecs^[0,1];
    Info.Matrice[1,3]:=bspvecs^[0,2];
    Info.Matrice[2,1]:=bspvecs^[1,0];
    Info.Matrice[2,2]:=bspvecs^[1,1];
    Info.Matrice[2,3]:=bspvecs^[1,2];
    Info.Matrice[3,1]:=NN.X;
    Info.Matrice[3,2]:=NN.Y;
    Info.Matrice[3,3]:=NN.Z;
    Info.Matrice:=MatriceInverse(Info.Matrice);
    P1.X:=-bspvecs^[0,3];
    P1.Y:=-bspvecs^[1,3];
    P1.Z:=PlaneDist;
    TransformationLineaire(P1);
    P2.X:=EchelleTexture-bspvecs^[0,3];
    P2.Y:=-bspvecs^[1,3];
    P2.Z:=PlaneDist;
    TransformationLineaire(P2);
    P3.X:=-bspvecs^[0,3];
    P3.Y:=-EchelleTexture-bspvecs^[1,3];
    P3.Z:=PlaneDist;
    TransformationLineaire(P3);

    Face:=TFace.Create(IntToStr(I), Self);
    SubElements.Add(Face);
    if Faces^.side<>0 then
     with Face do
      begin
       Normale.X:=-NN.X;
       Normale.Y:=-NN.Y;
       Normale.Z:=-NN.Z;
       Dist:=-PlaneDist;
      end
     else
      with Face do
       begin
        Normale:=NN;
        Dist:=PlaneDist;
       end;
    if not Face.SetThreePointsEx(P1, P2, P3, Face.Normale) then
     begin
      SubElements.Remove(Face);
      Inc(InvFaces); LastError:='Err degenerate'; Continue;
     end;
    Face.Specifics.Add(CannotEditFaceYet+'=1'); 
    Surface1^.F:=Face;
    Face.LinkSurface(Surface1);
    Face.NomTex:=S;
    PChar(Surface1):=PChar(Dest);
   end;

  if InvFaces>0 then
   GlobalWarning(FmtLoadStr1(5638, [Index, InvFaces, LastError]));
 except
  on E: Exception do
   begin
    FBsp.VerticesAddRef(-1);
    FBsp.AddRef(-1);
    FBsp:=Nil;
    GlobalWarning(FmtLoadStr1(5634, [Index, GetExceptionMessage(E)]));
   end;
 end;
end;

{function TBSPHull.SingleLevel: Boolean;
begin
 SingleLevel:=True;
end;}

procedure TBSPHull.Dessiner;   { optimized (the inherited version would also do the job) }
type
 PProjVertices = ^TProjVertices;
 TProjVertices = array[0..0] of TPointProj;
var
 I, J: Integer;
 Faces: PbSurface;
 LEdges, Edges, Vertices, Limit: PChar;
 LEdge: PLEdge;
 NoEdge: LongInt;
 ProjVertices: PProjVertices;
{OutOfView: TBits;}
 OutOfViewChk: Boolean;
 Dest: PPointProj;
 Src: ^TVect;
 Sommets: array[0..1] of PVect;
 ProjSommets: array[0..1] of TPointProj;
{Pts: array[0..1] of TPoint;}
 OldPen, NewPen: HPen;
 PV0, PV1: PPointProj;
begin
 if (FBsp=Nil) or (SurfaceList=Nil) then Exit;

 FBsp.GetBspEntryData(eSurfaces, lump_faces, PChar(Faces));
 Inc(PChar(Faces), FirstFace * SizeOf(TbSurface));
 FBsp.GetBspEntryData(eListEdges, lump_surfedges, LEdges);
 FBsp.GetBspEntryData(eEdges, lump_edges, Edges);
 Vertices:=PChar(FBsp.FVertices);

 if Info.SelectedBrush<>0 then
  begin
   NewPen:=Info.SelectedBrush;
  {OldROP:=SetROP2(Info.DC, R2_CopyPen);}
  end
 else
  if HullNum=0 then
   NewPen:=CreatePen(ps_Solid, 0, MapColors(lcBSP))
  else
   NewPen:=GetStockObject(Info.BasePen);
 OldPen:=SelectObject(Info.DC, NewPen);
 SetROP2(Info.DC, R2_CopyPen);
 ProjVertices:=Nil;
{OutOfView:=Nil;}
 try
  if HullNum=0 then     { point projection optimization }
   begin
    J:=UsedVertex*SizeOf(TPointProj);
    GetMem(ProjVertices, J);
    PChar(Src):=Vertices;
    PChar(Dest):=PChar(ProjVertices);
    Limit:=PChar(Dest)+J;
    while PChar(Dest)<Limit do
     begin
      Dest^:=CCoord.Proj(Src^);
      CCoord.CheckVisible(Dest^);
      Inc(Dest);
      Inc(Src);
     end;
    OutOfViewChk:=(Info.ModeAff>0) and (Info.SelectedBrush=0);
  (*if (Info.ModeAff>0) and (Info.SelectedBrush=0) then
     begin
      OutOfView:=TBits.Create;
      OutOfView.Size:=UsedVertex;
      PChar(Dest):=PChar(ProjVertices);
      for I:=0 to UsedVertex-1 do
       begin
        if Dest^.OffScreen <> 0 then
         OutOfView.Bits[I]:=True;
        Inc(Dest);
       end;
     end;*)
    for I:=1 to NbFaces do   { fast version }
     begin
      PChar(LEdge):=LEdges + Faces^.ledge_id * SizeOf(TLEdge);
      for J:=1 to Faces^.ledge_num do
       begin
        NoEdge:=LEdge^;
        Inc(PChar(LEdge), SizeOf(TLEdge));
        if NoEdge > 0 then  { only draws half the edges - the other ones are drawn in the other direction another time anyway }
         with PEdge(Edges + NoEdge * SizeOf(TEdge))^ do
          begin
           PV0:=@ProjVertices^[Vertex0];
           PV1:=@ProjVertices^[Vertex1];
           if OutOfViewChk then
            begin
             if not ((PV0^.OffScreen<>0) and (PV1^.OffScreen<>0)) then
              begin
               if Info.ModeAff=1 then
                begin
                 SelectObject(Info.DC, NewPen);
                 SetROP2(Info.DC, R2_CopyPen);
                end;
              end
             else
              begin
               if Info.ModeAff=2 then
                Continue;
               SetROP2(Info.DC, Info.MaskR2);
               SelectObject(Info.DC, Info.GreyBrush);
              end;
            end;
           CCoord.Line95f(PV0^, PV1^);
          end;
       end;
      Inc(PChar(Faces), SizeOf(TbSurface));
     end;
   end
  else
   for I:=1 to NbFaces do   { slow version }
    begin
     PChar(LEdge):=LEdges + Faces^.ledge_id * SizeOf(TLEdge);
     for J:=1 to Faces^.ledge_num do
      begin
       NoEdge:=LEdge^;
       Inc(PChar(LEdge), SizeOf(TLEdge));
       if NoEdge > 0 then  { only draws half the edges - the other ones are drawn in the other direction another time anyway }
        with PEdge(Edges + NoEdge * SizeOf(TEdge))^ do
         begin
          PChar(Sommets[0]):=Vertices + Vertex0 * SizeOf(TVect);
          PChar(Sommets[1]):=Vertices + Vertex1 * SizeOf(TVect);
          ProjSommets[0]:=CCoord.Proj(Sommets[0]^);
          ProjSommets[1]:=CCoord.Proj(Sommets[1]^);
          CCoord.CheckVisible(ProjSommets[0]);
          CCoord.CheckVisible(ProjSommets[1]);
          if (Info.ModeAff>0) and (Info.SelectedBrush=0) then
           begin
          (*ModeProj:=TModeProj(1-Ord(ModeProj));
            Pts[0]:=Proj(Sommets[0]^);
            Pts[1]:=Proj(Sommets[1]^);
            ModeProj:=TModeProj(1-Ord(ModeProj));
            if PtInRect(Info.VisibleRect, Pts[0])
            or PtInRect(Info.VisibleRect, Pts[1]) then*)
            if (ProjSommets[0].OffScreen=0)
            or (ProjSommets[1].OffScreen=0) then
             begin
              if Info.ModeAff=1 then
               begin
                SelectObject(Info.DC, NewPen);
                SetROP2(Info.DC, R2_CopyPen);
               end;
             end
            else
             begin
              if Info.ModeAff=2 then
               Continue;
              SetROP2(Info.DC, Info.MaskR2);
              SelectObject(Info.DC, Info.GreyBrush);
             end;
           end;
          CCoord.Line95f(ProjSommets[0], ProjSommets[1]);
         end;
      end;
     Inc(PChar(Faces), SizeOf(TbSurface));
    end;
 finally
 {OutOfView.Free;}
  FreeMem(ProjVertices);
  SelectObject(Info.DC, OldPen);
  if Info.SelectedBrush<>0 then
  {SetROP2(Info.DC, OldROP)}
  else
   DeleteObject(NewPen);
 end;
end;

destructor TBSPHull.Destroy;
begin
 inherited;
 if FBsp<>Nil then
  begin
   FBsp.VerticesAddRef(-1);
   FBsp.AddRef(-1);
  end;
 FreeMem(SurfaceList);
end;

class function TBSPHull.TypeInfo: String;
begin
 TypeInfo:=':bsphull';
end;

procedure TBSPHull.EtatObjet;
begin
 inherited;
 E.IndexImage:=iiComponent;
end;

function TBSPHull.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 if Q is TFace then
  Result:=ieResult[True] + [ieInvisible]
 else
  Result:=[];
end;

(*function TBSPHull.AjouterRef(Liste: TList; Niveau: Integer) : Integer;
var
 ZMax1: LongInt;
 I: Integer;
 Vertices: PTableauPointsProj;
 S: PSurface;
begin
 if (FBsp<>Nil) and (SurfaceList<>Nil) then
  begin
   if HullNum=0 then
    begin
     GetMem(Vertices, Sommets.Count*SizeOf(TPointsProj)); try
     ZMax1:=-MaxInt;
     for I:=0 to Sommets.Count-1 do
      with Vertices^[I] do
       begin
        Src:=PSommet(Sommets[I]);
        Pt3D:=SceneCourante.Proj(Src^.P);
        if Pt3D.Z > ZMax1 then
         ZMax1:=Pt3D.Z;
      end;
     for I:=0 to Faces.Count-1 do
      begin
       S:=PSurface(Faces[I]);
       S^.F.AjouterSurfaceRef(Liste, S, Vertices, Sommets.Count, ZMax1, Odd(S^.F.SelMult));
        {Info.ColorTraits[esNormal]);}
      end;
     finally FreeMem(Vertices); end;
    end
   else
    inherited AjouterRef(Liste, -1);
   Result:=1;
  end
 else
  Result:=0;
end;*)

 {------------------------}

initialization
  RegisterQObject(TBSPHull, 'a');
end.

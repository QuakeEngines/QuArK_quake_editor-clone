(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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

http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.18  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.17  2001/03/20 21:44:19  decker_dk
Updated copyright-header

Revision 1.16  2001/03/15 20:50:55  aiv
split up get entities and get textures

Revision 1.15  2001/03/12 20:34:28  aiv
now get textures from .bsp files (Q1, H2, and any others that support textures in bsp files)

Revision 1.14  2001/03/12 03:41:04  aiv
bug fixes for entity tool.

Revision 1.13  2001/03/09 21:11:56  aiv
Misc. Bug fixes

Revision 1.12  2001/03/09 01:50:56  aiv
fixed treeview updating bug

Revision 1.11  2001/03/09 00:01:31  aiv
added texture linking to entity tool.

Revision 1.10  2001/03/08 23:22:53  aiv
entity tool finished completly i think.

Revision 1.9  2001/01/21 15:49:48  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.8  2001/01/15 19:21:27  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.7  2000/08/25 17:57:24  decker_dk
Layout indenting

Revision 1.6  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.4  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}


unit QkQuakeCtx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkFormVw, Python, PyObjects;

type
 QQuakeCtx = class(QFormObject)
             protected
               function GetConfigStr1: String; override;
             public
               class function TypeInfo: String; override;
               procedure ObjectState(var E: TEtatObjet); override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
               Procedure MakeAddonFromQctx;
               Procedure MakeTexturesFromQctx;
               function PyGetAttr(attr: PChar) : PyObject; override;
             end;

 {------------------------}

function GetQuakeContext: TQList;
function BuildQuakeCtxObjects(nClass: QObjectClass; const nName: String) : TQList;
procedure ClearQuakeContext;

function OpacityFromFlags(Flags: Integer) : Integer;
function OpacityToFlags(Flags: Integer; Alpha: Integer) : Integer;

 {------------------------}

implementation

uses Setup, QkGroup, Quarkx, QkObjectClassList, QuickWal, QkPak, QkBSP, ToolBox1,
     ToolBoxGroup, ExtraFunctionality, Game, QkMapObjects, FormCfg, QkExplorer,
     QkForm, Travail, QkFormCfg;

 {------------------------}

type
 TTexOpacityInfo = record
                    Loaded: Boolean;
                    Count: Byte;
                    Reserved1, Reserved2: Byte;
                    Opacity: array[0..31] of Byte;
                   end;

var
 TInfo: TTexOpacityInfo;

procedure LoadTInfo;
var
 I, J, K, L, M: Integer;
 Li: TQList;
 Val32: array[0..63] of Single;
begin
 FillChar(TInfo.Opacity, SizeOf(TInfo.Opacity), 255);
 TInfo.Loaded:=True;
 Li:=GetQuakeContext;
 for J:=0 to Li.Count-1 do
  begin
   K:=Li[J].GetFloatsSpecPartial('TexFlagsTransparent', Val32);
   for I:=0 to K div 2 - 1 do
    begin
     M:=Round(Val32[I*2]);
     L:=0;
     while not Odd(M) and (M<>0) do
      begin
       Inc(L);
       M:=M shr 1;
      end;
     if M=1 then
      begin
       M:=Round((1-Val32[I*2+1])*255);
       if M<0 then M:=0 else if M>255 then M:=255;
       TInfo.Opacity[L]:=M;
      end;
    end;
  end;
end;

function OpacityFromFlags(Flags: Integer) : Integer;
var
 L: Integer;
begin
 Result:=255;
 if Flags=0 then Exit;

 if not TInfo.Loaded then
  LoadTInfo;

 L:=0;
 repeat
  if Odd(Flags) and (TInfo.Opacity[L]<Result) then
   Result:=TInfo.Opacity[L];
  Flags:=Flags shr 1;
  Inc(L);
 until Flags=0;
end;

function OpacityToFlags(Flags: Integer; Alpha: Integer) : Integer;
var
 L, Best, DistMin, Dist: Integer;
begin
 if not TInfo.Loaded then
  LoadTInfo;
 Best:=0;
 DistMin:=255-Alpha;
 for L:=Low(TInfo.Opacity) to High(TInfo.Opacity) do
  if TInfo.Opacity[L]<255 then
   begin
    Dist:=Abs(Alpha-Integer(TInfo.Opacity[L]));
    if Dist<DistMin then
     begin
      DistMin:=Dist;
      Best:=1 shl L;
     end;
    Flags:=Flags and not (1 shl L);
   end;
 Result:=Flags or Best;
end;

 {------------------------}

var
 QuakeContext: TQList = Nil;

procedure ClearQuakeContext;
begin
 QuakeContext.Free;
 QuakeContext:=Nil;
 TInfo.Loaded:=False;
end;

function GetQuakeContext: TQList;
var
 Addons: QFileObject;
 I: Integer;
 Q: QObject;
 S: String;
begin
 if QuakeContext=Nil then
  begin
   Addons:=MakeAddOnsList;
   try
    QuakeContext:=TQList.Create;
    Addons.FindAllSubObjects('', QQuakeCtx, Nil, QuakeContext);
    for I:=QuakeContext.Count-1 downto 0 do
     begin
      Q:=QuakeContext[I];
      Q.Acces;
      if not GameModeOk((Q as QQuakeCtx).ObjectGameCode) then
       begin
        while (Q<>Nil) and (Q.Flags and ofFileLink = 0) do
         Q:=Q.FParent;
        if (Q=Nil) or not (Q is QFileObject) then
         S:=LoadStr1(5552)
        else
         S:=QFileObject(Q).Filename;
        GlobalWarning(FmtLoadStr1(5582, [S, SetupGameSet.Name, QuakeContext[I].Specifics.Values['Game']]));
        QuakeContext.Delete(I);
       end;
     end;
   finally
    Addons.AddRef(-1);
   end;
  end;
 GetQuakeContext:=QuakeContext;
end;

function BuildQuakeCtxObjects(nClass: QObjectClass; const nName: String) : TQList;
var
 L: TQList;
 I, J: Integer;
 Q, Q1: QObject;
begin
 Result:=TQList.Create;
 try
  L:=GetQuakeContext;
  for I:=0 to L.Count-1 do
   begin
    Q:=L[I];
    for J:=0 to Q.SubElements.Count-1 do
     begin
      Q1:=Q.SubElements[J];
      if (Q1 is nClass)
      and ((nName='') or (CompareText(Q1.Name, nName) = 0)) then
       begin
        {Q1.Acces;}
        Result.Add(Q1);
       end;
     end;
   end;
 except
  Result.Free;
  Raise;
 end;
end;

 {------------------------}

class function QQuakeCtx.TypeInfo;
begin
 TypeInfo:='.qctx';
end;

function QQuakeCtx.GetConfigStr1: String;
begin
 Result:='QuakeCtx';
end;

procedure QQuakeCtx.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiQCtx;
 E.MarsColor:=clMaroon;
end;

class procedure QQuakeCtx.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5155);
{Info.FileExt:=779;
 Info.WndInfo:=[wiWindow];}
end;

Function OpenFiles(dir: String; L: TStringList): TQList;
var
  i: Integer;
begin
  Result:=TQList.Create;
  For i:=0 to l.count-1 do
  begin
    Result.Add(ExactFileLink(dir+'\'+l.strings[i], nil, false));
  end;
end;

Function FindFiles(dir, filter: String): TQList;
var
  f: TSearchRec;
  f_e: Integer;
begin
  Result:=TQList.Create;
  f_e:=FindFirst(filter, faAnyFile, F);
  while f_e=0 do
  begin
    Result.add(ExactFileLink(dir+'\'+f.name, nil, false));
    f_e:=FindNext(F);
  end;
end;

function qMakeAddonFromQctx(self, args: PyObject) : PyObject; cdecl;
begin
   with QkObjFromPyObj(self) as QQuakeCtx do
     MakeAddonFromQctx;
   Result:=PyNoResult;
end;

function qMakeTexturesFromQctx(self, args: PyObject) : PyObject; cdecl;
begin
   with QkObjFromPyObj(self) as QQuakeCtx do
     MakeTexturesFromQctx;
   Result:=PyNoResult;
end;

const
  MethodTable: array[0..1] of TyMethodDef =
   ((ml_name: 'makeentitiesfromqctx';      ml_meth: qMakeAddonFromQctx;         ml_flags: METH_VARARGS),
    (ml_name: 'maketexturesfromqctx';      ml_meth: qMakeTexturesFromQctx;      ml_flags: METH_VARARGS));

function QQuakeCtx.PyGetAttr(attr: PChar) : PyObject;
var
  I: Integer;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  for I:=Low(MethodTable) to High(MethodTable) do
  begin
    if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
      Result:=PyCFunction_New(MethodTable[I], @PythonObj);
      Exit;
    end;
  end;
end;

Procedure QQuakeCtx.MakeTexturesFromQctx;
var
  i,j: integer;
  // Objects for getting bsp list
  paks: TQList;
  bsps: TQList;
  Pak, ExistingAddons: QFileObject;
  p_f: QPakFolder;
  bsp: QBsp;
  NewAddonsList: TQList;
  // Objects for creating new addon
  addonRoot: QFileObject;
  TexRoot: QToolBox;
  TexFolders, oldTexRoot: QObject;
  (*
    Get all .bsp files in & out of pak's
  *)
  procedure GetBSPFiles;
  var
    i,j: Integer;
    dir: String;
  begin
    dir:=IncludeTrailingBackslash(QuakeDir)+Specifics.Values['GameDir'];
    paks:=OpenFiles(dir, ListPakFiles(dir));
    bsps:=FindFiles(dir+'\maps', IncludeTrailingBackslash(QuakeDir)+Specifics.Values['GameDir']+'\maps\*.bsp');
    ProgressIndicatorStart(5458,paks.count);
    for i:=0 to paks.count-1 do
    begin
      ProgressIndicatorIncrement;
      pak:=QFileObject(paks[i]);
      try
        pak.acces;
      except
        continue;
      end;
      p_f:=QPakFolder(pak.FindSubObject('maps', QPakFolder, QPakFolder));
      if p_f=nil then continue;
      for j:=0 to p_f.subelements.count-1 do
      begin
        if p_f.subelements[j] is QBsp then
          bsps.add(p_f.subelements[j]);
      end;
    end;
    ProgressIndicatorStop;
  end;
  (*
    Go through list of .bsps and create addon based on each
  *)
  Procedure CreateAddons;
  var
    i: integer;
  begin
    ExistingAddons:=MakeAddonsList;
    ProgressIndicatorStart(5458,bsps.count);
    for i:=0 to bsps.count-1 do
    begin
      if not (bsps[i] is QBsp) then
        raise exception.create('Error: bsp list contains non QBSP object!');
      bsp := QBsp(bsps[i]);
      NewAddonsList.Add(bsp.CreateAddonFromEntities(ExistingAddons));
      ProgressIndicatorIncrement;
      Application.ProcessMessages;
    end;
    ProgressIndicatorStop;
    ExistingAddons.AddRef(-1);
  end;
  Function GetObject(nname, ntypeinfo, s: String): QObject;
  var
    i: Integer;
  begin
    Result:=nil;
    for i:=0 to FParent.SubElements.Count-1 do
    begin
      if FParent.SubElements[i].typeinfo = ntypeinfo then
      begin
        if FParent.Subelements[i].GetArg('ToolBox')=s then
        begin
          result:=FParent.SubElements[i];
          break;
        end;
      end;
    end;
    if Result=nil then
    begin
      Result:=ConstructQObject(nname+ntypeinfo, FParent);
      if s<>'' then
        Result.Specifics.Add('ToolBox='+s);
      FParent.SubElements.Add(Result);
    end;
  end;
begin
  NewAddonsList:=TQList.Create; // a list of AddonRoot (.qrk objects)
  GetBSPFiles;
  CreateAddons;

  addonRoot:=QFileObject(FParent);
  if addonRoot = nil then
  begin
    raise Exception.Create('addonRoot = nil');
  end;
  if addonRoot.specifics.IndexOfName('Description')=-1 then
    addonRoot.specifics.Add(format('Description=Addon for %s',[Specifics.Values['GameDir']]));
  (*
    Build Textures First
  *)
  TexFolders:=nil;
  TexRoot:=QToolBox(GetObject('Textures', QToolbox.TypeInfo, 'Texture Browser...'));
  if Specifics.Values['GameDir'] <> '' then
    BuildDynamicFolders(Specifics.Values['GameDir'], TexFolders, false, false, '');

  if TexFolders<>nil then
  begin
    TexFolders.Name:=Specifics.Values['GameDir']+' textures';
    TexRoot.Flags := TexRoot.Flags or ofTreeViewSubElement;
    if TexRoot.Specifics.IndexOfName('Root')=-1 then
      TexRoot.SpecificsAdd('Root='+TexFolders.GetFullName);
    if TexRoot.SubElements.FindShortName(TexFolders.Name)=nil then
      TexRoot.SubElements.Add(TexFolders)
    else
      TexFolders.free;
    TexFolders.FParent:=TexRoot;
  end;
  ProgressIndicatorStart(5458,NewAddonsList.Count);
  try
    for i:=0 to NewAddonsList.Count-1 do
    begin
      (*
        Textures from .bsps (Q1, H2)
      *)
      if TexFolders<>nil then
      begin
        oldTexRoot:=NewAddonsList.Items1[i].FindSubObject('Textures', QToolBox, nil);
        if oldTexRoot<>nil then
        begin
          for j:=0 to oldTexRoot.Subelements.Count-1 do
          begin
            TexFolders.Subelements.Add(oldTexRoot.Subelements[j].Clone(TexFolders, false));
          end;
        end;
      end;
    end;
  finally
    ProgressIndicatorStop;
  end;
  NewAddonsList.free;
  bsps.free;
  paks.free;
  ExplorerFromObject(FParent).Refresh;
end;

Procedure QQuakeCtx.MakeAddonFromQctx;
var
  i,j,k,l: integer;
  tb: string;
  // Objects for getting bsp list
  paks: TQList;
  bsps: TQList;
  Pak, ExistingAddons: QFileObject;
  p_f: QPakFolder;
  bsp: QBsp;
  NewAddonsList: TQList;
  // Objects for creating new addon
  addonRoot: QFileObject;
  TBX: QToolBox;
  entityTBX: QToolBoxGroup;
  entityTBX_2: QToolBoxGroup;
  Group: QToolBoxGroup;
  OldEntity, Entity: TTreeMapSpec;
  Objects: TQList;
  entityForms:QFormContext;
  OldForm, Form: QFormCfg;
  OldFormEl, FormEl: QObject;
  (*
    Get all .bsp files in & out of pak's
  *)
  procedure GetBSPFiles;
  var
    i,j: Integer;
    dir: String;
  begin
    dir:=IncludeTrailingBackslash(QuakeDir)+Specifics.Values['GameDir'];
    paks:=OpenFiles(dir, ListPakFiles(dir));
    bsps:=FindFiles(dir+'\maps', IncludeTrailingBackslash(QuakeDir)+Specifics.Values['GameDir']+'\maps\*.bsp');
    ProgressIndicatorStart(5458,paks.count);
    for i:=0 to paks.count-1 do
    begin
      ProgressIndicatorIncrement;
      pak:=QFileObject(paks[i]);
      try
        pak.acces;
      except
        continue;
      end;
      p_f:=QPakFolder(pak.FindSubObject('maps', QPakFolder, QPakFolder));
      if p_f=nil then continue;
      for j:=0 to p_f.subelements.count-1 do
      begin
        if p_f.subelements[j] is QBsp then
          bsps.add(p_f.subelements[j]);
      end;
    end;
    ProgressIndicatorStop;
  end;
  (*
    Go through list of .bsps and create addon based on each
  *)
  Procedure CreateAddons;
  var
    i: integer;
  begin
    ExistingAddons:=MakeAddonsList;
    ProgressIndicatorStart(5458,bsps.count);
    while bsps.count<>0 do
    begin
      if not (bsps[0] is QBsp) then
        raise exception.create('Error: bsp list contains non QBSP object!');
      bsp := QBsp(bsps[0]);
      NewAddonsList.Add(bsp.CreateAddonFromEntities(ExistingAddons));
      ProgressIndicatorIncrement;
      Application.ProcessMessages;
      bsps.Delete(0);
    end;
    ProgressIndicatorStop;
    ExistingAddons.AddRef(-1);
  end;
  Function GetObject(nname, ntypeinfo, s: String): QObject;
  var
    i: Integer;
  begin
    Result:=nil;
    for i:=0 to FParent.SubElements.Count-1 do
    begin
      if FParent.SubElements[i].typeinfo = ntypeinfo then
      begin
        if FParent.Subelements[i].GetArg('ToolBox')=s then
        begin
          result:=FParent.SubElements[i];
          break;
        end;
      end;
    end;
    if Result=nil then
    begin
      Result:=ConstructQObject(nname+ntypeinfo, FParent);
      if s<>'' then
        Result.Specifics.Add('ToolBox='+s);
      FParent.SubElements.Add(Result);
    end;
  end;
begin
  NewAddonsList:=TQList.Create; // a list of AddonRoot (.qrk objects)
  GetBSPFiles;
  CreateAddons;
  bsps.free;
  paks.free;

  addonRoot:=QFileObject(FParent);
  if addonRoot = nil then
  begin
    raise Exception.Create('addonRoot = nil');
  end;
  if addonRoot.specifics.IndexOfName('Description')=-1 then
    addonRoot.specifics.Add(format('Description=Addon for %s',[Specifics.Values['GameDir']]));
  TBX:=QToolBox(GetObject('Toolbox Folders', QToolbox.TypeInfo, 'New map items...'));
  EntityTBX:=QToolBoxGroup.Create(Format('%s Entities', [Specifics.Values['GameDir']]), TBX);
  TBX.Subelements.Add(EntityTBX);
  TBX.Specifics.Add('Root='+EntityTBX.GetFullName);
  EntityTBX_2:=EntityTBX;
  entityForms:=QFormContext(GetObject('Entity forms', QFormContext.Typeinfo, ''));
  (*
    Now build entities & include any textures found in .bsp files
  *)
  ProgressIndicatorStart(5458,NewAddonsList.Count);
  try
    Objects:=TQList.Create;
    for i:=0 to NewAddonsList.Count-1 do
    begin
      ProgressIndicatorIncrement;
      NewAddonsList[i].acces;
      (*
        Entities
      *)
      NewAddonsList[i].FindAllSubObjects('', TTreemapSpec, QObject, Objects);
      for j:=0 to Objects.Count-1 do
      begin
        if not(Objects[j] is TTreeMapSpec) then
          continue;
        OldEntity:=TTreeMapSpec(Objects.Items1[j]);
        Entity:=TTreeMapSpec(EntityTBX_2.FindSubObject(OldEntity.Name, TTreeMapSpec, QObject));
        if (Entity = nil) then
        begin
          if pos('_',OldEntity.name)<>0 then
          begin
            tb:=copy(OldEntity.name, 1,pos('_', OldEntity.Name))+'* entities';
            Group:=QToolboxGroup(EntityTBX_2.SubElements.FindName(tb+EntityTBX_2.typeinfo));
            if (Group = nil) then
            begin
              Group:=QToolBoxGroup.Create(tb, EntityTBX_2);
              EntityTBX_2.Subelements.add(Group);
            end
          end
          else
          begin
            Group:=EntityTBX_2;
          end;
          Entity:=TTreeMapSpec(ConstructQObject(OldEntity.GetFullName, Group));
          Group.SubElements.Add(Entity);
        end;
        for k:=0 to OldEntity.Specifics.Count-1 do
        begin
          if Entity.Specifics.IndexOfName(OldEntity.Specifics.Names[k])=-1 then
          begin
            Entity.Specifics.Add(OldEntity.Specifics[k]);
          end;
        end;
      end;
      Objects.Clear;
      (*
        Entity Forms
      *)
      NewAddonsList.Items1[i].FindAllSubObjects('', QFormCfg, QObject, Objects);
      for j:=0 to Objects.Count-1 do
      begin
        OldForm:=QFormCfg(Objects.Items1[j]);
        Form:=QFormCfg(entityForms.FindSubObject(OldForm.Name, QFormCfg, QObject));
        if (Form = nil) then
        begin
          Form:=QFormCfg(ConstructQObject(OldForm.GetFullName, entityForms));
          entityForms.SubElements.Add(Form);
        end;
        Form.Flags := Form.flags or ofTreeViewSubElement;
        for k:=0 to OldForm.Subelements.Count-1 do
        begin
          OldFormEl:=OldForm.Subelements[k];
          FormEl:=Form.FindSubObject(OldFormEl.Name, QObject, QObject);
          if FormEl=nil then
          begin
            FormEl:=ConstructQObject(OldFormEl.GetFullName, Form);
            Form.Subelements.Add(FormEl);
          end;
          FormEl.Flags := FormEl.flags or ofTreeViewSubElement;
          for l:=0 to OldFormEl.Specifics.Count-1 do
          begin
            if FormEl.Specifics.IndexOfName(OldFormEl.Specifics.Names[l])=-1 then
            begin
              FormEl.Specifics.Add(OldFormEl.Specifics[l]);
            end;
          end;
        end;
      end;
      Objects.Clear;
    end;
    Objects.Free;

    TBX.Flags := TBX.flags or ofTreeViewSubElement;
    entityForms.Flags := entityForms.flags or ofTreeViewSubElement;
  finally
    ProgressIndicatorStop;
  end;

  NewAddonsList.free;
  ExplorerFromObject(FParent).Refresh;
end;

 {------------------------}

initialization
  RegisterQObject(QQuakeCtx, 'a');
end.

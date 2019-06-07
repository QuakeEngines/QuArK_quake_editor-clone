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
               Function GetAllBSPsFiles: TQList;
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
     ToolBoxGroup, Game, QkMapObjects, FormCfg, QkExplorer, QkApplPaths,
     QkForm, Travail, QkFormCfg, QkExceptions, Logging, ExtraFunctionality;

 {------------------------}

type
 TTexOpacityInfo = record
                    Loaded: Boolean;
{DECKER 2003-03-12. These seems unused:
                    Count: Byte;
                    Reserved1, Reserved2: Byte;
/DECKER 2003-03-12}
                    Opacity: array[0..31] of Byte;
                   end;

var
 gTexFlagsTransparentInfo: TTexOpacityInfo;

procedure LoadTexFlagsTransparentInfo;
var
 I, J: Integer;
 NoOfOpacityFlags: Integer;
 BitValue: Integer;
 BitIndex: Integer;
 OpacityPercentage: Integer;
 Li: TQList;
 Val32: array[0..63] of Single;
begin
 FillChar(gTexFlagsTransparentInfo.Opacity, SizeOf(gTexFlagsTransparentInfo.Opacity), 255);
 gTexFlagsTransparentInfo.Loaded:=True;
 Li:=GetQuakeContext;
 for J:=0 to Li.Count-1 do
  begin
   // Get the list of "<bit-value> <opacity-%> [<bit-value> <opacity-%> ...]" from the .QRK
   NoOfOpacityFlags := Li[J].GetFloatsSpecPartial('TexFlagsTransparent', Val32);
   for I:=0 to NoOfOpacityFlags div 2 - 1 do
    begin
     BitValue:=Round(Val32[I*2]);
     BitIndex:=0;
     while not Odd(BitValue) and (BitValue<>0) do
      begin
       Inc(BitIndex);
       BitValue:=BitValue shr 1;
      end;
     if BitValue=1 then
      begin
       OpacityPercentage:=Round((1-Val32[I*2+1])*255);
       if OpacityPercentage<0 then OpacityPercentage:=0;
       if OpacityPercentage>255 then OpacityPercentage:=255;
       gTexFlagsTransparentInfo.Opacity[BitIndex]:=OpacityPercentage;
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

 if not gTexFlagsTransparentInfo.Loaded then
  LoadTexFlagsTransparentInfo;

 L:=0;
 repeat
  if Odd(Flags) and (gTexFlagsTransparentInfo.Opacity[L]<Result) then
   Result:=gTexFlagsTransparentInfo.Opacity[L];
  Flags:=Flags shr 1;
  Inc(L);
 until Flags=0;
end;

function OpacityToFlags(Flags: Integer; Alpha: Integer) : Integer;
var
 L, Best, DistMin, Dist: Integer;
begin
 if not gTexFlagsTransparentInfo.Loaded then
  LoadTexFlagsTransparentInfo;

 Best:=0;
 DistMin:=255-Alpha;
 for L:=Low(gTexFlagsTransparentInfo.Opacity) to High(gTexFlagsTransparentInfo.Opacity) do
  if gTexFlagsTransparentInfo.Opacity[L]<255 then
   begin
    Dist:=Abs(Alpha-Integer(gTexFlagsTransparentInfo.Opacity[L]));
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
 gTexFlagsTransparentInfo.Loaded:=False;
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
  try
    for i:=0 to l.count-1 do
    begin
      Result.Add(ExactFileLink(ConcatPaths([dir, l.strings[i]]), nil, false));
    end;
  except
    Result.Free;
    raise;
  end;
end;

Function FindFiles(const dir, filter: String): TQList;
var
  f: TSearchRec;
  f_e: Integer;
begin
  Result:=TQList.Create;
  try
    f_e:=FindFirst(filter, faAnyFile, F);
    try
      while f_e=0 do
      begin
        Result.add(ExactFileLink(ConcatPaths([dir, f.name]), nil, false));
        f_e:=FindNext(F);
      end;
    finally
      FindClose(f);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function qMakeAddonFromQctx(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=Nil;
 try
   with QkObjFromPyObj(self) as QQuakeCtx do
     MakeAddonFromQctx;
   Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function qMakeTexturesFromQctx(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=Nil;
 try
   with QkObjFromPyObj(self) as QQuakeCtx do
     MakeTexturesFromQctx;
   Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
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

Function QQuakeCtx.GetAllBSPsFiles: TQList;
var
  paks: TQList;
  dir: string;
  pak: QFileObject;
  p_f: QPakFolder;
  j: Integer;
begin
  dir:=ConcatPaths([QuakeDir, Specifics.Values['GameDir']]);
  paks:=OpenFiles(dir, ListPakFiles(dir));
  try
    Result:=FindFiles(ConcatPaths([dir, GameMapPath]), ConcatPaths([QuakeDir, Specifics.Values['GameDir'], GameMapPath, '*.bsp']));
    ProgressIndicatorStart(5458,paks.count);
    while (Paks.count <> 0) do
    begin
      ProgressIndicatorIncrement;
      pak:=QFileObject(paks[0]);
      try
        pak.acces;
      except
        continue;
      end;
      p_f:=QPakFolder(pak.FindSubObject(GameMapPath, QPakFolder, QPakFolder));
      if p_f<>nil then
      begin
        for j:=0 to p_f.subelements.count-1 do
        begin
          if p_f.subelements[j] is QBsp then
            Result.add(p_f.subelements[j]);
        end;
      end;
      paks.delete(0);
    end;
  finally
    paks.free;
  end;
  ProgressIndicatorStop;
end;

Procedure QQuakeCtx.MakeTexturesFromQctx;
var
  // Objects for getting bsp list
  bsps: TQList;
  // Objects for creating new addon
  addonRoot: QFileObject;
  TexRoot: QToolBox;
  TexFolders, oldTexRoot: QObject;
  Function GetObject(const nname, ntypeinfo, s: String): QObject;
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
  BSPs:=GetAllBSPsFiles;
  try
    addonRoot:=QFileObject(FParent);
    if addonRoot = nil then
      raise InternalE('addonRoot = nil');

    if addonRoot.specifics.IndexOfName('Description')=-1 then
      addonRoot.specifics.Add(format('Description=Addon for %s',[Specifics.Values['GameDir']]));
    (*
      Build Textures
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
        TexRoot.Specifics.Add('Root='+TexFolders.GetFullName);
      if TexRoot.SubElements.FindShortName(TexFolders.Name)=nil then
        TexRoot.SubElements.Add(TexFolders)
      else
        TexFolders.free;
      TexFolders.FParent:=TexRoot;
    end;
    ProgressIndicatorStart(5458,bsps.Count);
    try
      while (bsps.Count <> 0) do
      begin
        (*
          Textures from .bsps (Q1, H2)
        *)
        oldTexRoot:=QBSP(bsps[0]).GetTextureFolder;
        if (oldTexRoot <> nil) then
        begin
          oldTexRoot.fParent:=TexFolders;
          TexFolders.Subelements.Add(oldTexRoot);
        end;

        bsps.delete(0);
      end;
    finally
      ProgressIndicatorStop;
    end;
  finally
    bsps.free;
  end;
  ExplorerFromObject(FParent).Refresh;
end;

function IsAllNumbers(const arg: string): Boolean;
const
  Numbers = '0123456789-.';
var
  i: integer;
begin
  result:=true;
  for i:=1 to length(arg) do
    result:=result and (System.pos(arg[i], Numbers)<>0);
end;

function IsNumbersSeperated(const arg: string): Integer;
const
  Numbers = '0123456789-. ';
var
  i: integer;
  b: boolean;
begin
  b:=true;
  for i:=1 to length(arg) do
    b:=b and (System.pos(arg[i], Numbers)<>0);
  result:=0;
  if b then
  begin
    for i:=1 to length(arg) do
    begin
      if arg[i]=' ' then
      begin
        result:=result+1;
      end;
    end;
    inc(result);
  end;
end;

Procedure QQuakeCtx.MakeAddonFromQctx;
var
  i,j: integer;
  count: Integer;
  tb: string;
  ext: string;
  e_lump: String;
//  NewAddonsList: TQList;
  text_entities, e_sl: TStringList;
  // Objects for creating new addon
  addonRoot: QFileObject;
  TBX: QToolBox;
  entityTBX: QToolBoxGroup;
  Entity: TTreeMapSpec;
  entityForms:QFormContext;
  hasOrigin: Boolean;

  eSpec: QObject;
  eForm: QFormCfg;
  opt_tbx: QToolBoxGroup;
  entities: TQList;
  (*

  *)
  function GuessArgType(const spec, arg: string): String; // returns
  begin
    Result:='E';
    if spec='color' then Result:='L' else
    if spec='origin' then Result:='EF3' else
    if IsAllNumbers(arg) then result:='EF' else
    if IsNumbersSeperated(arg)<>0 then Result:='EF'+IntToStr(IsNumbersSeperated(arg));
  end;

  (*

  *)
  procedure GetEntities;
  var
    bsps: TQList;
    bsp: QBsp;
//    e: QObject;
  begin
    BSPs:=GetAllBSPsFiles;
    try
      ProgressIndicatorStart(5458, bsps.count);
      try
        e_lump:='';
        while bsps.count<>0 do
        begin
//          if not (bsps[0] is QBsp) then
  //          raise InternalE('Error: bsp list contains non QBSP object!');
          bsp := QBsp( bsps[0] );
          bsp.Acces;
          e_lump:=e_lump + bsp.GetEntityLump();
          bsps.Delete(0);
          ProgressIndicatorIncrement;
          Application.ProcessMessages;
        end;
      finally
        ProgressIndicatorStop;
      end;
    finally
      bsps.free;
    end;
  end;

  (*
    Go through list of .bsps and create addon based on each
  *)
  Procedure CreateAddons;
  var
    ExistingAddons: QFileObject;
  begin
    ExistingAddons:=MakeAddonsList;
    try
      StringListFromEntityLump(e_lump, ExistingAddons, text_entities);
    finally
      ExistingAddons.AddRef(-1);
    end;
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
  
  function getword(i: Integer): String;
  begin
    if (i=0) or (i>1) then result:='entities' else result:='entity';
  end;

begin
  text_entities:=TStringList.Create;
  try
    //NewAddonsList:=TQList.Create; // a list of AddonRoot (.qrk objects)
    GetEntities;
    CreateAddons;
    count:=text_entities.count;

    addonRoot:=QFileObject(FParent);
    if addonRoot = nil then
      raise InternalE('Error obtaining Root (addonRoot = nil)');

    if addonRoot.specifics.IndexOfName('Description')=-1 then
      addonRoot.specifics.Add(format('Description=Addon for %s',[Specifics.Values['GameDir']]));
    (*
      Now build entities found in .bsp files
    *)
//    ProgressIndicatorStart(5458,NewAddonsList.Count);
    try
      if text_entities.count>0 then // don't create new folders if entities don't exist
      begin
        TBX:=QToolBox.Create('Toolbox Folders', addonRoot);
        TBX.Flags := TBX.flags or ofTreeViewSubElement;
        addonRoot.Subelements.Add(TBX);
        TBX.Specifics.Add('ToolBox=New map items...');
        EntityTBX:=QToolBoxGroup.Create(Format('%s entities', [Specifics.Values['GameDir']]), TBX);
        TBX.Subelements.Add(EntityTBX);
        TBX.Specifics.Add('Root='+EntityTBX.GetFullName);
        (*
          Convert {...} entites to :e entities
        *)
        for i:=0 to text_entities.count-1 do
        begin
          Application.ProcessMessages;
          ext:=':e';
          e_sl:=TStringList(text_entities.Objects[i]);
          if e_sl.IndexOfName('model')<>-1 then
          begin
            if e_sl.Values['model'][1]='*' then
            begin
              ext:=':b';
            end
          end;
          Entity:=TTreeMapSpec(ConstructQObject(e_sl.Values['classname']+ext, EntityTBX));
          for j:=0 to e_sl.count-1 do
          begin
            if e_sl.Names[j] = 'classname' then continue // remove classname specific
            else if (e_sl.Names[j] = 'model') and (e_sl.Values['model'][1]='*') then continue; // remove model specifics if it points to a BSP model
            Entity.Specifics.Add(e_sl.Strings[j]);
          end;
          Entity.Specifics.Add(';desc=(insert description here)');
          if ext=':b' then
            Entity.Specifics.Add(';incl=defpoly');
          if pos('_',Entity.name)<>0 then
          begin
            tb:=copy(Entity.name, 1,pos('_', Entity.Name))+'* entities';
            opt_tbx:=QToolboxGroup(EntityTBX.SubElements.FindName(tb+EntityTBX.typeinfo));
            if (opt_tbx = nil) then
            begin
              opt_tbx:=QToolBoxGroup.Create(tb, EntityTBX);
              EntityTBX.Subelements.add(opt_tbx);
              Entity.FParent:=opt_tbx;
              opt_tbx.SubElements.Add(Entity);
            end
            else
            begin
              Entity.FParent:=opt_tbx;
              opt_tbx.SubElements.Add(Entity);
            end;
          end
          else
          begin
            EntityTBX.SubElements.Add(Entity);
            Entity.FParent:=EntityTBX;
          end;
        end;
        (*
          Create forms for each entity & guess type for each spec
        *)
        entityForms:=QFormContext.Create('Entity forms', addonRoot);
        addonRoot.SubElements.Add(entityForms);
        entities:=TQList.Create;
        try
          EntityTBX.FindAllSubObjects('',TTreeMapSpec, QObject, Entities);
          for i:=0 to entities.Count-1 do
          begin
            Entity:=TTreeMapSpec(entities[i]);
            eForm:=QFormCfg.Create(Entity.Name, entityForms);
            entityForms.Subelements.Add(eForm);
            hasOrigin:=false;
            eForm.Flags := eForm.flags or ofTreeViewSubElement;
            for j:=Entity.Specifics.Count-1 downto 0 do
            begin
              if Entity.Specifics.Names[j][1]=';' then continue; // skip ;desc, ;incl etc
              eSpec:=QInternal.Create(Entity.Specifics.Names[j], eForm);
              if uppercase(Entity.Specifics.Names[j])='ORIGIN' then
                hasOrigin:=true;
              eSpec.Specifics.Add('txt=&');
              eSpec.Specifics.Add('hint=(insert hint here)');
              eSpec.Specifics.Add('typ='+GuessArgType(Entity.Specifics.Names[j], Entity.Specifics.Values[Entity.Specifics.Names[j]]));
              eSpec.Flags := eSpec.flags or ofTreeViewSubElement;
              Entity.Specifics.Delete(J);
              eForm.SubElements.Add(eSpec);
            end;
            if (Entity.TypeInfo = ':e') and (hasOrigin) then
              Entity.Specifics.Add('Origin=0 0 0'); // Hack for map editor
          end;
        finally
          entities.free;
        end;
        entityForms.Flags := entityForms.flags or ofTreeViewSubElement;
      end;
    finally
      ProgressIndicatorStop;
    end;
  finally
    text_entities.free;
  end;
  ExplorerFromObject(FParent).Refresh;

  showmessage(format('%d new %s found in directory "%s"',[ count,getword(count), Specifics.Values['GameDir']]));
end;

 {------------------------}

initialization
  RegisterQObject(QQuakeCtx, 'a');
end.

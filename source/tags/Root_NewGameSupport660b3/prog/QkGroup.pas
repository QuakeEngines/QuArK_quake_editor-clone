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
Revision 1.15  2007/11/29 16:36:03  danielpharos
Fix an access violation when trying to drag an item to an empty place in the treeview.

Revision 1.14  2007/10/14 21:48:56  danielpharos
Fix the frame-dragging in the Model Editor.

Revision 1.13  2007/09/10 10:24:18  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.12  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2001/06/05 18:39:33  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.9  2001/03/20 21:46:07  decker_dk
Updated copyright-header

Revision 1.8  2001/01/21 15:48:25  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.7  2001/01/15 19:19:58  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.6  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.4  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkGroup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, QkObjects, QkFileObjects, Menus, TB97,
  QkForm, StdCtrls, Buttons, Python;

type
 QExplorerGroup = class(QFileObject)
                  protected
                    function OpenWindow(nOwner: TComponent) : TQForm1; override;
                  public
                    class function TypeInfo: String; override;
                    procedure ObjectState(var E: TEtatObjet); override;
                    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                    procedure CopierObjets(Complet: Boolean);
                    procedure ReadObjectStream(F: TStream);
                    procedure WriteObjectStream(F: TStream);
                    function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                    function AccepteDestination(Q: QObject) : Boolean;
                    procedure GO(Method: Integer);
                    procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
                    procedure RenderAsText;
                    function AllowDrag(Target: QObject) : Boolean;
                  end;

type
  TFQGroup = class(TQForm1)
    Panel1: TPanel;
    Label1: TLabel;
    GoPanel: TPanel;
    GoBtn: TBitBtn;
    Panel2: TPanel;
    ListBox1: TListBox;
    procedure GoBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
  public
  end;

{type
 TBasicSibling = class(TInfoEnreg1)
                 public
                   Target: QFileObject;
                   procedure WriteSibling(const Path: String; Obj: QObject); override;
                 end;}

 {------------------------}

var
 g_DelayedClipboardGroup : QExplorerGroup = Nil;
 g_LargeDataInClipboard : Boolean = False;

function ClipboardGroup : QExplorerGroup;
function CopyToOutside(Gr: QExplorerGroup) : QExplorerGroup;
procedure InitGamesMenu(L: TStrings);

 {------------------------}

implementation

uses Qk1, QkMapPoly, Setup, QkInclude, QkMacro, Quarkx, Travail, QkQuakeC, QkObjectClassList;

{$R *.DFM}

 {------------------------}

var
 Chain1: TClipboardHandler;

function ClipboardGroup : QExplorerGroup;
begin
 Result:=QExplorerGroup.Create(LoadStr1(5122), Nil);
end;

function CopyToOutside(Gr: QExplorerGroup) : QExplorerGroup;
{var
 I: Integer;}
begin
 Gr.Acces;
 Result:=Gr.Clone(Nil, False) as QExplorerGroup;
{for I:=0 to Result.SubElements.Count-1 do
  ProcessMacros(Result.SubElements[I], Gr.SubElements[I]);}
 ProcessMacros(Result, Gr);
end;

function CollerObjets(PasteNow: QObject) : Boolean;
var
 Source: TMemoryStream;
 H: THandle;
 P: PChar;
 SourceTaille: Integer;
begin
 Result:=IsClipboardFormatAvailable(g_CF_QObjects);
 if Result and Assigned(PasteNow) then
  begin
   Source:=Nil; try
   OpenClipboard(g_Form1.Handle); try
   H:=GetClipboardData(g_CF_QObjects);
   if H=0 then
    Result:=False
   else
    begin
     SourceTaille:=GlobalSize(H);
     Source:=TMemoryStream.Create;
     Source.SetSize(SourceTaille);
     Move(GlobalLock(H)^, Source.Memory^, SourceTaille);
     GlobalUnlock(H);
    end;
   finally CloseClipboard; end;
   if Result then
    (PasteNow as QExplorerGroup).ReadObjectStream(Source);
   finally Source.Free; end;
  end
 else
  if not Result and IsClipboardFormatAvailable(CF_Text) and OpenClipboard(0) then
   begin
    H:=GetClipboardData(CF_Text);
    if H<>0 then
     begin
      P:=GlobalLock(H);
      if P<>Nil then
       begin
        Result:=CheckFileSignature(P);
        if Result and Assigned(PasteNow) then
         ConstructObjsFromText(PasteNow, P, StrLen(P));
       end;
      GlobalUnlock(H);
     end;
    CloseClipboard;
   end;
 Result:=Result or Chain1(PasteNow);
end;

procedure InitGamesMenu(L: TStrings);
var
 list, obj: PyObject;
 I, Count: Integer;
 P: PChar;
begin
 try
  list:=GetQuarkxAttr('buildmodes');
  if list<>Nil then
   begin
    Count:=PyObject_Length(list);
    if Count<0 then Exit;
    for I:=0 to Count-1 do
     begin
      obj:=PyList_GetItem(list, I);
      if obj=Nil then Exit;
      P:=PyString_AsString(obj);
      if P=Nil then Exit;
      L.Add(P);
     end;
   end;
 finally
  PythonCodeEnd;
 end;
end;

(*procedure TBasicSibling.WriteSibling(const Path: String; Obj: QObject);
var
 Q1: QObject;
begin
 Target.Acces;
 Obj.Name:=Path;
 Q1:=Target.SubElements.FindName(Path);
 if Q1=Nil then
  Target.SubElements.Add(Obj)
 else
  Target.SubElements[Target.SubElements.IndexOf(Q1)]:=Obj;
end;*)

 {------------------------}

class function QExplorerGroup.TypeInfo;
begin
 Result:='.qrk';
end;

function QExplorerGroup.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=TFQGroup.Create(nOwner);
end;

procedure QExplorerGroup.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiExplorerGroup;
 E.MarsColor:=clBlue;
end;

class procedure QExplorerGroup.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5120);
{Info.FileExtCount:=1;}
 Info.FileExt{[0]}:=772;
{Info.DefaultExt[0]:='qrk';}
 Info.WndInfo:=[wiForm1];
 Info.QuArKFileObject:=True;
end;

procedure QExplorerGroup.CopierObjets(Complet: Boolean);
var
 H: THandle;
 M: TMemoryStream;
 HasText: Boolean;
begin
 M:=TMemoryStream.Create; try
 WriteObjectStream(M);
 H:=GlobalAlloc(gmem_Moveable or gmem_DDEShare, M.Size);
 Move(M.Memory^, GlobalLock(H)^, M.Size);
 GlobalUnlock(H);
 finally M.Free; end;
 OpenClipboard(g_Form1.Handle); try
 EmptyClipboard;
 SetClipboardData(g_CF_QObjects, H);
 HasText:=False;
 if SubElements.Count=1 then
  SubElements[0].CopyExtraData(HasText);
 if not HasText then
  if Complet or (GetObjectSize(Nil, False) < 16*1024) then
   RenderAsText
  else
   begin
    SetClipboardData(CF_TEXT, 0);
    AddRef(+1);
    g_DelayedClipboardGroup:=Self;
    g_LargeDataInClipboard:=True;
   end;
 finally CloseClipboard; end;
end;

procedure QExplorerGroup.RenderAsText;
var
 L: TStringList;
 Data: String;
 P: PChar;
 H: THandle;
begin
 g_DelayedClipboardGroup.AddRef(-1);
 g_DelayedClipboardGroup:=Nil;
 L:=TStringList.Create; try
 ConvertObjsToText(Self, L, False);
 Data:=L.Text;
 finally L.Free; end;
 H:=GlobalAlloc(gmem_Moveable or gmem_DDEShare, Length(Data)+1);
 if H<>0 then
  begin
   P:=GlobalLock(H);
   Move(Data[1], P^, Length(Data)+1);
   GlobalUnlock(H);
   SetClipboardData(CF_TEXT, H);
  end;
end;

procedure QExplorerGroup.ReadObjectStream(F: TStream);
var
 OldReadFormat: Integer;
begin
 OldReadFormat:=ReadFormat;
 try
  ReadFormat:=rf_Default;
  LoadFromStream(F);
 finally
  ReadFormat:=OldReadFormat;
 end;
end;

procedure QExplorerGroup.WriteObjectStream(F: TStream);
var
 I: Integer;
 Links, Modif: TQList;
 Info1: {TBasicSibling;}TInfoEnreg1;
begin
 Acces;
{Info1:=TBasicSibling.Create; try
 Info1.Target:=Self;
 WriteSiblingsTo(Info1);}
 try
  Links:=Nil;
  Modif:=Nil;
  try
   for I:=0 to SubElements.Count-1 do
    with SubElements[I] do
     begin
      Acces;
      if Flags and ofFileLink <> 0 then
       begin
        if Links=Nil then
         Links:=TQList.Create;
        Links.Add(Self.SubElements[I]);
        Flags:=Flags and not ofFileLink;
       end;
      if Flags and ofModified <> 0 then
       begin
        if Modif=Nil then
         Modif:=TQList.Create;
        Modif.Add(Self.SubElements[I]);
       end;
     end;
   Info1:=TInfoEnreg1.Create; try
   Info1.Format:=rf_Default;
   Info1.F:=F;
   SaveFile1(Info1);
   finally Info1.Free; end;
  finally
   if Links<>Nil then
    begin
     for I:=Links.Count-1 downto 0 do
      with Links[I] do
       Flags:=Flags or ofFileLink;
     Links.Free;
    end;
   if Modif<>Nil then
    begin
     for I:=Modif.Count-1 downto 0 do
      with Modif[I] do
       Flags:=Flags or ofModified;
     Modif.Free;
    end;
  end;
 finally
  FixupAllReferences;
 end;
{finally Info1.Free; end;}
end;

function QExplorerGroup.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[Q is QFileObject];
end;

function QExplorerGroup.AccepteDestination(Q: QObject) : Boolean;
var
 I: Integer;
begin
 Result:=False;
 if Q=Nil then
  Exit;
 for I:=0 to SubElements.Count-1 do
  if ieCanDrop in Q.IsExplorerItem(SubElements[I]) then
   begin
    Result:=True;
    Exit;
   end;
end;

 {------------------------}

procedure QExplorerGroup.GO;
var
 maplist, extracted: PyObject;
 FirstMap, CfgFile: String;
 QCList: TQList;
 args: PyObject;
begin
 extracted:=PyList_New(0);
 maplist:=PyList_New(0);
 try
  FirstMap:='';
  QCList:=TQList.Create; try
  Go1(maplist, extracted, FirstMap, QCList);
  CfgFile:='';
  CompilerPatches(QCList, CfgFile);
  finally QCList.Free; end;
  if (FirstMap='') or (FirstMap='*') then
   args:=Py_BuildValueX('OiOs', [maplist, Method, extracted, PChar(CfgFile)])
  else
   args:=Py_BuildValueX('OiOss', [maplist, Method, extracted, PChar(CfgFile), PChar(FirstMap)]);
  Py_XDECREF(CallMacroEx(args, 'buildmaps'));
 finally
  Py_DECREF(maplist);
  Py_DECREF(extracted);
  PythonCodeEnd;
 end;
end;

procedure QExplorerGroup.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
var
 I: Integer;
begin
 Acces;
 ProgressIndicatorStart(175, SubElements.Count); try
 for I:=0 to SubElements.Count-1 do
  begin
   if SubElements[I] is QFileObject then
    QFileObject(SubElements[I]).Go1(maplist, extracted, FirstMap, QCList);
   ProgressIndicatorIncrement;
  end;
 finally ProgressIndicatorStop; end;
end;

function QExplorerGroup.AllowDrag(Target: QObject) : Boolean;
var
  I: Integer;
  Q: QObject;
begin
  Result:=True;
  for I:=0 to SubElements.Count-1 do
  begin
    Q:=SubElements[I];
    if ((Target=nil) or (Q.ClassType <> Target.ClassType)) and (Q.IsAllowedParent(Target) = false) then
    begin
      Result:=False;
      Exit;
    end;
  end;
end;

 {------------------------}

procedure TFQGroup.wmInternalMessage(var Msg: TMessage);
var
 Q: QObject;
 I, HiddenCount, HiddenSize: Integer;
begin
 case Msg.wParam of
  wp_AfficherObjet:
    if FileObject<>Nil then
     begin
      Panel2.Caption:=FmtLoadStr1(5401, [FileObject.Name]);
      HiddenCount:=0;
      HiddenSize:=0;
      for I:=0 to FileObject.SubElements.Count-1 do
       begin
        Q:=FileObject.SubElements[I];
        if not (Q is QFileObject) then
         begin
          Inc(HiddenCount);
          Inc(HiddenSize, Q.GetObjectSize(Nil, False));
         end;
       end;
      Label1.Visible:=HiddenCount>0;
      if HiddenCount>0 then
       Label1.Caption:=FmtLoadStr1(5402, [(HiddenSize+512) div 1024, HiddenCount]);
     end;
 end;
 inherited;
end;

function TFQGroup.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QExplorerGroup) and inherited AssignObject(Q, State);
end;

procedure TFQGroup.GoBtnClick(Sender: TObject);
begin
 if FileObject is QExplorerGroup then
  QExplorerGroup(FileObject).GO(ListBox1.ItemIndex);
end;

procedure TFQGroup.FormCreate(Sender: TObject);
begin
 inherited;
 InitGamesMenu(ListBox1.Items);
 ListBox1.ItemIndex:=0;
end;

initialization
  RegisterQObject(QExplorerGroup, 'z');
  Chain1:=g_ClipboardChain;
  g_ClipboardChain:=CollerObjets;
end.

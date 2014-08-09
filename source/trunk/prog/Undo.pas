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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.17  2010/11/06 17:10:15  danielpharos
Consted a bunch of strings.

Revision 1.16  2009/07/15 10:38:00  danielpharos
Updated website link.

Revision 1.15  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.14  2008/11/06 21:11:50  danielpharos
Made type Specifics soft-coded: Will lated be changed into a new, yet-to-be-defined type.

Revision 1.13  2008/10/07 21:05:14  danielpharos
Fixed a typo in a comment.

Revision 1.12  2008/09/06 15:57:01  danielpharos
Moved exception code into separate file.

Revision 1.11  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.9  2001/11/11 01:28:49  tiglari
icon leak fixes

Revision 1.8  2001/06/05 18:42:24  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.7  2001/03/20 21:41:11  decker_dk
Updated copyright-header

Revision 1.6  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.5  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.4  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.3  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit Undo;

interface

uses SysUtils, WinProcs, Classes, Controls, Forms, Dialogs,
     QkObjects, QkExplorer, QkFileObjects;

(*var
 MaxUndoLevel  : Integer = 50;
 MinUndoLevel  : Integer = 10;
 UndoBufferSize: Integer = 2*1024*1024;   { 2 MB }
 FreeMaxMemory : Boolean = True;*)

type
 TUndoObject = class
               private
                FText: String;
               protected
                procedure DoUndo; virtual; abstract;
                function MemorySize(Loaded: TQStream; LoadNow: Boolean) : Integer; virtual;
               {function CheckUsage: Boolean; virtual; abstract;}
               public
                procedure Faire;
                constructor Create(const nText: String);
                property Text: String read FText;
               end;

 TAtomicUndo = class(TUndoObject)
               protected
                procedure DoOp1; virtual; abstract;
                procedure DoAtom; virtual; abstract;
                procedure DoOp2; virtual; abstract;
                procedure DoUndo; override;
               end;

 TQObjectUndo = class(TAtomicUndo)
                private
                 Ancien: QObject;
                 Nouveau: QObject;
                 {CommonParent: QObject;}
                 TopLevelExplorer: TQkExplorer;
                protected
                 procedure DoOp1; override;
                 procedure DoAtom; override;
                 procedure DoOp2; override;
                 property Actuel: QObject read Ancien;
                 function MemorySize(Loaded: TQStream; LoadNow: Boolean) : Integer; override;
                {function CheckUsage: Boolean; override;}
                public
                 InsererAvant: QObject;
                 constructor Create(const nText: String; nAncien, nNouveau: QObject);
                 destructor Destroy; override;
                end;

 TObjPropUndo = class(TAtomicUndo)
                protected
                 AppliqueA: QObject;
                 procedure DoOp1; override;
                 procedure DoOp2; override;
                 function MemorySize(Loaded: TQStream; LoadNow: Boolean) : Integer; override;
                {function CheckUsage: Boolean; override;}
                public
                 constructor Create(const nText: String; nAppliqueA: QObject);
                 destructor Destroy; override;
                end;

 TSpecificUndo = class(TObjPropUndo)
                 private
                  Spec: String;
                  Arg: String;
                  Position: Integer;
                  SubStrStart: Integer;
                  SubStrEnd: Integer;
                 protected
                  procedure DoAtom; override;
                  function MemorySize(Loaded: TQStream; LoadNow: Boolean) : Integer; override;
                 public
                  constructor Create(const nText, nSpec, nArg: String; nPosition: Integer; nAppliqueA: QObject);
                 end;

 TSetSpecificsUndo = class(TObjPropUndo)
                     private
                      OldSpec: TSpecificsList;
                     protected
                      procedure DoAtom; override;
                      function MemorySize(Loaded: TQStream; LoadNow: Boolean) : Integer; override;
                     public
                      constructor Create(const nText: String; nSpec: TSpecificsList; nAppliqueA: QObject);
                      destructor Destroy; override;
                     end;

 TNameUndo = class(TObjPropUndo)
             private
              Name: String;
             protected
              procedure DoAtom; override;
             public
              constructor Create(const nText, nName: String; nAppliqueA: QObject);
             end;

 TMoveUndo = class(TAtomicUndo)
             private
              FElement: QObject;
              Destination: QObject;
              InsererAvant: QObject;
             {function CheckUsage: Boolean; override;}
             protected
              procedure DoOp1; override;
              procedure DoAtom; override;
              procedure DoOp2; override;
              function MemorySize(Loaded: TQStream; LoadNow: Boolean) : Integer; override;
             public
              constructor Create(const nText: String; nElement: QObject; nDestination: QObject; nInsererAvant: QObject);
              destructor Destroy; override;
              property Element: QObject read FElement;
             end;

 TMultipleUndo = class(TUndoObject)
                 private
                  FToDo: TList;
                 protected
                  procedure DoUndo; override;
                  function MemorySize(Loaded: TQStream; LoadNow: Boolean) : Integer; override;
                 {function CheckUsage: Boolean; override;}
                 public
                  constructor Create(const nText: String);
                  constructor CreateList(const nText: String; nList: TList);
                  destructor Destroy; override;
                  property ToDo: TList read FToDo;
                 end;

const
 sp_Supprime  = -1;
 sp_Auto      = -2;
 sp_AutoSuppr = -3;
 sp_Fin       = MaxInt;

 na_Select   = 1;
 na_Action   = 2;
 na_Local    = 4;
 na_Cancel   = 8;

 CannotUndo  = 599;

type
 PUndoRoot = ^TUndoRoot;
 TUndoRoot = record
              Root: QFileObject;
              UndoList: TList;
              Undone: Integer;
              Next: PUndoRoot;
              Tag: Boolean;
             end;

var
{Modified: Boolean;}
 g_ListeActions: TList = Nil;
{ExplorersActifs: TList;}
 g_NiveauAction: Integer = 0;

procedure ClearUndo(Reste: Integer; Charge: TQStream);
procedure Action(Q: QObject; UndoObject: TUndoObject);
procedure ActionEx(nNiveau: Integer; Q: QObject; UndoObject: TUndoObject);
procedure AccepteAction(Q: QObject; UndoObject: TUndoObject);
procedure UndoOne(R: PUndoRoot);
procedure RedoOne(R: PUndoRoot);
procedure FusionneGroupe(Group: QObject);
procedure DebutAction;
procedure FinActionEx(nNiveau: Integer; Q: QObject; const Texte: String);
procedure FinAction(Q: QObject; const Texte: String);
procedure FreeAction(Q: QObject);
procedure AnnuleAction;
function ActionQObject(T: QObject) : QObject;
procedure CloseUndoRoot(Q: QObject);
function GetUndoRoot(Q: QObject) : PUndoRoot;
function NeedUndoRoot(Q: QObject) : PUndoRoot;
{procedure ReplaceUndoRoot(Old, New: QObject);}
procedure GetListOfModified(L: TQList);
procedure SaveTag(Q: QObject);
procedure EnumUndoRoots(L: TStrings; Roots: TList);
procedure ListeActionsCopie(Source, Dest: QObject; Efface: Boolean);

 {------------------------}

implementation

uses Travail, Setup, QkForm, TbUndoMenu, Qk1, Quarkx, QkExceptions, Logging;

var
 UndoRoots: PUndoRoot = Nil;
 GlobalUndoList: TList = Nil;

 {------------------------}

function MaxUndoLevel: Integer;
begin
  Result:=Round(SetupSubSet(ssGeneral, 'Memory').GetFloatSpec('MaxUndo', 50));
  if Result<=0 then
    Result:=1;
end;

function MinUndoLevel: Integer;
begin
  Result:=Round(SetupSubSet(ssGeneral, 'Memory').GetFloatSpec('MinUndo', 10));
  if Result<=0 then
    Result:=1;
end;

function UndoBufferSize: Integer;
begin
  Result:=Round(SetupSubSet(ssGeneral, 'Memory').GetFloatSpec('UndoBufferSize', 2) * (1024*1024));
end;

 {------------------------}

procedure CloseUndoRoot(Q: QObject);
var
 P: ^PUndoRoot;
 Root: PUndoRoot;
 I: Integer;
begin
 if UndoRoots=Nil then Exit;
 P:=@UndoRoots;
 while P^.Root<>Q do
  begin
   P:=@P^^.Next;
   if P^=Nil then Exit;  { not found }
  end;
 Root:=P^;
 P^:=Root^.Next;   { breaks the link in the linked list }
 with Root^.UndoList do
  for I:=Count-1 downto 0 do
   begin
    GlobalUndoList.Remove(Items[I]);
    TUndoObject(Items[I]).Free;  { free the Undo objects }
   end;
 Root^.UndoList.Free;
 Dispose(Root);
end;

function GetUndoRoot(Q: QObject) : PUndoRoot;
begin  { searches the root object over Q }
 while Q.Flags and (ofFileLink or ofTreeViewSubElement) <> ofFileLink do
  begin
   Q:=Q.FParent;
   if Q=Nil then
   begin
     Log(LOG_VERBOSE, 'GetUndoRoot: Unable to find parent of object ' + Q.Name);
     Raise EError(5530);
   end;
  end;
 Result:=UndoRoots;   { searches the TUndoRoot structure in the list }
 while (Result<>Nil) and (Result^.Root<>Q) do
  Result:=Result^.Next;
end;

procedure RootChanges(R: PUndoRoot);
begin
 if g_Form1.Explorer.Roots.IndexOf(R^.Root)>=0 then
  UpdateAddOnsContent;
end;

function NeedUndoRoot(Q: QObject) : PUndoRoot;
var
 S: String;
begin
 Result:=GetUndoRoot(Q);
 if Result=Nil then
  begin  { creates a new TUndoRoot structure }
   repeat
    if Q.Flags and ofWarnBeforeChange <> 0 then
     begin
      S:=(Q as QFileObject).Filename;
      if S='' then
       S:=LoadStr1(5528)
      else
       S:=FmtLoadStr1(5529, [S]);
      if Q.Specifics.Values['QuArKProtected']<>'' then
       S:=S+LoadStr1(5553);
      if MessageDlg(S, mtWarning, mbOkCancel, 0) <> mrOk then
       Abort;
      Q.Flags:=Q.Flags and not ofWarnBeforeChange;
     end;
    if Q.Flags and (ofFileLink or ofTreeViewSubElement) = ofFileLink then
     Break;  { found the needed root }
    Q:=Q.FParent;
   until False;
   New(Result);
   Result^.Root:=Q as QFileObject;
   Result^.UndoList:=TList.Create;
   Result^.Undone:=0;
   Result^.Next:=UndoRoots;
   UndoRoots:=Result;
  end;
 RootChanges(Result);
end;

{procedure ReplaceUndoRoot(Old, New: QObject);
var
 R: PUndoRoot;
begin
 R:=UndoRoots;
 while R<>Nil do
  begin
   if R^.Root=Old then
    R^.Root:=New as QFileObject;
   R:=R^.Next;
  end;
end;}

procedure GetListOfModified(L: TQList);
var
 R: PUndoRoot;
 I: Integer;
 F: TForm;
begin
 R:=UndoRoots;
 while R<>Nil do
  begin
   R^.Tag:=R^.Root.Flags and ofModified = 0;
   R:=R^.Next;
  end;
 for I:=0 to Screen.FormCount-1 do
  begin
   F:=Screen.Forms[I];
   if F.Visible then
    F.Perform(wm_InternalMessage, wp_FileMenu, fm_SaveTagOnly);
  end;
 R:=UndoRoots;
 while R<>Nil do
  begin
   if not R^.Tag then
    L.Add(R^.Root);
   R:=R^.Next;
  end;
end;

procedure SaveTag(Q: QObject);
var
 P: PUndoRoot;
begin
 P:=GetUndoRoot(Q);
 if P<>Nil then
  P^.Tag:=True;
end;

procedure EnumUndoRoots(L: TStrings; Roots: TList);
var
 R: PUndoRoot;
begin
 R:=UndoRoots;
 while R<>Nil do
  begin
   with R^.Root do
    L.Add(Name+TypeInfo);
   Roots.Add(R);
   R:=R^.Next;
  end;
end;

(*procedure DoneUndo;
begin
 AnnuleAction;
 ClearUndo(0, Nil);
 UndoList.Free;
end;

procedure InitUndo;
begin
 UndoList:=TList.Create;
 Undone:=0;
 g_ListeActions:=Nil;
{ExplorersActifs:=TList.Create;}
{AddExitProc(DoneUndo);}
end;*)

procedure AnnuleAction;
var
 I: Integer;
begin
 if g_ListeActions<>Nil then
  begin
   for I:=g_ListeActions.Count-1 downto 0 do
    TUndoObject(g_ListeActions[I]).Free;
   g_ListeActions.Free;
   g_ListeActions:=Nil;
  end;
end;

procedure RemoveGlobalUndo(I: Integer);
var
 U: TUndoObject;
 R: PUndoRoot;
 J: Integer;
begin
 U:=TUndoObject(GlobalUndoList[I]);
 GlobalUndoList.Delete(I);
 R:=UndoRoots;
 repeat
  if R=Nil then
   Raise InternalE('RemoveGlobalUndo');
  J:=R^.UndoList.IndexOf(U);
  if J>=0 then Break;
  R:=R^.Next;
 until False;
 R^.UndoList.Delete(J);
 U.Free;
end;

procedure ClearUndo(Reste: Integer; Charge: TQStream);
var
 I, J, FreeSize: Integer;
 Remove: Boolean;
 U: TUndoObject;
 R: PUndoRoot;
begin
 R:=UndoRoots;
 while R<>Nil do
  with R^ do
   begin
    for I:=UndoList.Count-1 downto UndoList.Count-Undone do
     begin   { can't redo anything now }
      GlobalUndoList.Remove(UndoList[I]);
      TUndoObject(UndoList[I]).Free;
      UndoList.Delete(I);
      Dec(Undone);
     end;
    R:=Next;
   end;
 if GlobalUndoList=Nil then
  GlobalUndoList:=TList.Create;
 if Reste<GlobalUndoList.Count then  { Reste might be MaxInt, let's be careful }
  for I:=GlobalUndoList.Count-Reste-1 downto 0 do
   RemoveGlobalUndo(I)
 else
  if GlobalUndoList.Count=0 then
   Exit;
 FreeSize:=UndoBufferSize;
 Remove:=False;
 for I:=GlobalUndoList.Count-1 downto 0 do
  begin
   U:=TUndoObject(GlobalUndoList[I]);
   if not Remove then
  (*if not U.CheckUsage then
     Remove:=True      { no more useful }
    else*)
     begin
      Dec(FreeSize, U.MemorySize(Charge, False));
      if FreeSize<=0 then
       begin   { undo buffer overflow }
        Remove:=True;
        Reste:=GlobalUndoList.Count-I-1;  { number of objects to keep }
        if Reste < MinUndoLevel then
         begin  { warn the user }
          J:=5214+Ord(Reste>0);
          if MessageDlg(FmtLoadStr1(J, [Reste]) + FmtLoadStr1(5213, [Reste]),
           mtWarning, mbOkCancel, 0) <> mrOk then
            Abort;
         end;
       end;
     end;
   if Remove then   { Undo Object has to be removed }
    RemoveGlobalUndo(I);
  end;
 if Charge<>Nil then   { load data now }
  for I:=GlobalUndoList.Count-1 downto 0 do
   TUndoObject(GlobalUndoList[I]).MemorySize(Charge, True);
end;

procedure DebutAction;
begin
 AnnuleAction;
 g_ListeActions:=TList.Create;
end;

procedure FinAction(Q: QObject; const Texte: String);
var
 U: TUndoObject;
begin
 if g_ListeActions=Nil then Exit;
 if g_ListeActions.Count > 1 then
  U:=TMultipleUndo.CreateList(Texte, g_ListeActions)
 else
  begin
   if g_ListeActions.Count = 1 then
    begin
     U:=TUndoObject(g_ListeActions.First);
     U.FText:=Texte;
    end
   else
    U:=Nil;
   g_ListeActions.Free;
  end;
 g_ListeActions:=Nil;
 Action(Q, U);
end;

procedure FreeAction(Q: QObject);
var
 Effectue, I: Integer;
 CurrentExplorer: TQkExplorer;
begin
 if g_ListeActions=Nil then Exit;
 try
  try
   g_NiveauAction:=g_NiveauAction or na_Action;
   CurrentExplorer:=ExplorerFromObject(Q);
   if CurrentExplorer<>Nil then
    begin
     CurrentExplorer.Invalidate;
     CurrentExplorer.TMSelUnique:=Nil;
    end;
  {if CurrentExplorer=Nil then
    Data:=Nil
   else
    Data:=CurrentExplorer.MsgUndo(muBegin, Nil);}
  {try}
    Effectue:=-1;
    try
     for I:=0 to g_ListeActions.Count-1 do
      begin
       TUndoObject(g_ListeActions[I]).Faire;
       Effectue:=I;
      end;
    {if CurrentExplorer<>Nil then
      CurrentExplorer.MsgUndo(muOk, Data);}
    except
     g_NiveauAction:=g_NiveauAction and not na_Action;
     for I:=Effectue downto 0 do
      TUndoObject(g_ListeActions[I]).DoUndo;
     FinOpDansScene;
     Raise;
    end;
  {finally
    if CurrentExplorer<>Nil then
     CurrentExplorer.MsgUndo(muEnd, Data);
   end;}
  finally
   g_NiveauAction:=g_NiveauAction and not na_Action;
  {UndoObject.Free;}
  end;
 except
  OperationDansScene(Q, asModifie, Nil);
  Raise;
 end;
 AnnuleAction;
end;

procedure Action(Q: QObject; UndoObject: TUndoObject);
var
 Effectue: Boolean;
 Data: Pointer;
 CurrentExplorer: TQkExplorer;
 R: PUndoRoot;
begin
 UndoDlgOp1;
 try
  try
   AnnuleAction;
   if UndoObject=Nil then Exit;
   ClearUndo(MaxUndoLevel-1, Nil);
   R:=NeedUndoRoot(Q);
   g_NiveauAction:=g_NiveauAction or na_Action;
   CurrentExplorer:=ExplorerFromObject(Q);
  {Modified:=True;}
  {ModificationScene;}
  (*NouveauNumero:=-1;
   AncienControl:=Nil;
   if Form4Actif then
    begin  { mode Map Editor }
     if Screen.ActiveForm=Form4 then
      begin
       AncienControl:=Form4.ActiveControl;
       if (AncienControl<>Nil) and (AncienControl.Name='') then
        AncienControl:=Nil;
      end;
     if (Form4.Notebook1.PageIndex=1) and (Form4.ListView2.Selected<>Nil) then
      NouveauNumero:=Form4.ListView2.Selected.Index;
     Focus:=Form4.TMSelFocus;
     Form4.EffacerSelection;
     with Form4 do
      if PlanSelDist<>PlanSel_Aucun then
       begin
        PlanSelDist:=PlanSel_Aucun;
       {PaintBox1.Invalidate;
        PaintBox2.Invalidate;}
        ScrollBox1.Invalidate;
        ScrollBox2.Invalidate;
       end;
     Inc(Form4.FSelecting);
     TForm3D.Modifs;
    end
   else  { mode Model Editor }
    begin
     ModelDsgn.AnnuleMode;
     Focus:=Nil;
    end; *)
   if CurrentExplorer=Nil then
    Data:=Nil
   else
    Data:=CurrentExplorer.MsgUndo(muBegin, Nil);
   try
    Effectue:=False;
    try
    {if Form4Actif then
      Form4.FSelection1:=Focus;}
     UndoObject.Faire;
     Effectue:=True;
     if CurrentExplorer<>Nil then
      CurrentExplorer.MsgUndo(muOk, Data);
    {if Form4Actif then
      begin
       if (Form4.FSelection1<>Nil) and not Odd(Form4.FSelection1.SelMult) then
        Form4.FSelection1:=Nil;
       Form4.TestsDUsage(Form4.FSelection1, NouveauNumero);
       if AncienControl<>Nil then
        PostMessage(Form4.Handle, wm_InternalMessage, wp_RestoreFocus,
         LongInt(AncienControl));
      end;}
     R^.UndoList.Add(UndoObject);
     GlobalUndoList.Add(UndoObject);
     UndoObject:=Nil;
    except
    {CurrentExplorer.MsgUndo(muExceptBegin, Data);}
    {if Form4Actif then
      Form4.FSelection1:=Nil;}
     g_NiveauAction:=g_NiveauAction and not na_Action;
     if Effectue then
      UndoObject.DoUndo;
     FinOpDansScene;
    {CurrentExplorer.MsgUndo(muExceptEnd, Data);}
    {if Form4Actif then
      begin
       Form4.TMFocus:=Nil;
       Form4.TestsDUsage(Nil, -1);
      end;}
     Raise;
    end;
   finally
    if CurrentExplorer<>Nil then
     CurrentExplorer.MsgUndo(muEnd, Data);
   end;
  finally
   g_NiveauAction:=g_NiveauAction and not na_Action;
   UndoObject.Free;
  end;
 except
  OperationDansScene(Q, asModifie, Nil);
  Raise;
 end;
end;

procedure ActionEx(nNiveau: Integer; Q: QObject; UndoObject: TUndoObject);
begin
  g_NiveauAction:=nNiveau;
  try
    Action(Q, UndoObject);
  finally
    g_NiveauAction:=0;
  end;
end;

procedure FinActionEx(nNiveau: Integer; Q: QObject; const Texte: String);
begin
  g_NiveauAction:=nNiveau;
  try
    FinAction(Q, Texte);
  finally
    g_NiveauAction:=0;
  end;
end;

procedure AccepteAction(Q: QObject; UndoObject: TUndoObject);
var
 R: PUndoRoot;
begin
 try
  AnnuleAction;
  if UndoObject=Nil then Exit;
  ClearUndo(MaxUndoLevel-1, Nil);
  R:=NeedUndoRoot(Q);
  R^.UndoList.Add(UndoObject);
  GlobalUndoList.Add(UndoObject);
 except
  UndoObject.Free;
  OperationDansScene(Q, asModifie, Nil);
  Raise;
 end;
end;

(*procedure ReplaceRoot(OldRoot, NewRoot: QObject);
var
 I: Integer;
begin
 for I:=0 to UndoList.Count-1 do
  with TUndoObject(UndoList[I]) do
   if Root = OldRoot then
    Root:=NewRoot;
end;

procedure CloseRoot(Root: QObject);
var
 I: Integer;
 Remove: Boolean;
 U: TUndoObject;
begin
 Remove:=False;
 I:=UndoList.Count-Undone;
 while I<UndoList.Count do
  begin
   U:=TUndoObject(UndoList[I]);
   Remove:=Remove or (U.Root = Root);
   if Remove then
    begin
     U.Free;
     UndoList.Delete(I);
     Dec(Undone);
    end
   else
    Inc(I);
  end;
 Remove:=False;
 for I:=UndoList.Count-Undone-1 downto 0 do
  begin
   U:=TUndoObject(UndoList[I]);
   Remove:=Remove or (U.Root = Root);
   if Remove then
    begin
     U.Free;
     UndoList.Delete(I);
    end;
  end;
end;*)

procedure UndoOne;
begin
 RootChanges(R);
 UndoDlgOp1;
 with TUndoObject(R^.UndoList[R^.UndoList.Count-R^.Undone-1]) do
  begin
   if FText=LoadStr1(CannotUndo) then
    Raise EError(5567);
   Faire;
  end;
 Inc(R^.Undone);
{Modified:=True;}
 FinOpDansScene;
end;

procedure RedoOne;
begin
 RootChanges(R);
 UndoDlgOp1;
 TUndoObject(R^.UndoList[R^.UndoList.Count-R^.Undone]).Faire;
 Dec(R^.Undone);
{Modified:=True;}
 FinOpDansScene;
end;

(*procedure UndoOne;
begin
{if Form4Actif then
  begin
   Form4.TMSelUnique:=Nil;
   TForm3D.Modifs;
  end;}
 with TUndoObject(UndoList[UndoList.Count-Undone-1]) do
  begin
   ControlerExplorer(CurrentExplorer);
   CurrentExplorer.MsgUndo(muOneBegin, Nil);
   DoUndo(False);
   Inc(Undone);
   Modified:=True;
   CurrentExplorer.MsgUndo(muOneEnd, Nil);
  end;
{if Form4Actif then
  Form4.TestsDUsage(Nil, -1);}
end;

procedure RedoOne;
begin
{if Form4Actif then
  begin
   Form4.TMSelUnique:=Nil;
   TForm3D.Modifs;
  end;}
 with TUndoObject(UndoList[UndoList.Count-Undone]) do
  begin
   ControlerExplorer(CurrentExplorer);
   CurrentExplorer.MsgUndo(muOneBegin, Nil);
   DoUndo(False);
   Dec(Undone);
   Modified:=True;
   CurrentExplorer.MsgUndo(muOneEnd, Nil);
  end;
{if Form4Actif then
  Form4.TestsDUsage(Nil, -1);}
end;*)

 {------------------------}

constructor TUndoObject.Create;
begin
 FText:=nText;
{enmemoire.add(self);}
end;

(*destructor tundoobject.destroy;
begin
{enmemoire.remove(self);}
end;*)

procedure TUndoObject.Faire;
begin
{if not CheckUsage then
  Raise EErrorFmt(5217, [FText]);}
 DoUndo;
end;

{function TUndoObject.Simplifie: TUndoObject;
begin
 Result:=Self;
end;}

function TUndoObject.MemorySize;
begin
 Result:=0;
end;

 {------------------------}

procedure TAtomicUndo.DoUndo;
begin
{$IFDEF Debug}
 DebugCheck;
{$ENDIF}
 DoOp1;     { detach objects from their relationships }
 try
  DoAtom;   { do the changes }
  try
   DoOp2;   { reattach object to their relationships }
  except    { if DoOp2 failed }
   DoAtom;  { undo the changes }
   Raise;
  end;
 except     { if DoOp2 or DoAtom failed }
  g_NiveauAction:=g_NiveauAction or na_Cancel;
  try
   DoOp2;    { reattach to the original relationships }
  finally
   g_NiveauAction:=g_NiveauAction and not na_Cancel;
  end;
  Raise;
 end;
{$IFDEF Debug}
 DebugCheck;
{$ENDIF}
end;

 {------------------------}

constructor TQObjectUndo.Create;
begin
 inherited Create(nText);
(*if nAncien<>Nil then
  begin
   CommonParent:=nAncien.FParent;
   {$IFDEF Debug}
   if (nNouveau<>Nil) and (CommonParent<>nNouveau.FParent) then
    Raise DebugError;
   {$ENDIF}
  end
 else
  if nNouveau<>Nil then
   CommonParent:=nNouveau.FParent
  else
   CommonParent:=Nil;
 if CommonParent<>Nil then CommonParent.AddRef(+1);*)
 Ancien:=nAncien;
 if Ancien<>Nil then Ancien.AddRef(+1);
 Nouveau:=nNouveau;
 if Nouveau<>Nil then
  begin
   Nouveau.AddRef(+1);
   if (Ancien=Nil) or (Ancien.Flags or ofTreeViewSubElement <> 0) then
    Nouveau.Flags:=Nouveau.Flags or ofTreeViewSubElement;
  end;
 if (Ancien<>Nil) and (Nouveau<>Nil) and (Ancien.FParent<>Nil)
 and (Ancien.FParent=Nouveau.FParent) then
  InsererAvant:=Ancien.NextInGroup
 else
  InsererAvant:=Nil;
end;

(*function TQObjectUndo.CheckUsage;
begin
 CheckUsage:=((Ancien=Nil) or (Ancien.Flags and ofTvNode <> 0))
       {and ((Nouveau=Nil) or (Nouveau.Flags and ofTvNode <> 0))};
end;*)

function TQObjectUndo.MemorySize;
begin
 if Nouveau=Nil then
  Result:=0
 else
  Result:=Nouveau.GetObjectSize(Loaded, LoadNow);
end;

destructor TQObjectUndo.Destroy;
begin
 if Nouveau<>Nil then Nouveau.AddRef(-1);
 if Ancien<>Nil then Ancien.AddRef(-1);
{if CommonParent<>Nil then CommonParent.AddRef(-1);}
 inherited Destroy;
end;

procedure TQObjectUndo.DoOp1;
begin
{N0:=g_NiveauAction;
 g_NiveauAction:=g_NiveauAction or na_Select;}
 TopLevelExplorer:=Nil;
 if Ancien<>Nil then
  begin
   if Ancien.TopLevel then
    TopLevelExplorer:=ExplorerFromObject(Ancien);
   OperationDansScene(Ancien, asRetire, TopLevelExplorer);   { detach }
  end;
end;

procedure TQObjectUndo.DoOp2;
begin
{try}
  if Ancien<>Nil then
   begin
    OperationDansScene(Ancien, asAjoute, TopLevelExplorer);   { (re)attach }
    if g_NiveauAction and na_Local = 0 then
     Ancien.SetSelMult;
   end;
{finally
  g_NiveauAction:=N0;
 end;}
end;

procedure TQObjectUndo.DoAtom;
var
 Temp, AncienSuivant, AncienParent: QObject;
 I: Integer;
begin
 AncienSuivant:=Nil;
 if Ancien<>Nil then
  begin
   {$IFDEF Debug}
  {if Ancien.FParent<>CommonParent then Raise DebugError;}
   {$ENDIF}
   if Ancien.FParent<>Nil then
    with Ancien.FParent do
     begin
      Modified;
      I:=SubElements.IndexOf(Ancien);
      if I>=0 then
       begin
        AncienParent:=Ancien.TvParent;
        SubElements.Delete(I);
        if I<SubElements.Count then
         AncienSuivant:=SubElements[I];
        if AncienParent<>Nil then
         try
          OperationDansScene(AncienParent, asRetireEnfant, TopLevelExplorer);
         except
          SubElements.Insert(I, Ancien);
          Raise;
         end;
       end
      {$IFDEF Debug}
      else Raise InternalE('QObjectUndo.DoAtom')
      {$ENDIF};
     end;
   if TopLevelExplorer<>Nil then
    begin   { Ancien was a top-level }
       { we can only replace a top-level with another one at the same position }
     if Nouveau=Nil then
      Raise InternalE('Nouveau=Nil');
     TopLevelExplorer.ReplaceRoot(Ancien, Nouveau);
    end;
  {if Nouveau<>Nil then
    ReplaceUndoRoot(Ancien, Nouveau);}
  {if g_DrawInfo.SelectionVisuelle=Ancien then
    g_DrawInfo.SelectionVisuelle:=Nouveau;}
  end;
{CurrentExplorer.MsgUndo(muQObjectUndo, Self);}
 if Nouveau<>Nil then
  begin
   Nouveau.Modified;
   {$IFDEF Debug}
  {if Nouveau.FParent<>CommonParent then Raise DebugError;}
   {$ENDIF}
   if Nouveau.FParent<>Nil then
    with Nouveau.FParent.SubElements do
     begin
      if InsererAvant=Nil then
       I:=-1
      else
       I:=IndexOf(InsererAvant);
      if (I<0) or (I>=Count) then
       Add(Nouveau)
      else
       Insert(I, Nouveau);
     end;
  end;
{if (Ancien<>Nil) and (Ancien=CurrentExplorer.FSelection1) then
  CurrentExplorer.FSelection1:=Nouveau;}
 Temp:=Ancien;
 Ancien:=Nouveau;
 Nouveau:=Temp;
 InsererAvant:=AncienSuivant;
end;

 {------------------------}

constructor TObjPropUndo.Create;
begin
 inherited Create(nText);
 AppliqueA:=nAppliqueA;
 AppliqueA.AddRef(+1);
end;

(*function TObjPropUndo.CheckUsage;
begin
 CheckUsage:=AppliqueA.Flags and ofTvNode <> 0;
end;*)

function TObjPropUndo.MemorySize;
begin
 Result:=AppliqueA.GetObjectSize(Loaded, LoadNow);
end;

destructor TObjPropUndo.Destroy;
begin
 AppliqueA.AddRef(-1);
 inherited Destroy;
end;

procedure TObjPropUndo.DoOp1;
begin
 OperationDansScene(AppliqueA, asAucun, Nil);   { just to tag the QkExplorer }
end;

procedure TObjPropUndo.DoOp2;
begin
 OperationDansScene(AppliqueA, asModifie, Nil);
 if g_NiveauAction and na_Local = 0 then
  begin
   AppliqueA.SetSelMult;
   AppliqueA.FixupAllReferences;
  end;
end;

 {------------------------}

constructor TSpecificUndo.Create;
begin
 inherited Create(nText, nAppliqueA);
 Spec:=nSpec;
 Arg:=nArg;
 if nPosition=sp_AutoSuppr then
  if nArg='' then
   Position:=sp_Supprime
  else
   Position:=sp_Auto
 else
  Position:=nPosition;
end;

function TSpecificUndo.MemorySize;
begin
 Result:=inherited MemorySize(Loaded, LoadNow) + Length(Arg);
end;

procedure TSpecificUndo.DoAtom;
var
 nPosition, nArgEnd: Integer;
 nArg: String;
begin
 AppliqueA.Modified;
 with AppliqueA.Specifics do
  begin
   nPosition:=IndexOfName(Spec);
   if Position=sp_Auto then
    if nPosition<0 then
     Position:=sp_Fin
    else
     Position:=nPosition;
   if nPosition>=0 then   { suppression de l'ancien Specific }
    begin
     nArg:=Strings[nPosition];
     nArg:=Copy(nArg, Pos('=',nArg)+1, MaxInt);
     Delete(nPosition);
     if Position>nPosition then
      Dec(Position);
    end
   else
    nArg:='';
   if SubStrStart>0 then
    begin
     Arg:=Copy(nArg, 1, SubStrStart-1)
        + Arg + Copy(nArg, SubStrEnd+1, MaxInt);
     SubStrStart:=0;
    end;
   if Position>=0 then   { ajout d'un Specific }
    if Position>=Count then
     Add(Spec+'='+Arg)
    else
     Insert(Position, Spec+'='+Arg)
   else
    Arg:='';
  end;
 Position:=nPosition;
 if (Arg<>'') and (nArg<>'') then
  begin  { cherche si seulement une sous-chaîne a changé :
    nArg est égal à Arg dans lequel la chaîne commençant en
    SubStrStart et finissant en SubStrEnd a été modifiée }
   repeat
    Inc(SubStrStart);
   until (SubStrStart>Length(Arg))
      or (Arg[SubStrStart]<>nArg[SubStrStart]);
   SubStrEnd:=Length(Arg);
   nArgEnd:=Length(nArg);
   while (SubStrEnd>SubStrStart)
     and (Arg[SubStrEnd]=nArg[nArgEnd]) do
    begin
     Dec(SubStrEnd);
     Dec(nArgEnd);
    end;
   nArg:=Copy(nArg, SubStrStart, 1+nArgEnd-SubStrStart);
  end;
 Arg:=nArg;
end;

 {------------------------}

constructor TSetSpecificsUndo.Create;
begin
 inherited Create(nText, nAppliqueA);
 OldSpec:=TSpecificsList.Create;
 OldSpec.Assign(nSpec);
end;

destructor TSetSpecificsUndo.Destroy;
begin
 OldSpec.Free;
 inherited;
end;

function TSetSpecificsUndo.MemorySize;
var
 I: Integer;
begin
 Result:=inherited MemorySize(Loaded, LoadNow);
 for I:=0 to OldSpec.Count-1 do
  Inc(Result, 6+Length(OldSpec[I]));
end;

procedure TSetSpecificsUndo.DoAtom;
var
 SwapSpec: TSpecificsList;
begin
 AppliqueA.Modified;
 SwapSpec:=AppliqueA.Specifics;
 AppliqueA.Specifics:=OldSpec;
 OldSpec:=SwapSpec;
end;

 {------------------------}

constructor TNameUndo.Create;
begin
 inherited Create(nText, nAppliqueA);
 Name:=nName;
end;

procedure TNameUndo.DoAtom;
var
 oName: String;
begin
 AppliqueA.Modified;
 oName:=AppliqueA.Name;
 AppliqueA.Name:=Name;
 Name:=oName;
end;

 {------------------------}

constructor TMoveUndo.Create;
begin
 inherited Create(nText);
 FElement:=nElement;
 FElement.AddRef(+1);
 Destination:=nDestination;
 InsererAvant:=nInsererAvant;
end;

(*function TMoveUndo.CheckUsage;
begin
 CheckUsage:=FElement.Flags and ofTvNode <> 0;
end;*)

function TMoveUndo.MemorySize;
begin
 if LoadNow then
  FElement.GetObjectSize(Loaded, True);
 Result:=0;  { FElement doesn't count for undo buffer size }
end;

destructor TMoveUndo.Destroy;
begin
 FElement.AddRef(-1);
 inherited;
end;

procedure TMoveUndo.DoOp1;
begin
{N0:=g_NiveauAction;
 g_NiveauAction:=g_NiveauAction or na_Select;}
 OperationDansScene(Element, asDeplace1, Nil);    { deattach }
end;

procedure TMoveUndo.DoOp2;
begin
{try}
  OperationDansScene(Element, asDeplace2, Nil);   { reattach }
  if g_NiveauAction and na_Local = 0 then
   Element.SetSelMult;
{finally
  g_NiveauAction:=N0;
 end;}
end;

procedure TMoveUndo.DoAtom;
var
 AncienParent: QObject;
 AncienSuivant: QObject;
 T: QObject;
 DestIndex: Integer;
begin
{$IFDEF Debug}
 DebugCheck;
{$ENDIF}
 T:=Destination;
 while T<>Nil do
  begin
   if T = Element then
    Raise EError(216);
   T:=T.TvParent;
  end;
 AncienSuivant:=Nil;
 AncienParent:=Element.TvParent;
 AncienParent.Modified;
 with AncienParent.SubElements do
  begin
   DestIndex:=IndexOf(Element);
   if DestIndex>=0 then
    begin
     Delete(DestIndex);
     if DestIndex<Count then
      AncienSuivant:=Items1[DestIndex];
     try
      OperationDansScene(AncienParent, asRetireEnfant, Nil);
     except
      Insert(DestIndex, Element);
      Raise;
     end;
    end
   {$IFDEF Debug}
   else Raise InternalE('MoveUndo.DoAtom')
   {$ENDIF};
  end;
 Destination.Modified;
 Element.TvParent:=Destination;
 with Destination.SubElements do
  begin
   if InsererAvant=Nil then
    DestIndex:=-1
   else
    DestIndex:=IndexOf(InsererAvant);
   if (DestIndex<0) or (DestIndex>=Count) then
    Add(Element)
   else
    Insert(DestIndex, Element);
  end;
 Destination:=AncienParent;
 InsererAvant:=AncienSuivant;
{$IFDEF Debug}
 DebugCheck;
{$ENDIF}
end;

(*constructor TFaceUndo.Create;
begin
 inherited Create(nText);
 Move(nNouveau.Params, NouveauParm, TailleSurfVis);
 AppliqueA:=nAppliqueA;
 Concerne:=nConcerne;
end;

procedure TFaceUndo.DoAtom;
var
 Ancien: TSurface;
begin
 Ancien:=AppliqueA^;
 Move(NouveauParm, AppliqueA^.Params, TailleSurfVis);
 Move(Ancien.Params, NouveauParm, TailleSurfVis);
 Concerne.SetSelMult;
end;

procedure TFaceUndo.DoProtected;
begin
 Concerne.OperationDansScene(asModifie);
end;*)

 {------------------------}

constructor TMultipleUndo.Create;
begin
 inherited Create(nText);
 FToDo:=TList.Create;
end;

constructor TMultipleUndo.CreateList(const nText: String; nList: TList);
begin
 inherited Create(nText);
 FToDo:=nList;
end;

(*function TMultipleUndo.CheckUsage;
var
 I: Integer;
begin
 Result:=False;
 for I:=0 to ToDo.Count-1 do
  if not TUndoObject(ToDo[I]).CheckUsage then
   Exit;
 CheckUsage:=True;
end;*)

function TMultipleUndo.MemorySize;
var
 I: Integer;
begin
 Result:=0;
 for I:=0 to ToDo.Count-1 do
  Inc(Result, TUndoObject(ToDo[I]).MemorySize(Loaded, LoadNow));
end;

destructor TMultipleUndo.Destroy;
var
 I: Integer;
begin
 for I:=ToDo.Count-1 downto 0 do
  TUndoObject(ToDo[I]).Free;
 ToDo.Free;
 inherited Destroy;
end;

procedure TMultipleUndo.DoUndo;
var
 I: Integer;
begin
 ProgressIndicatorStart(509, ToDo.Count);
 try
  I:=0;
  try
   while I<ToDo.Count do
    begin
     if ToDo[I]=Nil then
      ToDo.Delete(I)
     else
      begin
       TUndoObject(ToDo[I]).DoUndo;
       Inc(I);
      end;
     ProgressIndicatorIncrement;
    end;
  except
   while I>0 do
    begin  { annule les opérations effectuées avant l'exception }
     Dec(I);
     TUndoObject(ToDo[I]).DoUndo;
    end;
   Raise;
  end;
  for I:=0 to ToDo.Count div 2 - 1 do
   ToDo.Exchange(I, ToDo.Count-1-I);
 finally
  ProgressIndicatorStop;
 end;
{$IFDEF Debug}
 DebugCheck;
{$ENDIF}
end;

 {------------------------}

procedure FusionneGroupe(Group: QObject);
var
 I: Integer;
begin
 with Group do
 begin
  for I:=0 to SubElements.Count-1 do
   g_ListeActions.Add(TMoveUndo.Create('', SubElements[I], TvParent, Group));
 end;
 g_ListeActions.Add(TQObjectUndo.Create('', Group, Nil));
end;

function ActionQObject(T: QObject) : QObject;
begin
 Result:=T.Clone(T.FParent, False);
 g_ListeActions.Add(TQObjectUndo.Create('', T, Result));
end;

(*procedure ActionFace(Face: TFaceUndo);
begin
 Action(Face);
 Form4.SelectionneFace(Face.AppliqueA);
end;*)

procedure ListeActionsCopie(Source, Dest: QObject; Efface: Boolean);
var
 I, P: Integer;
 Q: QObject;
 SpecArg: String;
begin
 if Efface then
  g_ListeActions.Add(TSetSpecificsUndo.Create('', Source.Specifics, Dest))
 else
  for I:=0 to Source.Specifics.Count-1 do
   begin
    SpecArg:=Source.Specifics[I];
    P:=Pos('=', SpecArg);
    g_ListeActions.Add(TSpecificUndo.Create('', Copy(SpecArg,1,P-1), Copy(SpecArg,P+1,MaxInt), sp_Auto, Dest));
   end;
 for I:=0 to Dest.SubElements.Count-1 do
  begin
   Q:=Dest.SubElements[I];
   if Source.SubElements.IndexOf(Q)<0 then
    g_ListeActions.Add(TQObjectUndo.Create('', Q, Nil));
  end;
 for I:=0 to Source.SubElements.Count-1 do
  begin
   Q:=Source.SubElements[I];
   if Dest.SubElements.IndexOf(Q)<0 then
    g_ListeActions.Add(TQObjectUndo.Create('', Nil, Q.Clone(Dest, False)));
  end;
end;

initialization
 { InitUndo;}

finalization
  GlobalUndoList.Free;
end.

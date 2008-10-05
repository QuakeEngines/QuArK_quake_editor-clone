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
Revision 1.7  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2001/06/05 18:42:41  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.4  2001/03/20 21:37:46  decker_dk
Updated copyright-header

Revision 1.3  2000/10/11 19:01:08  aiv
Small updates

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}


unit form_Model;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkForm, QkImages, Python, PyMapView,
  StdCtrls, EnterEditCtrl, ExtCtrls, PyMath, qmatrices, qmath, QkMdlObject,
  QkTextures, QkSin, CursorScrollBox;

type
  TFQMdl = class(TQForm1)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    EnterEdit1: TEnterEdit;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure EnterEdit1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FRoot: QMdlObject;
    procedure ScrollBox1Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    procedure ReadSetupInformation(Level: Integer); override;
  public
    ScrollBox1: TPyMapView;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  end;

implementation

uses QuarkX, Setup, PyForms, Undo, QkModel, QkMapObjects, Qk3D;

{$R *.dfm}

function TFQMdl.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
  Result:=(Q is QModel) and (State<>cmWindow) and inherited AssignObject(Q, State);
end;

procedure TFQMdl.ReadSetupInformation(Level: Integer);
begin
  inherited;
  ScrollBox1.Invalidate;
  ScrollBox1.Color:=MapColors(lcVueXZ);
end;

procedure TFQMdl.wmInternalMessage(var Msg: TMessage);
var
  S: String;
  Min, Max, D: TVect;
  Racine: QObject;
  M: TMatrixTransformation;
begin
  if Msg.wParam=wp_AfficherObjet then begin
    if FileObject=Nil then
      S:=''
    else begin
      FileObject.Acces;
      S:=FileObject.Specifics.Values['Game'];
      if S='' then
        S:=LoadStr1(182)
      else
        S:=FmtLoadStr1(184, [S]);
    end;
    Label1.Caption:=S;
    if FileObject<>Nil then
      S:=FileObject.Specifics.Values['FileName'];
    EnterEdit1.Text:=S;
    if FileObject=Nil then
      Exit;
    S:=FileObject.Specifics.Values['Root'];
    if S='' then
      Exit;  { no data }
    Racine:=FileObject.SubElements.FindName(S);
    if (Racine=Nil) or not (Racine is QMdlObject) then
      Exit;  { no data }
    Racine.ClearAllSelection;
    Min.X:=-10;
    Min.Y:=-10;
    Min.Z:=-10;
    Max.X:=+10;
    Max.Y:=+10;
    Max.Z:=+10;
    QMdlObject(Racine).ChercheExtremites(Min, Max);

    D.X:=(ScrollBox1.ClientWidth-20)/(Max.X-Min.X);
    D.Y:=(ScrollBox1.ClientHeight-18)/(Max.Y-Min.Y);
    if D.Y<D.X then
      D.X:=D.Y;
    ScrollBox1.MapViewProj.Free;
    ScrollBox1.MapViewProj:=Nil;
    M:=MatriceIdentite;
    // Change To YZ View ie rotate around x axis by 90 deg.
    M[1,1]:=D.X;
    M[2,2]:=cos(90*(pi / 180))*D.X;
    M[3,2]:=sin(90*(pi / 180))*D.X;
    M[2,3]:=-M[3,2];
    M[3,3]:=M[2,2];
    ScrollBox1.MapViewProj:=GetMatrixCoordinates(M);
    ScrollBox1.HorzScrollBar.Range:=ScrollBox1.ClientWidth;
    ScrollBox1.VertScrollBar.Range:=ScrollBox1.ClientHeight;
    D.X:=(Min.X+Max.X)*0.5;
    D.Y:=(Min.Y+Max.Y)*0.5;
    D.Z:=(Min.Z+Max.Z)*0.5;
    FRoot:=QMdlObject(Racine);
    ScrollBox1.CentreEcran:=D;
  end else
    inherited;
end;

procedure TFQMdl.Button1Click(Sender: TObject);
begin
  with ValidParentForm(Self) as TQkForm do
    ProcessEditMsg(edOpen);
end;

procedure TFQMdl.EnterEdit1Accept(Sender: TObject);
var
  Q: QModel;
  S: String;
begin
  Q:=FileObject as QModel;
  S:=EnterEdit1.Text;
  Undo.Action(Q, TSpecificUndo.Create(LoadStr1(615), 'FileName',
    S, sp_AutoSuppr, Q));
end;

procedure TFQMdl.FormCreate(Sender: TObject);
begin
  inherited;
  ScrollBox1:=TPyMapView.Create(Self);
  ScrollBox1.MapViewObject^.Parent:=Nil;
  ScrollBox1.Parent:=Panel2;
  ScrollBox1.Align:=alClient;
 {FOldPaint:=ScrollBox1.OnPaint;}
  ScrollBox1.OnPaint:=ScrollBox1Paint;
end;

procedure TFQMdl.ScrollBox1Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
var
  Pen: HPen;
  Brush: HBrush;
begin
  if FRoot=Nil then
    Exit;
 {FOldPaint(Sender, PaintInfo);}
  Canvas.Handle:=DC;
  try
    SetupWhiteOnBlack(g_DrawInfo.DefWhiteOnBlack);
    ScrollBox1.MapViewProj.SetAsCCoord(DC);
    Pen:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_Pen));
    Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_Brush));
    g_DrawInfo.GreyBrush:=CreatePen(ps_Solid, 0, MapColors(lcOutOfView));
    try
      FRoot.Dessiner;
    finally
      SelectObject(g_DrawInfo.DC, Brush);
      SelectObject(g_DrawInfo.DC, Pen);
      DeleteObject(g_DrawInfo.GreyBrush);
    end;
  finally
    Canvas.Handle:=0;
  end;
end;

procedure TFQMdl.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FRoot:=Nil;
  inherited;
end;

end.

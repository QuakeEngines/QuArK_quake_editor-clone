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
Revision 1.75  2007/07/05 10:19:45  danielpharos
Moved the Quake .map format code to a separate file.

Revision 1.74  2007/06/06 21:27:38  danielpharos
Fix a problem where several things weren't saved in .map files.

Revision 1.73  2007/05/29 13:06:11  danielpharos
Added ignore-what-I-don't-understand support for iwmap 4 .map files, which is used by Call of Duty 2.

Revision 1.72  2007/05/24 20:42:45  danielpharos
Reserved gamecodes for Call of Duty 1 and 2.

Revision 1.71  2007/05/06 21:20:50  danielpharos
Fixed a bunch of missing BrushDef's in Q3 .map files.

Revision 1.70  2007/04/16 11:34:55  danielpharos
Added begin of support for EF2. Changed STVEF naming to be more consistent. Added ForceFaceFlags option.

Revision 1.69  2007/04/15 10:44:41  danielpharos
Doom 3 and Quake 4 will now always export the new mapversion files. Fixed a quote mistake in the brush texture names.

Revision 1.68  2007/04/14 11:25:12  danielpharos
Putted some end-of-line checks back in.

Revision 1.67  2007/04/12 22:41:31  danielpharos
Possible fix for entities disappearing when exporting .map file.

Revision 1.66  2007/04/12 22:18:13  danielpharos
Small fix, mainly for Quake 2.

Revision 1.65  2007/04/12 20:54:07  danielpharos
Another BIG update for Doom 3 and Quake 4: patchdef2 should be saving correctly now.

Revision 1.64  2007/04/12 15:28:11  danielpharos
Minor clean up.

Revision 1.63  2007/04/12 15:04:43  danielpharos
BIG moving around of code. All the .map save routines should now be in QkMap. This will allow easy changes, and will simplify future map format support.

Revision 1.62  2007/04/12 10:51:10  danielpharos
Added brushdef2 loading support.

Revision 1.61  2007/04/12 10:05:21  danielpharos
Improved texture name handling for Doom 3 and Quake 4.

Revision 1.60  2007/04/09 21:44:24  danielpharos
Started work on Doom 3 map version 2 and Quake 4 map version 3.

Revision 1.59  2007/03/25 13:52:25  danielpharos
Moved a few dictionnary words around.

Revision 1.58  2006/04/27 06:19:59  cdunde
To setup Quake4 support and code changes for Doom3 and material handling of both.
Related file changes
QkD3.pas
Added counter for phrasing of material list that kept their textures from
displaying and sometimes caused an overload and system lockup.
Added list of "Keywords" for the "Default texture" to display more of them.
QkMap.pas
To allow Quake4 Version 3 .mqp files to be read, previously set to only
allow Doom3 Version 1 .map files to be read and error on Version 2.
This still is the case for Doom3 with the above change for Quake4.
Setup.pas
Add game code "m" to start game support for Quake4.
QkTextures.pas
Added Quake4 game code mjQuake4 in Doom3 material file section
to point to Quake4 material files and display their related textures.

Revision 1.57  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.55  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.54  2004/11/11 18:43:49  alexander
fixed malformed comment line with 0D inside fooling D5 and not D6

Revision 1.53  2004/11/06 08:18:38  cdunde
Reversed last change to end statement due to compiling problem

Revision 1.52  2004/10/30 14:06:39  alexander
made it compileable

Revision 1.51  2004/05/21 01:11:10  cdunde
To add support for Sylphis game engine. Code by Harry Kalogirou.

Revision 1.50  2002/12/22 05:52:46  tiglari
restoring projecting points to planes, to make lighting work out

Revision 1.49  2002/12/21 09:25:13  tiglari
steamline v220 map-reading

Revision 1.48  2002/12/15 14:05:14  tiglari
improve efficiency of wc33 reading by eliminating unnecessary projections of
 texture points to planes (this should be tested more to check that it's
 really right)

Revision 1.47  2002/05/15 22:04:51  tiglari
fixes to map reading error recording (so that new maps can be created ..)

Revision 1.46  2002/05/15 00:08:38  tiglari
Record Map Errors for possible write to console or elsewhere

Revision 1.45  2002/05/14 21:24:50  tiglari
comment on bad texture scale while reading valve 220 maps

Revision 1.44  2002/05/07 23:23:46  tiglari
Mohaa map reading

Revision 1.43  2002/05/07 09:12:04  tiglari
prevent reversion to Quake mode when loading Torque maps (Desmond Fletcher)

Revision 1.42  2002/04/30 12:27:49  tiglari
Removed switch to Q3A mode when brushdef encountered: any game
 could have a brush-primitives-reading tool written for it.

Revision 1.41  2002/03/27 00:24:49  tiglari
delete/write mapversion 220 specific as needed (removed when map
 read, added back in if written out in V220 format).

Revision 1.40  2002/03/26 22:21:59  tiglari
support UseIntegralVertexes flag

Revision 1.39  2002/03/26 10:16:40  tiglari
Englishification: TPolyedre->TPolyhedron

Revision 1.38  2002/03/26 10:11:30  tiglari
get rid of soDisableEnhTex, soWriteValve220 (obsoleted by OutputMapFormat)

Revision 1.37  2001/07/19 09:49:46  tiglari
rearrange QMap hierarchy to make QHfmFile work properly

Revision 1.34  2001/06/05 18:39:33  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.33  2001/05/06 21:13:37  tiglari
fix mode change to q3a mode when reading stvef maps

Revision 1.32  2001/03/31 04:27:28  tiglari
WC33 mapversion 220 flag now kept, map will be written in 220 if present
 (concurrent update to QkMapPoly.pas)

Revision 1.31  2001/03/20 21:50:54  decker_dk
Updated copyright-header

Revision 1.30  2001/03/20 07:44:05  tiglari
wc33 reading offset fix

Revision 1.29  2001/03/18 01:35:48  tiglari
wc33 map format read (not fully tested, offsets might be off)

Revision 1.28  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.27  2001/02/17 09:15:20  tiglari
brush primitives texture reading working (at least sort of ...)

Revision 1.26  2001/02/17 06:10:12  tiglari
nonfunctional brush primitives reading (syntax OK, but brushes broken)

Revision 1.25  2001/02/07 19:28:43  decker_dk
Fixed problem in ReadSymbol() case '"', where introducing begin-end's created a wrongly statement-sequence. Always ALWAYS remember to put begin-ends at multiple if-statements!!!

Revision 1.24  2001/01/28 17:25:52  decker_dk
Made QMapFile.SaveFile() use the function 'CommentMapLine(string)' to write the .MAP comment-header.

Revision 1.23  2001/01/21 15:49:03  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.22  2001/01/15 19:20:19  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.21  2000/12/10 21:38:36  decker_dk
Able to read exponent-values from .MAP files

Revision 1.20  2000/11/19 15:31:49  decker_dk
- Added 'ImageListTextureDimension' and 'ImageListLoadNoOfTexAtEachCall' to
Defaults.QRK, for manipulating the TextureBrowser-TextureLists.
- Modified TFQWad.PopulateListView, so it reads the above settings.
- Changed two 'goto bail' statements to 'break' statements, in QkObjects.
- Found the problem in the .MAP exporting entity-numbering, and corrected it.
- Changed the '|' delimiting character in QObject.Ancestry to '->', as I think
it will be more readable in the .MAP file.
- Replaced the function-names:
  = SauverTexte         -> SaveAsText
  = SauverTextePolyedre -> SaveAsTextPolygon
  = SauverTexteBezier   -> SaveAsTextBezier
  = SauverSpec          -> SaveAsTextSpecArgs

Revision 1.19  2000/10/26 18:12:40  tiglari
fixed bug with a build flag (disableFP vs. Enhanced Tex mixup)

Revision 1.18  2000/10/26 17:09:52  tiglari
read soEnableBrushPrim

Revision 1.17  2000/09/24 23:45:16  alexander
committed tiglaris .map loading and bezier texture missing fix

Revision 1.16  2000/09/14 18:00:22  decker_dk
Moved QTexture1 and QTexture2 into QkQ1.PAS and QkQ2.PAS

Revision 1.15  2000/08/20 11:16:47  aiv
Removed (not req'd)

Revision 1.14  2000/07/30 11:21:03  tiglari
put in pascal version of map saving flag code from mapquakemenu.py
to make save flags apply when map is saved from  File menu.  Question:
why did Armin put it in the Python in the first place?

Revision 1.13  2000/07/19 18:23:26  decker_dk
Read mapversion 220 maps (WC33 format)

Revision 1.12  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.11  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.10  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.9  2000/06/03 10:46:49  alexander
added cvs headers
}


unit QkMap;

interface

uses
  Windows, Messages, Classes, Forms, Controls, StdCtrls, EnterEditCtrl, ExtCtrls, TB97,
  qmath, QkForm, QkObjects, QkFileObjects, QkMapObjects, PyMapView;

type
  TFQMap = class(TQForm1)
    Panel2: TPanel;
    Panel1: TPanel;
    Button1: TButton;
    EnterEdit1: TEnterEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure EnterEdit1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   {FOldPaint: TCSBPaintEvent;}
    FRoot: TTreeMap;
    procedure ScrollBox1Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    procedure ReadSetupInformation(Level: Integer); override;
  public
    ScrollBox1: TPyMapView;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  end;

 {------------------------}

implementation

uses
  Setup, Undo,
  Quarkx, qmatrices, Qk3D, PyMath, QkQuakeMap;

{$R *.DFM}

 {------------------------}

function TFQMap.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QMap) and (State<>cmWindow) and inherited AssignObject(Q, State);
end;

procedure TFQMap.ReadSetupInformation(Level: Integer);
begin
 inherited;
 ScrollBox1.Invalidate;
 ScrollBox1.Color:=MapColors(lcVueXY);
end;

procedure TFQMap.Button1Click(Sender: TObject);
begin
 with ValidParentForm(Self) as TQkForm do
  ProcessEditMsg(edOpen);
end;

procedure TFQMap.wmInternalMessage(var Msg: TMessage);
var
 S: String;
 Min, Max, D: TVect;
 Root: QObject;
 M: TMatrixTransformation;
begin
 if Msg.wParam=wp_AfficherObjet then
  begin
   if FileObject=Nil then
    S:=''
   else
    begin
     FileObject.Acces;
     S:=FileObject.Specifics.Values['Game'];
     if S='' then
      S:=LoadStr1(182)
     else
      S:=FmtLoadStr1(181, [S]);
    end;
   Label1.Caption:=S;
   if FileObject<>Nil then
    S:=(FileObject as QMap).GetOutputMapFileName;
   EnterEdit1.Text:=S;
   if FileObject=Nil then Exit;
   S:=FileObject.Specifics.Values['Root'];
   if S='' then Exit;  { no data }
   Root:=FileObject.SubElements.FindName(S);
   if (Root=Nil) or not (Root is TTreeMap) then Exit;  { no data }
   CheckTreeMap(TTreeMap(Root));
   Root.ClearAllSelection;

   Min.X:=-10;
   Min.Y:=-10;
   Min.Z:=-10;
   Max.X:=+10;
   Max.Y:=+10;
   Max.Z:=+10;
   TTreeMap(Root).ChercheExtremites(Min, Max);

   D.X:=(ScrollBox1.ClientWidth-20)/(Max.X-Min.X);
   D.Y:=(ScrollBox1.ClientHeight-18)/(Max.Y-Min.Y);
   if D.Y<D.X then D.X:=D.Y;
   ScrollBox1.MapViewProj.Free;
   ScrollBox1.MapViewProj:=Nil;
  {ScrollBox1.MapViewProj:=GetTopDownAngle(0, D.X, False);}
   M:=MatriceIdentite;
   M[1,1]:=D.X;
   M[2,2]:=-D.X;
   M[3,3]:=-D.X;
   ScrollBox1.MapViewProj:=GetMatrixCoordinates(M);
   ScrollBox1.HorzScrollBar.Range:=ScrollBox1.ClientWidth;
   ScrollBox1.VertScrollBar.Range:=ScrollBox1.ClientHeight;
   D.X:=(Min.X+Max.X)*0.5;
   D.Y:=(Min.Y+Max.Y)*0.5;
   D.Z:=(Min.Z+Max.Z)*0.5;
   FRoot:=TTreeMap(Root);
   ScrollBox1.CentreEcran:=D;
  end
 else
  inherited;
end;

procedure TFQMap.EnterEdit1Accept(Sender: TObject);
var
 Q: QMap;
 S: String;
begin
 Q:=FileObject as QMap;
 S:=EnterEdit1.Text;
 Undo.Action(Q, TSpecificUndo.Create(LoadStr1(615), 'FileName',
  S, sp_AutoSuppr, Q));
end;

procedure TFQMap.FormCreate(Sender: TObject);
begin
 inherited;
 ScrollBox1:=TPyMapView.Create(Self);
 ScrollBox1.MapViewObject^.Parent:=Nil;
 ScrollBox1.Parent:=Panel2;
 ScrollBox1.Align:=alClient;
{FOldPaint:=ScrollBox1.OnPaint;}
 ScrollBox1.OnPaint:=ScrollBox1Paint;
end;

procedure TFQMap.ScrollBox1Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
var
 Pen: HPen;
 Brush: HBrush;
begin
 if FRoot=Nil then Exit;
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

procedure TFQMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 FRoot:=Nil;
 inherited;
end;

end.

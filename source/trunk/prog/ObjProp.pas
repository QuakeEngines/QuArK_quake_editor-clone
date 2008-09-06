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
Revision 1.9  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.7  2001/03/20 21:47:27  decker_dk
Updated copyright-header

Revision 1.6  2001/01/15 19:19:21  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.5  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.4  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}


unit ObjProp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkForm, TB97, StdCtrls, ExtCtrls, QkObjects, QkFileObjects;

type
  TFormObjProp = class(TQkForm)
    Image1: TImage;
    Bevel1: TBevel;
    Label1: TLabel;
    Bevel2: TBevel;
    Label2: TLabel;
    Label3: TLabel;
    Bevel3: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListBox1: TListBox;
    PasteBtn: TToolbarButton97;
    OkBtn: TToolbarButton97;
    SaveBtn: TToolbarButton97;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure PasteBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    List: TQList;
    PasteTo: TQkForm;
  public
  end;

 {------------------------}

procedure ObjectProperties(QL: TList; nPasteTo: TQkForm);  { assume QL is non-empty }

 {------------------------}

implementation

uses Qk1, QkGroup, Quarkx, QkExceptions, PyImages, Python, Travail, QkPixelSet;

{$R *.DFM}

 {------------------------}

procedure ObjectProperties(QL: TList; nPasteTo: TQkForm);
var
 nQ: QObject;
 FormObjProp: TFormObjProp;
 I, Chk, Total: Integer;
 ConvertClass: QFileObjectClass;
 Info: TFileObjectClassInfo;
 E: TDisplayDetails;
 Sz: TPoint;
 CommonType: QObjectClass;
begin
 if QL.Count=1 then
  nQ:=QL[0]
 else
  nQ:=Nil;
 FormObjProp:=TFormObjProp.Create(Application);
 with FormObjProp do
  begin
   if nQ<>Nil then
    begin
     nQ.DisplayDetails(True, E);
     if E.Icon<>Nil then
      try
       Sz:=PyImage1(E.Icon)^.GetSize;
       Image1.Picture.Bitmap.Width:=Sz.X;
       Image1.Picture.Bitmap.Height:=Sz.Y;
       with Image1.Picture.Bitmap.Canvas do
        begin
        {Brush.Color:=clBtnFace;
         FillRect(Rect(0,0, Sz.X, Sz.Y));}
         PyImage1(E.Icon)^.Draw(Handle, 0,0, ColorToRGB(clBtnFace));
        end;
      finally
       Py_DECREF(E.Icon);
      end;
     Label1.Caption:=nQ.Name;
    end
   else
    Label1.Caption:=FmtLoadStr1(5407, [QL.Count]);
   Total:=0;
   for I:=0 to QL.Count-1 do
    Inc(Total, QObject(QL[I]).GetObjectSize(Nil, False));
   Label5.Caption:=FmtLoadStr1(5392, [(Total+512) div 1024]);
   SaveBtn.Enabled:=nQ is QFileObject;

   CommonType:=QObjectClass(QObject(QL[0]).ClassType);
   for I:=1 to QL.Count-1 do
    if QObject(QL[I]).ClassType <> CommonType then
     begin
      CommonType:=Nil;
      Break;
     end;
   if (CommonType<>Nil) and CommonType.InheritsFrom(QFileObject) then
    begin
     QFileObjectClass(CommonType).FileObjectClassInfo(Info);
     Label4.Caption:=Info.FileObjectDescriptionText + '   ';
     Chk:=-1;
     I:=1;
     repeat
      ConvertClass:=QFileObject(QL[0]).TestConversionType(I);
      if ConvertClass=Nil then Break;
      ConvertClass.FileObjectClassInfo(Info);
      ListBox1.Items.Add(Info.FileObjectDescriptionText);
      if ConvertClass=CommonType then
       Chk:=I-1;
      Inc(I);
     until False;
     ListBox1.ItemIndex:=Chk;
    end;
   if CommonType=Nil then
    Label4.Caption:=LoadStr1(5391)
   else
    Label4.Caption:=Label4.Caption+'[ '+CommonType.TypeInfo+' ]';
   if ListBox1.Items.Count=0 then
    begin
     Label6.Font.Color:=clGrayText;
     ListBox1.Color:=clBtnFace;
     ListBox1.Enabled:=False;
    end;
  {ListBox1Click(Nil);}
   List:=TQList.Create;
   for I:=0 to QL.Count-1 do List.Add(QL[I]);

   if Assigned(nPasteTo) then
    begin
     PasteTo:=nPasteTo;
     PasteBtn.Caption:=LoadStr1(5393);
     Caption:=LoadStr1(5396);

     Chk:=nPasteTo.ProcessEditMsg(edEdEnable);
     PasteBtn.Enabled:=Chk and edPasteObj = edPasteObj;
    end;
   Show;
  end;
end;

 {------------------------}

procedure TFormObjProp.FormCreate(Sender: TObject);
begin
 MarsCap.ActiveBeginColor:=clGreen;
 MarsCap.ActiveEndColor:=clTeal;
 UpdateMarsCap;
{SetFormIcon(}
end;

procedure TFormObjProp.OkBtnClick(Sender: TObject);
begin
 Close;
end;

procedure TFormObjProp.PasteBtnClick(Sender: TObject);
var
 Gr: QExplorerGroup;
 ConvertClass: QFileObjectClass;
 I: Integer;
begin
 Gr:=ClipboardGroup;
 Gr.AddRef(+1); try
 if ListBox1.ItemIndex>=0 then
  begin
   ProgressIndicatorStart(5457, List.Count); try
   for I:=0 to List.Count-1 do
    if List[I] is QPixelSet then
     QPixelSet(List[I]).LoadPixelSet
    else
     List[I].Acces;
   for I:=0 to List.Count-1 do
    begin
     ConvertClass:=(List[I] as QFileObject).TestConversionType(ListBox1.ItemIndex+1);
     if ConvertClass=Nil then
      MessageBeep(0)
     else
      if not QFileObject(Gr.SubElements[Gr.SubElements.Add(ConvertClass.Create(List[I].Name, Gr))])
             .ConversionFrom(QFileObject(List[I])) then
       Raise EError(5538);
     ProgressIndicatorIncrement;
    end;
   finally ProgressIndicatorStop; end;
   if Gr.SubElements.Count=0 then Exit;
  end
 else
  for I:=0 to List.Count-1 do
   Gr.SubElements.Add(List[I]);

  { copy to clipboard }
 Gr.CopierObjets(False);
 finally Gr.AddRef(-1); end;

 if PasteTo<>Nil then
  begin   { paste }
   ActivateNow(PasteTo);
   PasteTo.ProcessEditMsg(edPasteObj);
   Close;
  end;
end;

procedure TFormObjProp.FormDestroy(Sender: TObject);
begin
 List.Free;
end;

{procedure TFormObjProp.ListBox1Click(Sender: TObject);
begin
 PasteBtn.Enabled:=ListBox1.ItemIndex>=0;
end;}

procedure TFormObjProp.SaveBtnClick(Sender: TObject);
begin
 SaveObject(List[0] as QFileObject, fm_SaveAsFile, ListBox1.ItemIndex+1, Nil)
  .AddRef(-1);
end;

end.

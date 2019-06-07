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
unit PyImages;

interface

{$I DelphiVer.inc}
{$INCLUDE PyVersions.inc}

uses Windows, Messages, SysUtils, Classes, Graphics, CommCtrl, Python,
     QkObjects, Controls, Forms, ComCtrls, PyControls, QkForm, qmath;

type
 PyImage1 = ^TyImage1;
 PyImageList = ^TyImageList;
 TyImageList = object(TyObject)
                Handle: HImageList;
                BkgndColor: TColorRef;
                Images: array[Boolean] of TList;
                function NeedImage(nDisabled: Boolean; i: Integer) : PyImage1;
               end;
 TyImage1 = object(TyObject)
             ImageList: PyImageList;
             Index: Integer;
             BitmapCopy: HBitmap;
             DisabledBmp: TBitmap;
             function GetDisabledImage : PyImage1;
             function GetSize : TPoint;
             procedure Draw(DC: HDC; X,Y: Integer; Opaque: TColorRef);
             procedure GetIcon(DestIcon: TIcon);
             function GetMenuBitmap : HBitmap;
            end;
 TPyImageControl = class(TGraphicControl)
                   private
                     FOnClick, FOnDraw: PyObject;
                     FImage1: PyObject;
                     procedure SetImage1(value: PyObject);
                     procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
                     procedure MouseMoveEvt(Sender: TObject; Shift: TShiftState; X, Y: Integer);
                   protected
                     procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
                     procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
                     procedure Paint; override;
                     procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
                   public
                     ImageObject: PyControlF;
                     property Image1: PyObject read FImage1 write SetImage1;
                     constructor Create(AOwner: TComponent); override;
                     destructor Destroy; override;
                     procedure DragDrop(Source: TObject; X, Y: Integer); override;
                   end;

 {------------------------}

function ImageList_length(self: PyObject) : {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; cdecl;
function ImageList_item(self: PyObject; i: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
procedure ImageListDestructor(o: PyObject); cdecl;
function GetImage1Attr(self: PyObject; const attr: PChar) : PyObject; cdecl;
procedure Image1Destructor(o: PyObject); cdecl;


const
 TyImageList_Seq: TySequenceMethods =
  (sq_length:      ImageList_length;
   sq_item:        ImageList_item);

var
 TyImageList_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'imagelist';
   tp_basicsize:   SizeOf(TyImageList);
   tp_dealloc:     ImageListDestructor;
   tp_as_sequence: @TyImageList_Seq;
   tp_doc:         'A list of fixed-size icon.');
 TyImage1_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'image1';
   tp_basicsize:   SizeOf(TyImage1);
   tp_dealloc:     Image1Destructor;
   tp_getattr:     GetImage1Attr;
   tp_doc:         'A single icon in an image list.');

 {-------------------}

function GetImageCtrlAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
function SetImageCtrlAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyImageCtrl_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'imagectrl';
   tp_basicsize:   SizeOf(TyControlF);
   tp_dealloc:     ControlDestructor;
   tp_getattr:     GetImageCtrlAttr;
   tp_setattr:     SetImageCtrlAttr;
   tp_doc:         'An image displayer with mouse input.');

 {------------------------}

var
 InternalImages: array[0..InternalImagesCount-1, 0..1] of PyObject; { PyImage1 or a function to call }

function NewImageList(Bitmap: TBitmap; cx: Integer; MaskX, MaskY: Integer; const cratio: TDouble) : PyImageList;
procedure OpenGlobalImageList(ListView1: TListView);
procedure CloseGlobalImageList(ListView1: TListView);
function LoadGlobalImageList(Q: QObject) : Integer;

 {-------------------}

implementation

uses Quarkx, QkExceptions, PyCanvas, Dialogs;

const
 DisabledNak = TBitmap(1);

 {-------------------}

function NewImageList(Bitmap: TBitmap; cx: Integer; MaskX, MaskY: Integer; const cratio: TDouble) : PyImageList;
var
 IWidth, IHeight: Integer;
    {Test: TImageInfo;
     Bmp: TBitmap;
     I: Integer;}
 DC: HDC;
 OldBmp: HBitmap;
{$IFNDEF CompiledWithDelphi2}
 Bmp: TBitmap;
{$ENDIF}
begin
{$IFNDEF CompiledWithDelphi2}
 Bmp:=TBitmap.Create; try
 Bmp.Width:=Bitmap.Width;
 Bmp.Height:=Bitmap.Height;
 Bmp.Canvas.Draw(0,0,Bitmap);
 Bitmap:=Bmp;
{$ENDIF}
 Result:=PyImageList(PyObject_NEW(@TyImageList_Type));
 with Result^ do
  begin
   IWidth:=Bitmap.Width;
   IHeight:=Bitmap.Height;
   if cx<=0 then cx:=Round(cratio*IHeight);
   if (MaskX<0) or (MaskY<0) or (MaskX>=IWidth) or (MaskY>=IHeight) then
    begin
     Handle:=ImageList_Create(cx, Bitmap.Height, ILC_COLORDDB, Bitmap.Width div cx, 2);
     ImageList_Add(Handle, Bitmap.Handle, 0);
     BkgndColor:=ColorToRGB(clBtnFace);
    end
   else
    begin
     Handle:=ImageList_Create(cx, IHeight, ILC_COLORDDB or ILC_MASK, IWidth div cx, 2);
     {BkgndColor:=Bitmap.Canvas.Pixels[MaskX, MaskY];}
     DC:=CreateCompatibleDC(0);
     OldBmp:=SelectObject(DC, Bitmap.Handle);
     BkgndColor:=GetPixel(DC, MaskX, MaskY);
     SelectObject(DC, OldBmp);
     DeleteDC(DC);
     ImageList_AddMasked(Handle, Bitmap.Handle, BkgndColor);
    end;

   (*ImageList_GetImageInfo(Handle, 0, Test);
     I:=Random(10000);
     Bmp:=TBitmap.Create;
     Bmp.Handle:=Test.hbmImage;
     Bmp.SaveToFile(Format('c:\windows\bureau\image %d.bmp', [I]));
     {if Test.hbmMask<>0 then
      begin
       Bmp.Handle:=Test.hbmMask;
       Bmp.SaveToFile(Format('c:\windows\bureau\masque %d.bmp', [I]));
      end;}
     Bmp.Free;*)

   ImageList_SetBkColor(Handle, BkgndColor);
   Images[False]:=TList.Create;
   Images[True]:=TList.Create;
  end;
{$IFNDEF CompiledWithDelphi2}
 finally Bmp.Free; end;
{$ENDIF}
end;

 {-------------------}

const
 ConstImageSize = 16;

var
 GlobalImageList: TImageList = Nil;
 ImageSources: TList = Nil;
 ListViews: TList = Nil;

procedure OpenGlobalImageList(ListView1: TListView);
begin
 if GlobalImageList=Nil then
  begin
   GlobalImageList:=TImageList.Create(Application);
   GlobalImageList.Handle:=ImageList_Create(ConstImageSize, ConstImageSize,
    ILC_COLORDDB or ILC_MASK, 0, 4);
  end;
 if ImageSources=Nil then
  ImageSources:=TList.Create;
 if ListViews=Nil then
  ListViews:=TList.Create;
 ListViews.Add(ListView1);
 ListView1.SmallImages:=GlobalImageList;
end;

procedure CloseGlobalImageList(ListView1: TListView);
var
 I, J: Integer;
 Keeping: TBits;
begin
 if (ListViews<>Nil) and (ListView1.SmallImages=GlobalImageList)
 and (ListViews.Remove(ListView1)>=0) then
  begin
   Keeping:=TBits.Create; try
   Keeping.Size:=ImageSources.Count;
   for J:=0 to ListViews.Count-1 do
    with TListView(ListViews[J]) do
     for I:=0 to Items.Count-1 do
      Keeping.Bits[Items[I].ImageIndex]:=True;
   for I:=0 to ImageSources.Count-1 do
    if not Keeping.Bits[I] then
     ImageSources[I]:=Nil;
   finally Keeping.Free; end;
  end;
 ListView1.SmallImages:=nil;
end;

function LoadGlobalImageList(Q: QObject) : Integer;
var
 Etat: TDisplayDetails;
 Image1: PyImage1;
 Bitmap: TBitmap;
 I: Integer;
 BkColor: TColorRef;
begin
 if Q=Nil then
  Etat.Icon:=Nil
 else
  Q.DisplayDetails(True, Etat);
 if Etat.Icon=Nil then
  begin
   Etat.Icon:=InternalImages[iiUnknown, 0];
   Py_XINCREF(Etat.Icon);
  end;
 if Etat.Icon<>Nil then
  try
   Image1:=PyImage1(Etat.Icon);
   Result:=ImageSources.IndexOf(Image1);
   if Result>=0 then Exit;
   BkColor:=Image1^.ImageList^.BkgndColor;
   Bitmap:=TBitmap.Create; try
   Bitmap.Width:=ConstImageSize;
   Bitmap.Height:=ConstImageSize;
   with Bitmap.Canvas do
    begin
     Brush.Color:=BkColor;
     FillRect(Rect(0,0, ConstImageSize, ConstImageSize));
     Image1^.Draw(Handle, 0,0, BkColor);
    end;
   Result:=0;
   while (Result<ImageSources.Count) and (ImageSources[Result]<>Nil) do
    Inc(Result);
   if Result=ImageSources.Count then
    begin
     Result:=GlobalImageList.AddMasked(Bitmap, BkColor);
     if Result<0 then Exit;
     for I:=ImageSources.Count to Result do
      ImageSources.Add(Nil);
    end
   else
    GlobalImageList.ReplaceMasked(Result, Bitmap, BkColor);
   finally Bitmap.Free; end;
   ImageSources[Result]:=Image1;
  finally
   Py_DECREF(Etat.Icon);
  end
 else
  Result:=-1;
end;

 {-------------------}

function GetImage1Attr(self: PyObject; const attr: PChar) : PyObject; cdecl;
begin
 Result:=nil;
 try
  case attr[0] of
   'd': if StrComp(attr, 'disabledimage')=0 then
         begin
          Result:=PyImage1(self)^.GetDisabledImage;
          Exit;
         end;
   's': if StrComp(attr, 'size')=0 then
         begin
          with PyImage1(self)^.GetSize do
           Result:=Py_BuildValueX('ii', [X,Y]);
          Exit;
         end;
  end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
  Result:=Nil;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

procedure Image1Destructor(o: PyObject); cdecl;
var
 I: Integer;
begin
 if ImageSources<>Nil then
  begin
   I:=ImageSources.IndexOf(o);
   if I>=0 then
    ImageSources[I]:=nil;
  end;
 try
  with PyImage1(o)^ do
   begin
    ImageList^.Images[DisabledBmp<>Nil][Index]:=Nil;
    Py_DECREF(ImageList);
    if (DisabledBmp<>nil) and (DisabledBmp<>DisabledNak) then
      DisabledBmp.free;
    if BitmapCopy<>0 then
     DeleteObject(BitmapCopy);
   end;
  FreeMem(o);
 except
  EBackToPython;
 end;
end;

function ImageList_length(self: PyObject) : {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; cdecl;
begin
 try
  Result:=ImageList_GetImageCount(PyImageList(self)^.Handle);
 except
  EBackToPython;
  Result:=-1;
 end;
end;

function ImageList_item(self: PyObject; i: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
begin
 Result:=nil;
 try
  if (i>=ImageList_length(self)) then
   Raise EError(4445);
  Result:=PyImageList(self)^.NeedImage(False, i);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

procedure ImageListDestructor(o: PyObject); cdecl;
{$IFDEF Debug}
var
 I: Integer;
 B: Boolean;
{$ENDIF}
begin
 try
  with PyImageList(o)^ do
   begin
    {$IFDEF Debug}
    for B:=False to True do
     for I:=Images[B].Count-1 downto 0 do
      if Images[B][I]<>Nil then
       Raise InternalE('ImageListDestructor non-empty');
    {$ENDIF}
    ImageList_Destroy(Handle);
    Images[True].Free;
    Images[False].Free;
   end;
  FreeMem(o);
 except
  EBackToPython;
 end;
end;

 {-------------------}

procedure MakeDisabledBitmap(Source: PyImage1);
{ code from TB97 }
const
  ROP_DSPDxax = $00E20746;
  ROP_PSDPxax = $00B8074A;
const
  Add = 1;
var
  MonoBmp, TmpImage: TBitmap;
  IWidth, IHeight: Integer;
  {ORect,} IRect: TRect;
  FTransparentColor: TColor;
begin
  with Source^.GetSize do
   begin
    IWidth:=X+Add;
    IHeight:=Y+Add;
   end;
  {ORect:=Rect(0,0,IWidth-Add,IHeight-Add);}
  IRect:=Rect(0,0,IWidth,IHeight);
  FTransparentColor:=Source^.ImageList^.BkgndColor;

  MonoBmp:=TBitmap.Create;
  TmpImage:=TBitmap.Create;
  TmpImage.Width:=IWidth;
  TmpImage.Height:=IHeight;

  { The new Office 97 / MFC look }
  MonoBmp.Monochrome := True;
  MonoBmp.Width := IWidth;
  MonoBmp.Height := IHeight;

  with TmpImage.Canvas do begin
    Brush.Color := FTransparentColor;
    FillRect (IRect);
    {CopyRect (Rect(0, 0, IWidth-Add, IHeight-Add), DDB.Canvas, ORect);}
    Source^.DisabledBmp:=Nil; try
    Source^.Draw(Handle, 0,0, FTransparentColor);
    finally Source^.DisabledBmp:=TmpImage; end;

    { Generate the mask in MonoBmp. Mask FTransparentColor }
    SetBkColor (Handle, {ColorToRGB(}FTransparentColor{)});
    BitBlt (MonoBmp.Canvas.Handle, 0, 0, IWidth, IHeight, Handle,
      {ORect.Left, ORect.Top,} 0,0, SRCCOPY);
    { and clWhite }
    SetBkColor (Handle, clWhite);
    BitBlt (MonoBmp.Canvas.Handle, 0, 0, IWidth, IHeight, Handle,
      {ORect.Left, ORect.Top,} 0,0, SRCPAINT);
    { and clSilver }
    SetBkColor (Handle, clSilver);
    BitBlt (MonoBmp.Canvas.Handle, 0, 0, IWidth, IHeight, Handle,
      {ORect.Left, ORect.Top,} 0,0, SRCPAINT);

    Brush.Color := clBtnFace;
    FillRect (IRect);
    Brush.Color := clBtnHighlight;
    SetTextColor (Handle, clBlack);
    SetBkColor (Handle, clWhite);
    BitBlt (Handle, 1, 1, IWidth-1, IHeight-1,
      MonoBmp.Canvas.Handle, 0, 0, ROP_PSDPxax);
    Brush.Color := clBtnShadow;
    SetTextColor (Handle, clBlack);
    SetBkColor (Handle, clWhite);
    BitBlt (Handle, 0, 0, IWidth, IHeight,
      MonoBmp.Canvas.Handle, 0, 0, ROP_PSDPxax);
  end;
  MonoBmp.Free;
end;

function TyImageList.NeedImage(nDisabled: Boolean; i: Integer) : PyImage1;
var
 J: Integer;
begin
 if i<Images[nDisabled].Count then
  Result:=Images[nDisabled][i]
 else
  begin
   for J:=Images[nDisabled].Count to i do
    Images[nDisabled].Add(Nil);
   Result:=Nil;
  end;
 if Result<>Nil then
  Py_INCREF(Result)
 else
  begin
   Result:=PyImage1(PyObject_NEW(@TyImage1_Type));
   with Result^ do
    begin
     ImageList:=@Self;
     Py_INCREF(@Self);
     Index:=i;
     if nDisabled then
      DisabledBmp:=DisabledNak
     else
      DisabledBmp:=Nil;
     BitmapCopy:=0;
    end;
   Images[nDisabled][i]:=Result;
  end;
end;

(*function TyImageList.GetDisabledImage;
var
 I: Integer;
begin
 Result:=NeedImage(True, Source^.Index);
 if Result=Nil then
  begin
   Result:=MakeDisabledBitmap(Source);
   Images[True][Source^.Index]:=Result;
  end;
end;*)

 {-------------------}

function TyImage1.GetDisabledImage : PyImage1;
begin
 Result:=ImageList^.NeedImage(True, Index);
{Result:=PyImage1(PyObject_NEW(@TyImage1_Type));
 Result^.ImageList:=ImageList;
 Py_INCREF(ImageList);
 Result^.Index:=Index;
 Result^.Disabled:=True;
 Result^.BitmapCopy:=0;}
end;

function TyImage1.GetSize : TPoint;
begin
 Result.X:=0;
 Result.Y:=0;
 ImageList_GetIconSize(ImageList^.Handle, Integer(Result.X), Integer(Result.Y));
end;

procedure TyImage1.Draw(DC: HDC; X,Y: Integer; Opaque: TColorRef);
begin
 if DisabledBmp<>Nil then
  begin
   if DisabledBmp = DisabledNak then
    MakeDisabledBitmap(@Self);
   BitBlt(DC, X,Y, DisabledBmp.Width, DisabledBmp.Height,
    DisabledBmp.Canvas.Handle, 0, 0, srcCopy);
  end
 else
  ImageList_DrawEx(ImageList^.Handle, Index, DC, X,Y, 0,0,
   Opaque, CLR_NONE, ILD_NORMAL);
end;

procedure TyImage1.GetIcon(DestIcon: TIcon);
begin
 DestIcon.Handle:=ImageList_ExtractIcon(0, ImageList^.Handle, Index);
end;

function TyImage1.GetMenuBitmap : HBitmap;
var
 DC, MemDC, MemDC2: HDC;
 P, Dest: TPoint;
 TempBmp, OldBmp, OldBmp2: HBitmap;
 B: HBrush;
begin
 if BitmapCopy=0 then
  begin
   //Log('BitmapCopy for ' + IntToHex(Integer(@Self), 8));}

   P:=GetSize;
   Dest.X:=GetSystemMetrics(sm_CxMenuCheck);
   Dest.Y:=GetSystemMetrics(sm_CyMenuCheck);

   DC:=GetDC(0);
   TempBmp:=CreateCompatibleBitmap(DC, P.X, P.Y);
   MemDC:=CreateCompatibleDC(DC);
   OldBmp:=SelectObject(MemDC, TempBmp);

   B:=CreateSolidBrush({ColorToRGB(clMenu)}clWhite);
   FillRect(MemDC, Rect(0,0,P.X,P.Y), B);
   DeleteObject(B);
   Draw(MemDC, 0,0, {ColorToRGB(clMenu)}clWhite);

   BitmapCopy:=CreateCompatibleBitmap(DC, Dest.X, Dest.Y);
   MemDC2:=CreateCompatibleDC(DC);
   OldBmp2:=SelectObject(MemDC2, BitmapCopy);
   StretchBlt(MemDC2, 0, 0, Dest.X, Dest.Y, MemDC, 0, 0, P.X, P.Y, srcCopy);
   SelectObject(MemDC2, OldBmp2);
   DeleteDC(MemDC2);

   SelectObject(MemDC, OldBmp);
   DeleteDC(MemDC);
   DeleteObject(TempBmp);

   ReleaseDC(0, DC);
  end;
 Result:=BitmapCopy;
end;

 {------------------------}

constructor TPyImageControl.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FOnClick:=PyNoResult;
 FOnDraw:=PyNoResult;
 ImageObject:=NewControl(TyImageCtrl_Type, Self);
 FImage1:=PyNoResult;
{Visible:=False;}
 Color:=clBtnFace;
 ControlStyle:=[csOpaque];
end;

destructor TPyImageControl.Destroy;
begin
 ImageObject^.Close;
 Py_DECREF(FOnDraw);
 Py_DECREF(FOnClick);
 Py_DECREF(Image1);
 inherited;
end;

procedure TPyImageControl.MouseDown;
begin
 inherited;
 if {(Button=mbLeft) and} (FOnClick<>Py_None) then
  MouseMoveEvt(Nil, Shift, X, Y);
end;

procedure TPyImageControl.MouseMoveEvt(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
 arglist, rslt: PyObject;
{P: TPoint;}
 Flags: Integer;
 FlagsS: TShiftState absolute Flags;
begin
{if not (ssLeft in Shift) then
  begin
   OnMouseMove:=Nil;
   Exit;
  end;}
{if Image1^.ob_type = @TyImage1_Type then
  begin
   P:=PyImage1(Image1)^.GetSize;
   if (X>=P.X) or (Y>=P.Y) then Exit;
  end;}
 Flags:=0;
 FlagsS:=Shift;
 if Sender=Nil then
  Flags:=Flags or mbClick
 else
  Flags:=Flags or mbDragging;
 try
  arglist:=Py_BuildValueX('Oiii', [ImageObject, X, Y, Flags]);
  if arglist=Nil then Exit;
  try
   rslt:=PyEval_CallObject(FOnClick, arglist);
  finally
   Py_DECREF(arglist);
  end;
  if rslt=nil then Exit;
  try
   if (rslt<>Nil) and PyObject_IsTrue(rslt) then
    begin
     MouseCapture:=True;
     OnMouseMove:=MouseMoveEvt;
    end
   else
    begin
     MouseCapture:=False;
     OnMouseMove:=Nil;
    end;
  finally
   Py_XDECREF(rslt);
  end;
 finally
  PythonCodeEnd;
 end;
end;

procedure TPyImageControl.MouseUp;
begin
 OnMouseMove:=Nil;
 SetCaptureControl(Nil);
end;

procedure TPyImageControl.Paint;
var
 R: TRect;
 P: TPoint;
 DC: HDC;
begin
 DC:=Canvas.Handle;
 GetClipBox(DC, R);
 if Image1^.ob_type = @TyImage1_Type then
  begin
   P:=PyImage1(Image1)^.GetSize;
   PyImage1(Image1)^.Draw(DC, 0, 0, CLR_DEFAULT);
  end
 else
  P:=Point(0,0);
 Canvas.Brush.Color:=Color{PyImage1(Image1)^.ImageList^.BkgndColor};
 R.Left:=P.X;
 Canvas.FillRect(R);
 R.Left:=0;
 R.Right:=P.X;
 R.Top:=P.Y;
 Canvas.FillRect(R);
 CallNotifyEvent(ImageObject, FOnDraw, False);
end;

procedure TPyImageControl.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_GetPyControl: Msg.Result:=LongInt(ImageObject);
 else
  if not DefControlMessage(Msg) then
   inherited;
 end;
end;

procedure TPyImageControl.SetImage1;
begin
 Py_DECREF(FImage1);
 if value^.ob_type = @TyImage1_Type then
  begin
   FImage1:=value;
   Py_INCREF(FImage1);
   with PyImage1(value)^.GetSize do
    begin
     Width:=X;
     Height:=Y;
    end;
  {Visible:=True;}
  end
 else
  begin
   FImage1:=PyNoResult;
  {Visible:=False;}
  end;
 Repaint;
end;

procedure TPyImageControl.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept:=ImageObject.DragOver;
end;

procedure TPyImageControl.DragDrop(Source: TObject; X, Y: Integer);
begin
 ImageObject.DragDrop(Source, Self, X,Y);
end;

 {-------------------}

function iCanvas(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  if PyControlF(self)^.QkControl<>Nil then
   begin
    Result:=PyObject_NEW(@TyCanvas_Type);
    PyCanvasObj(Result)^.Canvas:=(PyControlF(self)^.QkControl as TPyImageControl).Canvas;
    with PyCanvasObj(Result)^.Canvas do
     begin
      Brush.Style:=bsSolid;
      Brush.Color:=clWhite;
      Pen.Style:=psSolid;
      Pen.Color:=clBlack;
      CopyMode:=cmSrcCopy;
     end;
   end
  else
   Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'canvas';          ml_meth:    iCanvas;          ml_flags: METH_VARARGS));

function GetImageCtrlObject(self: PyObject; attr: PChar) : PyObjectPtr;
begin
 Result:=Nil;
 with PyControlF(self)^ do
  case attr[0] of
   'o': if StrComp(attr, 'onclick')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyImageControl).FOnClick;
          Exit;
         end
        else if StrComp(attr, 'ondraw')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyImageControl).FOnDraw;
          Exit;
         end;
  end;
end;

function GetImageCtrlAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
var
 Attr1: PyObjectPtr;
 I: Integer;
begin
 Result:=nil;
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  with PyControlF(self)^ do
   case attr[0] of
    'c': if StrComp(attr, 'color')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong(ColorToRGB((QkControl as TPyImageControl).Color))
           else
            Result:=PyNoResult;
           Exit;
          end;
    'i': if StrComp(attr, 'image')=0 then
          begin
           if QkControl<>Nil then
            Result:=(QkControl as TPyImageControl).Image1
           else
            Result:=Py_None;
           Py_INCREF(Result);
           Exit;
          end;
   end;
  Attr1:=GetImageCtrlObject(self, attr);
  if Attr1=Nil then
   Result:=GetControlAttr(self, attr, 'imagectrl')
  else
   begin
    Result:=Attr1^;
    Py_INCREF(Result);
   end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetImageCtrlAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;
var
 Attr1: PyObjectPtr;
begin
{Result:=-1;}
 try
  with PyControlF(self)^ do
   case attr[0] of
    'c': if StrComp(attr, 'color')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyImageControl).Color:=PyInt_AsLong(value);
           Result:=0;
           Exit;
          end;
    'i': if StrComp(attr, 'image')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyImageControl).Image1:=value;
           Result:=0;
           Exit;
          end;
   end;
  Attr1:=GetImageCtrlObject(self, attr);
  if Attr1=Nil then
   Result:=SetControlAttr(self, attr, value)
  else
   begin
    Py_DECREF(Attr1^);
    Attr1^:=value;
    Py_INCREF(value);
    Result:=0;
   end;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {-------------------}

procedure FinalizeInternalImages;
var
 I: Integer;
begin
 for I:=High(InternalImages) downto Low(InternalImages) do
  begin
   Py_XDECREF(InternalImages[I,1]);
   Py_XDECREF(InternalImages[I,0]);
  end;
end;


initialization
  FillChar(InternalImages, SizeOf(InternalImages), 0);

finalization
  FinalizeInternalImages;
  {if GlobalImageList<>nil then  //Has parent Application, so it automatically destroyed
    GlobalImageList.Free;}
  if ImageSources<>nil then
    ImageSources.Free;
  if ListViews<>nil then
    ListViews.Free;
end.

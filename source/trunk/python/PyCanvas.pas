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
unit PyCanvas;

interface

uses Windows, SysUtils, Classes, Graphics, Python, Quarkx, CommCtrl;

type
 PyCanvasObj = ^TyCanvasObj;
 TyCanvasObj = object(TyObject)
                Canvas: TCanvas;
               end;

 {------------------------}

function GetCanvasAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
function SetCanvasAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyCanvas_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'canvas';
   tp_basicsize:   SizeOf(TyCanvasObj);
   tp_dealloc:     SimpleDestructor;
   tp_getattr:     GetCanvasAttr;
   tp_setattr:     SetCanvasAttr;
   tp_doc:         'A canvas to draw on.');

 {------------------------}

implementation

uses qmath, PyObjects, PyImages, QkObjects, QkTextures, Game, Setup,
     PyMath, Qk3D, QkImages, QkPixelSet, QkExceptions;

 {------------------------}

function cLine(self, args: PyObject) : PyObject; cdecl;
var
 P1, P2: TPoint;
 v1, v2: PyVect;
begin
 Result:=Nil;
 try
  if Assigned(PyCanvasObj(self)^.Canvas) then
   begin
    g_DrawInfo.DC:=PyCanvasObj(self)^.Canvas.Handle;
    if PyObject_Length(args)=2 then
     begin
      if not PyArg_ParseTupleX(args, 'O!O!', [@TyVect_Type, @v1, @TyVect_Type, @v2]) then
       Exit;
      if (v1^.Source3D=Nil) or (v1^.Source3D<>v2^.Source3D) then
       Raise EError(4447);
      v1^.Source3D.Line95f(PyVect_AsPP(v1), PyVect_AsPP(v2));
     end
    else
     begin
      if not PyArg_ParseTupleX(args, 'iiii', [@P1.X, @P1.Y, @P2.X, @P2.Y]) then
       Exit;
      Windows.MoveToEx(g_DrawInfo.DC, P1.X, P1.Y, Nil);
      Windows.LineTo(g_DrawInfo.DC, P2.X, P2.Y);
     end;
   end;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cRectangle(self, args: PyObject) : PyObject; cdecl;
var
 P1, P2: TPoint;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'iiii', [@P1.X, @P1.Y, @P2.X, @P2.Y]) then
   Exit;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   Rectangle95(PyCanvasObj(self)^.Canvas.Handle, P1.X, P1.Y, P2.X, P2.Y);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cEllipse(self, args: PyObject) : PyObject; cdecl;
var
 P1, P2: TPoint;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'iiii', [@P1.X, @P1.Y, @P2.X, @P2.Y]) then
   Exit;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   Ellipse95(PyCanvasObj(self)^.Canvas.Handle, P1.X, P1.Y, P2.X, P2.Y);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cSetPixel(self, args: PyObject) : PyObject; cdecl;
var
 P: TPoint;
 Color: TColorRef;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'iii', [@P.X, @P.Y, @Color]) then
   Exit;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   Color:=SetPixel(PyCanvasObj(self)^.Canvas.Handle, P.X, P.Y, Color)
  else
   Color:=TColorRef(-1);
  Result:=PyInt_FromLong(Color);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cGetPixel(self, args: PyObject) : PyObject; cdecl;
var
 P: TPoint;
 Color: TColorRef;
begin
 Result:=Nil;
 try
  //Color:=CLR_NONE; //Not needed: Color is a dummy parameter
  if not PyArg_ParseTupleX(args, 'ii|i', [@P.X, @P.Y, @Color]) then
   Exit;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   Color:=GetPixel(PyCanvasObj(self)^.Canvas.Handle, P.X, P.Y)
  else
   Color:=TColorRef(-1);
  Result:=PyInt_FromLong(Color);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cDraw(self, args: PyObject) : PyObject; cdecl;
var
 P: TPoint;
 Color: TColorRef;
 im: PyImage1;
begin
 Result:=Nil;
 try
  Color:=CLR_NONE;
  if not PyArg_ParseTupleX(args, 'O!ii', [@TyImage1_Type, @im, @P.X, @P.Y, @Color]) then
   Exit;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   im^.Draw(PyCanvasObj(self)^.Canvas.Handle, P.X, P.Y, Color);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cPaintTexture(self, args: PyObject) : PyObject; cdecl;
var
 tx: PyObject;
 R: TRect;
 lgr: Integer;
 Tex1: QObject;
const
 Q2ColorMapCount = 64;
type
 PColorMapQ = ^TColorMapQ;
 TColorMapQ = array[0..255] of Byte;
var
 {Texture,} ColorMapData: String;
 IgnoreData: Integer;
 {Header: TQ1Miptex;}
 X,Y,W,H, TotalW: Integer;
 ColorMap: QObject;
 Base, Echelle, ColorT, ColorT1, PX, PY: Integer;
 ColorMap1: PColorMapQ;
 TexBits, Source1, Source, Dest1, Dest: PByte;
 {GameInfo: PGameBuffer;}
 DC: HDC;
 Palette, Pal1: HPalette;
 PSD, LightPatch: TPixelSetDescription;
 BmpInfo: TBitmapInfo256;
 BitmapInfo: PBitmapInfo;
begin
 Result:=Nil;
 try
  if Assigned(PyCanvasObj(self)^.Canvas) then
   begin
    lgr:=1;
    if not PyArg_ParseTupleX(args, 'O!(iiii)|i', [@TyObject_Type, @tx, @R.Left, @R.Top, @R.Right, @R.Bottom, @lgr]) then
     Exit;
    Tex1:=QkObjFromPyObj(tx);
    if not (Tex1 is QPixelSet) then
     Raise EErrorFmt(4438, ['PixelSet']);
    PSD:=QPixelSet(Tex1).Description; try
    W:=PSD.Size.X;
    H:=PSD.Size.Y;
    if (W>=8) and (H>=8) then
     begin
      if not PSD.IsGamePalette(mjAny) then
       lgr:=0;  { cannot fade out textures with their custom palette }
      {GameInfo:=QTextureFile(Tex).LoadPaletteInfo; try}
      if SetupGameSet.Specifics.Values['Gradient']='' then
       lgr:=0;  { cannot fade out textures without a global game palette }
      {GameInfo^.BitmapInfo.bmiHeader.biWidth:=W;
      GameInfo^.BitmapInfo.bmiHeader.biHeight:=H;}
      TotalW:=R.Right-R.Left;
      Echelle:=0;
      ColorT1:=0;
      Pal1:=0;
      Palette:=0;
      DC:=PyCanvasObj(self)^.Canvas.Handle;
      LightPatch:=PSDToDIB(PSD, True);
      try
       if lgr<>0 then
        begin
         ColorMap:=NeedGameFile(SetupGameSet.Specifics.Values['Gradient'], '');
         ColorMap.Acces;
         if ColorMap is QImage then
          begin
           ColorMapData:=ColorMap.GetSpecArg('Image1');
           IgnoreData:=Length(ColorMapData)-Q2ColorMapCount*SizeOf(TColorMapQ);
           if IgnoreData<0 then
            IgnoreData:=0;
          end
         else
          begin
           ColorMapData:=ColorMap.GetSpecArg('Data');
           IgnoreData:=Length('Data=');
           lgr:=-lgr;
          end;
         ColorT1:=(Length(ColorMapData)-IgnoreData) div SizeOf(TColorMapQ);
        end
       else
        IgnoreData:=0;
       if ColorT1>0 then
        begin
         TexBits:=PByte(PSD.StartPointer);
(*Decker 2002.02.25
         {Decker - Removed this single statement, as data has already been
          allocated in the assigment from PSDToDIB() further up. Nor Could I
          see where LightPatch used its newly allocated data, in the following
          statements of this function.}
         LightPatch.AllocData;
/Decker 2002.02.25*)
         Echelle := TotalW div 2;
         Base    := Echelle*Echelle*Echelle;
         Echelle := Base*2 div ColorT1 + 1;
        end
       else
        begin
         TexBits:=PByte(LightPatch.StartPointer);
         Base:=0;
        end;
       BitmapInfo:=PBitmapInfo(LightPatch.GetBitmapInfo(BmpInfo, @Palette));
       if Palette<>0 then
        begin
         Pal1:=SelectPalette(DC, Palette, False);
         RealizePalette(DC);
        end;
       X:=0;
       repeat
        if Echelle>0 then
         begin
          Source1:=TexBits;
          Dest1:=PByte(LightPatch.StartPointer);
          for PX:=X to X+W-1 do
           begin
            if PX=TotalW then Break;
            Source:=Source1; Inc(Source1);
            Dest:=Dest1;     Inc(Dest1);
            ColorT := PX - TotalW div 2;
            if lgr<0 then ColorT:=-ColorT;
            ColorT := ColorT*ColorT*ColorT;
            ColorT := (ColorT+Base) div Echelle;
            {$IFDEF Debug}
            if ColorT>=ColorT1 then Raise InternalE('ColorT>=ColorT1');
            {$ENDIF}
            ColorMap1:=PColorMapQ(PChar(ColorMapData)+IgnoreData);
            Inc(ColorMap1, ColorT);
            for PY:=0 to H-1 do
             begin
              Dest^:=ColorMap1^[Source^];
              Inc(Dest, LightPatch.ScanLine);
              Inc(Source, PSD.ScanLine);
             end;
           end;
         end;
        Y:=R.Top;
        repeat
         DrawToDC(DC, BitmapInfo^, LightPatch.Data, R.Left+X, Y);
         Inc(Y, H);
        until Y>=R.Bottom;
        Inc(X, W);
       until X>=TotalW;
      finally
       if Pal1<>0 then
        SelectPalette(DC, Pal1, False);
       LightPatch.ReleasePalette(Palette);
       LightPatch.Done;
      end;
      {finally DeleteGameBuffer(GameInfo); end;}
     end;
    finally PSD.Done; end;
   end;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cPolygon(self, args: PyObject) : PyObject; cdecl;
var
 lst, ccw, obj: PyObject;
 I, Count: Integer;
 PPts, PPts1: PPointProj;
 Pt, Pt1: PPoint;
 Coord: TCoordinates;
begin
 Result:=Nil;
 try
  if Assigned(PyCanvasObj(self)^.Canvas) then
   begin
    ccw:=Py_None;
    if not PyArg_ParseTupleX(args, 'O!|O', [PyList_Type, @lst, @ccw]) then
     Exit;
    obj:=PyList_GetItem(lst, 0);
    if obj=Nil then Exit;
    g_DrawInfo.DC:=PyCanvasObj(self)^.Canvas.Handle;
    Count:=PyObject_Length(lst);
    if obj^.ob_type = @TyVect_Type then
     begin
      Coord:=PyVect(obj)^.Source3D;
      GetMem(PPts, SizeOf(TPointProj)*Count); try
      PPts1:=PPts;
      for I:=0 to Count-1 do
       begin
        obj:=PyList_GetItem(lst, I);
        if obj=Nil then Exit;
        if obj^.ob_type <> @TyVect_Type then
         Raise EError(4441);
        if (PyVect(obj)^.Source3D=Nil) or (PyVect(obj)^.Source3D<>Coord) then
         Raise EError(4447);
        PPts1^:=PyVect_AsPP(PyVect(obj));
        Inc(PPts1);
       end;
      Coord.Polygon95(PPts^, Count, PyObject_IsTrue(ccw));
      finally FreeMem(PPts); end;
     end
    else
     begin
      GetMem(Pt, SizeOf(TPoint)*Count); try
      Pt1:=Pt;
      for I:=0 to Count-1 do
       begin
        obj:=PyList_GetItem(lst, I);
        if obj=Nil then Exit;
        if not PyArg_ParseTupleX(obj, 'ii', [@Pt1^.X, @Pt1^.Y]) then
         Exit;
        if (Pt1^.X<-Max95) or (Pt1^.X>Max95)
        or (Pt1^.Y<-Max95) or (Pt1^.Y>Max95) then
         begin
          Result:=PyNoResult;
          Exit;
         end;
        Inc(Pt1);
       end;
      Windows.Polygon(g_DrawInfo.DC, Pt^, Count);
      finally FreeMem(Pt); end;
     end;
   end;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cTextOut(self, args: PyObject) : PyObject; cdecl;
var
 X, Y: Integer;
 Text: PChar;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'iis', [@X, @Y, @Text]) then
   Exit;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   PyCanvasObj(self)^.Canvas.TextOut(X, Y, Text);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cTextSize(self, args: PyObject) : PyObject; cdecl;
var
 Text: PChar;
 w,h: Integer;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 's', [@Text]) then
   Exit;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   with PyCanvasObj(self)^.Canvas do
    begin
     w:=TextWidth(Text);
     h:=TextHeight(Text);
    end
  else
   begin
    w:=0;
    h:=0;
   end;
  Result:=Py_BuildValueX('ii', [w,h]);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cReset(self, args: PyObject) : PyObject; cdecl;
var
 C: TColor;
begin
 Result:=nil;
 try
  if g_DrawInfo.BasePen=White_pen then
   C:=clWhite
  else
   C:=clBlack;
  if Assigned(PyCanvasObj(self)^.Canvas) then
   with PyCanvasObj(self)^.Canvas do
    begin
     Brush.Style:=bsSolid;
     Brush.Color:=C xor clWhite;
     Pen.Style:=psSolid;
     Pen.Color:=C;
     Pen.Width:=0;
     CopyMode:=cmSrcCopy;
    end;
  Result:=self;
  Py_INCREF(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

 {------------------------}

const
 MethodTable: array[0..10] of TyMethodDef =
  ((ml_name: 'line';         ml_meth: cLine;         ml_flags: METH_VARARGS),
   (ml_name: 'rectangle';    ml_meth: cRectangle;    ml_flags: METH_VARARGS),
   (ml_name: 'ellipse';      ml_meth: cEllipse;      ml_flags: METH_VARARGS),
   (ml_name: 'polygon';      ml_meth: cPolygon;      ml_flags: METH_VARARGS),
   (ml_name: 'setpixel';     ml_meth: cSetPixel;     ml_flags: METH_VARARGS),
   (ml_name: 'getpixel';     ml_meth: cGetPixel;     ml_flags: METH_VARARGS),
   (ml_name: 'draw';         ml_meth: cDraw;         ml_flags: METH_VARARGS),
   (ml_name: 'textout';      ml_meth: cTextOut;      ml_flags: METH_VARARGS),
   (ml_name: 'textsize';     ml_meth: cTextSize;     ml_flags: METH_VARARGS),
   (ml_name: 'painttexture'; ml_meth: cPaintTexture; ml_flags: METH_VARARGS),
   (ml_name: 'reset';        ml_meth: cReset;        ml_flags: METH_VARARGS));

function GetCanvasAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
var
 I: Integer;
 S: String;
begin
 Result:=nil;
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  with PyCanvasObj(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'brushcolor')=0 then
          begin
           if Assigned(Canvas) then
            I:=ColorToRGB(Canvas.Brush.Color)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end
         else if StrComp(attr, 'brushstyle')=0 then
          begin
           if Assigned(Canvas) then
            I:=Ord(Canvas.Brush.Style)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end;
    'f': if StrComp(attr, 'fontcolor')=0 then
          begin
           if Assigned(Canvas) then
            I:=ColorToRGB(Canvas.Font.Color)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end
         else if StrComp(attr, 'fontname')=0 then
          begin
           if Assigned(Canvas) then
            S:=Canvas.Font.Name
           else
            S:='';
           Result:=PyString_FromString(PChar(S));
           Exit;
          end
         else if StrComp(attr, 'fontsize')=0 then
          begin
           if Assigned(Canvas) then
            I:=Canvas.Font.Size
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end
         else if StrComp(attr, 'fontbold')=0 then
          begin
           if Assigned(Canvas) then
            I:=Ord(fsBold in Canvas.Font.Style)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end
         else if StrComp(attr, 'fontitalic')=0 then
          begin
           if Assigned(Canvas) then
            I:=Ord(fsItalic in Canvas.Font.Style)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end
         else if StrComp(attr, 'fontunderline')=0 then
          begin
           if Assigned(Canvas) then
            I:=Ord(fsUnderline in Canvas.Font.Style)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end;
    'p': if StrComp(attr, 'pencolor')=0 then
          begin
           if Assigned(Canvas) then
            I:=ColorToRGB(Canvas.Pen.Color)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end
         else if StrComp(attr, 'penwidth')=0 then
          begin
           if Assigned(Canvas) then
            I:=Canvas.Pen.Width
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end
         else if StrComp(attr, 'penstyle')=0 then
          begin
           if Assigned(Canvas) then
            I:=Ord(Canvas.Pen.Style)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
           Exit;
          end;
    't': if StrComp(attr, 'transparent')=0 then
          begin
           if Assigned(Canvas) then
            I:=Ord(GetBkMode(Canvas.Handle)=Transparent)
           else
            I:=0;
           Result:=PyInt_FromLong(I);
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

function SetCanvasAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;
var
 Mode1: Integer;
begin
 Result:=-1;
 try
  with PyCanvasObj(self)^ do
   if Assigned(Canvas) then
    case attr[0] of
     'b': if StrComp(attr, 'brushcolor')=0 then
           begin
            Canvas.Brush.Color:=PyInt_AsLong(value);
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'brushstyle')=0 then
           begin
            Canvas.Brush.Style:=TBrushStyle(PyInt_AsLong(value));
            Result:=0;
            Exit;
           end;
     'f': if StrComp(attr, 'fontcolor')=0 then
           begin
            Canvas.Font.Color:=PyInt_AsLong(value);
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'fontname')=0 then
           begin
            Canvas.Font.Name:=PyString_AsString(value);
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'fontsize')=0 then
           begin
            Canvas.Font.Size:=PyInt_AsLong(value);
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'fontbold')=0 then
           begin
            if PyObject_IsTrue(value) then
             Canvas.Font.Style:=Canvas.Font.Style+[fsBold]
            else
             Canvas.Font.Style:=Canvas.Font.Style-[fsBold];
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'fontitalic')=0 then
           begin
            if PyObject_IsTrue(value) then
             Canvas.Font.Style:=Canvas.Font.Style+[fsItalic]
            else
             Canvas.Font.Style:=Canvas.Font.Style-[fsItalic];
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'fontunderline')=0 then
           begin
            if PyObject_IsTrue(value) then
             Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline]
            else
             Canvas.Font.Style:=Canvas.Font.Style-[fsUnderline];
            Result:=0;
            Exit;
           end;
     'p': if StrComp(attr, 'pencolor')=0 then
           begin
            Canvas.Pen.Color:=PyInt_AsLong(value);
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'penwidth')=0 then
           begin
            Canvas.Pen.Width:=PyInt_AsLong(value);
            Result:=0;
            Exit;
           end
          else if StrComp(attr, 'penstyle')=0 then
           begin
            Canvas.Pen.Style:=TPenStyle(PyInt_AsLong(value));
            Result:=0;
            Exit;
           end;
     't': if StrComp(attr, 'transparent')=0 then
           begin
            if PyObject_IsTrue(value) then
             Mode1:=Transparent
            else
             Mode1:=Opaque;
            SetBkMode(Canvas.Handle, Mode1);
            Result:=0;
            Exit;
           end;
    end
   else
    begin
     Result:=0;
     Exit;
    end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {------------------------}

end.

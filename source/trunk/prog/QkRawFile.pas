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
Revision 1.13  2008/09/06 15:57:07  danielpharos
Moved exception code into separate file.

Revision 1.12  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2001/03/20 21:44:00  decker_dk
Updated copyright-header

Revision 1.9  2000/12/06 18:43:35  decker_dk
- Implemented Rowdy's fix to play WAV-files correctly

Revision 1.8  2000/11/25 20:51:32  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.7  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.6  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.5  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkRawFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, StdCtrls, ComCtrls, ExtCtrls, PaintPanel,
  QkForm;

type
 QRawFileObject = class;
 TRawDataInfo = class
                protected
                  DestWnd: HWnd;
                  DestDC: HDC;
                  procedure InitDC(Wnd: HWnd);
                public
                  Length: Integer;
                  Stream: TStream;
                  StreamSource, StreamSize: Integer;
                  StaticData: Boolean;
                  procedure ReadProc(nPos: Integer); virtual; abstract;
                  function DisplayProc(nPos: Integer) : Boolean; virtual; abstract;
                  function InitThreads(nPos: Integer) : Integer; virtual; abstract;
                  procedure DoneThreads; virtual; abstract;
                  procedure StopThreads; virtual; abstract;
                  function GetConfigStr1 : String; virtual; abstract;
                end;
 QRawFileObject = class(QFileObject)
                  protected
                    procedure SaveFile(Info: TInfoEnreg1); override;
                    procedure LoadFile(F: TStream; FSize: Integer); override;
                    procedure SetWriteString(const WriteString: String);
                    function GetDataInfo : TRawDataInfo; virtual; abstract;
                    function OpenWindow(nOwner: TComponent) : TQForm1; override;
                  public
                    function GetReadStream(var S: TStream) : Integer;
                  end;

type
  TRFThread = class(TThread)
  protected
    Position: Integer;
    Info: TRawDataInfo;
    Semaphore: THandle;
    Reading: Boolean;
    ErrorMsg: String;
    procedure Execute; override;
  end;

type
  TFQRawFile = class(TQForm1)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    TrackBar1: TTrackBar;
    BackBtn: TToolbarButton97;
    StopBtn: TToolbarButton97;
    PlayBtn: TToolbarButton97;
    Box1: TPaintPanel;
    Timer1: TTimer;
    procedure Panel1Resize(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StopBtnClick(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure PlayBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Box1Paint(Sender: TObject; UpdateRect: PRect);
  private
    ReadThread, DisplayThread: TRFThread;
    PositionReset: Boolean;
    ThreadErrorMsg: String;
    KeepPlaying: boolean; // Rowdy
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
    function GetPlayPosition : Integer;
    procedure SetPlayPosition(nPos: Integer);
    procedure StoppingThreads;
    procedure WaitForThreads;
    procedure TerminateThread(Sender: TObject);
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr : String; override;
  public
    Info: TRawDataInfo;
    property PlayPosition: Integer read GetPlayPosition write SetPlayPosition;
  end;

 {------------------------}

implementation

uses QkUnknown, Quarkx, QkExceptions;

{$R *.DFM}

 {------------------------}

procedure TRawDataInfo.InitDC;
begin
 if DestDC<>0 then
  begin
   ReleaseDC(DestWnd, DestDC);
   DestDC:=0;
  end;
 DestWnd:=Wnd;
 if Wnd<>0 then
  begin
   DestDC:=GetDC(DestWnd);
   SetTextColor(DestDC, ColorToRGB(clInactiveCaptionText));
   SetBkColor(DestDC, ColorToRGB(clInactiveCaption));
  end;
end;

 {------------------------}

procedure TRFThread.Execute;
begin
 try
  while not Terminated and (Position<Info.Length) do
   if Reading then
    begin
     if WaitForSingleObject(Semaphore, 2222) = WAIT_TIMEOUT then
      begin
       ErrorMsg:=LoadStr1(5609);   { time out }
       Break;
      end;
     if Terminated then
      Break;
     Info.ReadProc(Position);
     Inc(Position);
    end
   else
    if Info.DisplayProc(Position) then
     begin
      Inc(Position);
      ReleaseSemaphore(Semaphore, 1, Nil);
     end;
 except
  on E: Exception do
   ErrorMsg:=GetExceptionMessage(E);
 end;
end;

 {------------------------}

function QRawFileObject.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=TFQRawFile.Create(nOwner);
end;

procedure QRawFileObject.LoadFile;
var
 Q: QUnknown;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      Q:=QUnknown.Create('Data', Self);
      SubElements.Add(Q);
      LoadedItem(rf_Default, F, Q, FSize);  { delayed read }
     end;
 else inherited;
 end;
end;

procedure QRawFileObject.SaveFile;
var
 Src: TStream;
 Taille: Integer;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      Taille:=GetReadStream(Src); try
      if Taille=0 then
       Raise EError(5611);
      F.CopyFrom(Src, Taille);
      finally ReleaseStream(Src); end;
     end;
 else inherited;
 end;
end;

procedure QRawFileObject.SetWriteString(const WriteString: String);
var
 Q: QObject;
 I: Integer;
begin
 Q:=SubElements.FindName('Data');
 if Q=Nil then
  begin
   Q:=QUnknown.Create('Data', Self);
   SubElements.Add(Q);
  end
 else
  begin
   I:=Q.Specifics.IndexOfName('Data');
   if I>=0 then
    Q.Specifics.Delete(I);
  end;
 Q.Specifics.Add(WriteString);
end;

function QRawFileObject.GetReadStream(var S: TStream) : Integer;
const
 Base = Length('Data=');
var
 Q: QObject;
 Data: String;
 I: Integer;
begin
 Data:='';
 Q:=SubElements.FindName('Data');
 if Q<>Nil then
  begin
   if Q.Flags and ofNotLoadedToMemory <> 0 then
    begin
     if Q.DirectDataAccess(S, Result) then
      Exit;
     Q.Acces;
    end;
   I:=Q.Specifics.IndexOfName('Data');
   if I>=0 then
    Data:=Q.Specifics[I];
  end;
 Result:=Length(Data)-Base;
 if Result<=0 then
  begin
   Result:=0;
   S:=Nil;
  end
 else
  S:=TRawDataStream.Create(PChar(Data)+Base, Result);
end;

 {------------------------}

procedure TFQRawFile.wmInternalMessage(var Msg: TMessage);
var
 I: Integer;
begin
 case Msg.wParam of
  wp_AfficherObjet:
    if FileObject<>Nil then
     with FileObject as QRawFileObject do
      begin
       Info:=GetDataInfo;
       TrackBar1.Enabled:=Info.Length>0;
       if Info.Length>0 then
        begin
         TrackBar1.Max:=Info.Length-1;
         I:=10*Info.Length div TrackBar1.Width;
         if I=0 then I:=1;
         TrackBar1.Frequency:=I;
         TrackBar1.PageSize:=I;
         PlayBtn.Enabled:=True;
        end
       else
        begin
         PlayBtn.Enabled:=False;
         TrackBar1.Frequency:=TrackBar1.Max;
        end;
       StopBtn.Enabled:=False;
       PlayPosition:=0;
      end;
 end;
 inherited;
end;

function TFQRawFile.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QRawFileObject) and inherited AssignObject(Q, State);
end;

function TFQRawFile.GetConfigStr;
begin
 if Info=Nil then
  Result:=''
 else
  Result:=Info.GetConfigStr1;
end;

procedure TFQRawFile.Panel1Resize(Sender: TObject);
begin
 TrackBar1.Width:=Panel1.Width-(409-288);
end;

function TFQRawFile.GetPlayPosition : Integer;
begin
 if DisplayThread=Nil then
  Result:=TrackBar1.Position
 else
  Result:=DisplayThread.Position;
end;

procedure TFQRawFile.SetPlayPosition(nPos: Integer);
begin
 StoppingThreads;
 if nPos>=Info.Length then
  nPos:=Info.Length-1;
 if nPos<0 then
  nPos:=0;
 TrackBar1.Position:=nPos;
 BackBtn.Enabled:=nPos>0;
 Box1Paint(Nil, Nil);
 PositionReset:=False;
end;

procedure TFQRawFile.TrackBar1Change(Sender: TObject);
begin
 // This is called when the trackbar position changes, whether the user dragged
 // the pointer or the timer's event updates the position.  If this is called
 // from the timer, we don't want to change the PlayPosition because that would
 // stop the sound playing.
 // PlayPosition is a property, when we write to it the SetPlayPosition event is
 // called, and the first thing that does is stop the read/play threads - bad.
 if not(KeepPlaying) then // Rowdy
  PlayPosition:=TrackBar1.Position;
end;

procedure TFQRawFile.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 StoppingThreads;
 WaitForThreads;
 inherited;
 Info.Free;
 Info:=Nil;
end;

procedure TFQRawFile.StopBtnClick(Sender: TObject);
begin
 PlayPosition:=PlayPosition;
end;

procedure TFQRawFile.StoppingThreads;
begin
 if Info=Nil then Exit;
 if DisplayThread<>Nil then
  begin
   Info.StopThreads;
   DisplayThread.Terminate;
  end;
 if ReadThread<>Nil then
  begin
   ReadThread.Terminate;
   ReleaseSemaphore(ReadThread.Semaphore, 1, Nil);
  end;
 Timer1.Enabled:=False;
 StopBtn.Enabled:=False;
end;

procedure TFQRawFile.BackBtnClick(Sender: TObject);
begin
 PlayPosition:=0;
end;

procedure TFQRawFile.PlayBtnClick(Sender: TObject);
var
 StartPos, BufCount: Integer;
 Semaphore: THandle;
begin
 if Info.Stream<>Nil then
  begin
   MessageBeep(0);
   Exit;
  end;
 StartPos:=PlayPosition;
 if StartPos=Info.Length-1 then
  StartPos:=0;
 KeepPlaying:=True; // Rowdy
 Info.InitDC(Box1.Handle);
 Info.StreamSize:=(FileObject as QRawFileObject).GetReadStream(Info.Stream);
 Info.StreamSource:=Info.Stream.Position;
 Semaphore:=0;
 try
  BufCount:=Info.InitThreads(StartPos);
  ThreadErrorMsg:='';
  ReadThread:=TRFThread.Create(True);          DisplayThread:=TRFThread.Create(True);
  ReadThread.FreeOnTerminate:=True;            DisplayThread.FreeOnTerminate:=True;
  ReadThread.OnTerminate:=TerminateThread;     DisplayThread.OnTerminate:=TerminateThread;
  ReadThread.Reading:=True;
  ReadThread.Position:=StartPos;               DisplayThread.Position:=StartPos;
  ReadThread.Info:=Info;                       DisplayThread.Info:=Info;
  Semaphore:=CreateSemaphore(Nil, BufCount, BufCount, Nil);
  ReadThread.Semaphore:=Semaphore;             DisplayThread.Semaphore:=Semaphore;
 except
  ReadThread.Free;
  ReadThread:=Nil;
  DisplayThread.Free;
  DisplayThread:=Nil;
  CloseHandle(Semaphore);
  Info.InitDC(0);
  ReleaseStream(Info.Stream);
  Info.Stream:=Nil;
  PlayPosition:=PlayPosition;
  Raise;
 end;
 ReadThread.Resume;                           DisplayThread.Resume;
 Timer1.Enabled:=True;
 PlayBtn.Enabled:=False;
 StopBtn.Enabled:=True;
 BackBtn.Enabled:=True;
 PositionReset:=True;
end;

procedure TFQRawFile.Timer1Timer(Sender: TObject);
begin
 if DisplayThread=Nil then
  Timer1.Enabled:=False
 else
  if Info.Length>0 then
   begin
    TrackBar1.Max:=Info.Length-1;
    // keep playing the sound, the Trackbar1 OnChange event will update the PlayPosition
    // based on TrackBar1.Position, but this would stop the sound playing, so set the
    // KeepPlaying flag so that the PlayPosition does not get updated
    KeepPlaying:=True; // Rowdy
    TrackBar1.Position:=DisplayThread.Position;
    KeepPlaying:=False; // Rowdy
   end;
end;

procedure TFQRawFile.TerminateThread;
var
 Semaphore: THandle;
 Rect: TRect;
begin
 if Sender=ReadThread then
  begin
   Semaphore:=ReadThread.Semaphore;
   if ReadThread.ErrorMsg<>'' then
    ThreadErrorMsg:=ReadThread.ErrorMsg;
   ReadThread:=Nil;
  end
 else
  begin
   if PositionReset then
    Timer1Timer(Nil);
   Semaphore:=DisplayThread.Semaphore;
   if DisplayThread.ErrorMsg<>'' then
    ThreadErrorMsg:=DisplayThread.ErrorMsg;
   DisplayThread:=Nil;
  end;
 if (ReadThread=Nil) and (DisplayThread=Nil) then
  begin
   CloseHandle(Semaphore);
   Info.DoneThreads;
   if ThreadErrorMsg<>'' then
    begin
     SetBkColor(Info.DestDC, ColorToRGB(clInactiveCaption));
     SetTextColor(Info.DestDC, ColorToRGB(clInactiveCaptionText));
     Rect:=Box1.ClientRect;
     DrawText(Info.DestDC, PChar(ThreadErrorMsg), Length(ThreadErrorMsg), Rect, DT_NOCLIP or DT_WORDBREAK);
     ThreadErrorMsg:='';
    end;
   Info.InitDC(0);
   ReleaseStream(Info.Stream);
   Info.Stream:=Nil;
   PlayBtn.Enabled:=True;
   PlayPosition:=PlayPosition;
  end
 else
  if ThreadErrorMsg<>'' then
   StoppingThreads;
end;

procedure TFQRawFile.WaitForThreads;
begin
 if (Info<>Nil) and (Info.Stream<>Nil) then
  begin
   Screen.Cursor:=crHourglass; try
   repeat
    Application.ProcessMessages;
   until Info.Stream=Nil;
   finally Screen.Cursor:=crDefault; end;
  end;
end;

procedure TFQRawFile.Box1Paint(Sender: TObject; UpdateRect: PRect);
var
 nPos: Integer;
begin
 if (Info=Nil) or (Info.Length=0) then Exit;
 if Info.Stream=Nil then
  begin
   nPos:=PlayPosition;
   if Info.StaticData then
    begin
     Info.StreamSize:=(FileObject as QRawFileObject).GetReadStream(Info.Stream);
     Info.StreamSource:=Info.Stream.Position;
    end;
   try
    Info.InitDC(Box1.Handle); try
    Info.ReadProc(nPos);
    Info.DisplayProc(nPos);
    finally Info.InitDC(0); end;
   finally
    if Info.Stream<>Nil then
     begin
      ReleaseStream(Info.Stream);
      Info.Stream:=Nil;
     end;
   end;
  end;
 TrackBar1.Max:=Info.Length-1;
end;

end.

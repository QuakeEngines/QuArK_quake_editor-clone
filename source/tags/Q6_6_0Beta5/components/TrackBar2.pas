{
  QuArK TrackBar2 - Delphi's TrackBar, but then better!

  Made by: DanielPharos
       in: 2010
}

unit TrackBar2;

interface

uses
  Windows, Messages, ComCtrls, CommCtrl, Graphics, Classes,
  Controls, SysUtils;

type
  TTBEndTrack = packed record //FIXME: Untested!
    Msg: Cardinal;
    Unused1: Longint;
    Unused2: Longint;
    Result: Longint;
  end;

  TTrackBar2 = class(TTrackBar)
  private
    FCanvas: TCanvas;
    FOnAccept: TNotifyEvent;
    FSteps: Integer;
    FAmIFloat: Boolean;
    FIntMin: Integer;
    FIntMax: Integer;
    FFloatMin: Single;
    FFloatMax: Single;
    FShowValue: Boolean;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure TBEndTrack(var Message: TTBEndTrack); message TB_ENDTRACK;
    procedure SetSteps(Value: Integer);
    procedure SetAmIFloat(Value: Boolean);
    function GetIntPosition() : Integer;
    procedure SetIntPosition(Value: Integer);
    procedure SetIntMin(Value: Integer);
    procedure SetIntMax(Value: Integer);
    function GetFloatPosition() : Single;
    procedure SetFloatPosition(Value: Single);
    procedure SetFloatMin(Value: Single);
    procedure SetFloatMax(Value: Single);
    procedure SetShowValue(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property Steps: Integer read FSteps write SetSteps;
    property AmIFloat: Boolean read FAmIFloat write SetAmIFloat;
    property IntPosition: Integer read GetIntPosition write SetIntPosition;
    property IntMax: Integer read FIntMax write SetIntMax;
    property IntMin: Integer read FIntMin write SetIntMin;
    property FloatPosition: Single read GetFloatPosition write SetFloatPosition;
    property FloatMax: Single read FFloatMax write SetFloatMax;
    property FloatMin: Single read FFloatMin write SetFloatMin;
    property ShowValue: Boolean read FShowValue write SetShowValue;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TTrackBar2]);
end;

// --------

constructor TTrackBar2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FFloatMax := 10.0;
  FFloatMin := 0.0;
end;

destructor TTrackBar2.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TTrackBar2.WMPaint(var Message: TWMPaint);
var
  S: String;
  TextRect: TRect;
begin
  inherited;

  if ShowValue then
  begin
    if AmIFloat then
      S:=FloatToStr(FloatPosition)
    else
      S:=IntToStr(IntPosition);

    FCanvas.Lock;
    try
      TextRect.Left := 0;
      TextRect.Right := Width;
      TextRect.Top := Height - 16; //FIXME: Assuming 16 as font-height...!
      TextRect.Bottom := Height;
      FillRect(FCanvas.Handle, TextRect, HBrush(COLOR_WINDOW+1));
      DrawText(FCanvas.Handle, PChar(S), -1, TextRect, DT_CENTER or DT_SINGLELINE);
    finally
      FCanvas.Unlock;
    end;
  end;
end;

procedure TTrackBar2.WMLButtonUp(var Message: TWMLButtonUp);
begin
  //For some reason, releasing the mouse doesn't trigger TBEndTrack 'on time'
  //FIXME: Not proper, but hey, it works!
  if Assigned(OnAccept) then
    OnAccept(Self);
  inherited;
end;

procedure TTrackBar2.TBEndTrack(var Message: TTBEndTrack);
begin
  if Assigned(OnAccept) then
    OnAccept(Self);
  inherited;
end;

procedure TTrackBar2.SetSteps(Value: Integer);
begin
  if not FAmIFloat then
    if (IntMax - IntMax) mod Value <> 0 then
      raise exception.create('TTrackBar2: Non-integer tick-size while in integer mode!');
  if Position > Value then
    Position := Value;
  Max := Value;
  FSteps := Value;
end;

procedure TTrackBar2.SetAmIFloat(Value: Boolean);
begin
  FAmIFloat := Value;
end;

function TTrackBar2.GetIntPosition() : Integer;
begin
  if AmIFloat then
    raise exception.create('TTrackBar2: Call to integer-function while in float mode!');
  Result:=IntMin + ((Position * (IntMax - IntMin)) div (Max - Min));
end;

procedure TTrackBar2.SetIntPosition(Value: Integer);
begin
  if AmIFloat then
    raise exception.create('TTrackBar2: Call to integer-function while in float mode!');
  Position:=(((Value - IntMin) * (Max - Min)) div (IntMax - IntMin));
end;

procedure TTrackBar2.SetIntMin(Value: Integer);
begin
  if AmIFloat then
    raise exception.create('TTrackBar2: Call to integer-function while in float mode!');
  if (Value > IntMax) then
    IntMax := Value;
  FIntMin := Value;
end;

procedure TTrackBar2.SetIntMax(Value: Integer);
begin
  if AmIFloat then
    raise exception.create('TTrackBar2: Call to integer-function while in float mode!');
  if (Value < IntMin) then
    IntMin := Value;
  FIntMax := Value;
end;

function TTrackBar2.GetFloatPosition() : Single;
begin
  if not AmIFloat then
    raise exception.create('TTrackBar2: Call to float-function while in integer mode!');
  Result:=FloatMin + ((Position / (Max - Min)) * (FloatMax - FloatMin));
end;

procedure TTrackBar2.SetFloatPosition(Value: Single);
begin
  if not AmIFloat then
    raise exception.create('TTrackBar2: Call to float-function while in integer mode!');
  Position:=Round((((Value  - FloatMin) / (FloatMax - FloatMin)) * (Max-Min)));
end;

procedure TTrackBar2.SetFloatMin(Value: Single);
begin
  if not AmIFloat then
    raise exception.create('TTrackBar2: Call to float-function while in integer mode!');
  if (Value > FFloatMax) then
    FFloatMax := Value;
  FFloatMin := Value;
end;

procedure TTrackBar2.SetFloatMax(Value: Single);
begin
  if not AmIFloat then
    raise exception.create('TTrackBar2: Call to float-function while in integer mode!');
  if (Value < FFloatMin) then
    FFloatMin := Value;
  FFloatMax := Value;
end;

procedure TTrackBar2.SetShowValue(Value: Boolean);
begin
  FShowValue := Value;
end;

end.

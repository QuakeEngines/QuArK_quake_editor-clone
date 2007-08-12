unit UnitDateUtil;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormDateUtil = class(TForm)
    TextResult: TEdit;
    LabelResult: TLabel;
    TextYear: TEdit;
    TextMonth: TEdit;
    TextDay: TEdit;
    LabelDay: TLabel;
    LabelMonth: TLabel;
    LabelYear: TLabel;
    ButtonExit: TButton;
    procedure Recalc;
    procedure FormCreate(Sender: TObject);
    procedure TextDayChange(Sender: TObject);
    procedure TextMonthChange(Sender: TObject);
    procedure TextYearChange(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
  private
    DoNotCalc: Boolean;
  end;

var
  FormDateUtil: TFormDateUtil;
  curYear, curMonth, curDay: Word;

implementation

uses DateUtils, StrUtils;

{$R *.dfm}

procedure TFormDateUtil.Recalc;
begin
  if DoNotCalc then Exit;
  TextResult.Text := Format('%.0f', [EncodeDate(curYear, curMonth, curDay)]);;
end;

procedure TFormDateUtil.FormCreate(Sender: TObject);
var
  S: String;
begin
  DoNotCalc := true;
  curDay := DayOf(Now);
  Str(curDay, S);
  TextDay.Text := S;
  curMonth := MonthOf(Now);
  Str(curMonth, S);
  TextMonth.Text := S;
  curYear := YearOf(Now);
  Str(curYear, S);
  TextYear.Text := S;
  DoNotCalc := false;
  Recalc;
end;

procedure TFormDateUtil.TextDayChange(Sender: TObject);
var
  S: String;
begin
  curDay := StrToInt(TextDay.Text);
  Str(curDay, S);
  Recalc;
end;

procedure TFormDateUtil.TextMonthChange(Sender: TObject);
var
  S: String;
begin
  curMonth := StrToInt(TextMonth.Text);
  Str(curMonth, S);
  Recalc;
end;

procedure TFormDateUtil.TextYearChange(Sender: TObject);
var
  S: String;
begin
  curYear := StrToInt(TextYear.Text);
  Str(curYear, S);
  Recalc;
end;

procedure TFormDateUtil.ButtonExitClick(Sender: TObject);
begin
  FormDateUtil.Close;
end;

end.

object FullScrDlg: TFullScrDlg
  Left = 204
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Full screen'
  ClientHeight = 149
  ClientWidth = 249
  Color = clBackground
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 136
    Width = 249
    Height = 13
    Align = alBottom
    Alignment = taCenter
    AutoSize = False
    Caption = 'Click here to cancel'
    Color = clBtnFace
    ParentColor = False
    OnMouseDown = FormMouseDown
    OnMouseMove = FormMouseMove
    OnMouseUp = FormMouseUp
  end
end

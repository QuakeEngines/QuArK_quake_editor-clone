inherited QForm2: TQForm2
  Caption = 'QForm2'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 9
    Top = 13
    Width = 409
    Height = 246
    Align = alClient
    OnDblClick = ListView1DblClick
    Columns = <>
    DragMode = dmAutomatic
    ReadOnly = True
    MultiSelect = True
    OnDragDrop = ListView1DragDrop
    OnDragOver = ListView1DragOver
    OnStartDrag = ListView1StartDrag
    OnEndDrag = ListView1EndDrag
    OnMouseDown = ListView1MouseDown
    TabOrder = 4
    ViewStyle = vsList
    OnKeyDown = ListView1KeyDown
  end
end

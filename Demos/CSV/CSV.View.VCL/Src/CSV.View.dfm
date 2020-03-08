object frmCSV: TfrmCSV
  Left = 0
  Top = 0
  Caption = 'Test CSV-MVVM'
  ClientHeight = 450
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    536
    450)
  PixelsPerInch = 96
  TextHeight = 13
  object lePath: TLabeledEdit
    Left = 8
    Top = 17
    Width = 484
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Path'
    TabOrder = 0
    TextHint = 'Introduce el path'
    OnChange = lePathChange
  end
  object btPath: TButton
    Left = 498
    Top = 15
    Width = 34
    Height = 25
    Action = DoSelectFile
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object mmInfo: TMemo
    Left = 8
    Top = 56
    Width = 521
    Height = 233
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object btTNP: TButton
    Left = 8
    Top = 295
    Width = 521
    Height = 50
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Test-No Parallel'
    TabOrder = 3
    OnClick = btTNPClick
  end
  object btTP: TButton
    Left = 8
    Top = 351
    Width = 521
    Height = 48
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Test-Parallel'
    TabOrder = 4
    OnClick = btTPClick
  end
  object btNuevaVista: TButton
    Left = 8
    Top = 417
    Width = 519
    Height = 25
    Action = DoCrearVista
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 5
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultExtension = '.csv'
    DefaultFolder = 'C:\Temp\'
    FavoriteLinks = <>
    FileNameLabel = 'Tes1.csv'
    FileTypes = <>
    Options = []
    Left = 224
    Top = 24
  end
  object ActionList1: TActionList
    Left = 224
    Top = 88
    object DoCrearVista: TAction
      Caption = 'Crear Otra Vista'
      OnExecute = DoCrearVistaExecute
    end
    object DoSelectFile: TAction
      Caption = '...'
      OnExecute = DoSelectFileExecute
    end
  end
end

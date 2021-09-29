object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderWidth = 8
  Caption = 'FormMain'
  ClientHeight = 426
  ClientWidth = 612
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -20
  Font.Name = 'Courier New'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 22
  object EditExpression: TEdit
    Left = 0
    Top = 0
    Width = 612
    Height = 30
    Align = alTop
    TabOrder = 0
    Text = '(6-1.5*2)*3/2 + -20'
  end
  object ButtonExecute: TButton
    AlignWithMargins = True
    Left = 3
    Top = 33
    Width = 606
    Height = 25
    Align = alTop
    Caption = 'Execute'
    TabOrder = 1
    OnClick = ButtonExecuteClick
  end
  object MemoLog: TMemo
    Left = 0
    Top = 61
    Width = 612
    Height = 365
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
  end
end

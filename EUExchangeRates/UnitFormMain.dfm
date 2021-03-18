object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'EU Exchange Rates'
  ClientHeight = 458
  ClientWidth = 582
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 25
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 582
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 0
    object NumberBoxEuro: TNumberBox
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 491
      Height = 25
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Align = alClient
      CurrencyString = #8364
      LargeStep = 100.000000000000000000
      Mode = nbmCurrency
      MaxValue = 1000.000000000000000000
      TabOrder = 0
      SpinButtonOptions.ButtonWidth = 25
      SpinButtonOptions.Placement = nbspInline
      SpinButtonOptions.ShowInlineDividers = False
      UseMouseWheel = True
      NegativeValueColor = clRed
      ExplicitHeight = 33
    end
    object ButtonRates: TButton
      Left = 506
      Top = 1
      Width = 75
      Height = 39
      Align = alRight
      Caption = 'Rates'
      TabOrder = 1
      OnClick = ButtonRatesClick
    end
  end
  object GridPanel1: TGridPanel
    Left = 0
    Top = 41
    Width = 582
    Height = 417
    Align = alClient
    Caption = 'GridPanel1'
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <>
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    RowCollection = <
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end
      item
        Value = 6.250000000000000000
      end>
    TabOrder = 1
  end
end

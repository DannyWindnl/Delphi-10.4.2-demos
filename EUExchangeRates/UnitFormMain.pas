unit UnitFormMain;

{===============================================================================
 Demo for the new Delphi 10.4.2 TNumberBox control

 Requests the current Euro exchange rates from thge European Central Bank and
 displays these in TNumberBox components

 TNumberBox overrides TFormatSettings loaded from the system if set in the
 TNumberBox properties.

 e.g TFormatSettings.CurrencyFormat

  CurrencyFormat - Defines the currency symbol placement and separation
  used in floating-point to decimal conversions. Possible values are:

    0 = '$1'
    1 = '1$'
    2 = '$ 1'
    3 = '1 $'

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.NumberBox,
  Vcl.ExtCtrls, System.Generics.Collections, Vcl.ToolWin, Vcl.ComCtrls;

type
  TFormMain = class(TForm)
    NumberBoxEuro: TNumberBox;
    Panel1: TPanel;
    ButtonRates: TButton;
    GridPanel1: TGridPanel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRatesClick(Sender: TObject);
  private
    { Private declarations }
    function NewNumberBox(const CurrencyOrd: Integer): TNumberBox;
  public
    { Public declarations }
    FNumberBoxes: TArray<TNumberBox>;
    procedure DisplayRates;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  System.Types, System.StrUtils, UnitDataMain;

procedure TFormMain.ButtonRatesClick(Sender: TObject);
begin
  DataModuleMain.LoadRates;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  lNumberBox: TNumberBox; {Hint on unused variable}
  lCol, lRow: Integer;
begin
  SetLength(FNumberBoxes, 32);
  for var i:Integer := Low(FNumberBoxes) to High(FNumberBoxes) do
  begin
    GridPanel1.ControlCollection.AddControl(NewNumberBox(i), lCol, lRow); {Warning on uninitialized variables}
  end;
end;


function TFormMain.NewNumberBox(const CurrencyOrd: Integer): TNumberBox;
begin
  Result := TNumberBox.Create(Self);
  Result.Parent := GridPanel1;
  Result.Mode := TNumberBoxMode.nbmCurrency;
  {Unicode has a currency block between U+20A0 and U+20BF - set these for the demo}
  Result.CurrencyString := '' + Char($20A0 + CurrencyOrd);
  Result.Margins.SetControlBounds(7,7,7,7,True);
  Result.Align := TAlign.alClient;
  Result.CurrencyFormat := 2; {2 = '$ 1'}
  Result.ReadOnly := True;
end;

procedure TFormMain.DisplayRates;
var
  lItems: TArray<TPair<string,Double>>;
  lMax: Integer;
begin
  lItems := DataModuleMain.Rates.ToArray;
  lMax := DataModuleMain.Rates.Count;
  if lMax > 32 then
    lMax := 32;
  for var i := 0 to lMax-1 do
  begin
    (GridPanel1.ControlCollection.Items[i].Control as TNumberBox).CurrencyString := lItems[i].Key;
    (GridPanel1.ControlCollection.Items[i].Control as TNumberBox).Value := NumberBoxEuro.Value * lItems[i].Value;
  end;
end;

end.

program EUExchangeRates;

uses
  Vcl.Forms,
  UnitFormMain in 'UnitFormMain.pas' {FormMain},
  Vcl.Themes,
  Vcl.Styles,
  UnitDataMain in 'UnitDataMain.pas' {DataModuleMain: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Tablet Dark');
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TDataModuleMain, DataModuleMain);
  Application.Run;
end.

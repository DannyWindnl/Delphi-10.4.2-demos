unit UnitDataMain;

{===============================================================================
 Uses current exchange rates as published by the European Central Bank
 through the api at https://exchangeratesapi.io/

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}
interface

uses
  System.SysUtils, System.Classes, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.Generics.Collections;

type
  TDataModuleMain = class(TDataModule)
    NetHTTPRequest: TNetHTTPRequest;
    NetHTTPClient: TNetHTTPClient;
    procedure NetHTTPRequestRequestCompleted(const Sender: TObject;
      const AResponse: IHTTPResponse);
    procedure DataModuleCreate(Sender: TObject);
  private
    FRates: TDictionary<string, Double>;
    { Private declarations }
  public
    { Public declarations }
    FJSONRawBytes: TArray<Byte>;
    procedure LoadRates;
    procedure Parse;
    property Rates: TDictionary<string, Double> read FRates;
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  System.JSON, Vcl.Dialogs, UnitFormMain;

procedure TDataModuleMain.DataModuleCreate(Sender: TObject);
begin
  FRates := TDictionary<string, Double>.Create;
end;

procedure TDataModuleMain.NetHTTPRequestRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
var
  lStream: TStream;
begin
  lStream := AResponse.ContentStream;
  lStream.Position := 0;
  SetLength(FJSONRawBytes, lStream.Size);
  {we go through FJSONRawBytes to show the new debug visualizers}
  lStream.ReadData(FJSONRawBytes, lStream.Size);
  Parse;
  {for this demo we directly call back to the mainform, this is ugly code,
   in real code use loose coupling through livebindings or similar}
  FormMain.DisplayRates;
end;

procedure TDataModuleMain.Parse;
var
  lJSONValue: TJSONValue;
  lJSONObject: TJSONObject;
  lJSONRates: TJSONValue;
  lJSONList: TJSONObject;
  lKey: string;
  lValue: Double;
begin
  lJSONValue := TJSONObject.ParseJSONValue(FJSONRawBytes, 0, Length(FJSONRawBytes),[TJSONObject.TJSONParseOption.IsUTF8]);
  if lJSONValue <> nil then
  begin
    lJSONObject := lJSONValue as TJSONObject;
    lJSONRates := lJSONObject.Values['rates'];
    if (lJSONRates <> nil) then
    begin
      lJSONList := lJSONRates as TJSONObject;
      for var i := 0 to lJSONList.Count-1 do
      begin
        lKey := lJSONList.Pairs[i].JsonString.Value;
        lValue := lJSONList.Pairs[i].JsonValue.AsType<Double>;
        FRates.AddOrSetValue(lKey, lValue);
      end;
    end;
  end;
end;

procedure TDataModuleMain.LoadRates;
begin
  NetHTTPRequest.Execute();
end;

end.

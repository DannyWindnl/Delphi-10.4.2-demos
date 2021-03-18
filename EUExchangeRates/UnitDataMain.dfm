object DataModuleMain: TDataModuleMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 188
  Width = 374
  object NetHTTPRequest: TNetHTTPRequest
    Asynchronous = True
    MethodString = 'GET'
    URL = 'https://api.exchangeratesapi.io/latest'
    Client = NetHTTPClient
    OnRequestCompleted = NetHTTPRequestRequestCompleted
    Left = 200
    Top = 64
  end
  object NetHTTPClient: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 88
    Top = 64
  end
end

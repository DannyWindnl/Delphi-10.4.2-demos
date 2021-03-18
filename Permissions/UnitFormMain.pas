unit UnitFormMain;

{===============================================================================
 Demo for Android 10/11 Permissions

 Required starting from November-2020 due to minimal target SDK requirement 29
 by Google.

 The permissions in the ComboBox are the default permissions that are used
 when starting development on an Android App (debug mode).

 Based on example code which is bundled in Delphi 10.4.2

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}


interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox,
  System.Permissions;

type
  TFormMain = class(TForm)
    ComboBoxPermissions: TComboBox;
    ButtonPermissions: TButton;
    MemoPermissions: TMemo;
    procedure ButtonPermissionsClick(Sender: TObject);
  private
    { Private declarations }
    procedure DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
    procedure AfterPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  FMX.DialogService;

procedure TFormMain.ButtonPermissionsClick(Sender: TObject);
var
  lPermission: string;
begin
  lPermission := ComboBoxPermissions.Items[ComboBoxPermissions.ItemIndex];
  PermissionsService.RequestPermissions([lPermission], AfterPermissionRequestResult, DisplayRationale)
end;

procedure TFormMain.DisplayRationale(Sender: TObject; const APermissions: TArray<string>; const APostRationaleProc: TProc);
begin
  {Async call to ShowMessage,
   calls APostRationaleProc after user clicks one of the dialog Buttons}
  TDialogService.ShowMessage('A good reason for the app to need this access right',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end);
end;

procedure TFormMain.AfterPermissionRequestResult(Sender: TObject; const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>);
var
  lPermission: string;
begin
  for var i: Integer := Low(APermissions) to High(APermissions) do
  begin
    lPermission := APermissions[i];
    case AGrantResults[i] of
      TPermissionStatus.Granted : lPermission := lPermission + ' granted';
      TPermissionStatus.Denied : lPermission := lPermission + ' denied';
      TPermissionStatus.PermanentlyDenied : lPermission := lPermission + ' permanently denied';
    end;
    MemoPermissions.Lines.Add(lPermission);
  end;
end;


end.

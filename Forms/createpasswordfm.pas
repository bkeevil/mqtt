unit createpasswordfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Buttons, Rand;

const
  RANDOM_PASSWORD_CHARS: set of char = ['A'..'Z','a'..'z','0'..'9'];

type

  { TCreatePasswordDialog }

  TCreatePasswordDialog = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    RandomPasswordBtn: TButton;
    ShowPasswordCheckbox: TCheckBox;
    PasswordEdit: TEdit;
    ConfirmEdit: TEdit;
    PasswordLabel: TLabel;
    ConfirmLabel: TLabel;
    RandomPasswordCharsEdit: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RandomPasswordBtnClick(Sender: TObject);
    procedure ShowPasswordCheckboxChange(Sender: TObject);
  private
    function GenerateRandomPasswordChar: Char;
  public

  end;

var
  CreatePasswordDialog: TCreatePasswordDialog;
  Entropy: TEntropyAccumulator;

function CreatePasswordDlg(out PasswordHash: String): Boolean;

implementation

uses
  Crypto;

function CreatePasswordDlg(out PasswordHash: String): Boolean;
begin
  if not Assigned(CreatePasswordDialog) then
    CreatePasswordDialog := TCreatePasswordDialog.Create(Application.MainForm);
  CreatePasswordDialog.ActiveControl := CreatePasswordDialog.PasswordEdit;
  CreatePasswordDialog.PasswordEdit.Text := '';
  CreatePasswordDialog.ConfirmEdit.Text := '';
  Result := CreatePasswordDialog.ShowModal = mrOK;
  if Result then
    PasswordHash := Block256ToBase64(SHA256String(CreatePasswordDialog.PasswordEdit.Text));
end;

{$R *.lfm}

{ TCreatePasswordDialog }

procedure TCreatePasswordDialog.ShowPasswordCheckboxChange(Sender: TObject);
begin
  if ShowPasswordCheckbox.Checked then
    begin
      PasswordEdit.PasswordChar := chr(0);
      ConfirmEdit.PasswordChar := chr(0);
    end
  else
    begin
      PasswordEdit.PasswordChar := '*';
      ConfirmEdit.PasswordChar := '*';
    end;
end;


function TCreatePasswordDialog.GenerateRandomPasswordChar: Char;
begin
  repeat
    FillRandom(@Result,1);
  until Result in RANDOM_PASSWORD_CHARS;
end;

procedure TCreatePasswordDialog.RandomPasswordBtnClick(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  S := '';
  for I := 1 to RandomPasswordCharsEdit.Value do
    S := S + GenerateRandomPasswordChar;
end;

procedure TCreatePasswordDialog.FormCreate(Sender: TObject);
begin
  RNG.Entropy := Entropy;
end;

procedure TCreatePasswordDialog.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Entropy.MouseMove(X,Y);
end;

initialization
  Entropy := TEntropyAccumulator.Create;
finalization
  Entropy.Free;
end.


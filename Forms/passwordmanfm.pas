unit passwordmanfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Grids,
  ExtCtrls, ComCtrls, StdCtrls, PasswordMan;

type

  { TPassManForm }

  TPassManForm = class(TForm)
    CancelBtn: TButton;
    OKBtn: TButton;
    ToolbarImages: TImageList;
    StringGrid: TStringGrid;
    ToolBar1: TToolBar;
    AddBtn: TToolButton;
    DeleteBtn: TToolButton;
    DisabledToolbarImages: TImageList;
    procedure AddBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
  private
    procedure InitRow(I: Integer);
    function ValidateRow(I: Integer): Boolean;
    { private declarations }
  public
    PassMan: TPasswordManager;
    procedure GridToPassman;
    procedure PassmanToGrid;
  end;

var
  PassManForm: TPassManForm;

implementation

{$R *.lfm}

{ TPassManForm }

procedure TPassManForm.InitRow(I: Integer);
begin
  StringGrid.Cells[0,I] := '1';
  StringGrid.Cells[3,I] := '0';
  StringGrid.Cells[4,I] := DateTimeToStr(Now);
end;

procedure TPassManForm.AddBtnClick(Sender: TObject);
begin
  StringGrid.RowCount := StringGrid.RowCount+1;
  InitRow(StringGrid.RowCount - 1);
end;

procedure TPassManForm.DeleteBtnClick(Sender: TObject);
begin
  if StringGrid.Selection.Top > 0 then
    StringGrid.DeleteRow(StringGrid.Selection.Top);
end;

function TPassManForm.ValidateRow(I: Integer): Boolean;
begin
  Result := (StringGrid.Cells[0,I] > '') and
            (StringGrid.Cells[1,I] > '') and
            (StringGrid.Cells[2,I] > '') and
            (StringGrid.Cells[3,I] > '');
end;

procedure TPassManForm.GridToPassman;
var
  I: Integer;
  O: TPasswordManagerAccount;
begin
  PassMan.Clear;
  for I := 1 to StringGrid.RowCount - 1 do
    begin
      if ValidateRow(I) then
        begin
          O := TPasswordManagerAccount.Create(PassMan);
          O.Enabled := StringGrid.Cells[0,I] = '1';
          O.Username := StringGrid.Cells[1,I];
          O.Password := StringGrid.Cells[2,I];
          O.Admin := StringGrid.Cells[3,I] = '1';
          O.LastActive := StrToDateTime(StringGrid.Cells[4,I]);
          O.Description := StringGrid.Cells[5,I];
        end;
    end;
end;

procedure TPassManForm.PassmanToGrid;
var
  I: Integer;
  O: TPasswordManagerAccount;
begin
  StringGrid.RowCount := 1;
  StringGrid.RowCount := PassMan.Count + 1;
  for I := 0 to PassMan.Count - 1 do
    begin
      O := PassMan[I];
      if O.Enabled then
        StringGrid.Cells[0,I+1] := '1'
      else
        StringGrid.Cells[0,I+1] := '0';
      StringGrid.Cells[1,I+1] := O.Username;
      StringGrid.Cells[2,I+1] := O.Password;
      if O.Admin then
        StringGrid.Cells[3,I+1] := '1'
      else
        StringGrid.Cells[3,I+1] := '0';
      StringGrid.Cells[4,I+1] := DateTimeToStr(O.LastActive);
      StringGrid.Cells[5,I+1] := O.Description;
    end;
end;

end.


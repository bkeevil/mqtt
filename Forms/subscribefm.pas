unit subscribefm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, Buttons;

type

  { TSubscribeForm }

  TSubscribeForm = class(TForm)
    AddBtn: TButton;
    SelectAllBtn: TButton;
    ValidateBtn: TButton;
    TestDataBtn: TButton;
    DeleteBtn: TButton;
    CancelBtn: TButton;
    OKBtn: TButton;
    Grid: TStringGrid;
    procedure AddBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure GridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure SelectAllBtnClick(Sender: TObject);
    procedure TestDataBtnClick(Sender: TObject);
    procedure ValidateBtnClick(Sender: TObject);
  private


  public
    procedure ValidateRows;
  end;

var
  SubscribeForm: TSubscribeForm;

implementation

{$R *.lfm}

{ TSubscribeForm }

procedure TSubscribeForm.DeleteBtnClick(Sender: TObject);
begin
  Grid.DeleteRow(Grid.Selection.Top);
end;

procedure TSubscribeForm.GridColRowInserted(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if (not IsColumn) then
    Grid.Cells[2,tIndex] := Grid.Columns[2].Picklist[2];
end;

procedure TSubscribeForm.SelectAllBtnClick(Sender: TObject);
var
  X: Integer;
begin
  for X := 1 to Grid.RowCount - 1 do
    Grid.Cells[0,X] := '1';
end;

procedure TSubscribeForm.TestDataBtnClick(Sender: TObject);
begin
  Grid.InsertRowWithValues(1,['0','Topic1/TopicA','EXACTLY ONCE']);
  Grid.InsertRowWithValues(2,['0','Topic1/TopicB','EXACTLY ONCE']);
  Grid.InsertRowWithValues(3,['0','Topic1/TopicB/Test','EXACTLY ONCE']);
  Grid.InsertRowWithValues(4,['0','Topic2/TopicA','EXACTLY ONCE']);
  Grid.InsertRowWithValues(5,['0','Topic2/TopicB','EXACTLY ONCE']);
  Grid.InsertRowWithValues(6,['0','Topic2/TopicB/Test','EXACTLY ONCE']);
end;

procedure TSubscribeForm.ValidateBtnClick(Sender: TObject);
begin
  ValidateRows;
end;

procedure TSubscribeForm.ValidateRows;
var
  X,Y: Integer;
begin
  for X := Grid.RowCount - 1 downto 1 do
    begin
      if Grid.Columns[2].PickList.IndexOf(Grid.Cells[2,X]) = -1 then
        Grid.DeleteRow(X)
      else
        if Grid.Cells[1,X] = '' then
          Grid.DeleteRow(X)
        else
          for Y := X - 1 downto 0 do
            if Grid.Cells[1,Y] = Grid.Cells[1,X] then
              begin
                Grid.DeleteRow(X);
                Break;
              end;
    end;

end;

procedure TSubscribeForm.AddBtnClick(Sender: TObject);
begin
  Grid.RowCount := Grid.RowCount + 1;
  Grid.Cells[0,Grid.RowCount-1] := '1';
  Grid.Cells[2,Grid.RowCount-1] := Grid.Columns[2].PickList[2];
  Grid.Cells[3,Grid.RowCount-1] := '0';
end;

end.


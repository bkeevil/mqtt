unit passmanfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ValEdit, ExtCtrls, ComCtrls, ActnList, StdActns, DBGrids, DbCtrls, StdCtrls,
  DBActns, PasswordMan, db, memds;

type

  { TPassManForm }

  TPassManForm = class(TForm)
    CoolBar: TCoolBar;
    DataSetCancel1: TDataSetCancel;
    DataSetDelete1: TDataSetDelete;
    DataSetEdit1: TDataSetEdit;
    DataSetFirst1: TDataSetFirst;
    DataSetInsert1: TDataSetInsert;
    DataSetLast1: TDataSetLast;
    DataSetNext1: TDataSetNext;
    DataSetPost1: TDataSetPost;
    DataSetPrior1: TDataSetPrior;
    DataSetRefresh1: TDataSetRefresh;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    FileClose: TAction;
    FileSave: TAction;
    FileNew: TAction;
    ActionList: TActionList;
    FileExit: TFileExit;
    FileOpen: TFileOpen;
    FileSaveAs: TFileSaveAs;
    CloseItm: TMenuItem;
    FileToolbar: TToolBar;
    MemDataset: TMemDataset;
    ExitToolbar: TToolBar;
    ToolbarButtons: TImageList;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    NewItm: TMenuItem;
    OpenItm: TMenuItem;
    SaveItm: TMenuItem;
    SaveAsItm: TMenuItem;
    MenuDividerItm: TMenuItem;
    ExitItm: TMenuItem;
    DisabledButtons: TImageList;
    FileNewBtn: TToolButton;
    FileOpenBtn: TToolButton;
    FileSaveBtn: TToolButton;
    FileSaveAsBtn: TToolButton;
    FileCloseBtn: TToolButton;
    ExitBtn: TToolButton;
    procedure DBGridEditButtonClick(Sender: TObject);
    procedure DBGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FileCloseAction(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileNewAction(Sender: TObject);
    procedure FileOpenAction(Sender: TObject);
    procedure FileSaveAsAction(Sender: TObject);
    procedure FileSaveAction(Sender: TObject);
    procedure MemDatasetNewRecord(DataSet: TDataSet);
  public
    PassMan: TPasswordManager;
    procedure GridToPassman;
    procedure PassmanToGrid;
  end;

var
  PassManForm: TPassManForm;

implementation

{$R *.lfm}

uses
  PassManParamsFM, CreatePasswordFM;

{ TPassManForm }

procedure TPassManForm.FormCreate(Sender: TObject);
begin
  PassMan := TPasswordManager.Create;
end;

procedure TPassManForm.DBGridEditButtonClick(Sender: TObject);
var
  P: String;
begin
  if DBGrid.SelectedField = MemDataset.FieldByName('ParamsField') then
    begin
      ParamsForm.ValueListEditor.Clear;
      ParamsForm.ValueListEditor.Strings.Text := MemDataset.FieldByName('ParamsField').AsString;
      if ParamsForm.ShowModal = mrOK then
        begin
          if MemDataset.State = dsBrowse then
            MemDataset.Edit;
          MemDataset.FieldByName('ParamsField').AsString := ParamsForm.ValueListEditor.Strings.Text;
        end;
    end
  else
  if DBGrid.SelectedField = MemDataset.FieldByName('PasswordField') then
    begin
      if CreatePasswordDlg(P) then
        begin
          if MemDataset.State = dsBrowse then
            MemDataset.Edit;
          MemDataset.FieldByName('PasswordField').AsString := P;
        end;
    end;
end;

procedure TPassManForm.DBGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Entropy.MouseMove(X,Y);
end;

procedure TPassManForm.FileCloseAction(Sender: TObject);
begin
  MemDataset.Close;
end;

procedure TPassManForm.FormDestroy(Sender: TObject);
begin
  PassMan.Free;
end;

procedure TPassManForm.FileNewAction(Sender: TObject);
begin
  Passman.Clear;
  MemDataset.Clear;
  FileOpen.Dialog.Filename := '';
  FileSaveAs.Dialog.Filename := '';
end;

procedure TPassManForm.FileOpenAction(Sender: TObject);
begin
  PassMan.LoadFromFile(FileOpen.Dialog.Filename);
  PassManToGrid;
  FileSaveAs.Dialog.Filename := FileOpen.Dialog.Filename;
end;

procedure TPassManForm.FileSaveAsAction(Sender: TObject);
begin
  GridToPassMan;
  PassManToGrid;
  PassMan.SaveToFile(FileSaveAs.Dialog.Filename);
  FileOpen.Dialog.Filename := FileSaveAs.Dialog.Filename;
end;

procedure TPassManForm.FileSaveAction(Sender: TObject);
begin
  if FileSaveAs.Dialog.Filename = '' then
    begin
      if FileSaveAs.Dialog.Execute then
        begin
          GridToPassMan;
          PassManToGrid;
          PassMan.SaveToFile(FileSaveAs.Dialog.Filename);
        end;
    end
  else
    begin
      GridToPassMan;
      PassManToGrid;
      PassMan.SaveToFile(FileSaveAs.Dialog.Filename);
    end;
end;

procedure TPassManForm.MemDatasetNewRecord(DataSet: TDataSet);
begin
  MemDataset.FieldByName('LastActiveField').AsDateTime := Now;
  MemDataset.FieldByName('EnabledField').AsBoolean := True;
  MemDataset.FieldByName('AdminField').AsBoolean := False;
end;

procedure TPassManForm.GridToPassman;
var
  O: TPasswordManagerAccount;
  BM: TBookmark;
begin
  PassMan.Clear;
  MemDataset.GetBookmark;
  MemDataset.First;
  while not MemDataset.EOF do
    begin
      O := TPasswordManagerAccount.Create(PassMan);
      O.Enabled     := MemDataset.FieldByName('EnabledField').AsBoolean;
      O.Username    := MemDataset.FieldByName('UsernameField').AsString;
      O.Password    := MemDataset.FieldByName('PasswordField').AsString;
      O.Admin       := MemDataset.FieldByName('AdminField').AsBoolean;
      O.LastActive  := MemDataset.FieldByName('LastActiveField').AsDateTime;
      O.Description := MemDataset.FieldByName('DescriptionField').AsString;
      O.Params.Text := MemDataset.FieldByName('ParamsField').AsString;
      MemDataset.Next;
    end;
  MemDataset.GotoBookmark(BM);
end;

procedure TPassManForm.PassmanToGrid;
var
  I: Integer;
  O: TPasswordManagerAccount;
begin
  MemDataset.Clear(False);
  MemDataset.Active := True;
  for I := 0 to PassMan.Count - 1 do
    begin
      O := PassMan[I];
      MemDataset.Insert;
      MemDataset.FieldByName('EnabledField').AsBoolean     := O.Enabled;
      MemDataset.FieldByName('UsernameField').AsString     := O.Username;
      MemDataset.FieldByName('PasswordField').AsString     := O.Password;
      MemDataset.FieldByName('AdminField').AsBoolean       := O.Admin;
      MemDataset.FieldByName('LastActiveField').AsDateTime := O.LastActive;
      MemDataset.FieldByName('DescriptionField').AsString  := O.Description;
      MemDataset.FieldByName('ParamsField').AsString       := O.Params.Text;
      MemDataset.Post;
    end;
end;

end.


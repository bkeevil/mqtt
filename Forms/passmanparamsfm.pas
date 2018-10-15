unit PassManParamsFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  StdCtrls;

type

  { TParamsForm }

  TParamsForm = class(TForm)
    AddBtn: TButton;
    RemoveBtn: TButton;
    OKBtn: TButton;
    CancelBtn: TButton;
    ValueListEditor: TValueListEditor;
    procedure AddBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ParamsForm: TParamsForm;

implementation

{$R *.lfm}

{ TParamsForm }

procedure TParamsForm.AddBtnClick(Sender: TObject);
begin
  ValueListEditor.InsertRow('Param','',True);
end;

procedure TParamsForm.RemoveBtnClick(Sender: TObject);
var
  S: Integer;
begin
  S := ValueListEditor.Selection.Top;
  if S > 0 then
    ValueListEditor.DeleteRow(S);
end;

end.


unit publishfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPublishForm }

  TPublishForm = class(TForm)
    CancelBtn: TButton;
    OKBtn: TButton;
    cbQOS: TComboBox;
    cbRetain: TCheckBox;
    edMessage: TEdit;
    edTopic: TEdit;
    lbTopic: TLabel;
    lbMessage: TLabel;
    lbQOS: TLabel;
  private

  public

  end;

var
  PublishForm: TPublishForm;

implementation

{$R *.lfm}

end.


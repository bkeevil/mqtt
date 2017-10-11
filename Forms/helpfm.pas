unit helpfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { THelpForm }

  THelpForm = class(TForm)
    CloseBtn: TButton;
    StaticText: TStaticText;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  HelpForm: THelpForm;

implementation

{$R *.lfm}

end.


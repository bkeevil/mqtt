unit clientfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ComCtrls, Menus, ExtCtrls, Buffers, Logging, lNetComponents,
  MQTTConsts, MQTTClient, MQTTSubscriptions, lNet;

type
  TDebugMessage = record
    MessageType: TLogMessageType;
    Module: String;
    Message: String;
  end;
  PDebugMessage = ^TDebugMessage;

  { TClientForm }

  TClientForm = class(TForm)
    CBEnabled: TCheckBox;
    CBFiltered: TCheckBox;
    cbShowDebugMessages: TCheckBox;
    ClearBtn: TButton;
    DisconnectBtn: TButton;
    Client: TMQTTClient;
    ConnectBtn: TButton;
    FilterText: TEdit;
    LogGrid: TStringGrid;
    LogToolbarPanel: TPanel;
    StatusBar: TStatusBar;
    LogTab: TTabSheet;
    TCP: TLTCPComponent;
    RefreshSubscriptionsItm: TMenuItem;
    PageControl: TPageControl;
    SubscriptionsPopup: TPopupMenu;
    SubscriptionsGrid: TStringGrid;
    MessagesGrid: TStringGrid;
    SubscriptionsTab: TTabSheet;
    MessagesTab: TTabSheet;
    PublishBtn: TButton;
    SubscribeBtn: TButton;
    UnsubscribeBtn: TButton;
    procedure CBEnabledChange(Sender: TObject);
    procedure CBFilteredChange(Sender: TObject);
    procedure cbShowDebugMessagesChange(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ClientDisconnected(Sender: TObject);
    procedure ClientReceiveMessage(AClient: TMQTTClient; Topic: UTF8String;
      Data: String; QOS: TMQTTQOSType; Retain: Boolean);
    procedure ClientSendData(AClient: TMQTTClient);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure FilterTextExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PublishBtnClick(Sender: TObject);
    procedure RefreshSubscriptionsItmClick(Sender: TObject);
    procedure SubscribeBtnClick(Sender: TObject);
    procedure TCPCanSend(aSocket: TLSocket);
    procedure TCPConnect(aSocket: TLSocket);
    procedure TCPDisconnect(aSocket: TLSocket);
    procedure TCPError(const msg: string; aSocket: TLSocket);
    procedure TCPReceive(aSocket: TLSocket);
    procedure UnsubscribeBtnClick(Sender: TObject);
  private
    FListener : TLogListener;
    FRecords  : TList;
    procedure ClearRecords;
    procedure HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
    function PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
  public
    Log: TLogDispatcher;
    procedure Filter(Filter: String);
  end;

var
  ClientForm: TClientForm;
  C: Integer = 0;

implementation

{$R *.lfm}

uses
  ConnectFM, PublishFM, SubscribeFM;

{ TClientForm }

procedure TClientForm.FormCreate(Sender: TObject);
begin
  FRecords := TList.Create;
  FListener := TLogListener.Create;
  FListener.OnMessage := @HandleMessage;
  Log := TLogDispatcher.Create('ClientForm');
end;

procedure TClientForm.FormDestroy(Sender: TObject);
begin
  Log.Free;
  FListener.Destroy;
  ClearRecords;
  FRecords.Destroy;
end;

procedure TClientForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TClientForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Client.State = csConnected then
    begin
      Client.Disconnect;
      TCP.Disconnect(True);
    end;
end;

procedure TClientForm.ConnectBtnClick(Sender: TObject);
begin
  ConnectDlg.ActiveControl := ConnectDlg.edClientID;
  if ConnectDlg.ShowModal = mrOK then
    begin
      TCP.Host                   := ConnectDlg.edIPAddress.Text;
      TCP.Port                   := ConnectDlg.sePort.Value;
      Client.ClientID            := ConnectDlg.edClientID.Text;
      Client.Username            := ConnectDlg.edUsername.Text;
      Client.Password            := ConnectDlg.edPassword.Text;
      Client.CleanSession        := ConnectDlg.cbClean.Checked;
      Client.KeepAlive           := ConnectDlg.edKeepAlive.Value;
      Client.WillMessage.Enabled := ConnectDlg.cbEnabled.Checked;
      Client.WillMessage.Topic   := ConnectDlg.edTopic.Text;
      Client.WillMessage.Message := ConnectDlg.edMessage.Text;
      Client.WillMessage.QOS     := TMQTTQOSType(ConnectDlg.cbQOS.ItemIndex);
      Client.WillMessage.Retain  := ConnectDlg.cbRetain.Checked;

      if TCP.Connect then
        Log.Send(mtInfo,'Connecting to %s on port %d',[TCP.Host,TCP.Port])
      else
        Log.Send(mtError,'Error initiating TCP connection');
    end;
end;

procedure TClientForm.TCPConnect(aSocket: TLSocket);
begin
  Log.Send(mtInfo,'TCP connection established to %s on port %d',[aSocket.PeerAddress,aSocket.PeerPort]);
  if Client.Connect then
    Caption := Client.ClientID
  else
    Log.Send(mtError,'Connection MQTT parameter error');
end;

procedure TClientForm.ClientDisconnected(Sender: TObject);
begin
  Client.Disconnected;
end;

procedure TClientForm.DisconnectBtnClick(Sender: TObject);
begin
  Client.Disconnect;
end;

procedure TClientForm.TCPDisconnect(aSocket: TLSocket);
begin
  Client.Disconnected;
end;

procedure TClientForm.TCPError(const msg: string; aSocket: TLSocket);
begin
  Log.Send(mtError,msg);
end;

procedure TClientForm.TCPReceive(aSocket: TLSocket);
var
  Data: Pointer;
  Size: Integer;
  Buffer: TBuffer;
begin
  GetMem(Data,32);
  try
    Buffer := TBuffer.Create;
    try
      repeat
        Size := aSocket.Get(Data^,32);
        if Size > 0 then
          Buffer.Write(Data,Size);
      until Size < 32;
      Client.RecvBuffer.WriteBuffer(Buffer);
      Client.DataAvailable;
    finally
      Buffer.Free;
    end;
  finally
    FreeMem(Data,32);
  end;
end;

procedure TClientForm.ClientReceiveMessage(AClient: TMQTTClient; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean);
var
  S: String;
begin
  if Retain then S := '1' else S := '0';
  MessagesGrid.InsertRowWithValues(1,[Topic,Data,MQTTQOSTypeNames[QOS],S]);
end;

procedure TClientForm.ClientSendData(AClient: TMQTTClient);
begin
  TCPCanSend(Client.Socket as TLSocket);
end;

procedure TClientForm.TCPCanSend(aSocket: TLSocket);
var
  Data: Pointer;
  Sent,Size: Integer;
begin
  Size := Client.SendBuffer.Size;
  GetMem(Data,Size);
  try
    Client.SendBuffer.Peek(Data,Size);
    Sent := TCP.Send(Data^,Size);
    Client.SendBuffer.Read(Data,Sent);
  finally
    FreeMem(Data,Size);
  end;
end;

{var
  Data: Pointer;
  Size,Sent: Integer;
begin
  if Client.SendBuffer.Size > 0 then
    begin
      GetMem(Data,32);
      try
        repeat
          Size := Client.SendBuffer.Peek(Data,32);
          if Size > 0 then
            begin
              Sent := TCP.Send(Data^,Size);
              if Sent > 0 then
                 Client.SendBuffer.Read(Data,Sent);
            end;
        until (Size = 0) or (Sent < Size) or (Size < 32);
      finally
        FreeMem(Data,32);
      end;
    end;
end;   }

procedure TClientForm.SubscribeBtnClick(Sender: TObject);
var
  LSubscriptions: TMQTTSubscriptionList;
  LSubscription: TMQTTSubscription;
  X: Integer;
  S: String;
  Q: TMQTTQOSType;
begin
  SubscribeForm.Grid.RowCount := 1;
  SubscribeForm.Grid.Columns.Items[0].Visible := False;
  SubscribeForm.TestDataBtn.Visible := True;
  SubscribeForm.SelectAllBtn.Visible := False;
  if SubscribeForm.ShowModal = mrOK then
    begin
      SubscribeForm.ValidateRows;
      LSubscriptions := TMQTTSubscriptionList.Create;
      try
        for X := 1 to SubscribeForm.Grid.RowCount - 1 do
          begin
            S := SubscribeForm.Grid.Cells[1,X];
            Q := TMQTTQOSType(SubscribeForm.Grid.Columns[2].PickList.IndexOf(SubscribeForm.Grid.Cells[2,X]));
            if S > '' then
              begin
                LSubscription := TMQTTSubscription.Create(S,Q);
                if LSubscription.Tokens.Valid then
                  LSubscriptions.Update(LSubscription)
                else
                  begin
                    LSubscription.Free;
                    Log.Send(mtWarning,'Subscription is invalid (%s)',[S]);
                    ShowMessage(Format('Subscription %s is invalid',[S]));
                  end;
              end;
          end;
        if LSubscriptions.Count > 0 then
          Client.Subscribe(LSubscriptions)
        else
          begin
            Log.Send(mtWarning,'There are no subscriptions to send');
            ShowMessage('There are no subscriptions to send');
          end;
      finally
        LSubscriptions.Free;
      end;
    end;
end;

procedure TClientForm.UnsubscribeBtnClick(Sender: TObject);
var
  LSubscriptions: TMQTTSubscriptionList;
  LSubscription: TMQTTSubscription;
  X: Integer;
  S: String;
  Q: TMQTTQOSType;
begin
  SubscribeForm.Grid.Columns.Items[0].Visible := True;
  SubscribeForm.TestDataBtn.Visible := False;
  SubscribeForm.SelectAllBtn.Visible := True;
  SubscribeForm.Grid.RowCount := 1;
  for X := 0 to Client.Subscriptions.Count - 1 do
    begin
      LSubscription := Client.Subscriptions[X];
      S := SubscribeForm.Grid.Columns[2].Picklist[ord(LSubscription.QOS)];
      SubscribeForm.Grid.InsertRowWithValues(X+1,['0',LSubscription.Filter,S]);
    end;
  if SubscribeForm.ShowModal = mrOK then
    begin
      SubscribeForm.ValidateRows;
      LSubscriptions := TMQTTSubscriptionList.Create;
      try
        for X := 1 to SubscribeForm.Grid.RowCount - 1 do
          begin
            S := SubscribeForm.Grid.Cells[1,X];
            Q := TMQTTQOSType(SubscribeForm.Grid.Columns[2].PickList.IndexOf(SubscribeForm.Grid.Cells[2,X]));
            if (S > '') and (SubscribeForm.Grid.Cells[0,X] = '1') then
              begin
                LSubscription := TMQTTSubscription.Create(S,Q);
                //LSubscription.Persistent := SubscribeForm.Grid.Cells[3,X] = '1';
                if LSubscription.Tokens.Valid then
                  LSubscriptions.Update(LSubscription)
                else
                  begin
                    LSubscription.Free;
                    Log.Send(mtWarning,'Subscription is invalid (%s)',[S]);
                    ShowMessage(Format('Subscription %s is invalid',[S]));
                  end;
              end;
          end;
        if LSubscriptions.Count > 0 then
          Client.Unsubscribe(LSubscriptions)
        else
          begin
            Log.Send(mtWarning,'There are no subscriptions to send');
            ShowMessage(Format('Subscription %s is invalid',[S]));
          end;
      finally
        LSubscriptions.Free;
      end;
    end;
end;

procedure TClientForm.RefreshSubscriptionsItmClick(Sender: TObject);
var
  X: Integer;
  S: TMQTTSubscription;
begin
  SubscriptionsGrid.RowCount := 1;
  for X := 0 to Client.Subscriptions.Count - 1 do
    begin
      S := Client.Subscriptions[X];
      SubscriptionsGrid.RowCount := SubscriptionsGrid.RowCount + 1;
      SubscriptionsGrid.Cells[0,X+1] := S.Filter;
      SubscriptionsGrid.Cells[1,X+1] := MQTTQOSTypeNames[S.QOS];
      SubscriptionsGrid.Cells[2,X+1] := IntToStr(S.Age);
      if S.Persistent then
        SubscriptionsGrid.Cells[3,X+1] := '1'
      else
        SubscriptionsGrid.Cells[3,X+1] := '0';
    end;
end;

procedure TClientForm.PublishBtnClick(Sender: TObject);
begin
  PublishForm.ActiveControl := PublishForm.edTopic;
  if PublishForm.ShowModal = mrOK then
    Client.Publish(PublishForm.edTopic.Text,PublishForm.edMessage.Text,TMQTTQOSType(PublishForm.cbQOS.ItemIndex),PublishForm.cbRetain.Checked);
end;

procedure TClientForm.ClearRecords;
var
  I: Integer;
  P: PDebugMessage;
begin
  for I := 0 to FRecords.Count - 1 do
    begin
      P := FRecords[I];
      Dispose(P);
      dec(C);
    end;
  FRecords.Clear;
end;

procedure TClientForm.FilterTextExit(Sender: TObject);
begin
  if CBFiltered.Checked then
    begin
      LogGrid.RowCount := 1;
      Filter(FilterText.Text);
    end;
end;

procedure TClientForm.ClearBtnClick(Sender: TObject);
begin
  ClearRecords;
  LogGrid.RowCount := 1;
end;

procedure TClientForm.HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
var
  P: PDebugMessage;
  LKind: String;
  LModule: String;
  LMessage: String;
begin
  New(P);
  inc(C);
  P^.MessageType := MessageType;
  if Assigned(Dispatcher) then
    P^.Module := Dispatcher.Name
  else
    P^.Module := '';
  P^.Message := Message;
  FRecords.Add(P);
  if (not CBFiltered.Checked) or PassesFilter(FilterText.Text,P) then
    begin
      LKind := MESSAGE_TYPE_STRINGS[P^.MessageType];
      LModule := P^.Module;
      LMessage := P^.Message;
      LogGrid.InsertColRow(False,LogGrid.RowCount);
      LogGrid.Cells[0,LogGrid.RowCount - 1] := LKind;
      LogGrid.Cells[1,LogGrid.RowCount - 1] := LModule;
      LogGrid.Cells[2,LogGrid.RowCount - 1] := LMessage;
      Show;
    end;
end;

procedure TClientForm.CBFilteredChange(Sender: TObject);
begin
  if CBFiltered.Checked then
    Filter(FilterText.Text)
  else
    Filter('');
end;

procedure TClientForm.cbShowDebugMessagesChange(Sender: TObject);
begin
  if cbShowDebugMessages.Checked then
    begin
      FListener.TypeFilter := ALL_LOG_MESSAGE_TYPES;
      Client.Log.Filter := ALL_LOG_MESSAGE_TYPES;
    end
  else
    begin
      FListener.TypeFilter := DEFAULT_LOG_MESSAGE_TYPES;
      Client.Log.Filter := DEFAULT_LOG_MESSAGE_TYPES;
    end;
end;


procedure TClientForm.CBEnabledChange(Sender: TObject);
begin
  FListener.Enabled := CBEnabled.Checked;
end;

function TClientForm.PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
begin
  if Filter = '' then
    Result := True
  else
    if Filter[1] = '-' then
      Result := (Pos(Copy(Filter, 2, Length(Filter) - 1), Rec^.Module) = 0) and
                (Pos(Copy(Filter, 2, Length(Filter) - 1), Rec^.Message) = 0)
    else
      Result := (Pos(Filter, Rec^.Module) > 0) or (Pos(Filter, Rec^.Module) > 0);
end;

procedure TClientForm.Filter(Filter: string);
var
  I: Integer;
  P: PDebugMessage;
begin
  LogGrid.RowCount := 1;
  for I := 0 to FRecords.Count - 1 do
    begin
      P := FRecords[I];
      if PassesFilter(Filter, P) then
        LogGrid.InsertRowWithValues(LogGrid.RowCount,[MESSAGE_TYPE_STRINGS[P^.MessageType],P^.Module,P^.Message]);
    end;
end;

end.


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
    LogTab: TTabSheet;
    LogToolbarPanel: TPanel;
    SSL: TLSSLSessionComponent;
    MessagesGrid: TStringGrid;
    MessagesTab: TTabSheet;
    PageControl: TPageControl;
    StatusBar: TStatusBar;
    TCP: TLTCPComponent;
    PublishBtn: TButton;
    SubscribeBtn: TButton;
    UnsubscribeBtn: TButton;
    procedure CBEnabledChange(Sender: TObject);
    procedure CBFilteredChange(Sender: TObject);
    procedure cbShowDebugMessagesChange(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ClientDisconnected(Sender: TObject);
    procedure ClientReceiveMessage(AClient: TMQTTClient; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean);
    procedure ClientSendData(AClient: TMQTTClient);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure FilterTextExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PublishBtnClick(Sender: TObject);
    procedure SSLSSLConnect(aSocket: TLSocket);
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
  MQTTPackets, MQTTPacketDefs, ConnectFM, PublishFM, SubscribeFM, LNetSSL, OpenSSL;

type
  TLSSLSocketHack = class(TLSSLSocket);  // Need to access some protected properties

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
  MQTTConnectDlg.ActiveControl := MQTTConnectDlg.edClientID;
  if MQTTConnectDlg.ShowModal = mrOK then
    begin
      TCP.Host                   := MQTTConnectDlg.edServer.Text;
      TCP.Port                   := MQTTConnectDlg.sePort.Value;
      SSL.CAFile                 := MQTTConnectDlg.edCertificateFile.Filename;
      SSL.KeyFile                := MQTTConnectDlg.edPrivateKeyFile.Filename;
      SSL.Method                 := MQTTConnectDlg.getSSLMethod;
      SSL.Password               := MQTTConnectDlg.edPrivateKeyPassword.Text;
      SSL.SSLActive              := MQTTConnectDlg.cbUseSSL.Checked;
      Client.ClientID            := MQTTConnectDlg.edClientID.Text;
      Client.Username            := MQTTConnectDlg.edUsername.Text;
      Client.Password            := MQTTConnectDlg.edPassword.Text;
      Client.CleanSession        := MQTTConnectDlg.cbClean.Checked;
      Client.KeepAlive           := MQTTConnectDlg.edKeepAlive.Value;
      Client.WillMessage.Enabled := MQTTConnectDlg.cbWillMessageEnabled.Checked;
      Client.WillMessage.Topic   := MQTTConnectDlg.edTopic.Text;
      Client.WillMessage.Message := MQTTConnectDlg.edMessage.Text;
      Client.WillMessage.QOS     := TMQTTQOSType(MQTTConnectDlg.cbQOS.ItemIndex);
      Client.WillMessage.Retain  := MQTTConnectDlg.cbRetain.Checked;

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
  MessagesGrid.InsertRowWithValues(1,[Topic,Data,GetQOSTypeName(QOS),S]);
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

procedure TClientForm.PublishBtnClick(Sender: TObject);
begin
  PublishForm.ActiveControl := PublishForm.edTopic;
  if PublishForm.ShowModal = mrOK then
    Client.Publish(PublishForm.edTopic.Text,PublishForm.edMessage.Text,TMQTTQOSType(PublishForm.cbQOS.ItemIndex),PublishForm.cbRetain.Checked);
end;

procedure TClientForm.SSLSSLConnect(aSocket: TLSocket);
var
  SSLSocket: TLSSLSocket;
  PeerCertificate: PX509;
  PSubjectName: PX509_NAME;
  PIssuerName: PX509_NAME;
  SSubjectName: String;
  SIssuerName: String;
begin
  Log.Send(mtInfo,'SSL/TLS Session Established');
  if (aSocket is TLSSLSocket) then
    begin
      SSLSocket := aSocket as TLSSLSocket;
      PeerCertificate := SslGetPeerCertificate(TLSSLSocketHack(SSLSocket).FSSL);
      PSubjectName := X509GetSubjectName(PeerCertificate);
      PIssuerName := X509GetSubjectName(PeerCertificate);
      SetLength(SSubjectName,255);
      SetLength(SIssuerName,255);
      X509NameOneline(PSubjectName,SSubjectName,255);
      X509NameOneline(PIssuerName,SIssuerName,255);
      Log.Send(mtInfo,'Subject Name: ' + Trim(SSubjectName));
      Log.Send(mtInfo,'Issuer Name: ' + Trim(SIssuerName));
      //function X509NameOneline(a: PX509_NAME; var buf: String; size: cInt):String;
      //function X509GetSubjectName(a: PX509):PX509_NAME;
      //function X509GetIssuerName(a: PX509):PX509_NAME;
    end;
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


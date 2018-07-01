unit MainFM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Logging, Buffers, mqttconsts, mqttclient, mqttsubscriptions, lNetComponents, lnet;

type

  { TForm1 }

  TForm1 = class(TForm)
    ClockLabel: TLabel;
    Client: TMQTTClient;
    Memo1: TMemo;
    TCP: TLTCPComponent;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ClientDisconnected(Sender: TObject);
    procedure ClientError(AClient: TMQTTClient; ErrCode: Word; ErrMsg: String);
    procedure ClientInitSession(Sender: TObject);
    procedure ClientReceiveMessage(AClient: TMQTTClient; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean);
    procedure ClientSendData(AClient: TMQTTClient);
    procedure TCPCanSend(aSocket: TLSocket);
    procedure TCPConnect(aSocket: TLSocket);
    procedure TCPDisconnect(aSocket: TLSocket);
    procedure TCPError(const msg: string; aSocket: TLSocket);
    procedure TCPReceive(aSocket: TLSocket);
    procedure LogMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
  private
    LogDispatcher: TLogDispatcher;
    Listener: TLogListener;
  public
    TheTime: TSystemTime;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Listener := TLogListener.Create;
  Listener.OnMessage := @LogMessage;
  LogDispatcher := TLogDispatcher.Create('Application');
  TCP.Connect;
  DateTimeToSystemTime(Now(),TheTime);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Client.Disconnect;
  TCP.Disconnect;
  LogDispatcher.Free;
  Listener.Free;
end;

procedure TForm1.TCPCanSend(aSocket: TLSocket);
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

procedure TForm1.TCPConnect(aSocket: TLSocket);
begin
  LogDispatcher.Send(mtInfo,'TCP connection established to %s on port %d',[aSocket.PeerAddress,aSocket.PeerPort]);
  if not Client.Connect then
    LogDispatcher.Send(mtError,'Connection MQTT parameter error');
end;

procedure TForm1.TCPDisconnect(aSocket: TLSocket);
begin
  LogDispatcher.Send(mtInfo,'TCP Disconnected');
  Client.Disconnected;
  Sleep(5000);
end;

procedure TForm1.TCPError(const msg: string; aSocket: TLSocket);
begin
  LogDispatcher.Send(mtError,'TCP Error: '+msg);
  TCP.Disconnect;
  Sleep(5000);
end;

procedure TForm1.TCPReceive(aSocket: TLSocket);
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

procedure TForm1.LogMessage(Dispatcher: TLogDispatcher;
  MessageType: TLogMessageType; Message: String);
begin
  Memo1.Lines.Add(Message);
end;

procedure TForm1.ClientDisconnected(Sender: TObject);
begin
  //Client.Disconnected;
end;

procedure TForm1.ClientReceiveMessage(AClient: TMQTTClient; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean);
var
  DT: TDateTime;
begin
  LogDispatcher.Send(mtInfo,'Received Message %s=%s',[Topic,Data]);
  if Topic = 'Clock/Year' then
    TheTime.Year := StrToInt(Data);
  if Topic = 'Clock/Month' then
    TheTime.Month := StrToInt(Data);
  if Topic = 'Clock/DayOfWeek' then
    TheTime.DayOfWeek := StrToInt(Data);
  if Topic = 'Clock/Day' then
    TheTime.Day := StrToInt(Data);
  if Topic = 'Clock/Hour' then
    TheTime.Hour := StrToInt(Data);
  if Topic = 'Clock/Minute' then
    TheTime.Minute := StrToInt(Data);
  if Topic = 'Clock/Active' then
    ClockLabel.Visible := Data = '1';
  DT := SystemTimeToDateTime(TheTime);
  ClockLabel.Caption := FormatDateTime('dddd mmmm d, h:nnampm',DT);
end;

procedure TForm1.ClientSendData(AClient: TMQTTClient);
begin
  TCPCanSend(Client.Socket as TLSocket);
end;

procedure TForm1.ClientError(AClient: TMQTTClient; ErrCode: Word; ErrMsg: String);
begin
  LogDispatcher.Send(mtError,'Error Code %d',[ErrCode]);
  // Errors are already logged
end;

procedure TForm1.ClientInitSession(Sender: TObject);
var
  Subscriptions: TMQTTSubscriptionList;
begin
  Subscriptions := TMQTTSubscriptionList.Create;
  try
    Subscriptions.New('Clock/Year',qtAT_LEAST_ONCE);
    Subscriptions.New('Clock/Month',qtAT_LEAST_ONCE);
    Subscriptions.New('Clock/DayOfWeek',qtAT_LEAST_ONCE);
    Subscriptions.New('Clock/Day',qtAT_LEAST_ONCE);
    Subscriptions.New('Clock/Hour',qtAT_LEAST_ONCE);
    Subscriptions.New('Clock/Minute',qtAT_LEAST_ONCE);
    Subscriptions.New('Clock/Active',qtAT_LEAST_ONCE);
    if not Client.Subscribe(Subscriptions) then
      LogDispatcher.Send(mtError,'Error registering subscriptions');
  finally
    Subscriptions.Free;
  end;
end;

end.


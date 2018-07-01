program MQTTClock;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, SysUtils, CustApp, CRT, MQTTConsts, MQTTClient, lnetbase, lnet,
  Logging, Buffers;

type

  { TMQTTClockApplication }

  TMQTTClockApplication = class(TCustomApplication)
  private
    procedure ClientDisconnected(Sender: TObject);
    procedure ClientError(AClient: TMQTTClient; ErrCode: Word; ErrMsg: String);
    procedure ClientInitSession(Sender: TObject);
    procedure ClientReceiveMessage(AClient: TMQTTClient; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean);
    procedure ClientSendData(AClient: TMQTTClient);
    procedure LoadCommandLineOptions;
    procedure TCPCanSend(aSocket: TLSocket);
    procedure TCPConnect(aSocket: TLSocket);
    procedure TCPDisconnect(aSocket: TLSocket);
    procedure TCPError(const msg: string; aSocket: TLSocket);
    procedure TCPReceive(aSocket: TLSocket);
    procedure SetupLogging;
    procedure ValidateCommandLineOptions;
    procedure WriteHelp;
  protected
    Listener       : TLogListener;
    LogDispatcher  : TLogDispatcher;
    TCP            : TLTCP;
    Client         : TMQTTClient;
    LastTime       : TSystemTime;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMQTTClockApplication }

procedure TMQTTClockApplication.DoRun;
var
  I: Integer;
  CurrentTime: TSystemTime;
begin
  I := 0;
  ValidateCommandLineOptions;
  if Terminated then Exit;
  repeat
    if Keypressed and (readKey = #3) then
      Terminate;
//    CheckSynchronize;
      if not (TCP.Connected or TCP.Connecting) then
        TCP.Connect
      else
        begin
          inc(I);
          if I >= 9 then
            begin
              DateTimeToSystemTime(Now,CurrentTime);
              if CurrentTime.Day <> LastTime.Day then
                begin
                  Client.Publish('Clock/Year',IntToStr(CurrentTime.Year),qtAT_LEAST_ONCE,True);
                  Client.Publish('Clock/Month',IntToStr(CurrentTime.Month),qtAT_LEAST_ONCE,True);
                  Client.Publish('Clock/DayOfWeek',IntToStr(CurrentTime.DayOfWeek),qtAT_LEAST_ONCE,True);
                  Client.Publish('Clock/Day',IntToStr(CurrentTime.Day),qtAT_LEAST_ONCE,True);
                end;
              if CurrentTime.Minute <> LastTime.Minute then
                begin
                  Client.Publish('Clock/Hour',IntToStr(CurrentTime.Hour),qtAT_LEAST_ONCE,True);
                  Client.Publish('Clock/Minute',IntToStr(CurrentTime.Minute),qtAT_LEAST_ONCE,True);
                end;
              LastTime := CurrentTime;
              I := 0;
            end;
        end;
    TCP.Callaction; // eventize the lNet
    Sleep(100);
  until Terminated; // until user quit
end;

procedure TMQTTClockApplication.ValidateCommandLineOptions;
var
  ErrorMsg: String;
begin
  // If the command line options are invalid then show the help page
  ErrorMsg := CheckOptions('s:p:l:dh','server: port: log: debug help');
  if ErrorMsg > '' then
    begin
      LogDispatcher.Send(mtError,ErrorMsg);
      WriteHelp;
      Terminate;
      Exit;
    end;
end;

procedure TMQTTClockApplication.WriteHelp;
begin
  writeln('MQTTClock Version 1.0 Useage:');
  writeln;
  writeln('  MQTTClock -s <server> -p <port> -l <filename>');
  writeln;
  writeln('  -s --server          The server hostname or IP address. Default localhost');
  writeln('  -p --port            The server port to connect to. Default 1883');
  writeln('  -l --log             Sets a log filename.  Default is stdout.');
  writeln('  -d --debug           Outputs more detailed info to the log.');
  writeln('  -h --help            Displays this help message');
end;

procedure TMQTTClockApplication.LoadCommandLineOptions;
var
  S: String;
  I: Integer;
begin
  if HasOption('s','server') then
    TCP.Host := GetOptionValue('s','server');
  if HasOption('help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;
  if HasOption('p','port') then
    begin
      S := GetOptionValue('p','port');
      if TryStrToInt(S,I) and (I > 80) and (I < 65535) then
        TCP.Port := I;
    end;
  if HasOption('d','debug') then
    begin
      LogDispatcher.Filter := ALL_LOG_MESSAGE_TYPES;
      Listener.TypeFilter := ALL_LOG_MESSAGE_TYPES;

    end
  else
    LogDispatcher.Filter := DEFAULT_LOG_MESSAGE_TYPES;
end;

procedure TMQTTClockApplication.SetupLogging;
var
  LogFilename: String;
begin
  if HasOption('l','log') then
    begin
      LogFilename := GetOptionValue('l','log');
      try
        Listener := TLogFileListener.Create(LogFilename);
      except
        Listener := nil;
      end;
      if not FileExists(LogFilename) then
        begin
          if Assigned(Listener) then
            Listener.Free;
          Listener := nil;
        end;
    end;
  if not Assigned(Listener) then
    Listener := TLogCRTListener.Create;
  if HasOption('d','debug') then
    Listener.TypeFilter := ALL_LOG_MESSAGE_TYPES;
  LogDispatcher := TLogDispatcher.Create('Application');
end;

constructor TMQTTClockApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=False;
  SetupLogging;
  DateTimeToSystemTime(Now,LastTime);
  TCP := TLTCP.Create(Self);
  TCP.Host := '127.0.0.1';
  TCP.Port := 1883;
  TCP.OnCanSend := @TCPCanSend;
  TCP.OnConnect := @TCPConnect;
  TCP.OnDisconnect := @TCPDisconnect;
  TCP.OnError := @TCPError;
  TCP.OnReceive := @TCPReceive;
  Client := TMQTTClient.Create(Self);
  Client.ClientID := 'Clock';
  Client.PingInterval := 45;
  Client.CleanSession := True;
  Client.WillMessage.Enabled := True;
  Client.WillMessage.Topic := 'Clock/Active';
  Client.WillMessage.Message := '0';
  Client.WillMessage.QOS := qtAT_LEAST_ONCE;
  Client.WillMessage.Retain := True;
  Client.OnDisconnected := @ClientDisconnected;
  Client.OnReceiveMessage := @ClientReceiveMessage;
  Client.OnSendData := @ClientSendData;
  Client.OnInitSession := @ClientInitSession;
  Client.OnError := @ClientError;
  LoadCommandLineOptions;
end;

destructor TMQTTClockApplication.Destroy;
begin
  if Assigned(Client) then
    begin
      Client.Disconnect;
      FreeAndNil(Client);
    end;
  if Assigned(TCP) then
    begin
      TCP.Disconnect(True);
      FreeAndNil(TCP);
    end;
  LogDispatcher.Free;
  Listener.Free;
  inherited Destroy;
end;

procedure TMQTTClockApplication.TCPCanSend(aSocket: TLSocket);
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

procedure TMQTTClockApplication.TCPConnect(aSocket: TLSocket);
begin
  LogDispatcher.Send(mtInfo,'TCP connection established to %s on port %d',[aSocket.PeerAddress,aSocket.PeerPort]);
  if not Client.Connect then
    LogDispatcher.Send(mtError,'Connection MQTT parameter error');
end;

procedure TMQTTClockApplication.TCPDisconnect(aSocket: TLSocket);
begin
  LogDispatcher.Send(mtInfo,'TCP Disconnected');
  Client.Disconnected;
  Sleep(5000);
end;

procedure TMQTTClockApplication.TCPError(const msg: string; aSocket: TLSocket);
begin
  LogDispatcher.Send(mtError,'TCP Error: '+msg);
  TCP.Disconnect;
  Sleep(5000);
end;

procedure TMQTTClockApplication.TCPReceive(aSocket: TLSocket);
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

procedure TMQTTClockApplication.ClientDisconnected(Sender: TObject);
begin
  //Client.Disconnected;
end;

procedure TMQTTClockApplication.ClientReceiveMessage(AClient: TMQTTClient; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean);
begin
  LogDispatcher.Send(mtDebug,'Received Message %s=%s',[Topic,Data]);
end;

procedure TMQTTClockApplication.ClientSendData(AClient: TMQTTClient);
begin
  TCPCanSend(Client.Socket as TLSocket);
end;

procedure TMQTTClockApplication.ClientError(AClient: TMQTTClient; ErrCode: Word; ErrMsg: String);
begin
  LogDispatcher.Send(mtError,'Error Code %d',[ErrCode]);
  // Errors are already logged
end;

procedure TMQTTClockApplication.ClientInitSession(Sender: TObject);
var
  CurrentTime: TSystemTime;
begin
  DateTimeToSystemTime(Now,CurrentTime);
  Client.Publish('Clock/Year',IntToStr(CurrentTime.Year),qtAT_LEAST_ONCE,True);
  Client.Publish('Clock/Month',IntToStr(CurrentTime.Month),qtAT_LEAST_ONCE,True);
  Client.Publish('Clock/DayOfWeek',IntToStr(CurrentTime.DayOfWeek),qtAT_LEAST_ONCE,True);
  Client.Publish('Clock/Day',IntToStr(CurrentTime.Day),qtAT_LEAST_ONCE,True);
  Client.Publish('Clock/Hour',IntToStr(CurrentTime.Hour),qtAT_LEAST_ONCE,True);
  Client.Publish('Clock/Minute',IntToStr(CurrentTime.Minute),qtAT_LEAST_ONCE,True);
  LastTime := CurrentTime;
  Client.Publish('Clock/Active','1',qtAT_LEAST_ONCE,True);
end;

var
  Application: TMQTTClockApplication;
begin
  Application:=TMQTTClockApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.


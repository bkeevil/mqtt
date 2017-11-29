
program MQTTServerCLI;

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, CRT, Logging, inifiles, lnetbase, lnet,
  mqttconsts, mqttserver;

type

  { TMQTTServerCLI }

  TMQTTServerCLI = class(TCustomApplication)
    private
      procedure LoadCommandLineOptions;
      procedure LoadConfiguration(Filename: String); overload;
      procedure LoadConfiguration; overload;
      procedure ServerAccepted(AConnection: TMQTTServerConnection);
      procedure ServerDisconnect(AConnection: TMQTTServerConnection);
      procedure ServerDisconnected(AConnection: TMQTTServerConnection);
      procedure ServerError(AConnection: TMQTTServerConnection; ErrCode: Word; ErrMsg: String);
      procedure ServerSendData(AConnection: TMQTTServerConnection);
      procedure ServerConnectionDestroy(AConnection: TMQTTServerConnection);
      procedure TCPAccept(aSocket: TLSocket);
      procedure TCPCanSend(aSocket: TLSocket);
      procedure TCPDisconnect(aSocket: TLSocket);
      procedure TCPError(const msg: string; aSocket: TLSocket);
      procedure TCPReceive(aSocket: TLSocket);
      procedure ValidateCommandLineOptions;
    protected
      procedure DoRun; override;
    public
      Listener       : TLogCRTListener;
      LogDispatcher  : TLogDispatcher;
      ConfigFilename : String;
      TCP            : TLTCP;
      Server         : TMQTTServer;
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure WriteHelp; virtual;
  end;

{ TMQTTServerCLI }

procedure TMQTTServerCLI.DoRun;
begin
  repeat
    TCP.Callaction; // eventize the lNet
    if Keypressed and (readKey = #27) then
      Terminate;
//    CheckSynchronize;
    Sleep(100);
  until Terminated; // until user quit
  Terminate;
end;

constructor TMQTTServerCLI.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := False;
  Listener := TLogCRTListener.Create;
  LogDispatcher := TLogDispatcher.Create('Application');
  TCP := TLTCP.Create(nil);
  TCP.Port := 1883;
  TCP.ReuseAddress := True;
  TCP.Timeout := 100;
  TCP.OnAccept := @TCPAccept;
  TCP.OnDisconnect := @TCPDisconnect;
  TCP.OnError := @TCPError;
  TCP.OnReceive := @TCPReceive;
  TCP.OnCanSend := @TCPCanSend;
  Server := TMQTTServer.Create(nil);
  Server.AllowNullClientIDS := True;
  Server.MaximumQOS := qtEXACTLY_ONCE;
  Server.RequireAuthentication := False;
  Server.SystemClock := False;
  Server.OnAccepted := @ServerAccepted;
  Server.OnDisconnect := @ServerDisconnect;
  Server.OnDisconnected := @ServerDisconnected;
  Server.OnError := @ServerError;
  Server.OnSendData := @ServerSendData;
  Server.OnConnectionDestroy := @ServerConnectionDestroy;
  LoadConfiguration;
  if Terminated then Exit;
  if TCP.Listen then
    LogDispatcher.Send(mtInfo,'Server listening on port %d',[TCP.Port])
  else
    LogDispatcher.Send(mtError,'Server could not enter listening state.');
end;

destructor TMQTTServerCLI.Destroy;
begin
  Server.Destroy;
  TCP.Destroy;
  LogDispatcher.Destroy;
  Listener.Destroy;
  inherited Destroy;
end;

procedure TMQTTServerCLI.ValidateCommandLineOptions;
var
  ErrorMsg: String;
begin
  // If the command line options are invalid then show the help page
  ErrorMsg := CheckOptions('i:p:vanshc:','interface: port: verbose authenticate null-clientid strict-clientid help config:');
  if ErrorMsg > '' then
    begin
      LogDispatcher.Send(mtError,ErrorMsg);
      WriteHelp;
      Terminate;
      Exit;
    end;
end;

procedure TMQTTServerCLI.LoadConfiguration;
begin
  // Retrieve current working directory
  LogDispatcher.Send(mtInfo,'Current working directory is "%s"',[GetCurrentDir]);

  // Retrieve configuration filename
  if HasOption('c','config') then
    ConfigFilename := GetOptionValue('c','config')
  else
    if FileExists(MQTT_DEFAULT_CONFIG_FILENAME1) then
      ConfigFilename := MQTT_DEFAULT_CONFIG_FILENAME1
    else
      ConfigFilename := MQTT_DEFAULT_CONFIG_FILENAME2;

  ConfigFilename := ExpandFilename(ConfigFilename);
  // Load the configuration file
  if FileExists(ConfigFilename) then
    begin
      LoadConfiguration(ConfigFilename);
      LogDispatcher.Send(mtInfo,'Loaded config file %s',[ConfigFilename]);
    end
  else
    LogDispatcher.Send(mtWarning,'Config file %s not found. Using default values.',[ConfigFilename]);

  // Command line options override the content of the configuration file
  LoadCommandLineOptions;

  // Output some information about the configuration to the log
  LogDispatcher.Send(mtDebug,'RequireAuthentication=%s',[BoolToStr(Server.RequireAuthentication,'True','False')]);
  LogDispatcher.Send(mtDebug,'AllowNullClientIDs=%s',[BoolToStr(Server.AllowNullClientIDs,'True','False')]);
  LogDispatcher.Send(mtDebug,'StrictClientIDValidation=%s',[BoolToStr(Server.StrictClientIDValidation,'true','false')]);
end;

procedure TMQTTServerCLI.WriteHelp;
begin
  writeln('mqttserver Version 1.0 Useage:');
  writeln;
  writeln('  mqttserver --config <filename>');
  writeln('  mqttserver -i <interface> -p port');
  writeln('  mqttserver --authenticate --strict-clientid');
  writeln;
  writeln('  -c --config          Sets the default configuration file.  Default is');
  writeln('                       /etc/mqtt/mqttserver.ini or mqttserver.ini in the');
  writeln('                       current working directory if that is not found.');
  writeln('  -v --verbose         Makes the log output more verbose.');
  writeln('  -i --interface       Sets the IP address of the interface to listen on.');
  writeln('                       Default is all interfaces.');
  writeln('  -p --port            Sets the TCP port number to listen on.  Default 1883.');
  writeln('  -a --authenticate    When specified, clients are authenticated against the');
  writeln('                       password database.  Default is no authentication.');
  writeln('  -n --null-clientid   When no ClientID is provided by a client, generate a');
  writeln('                       unique one automatically.  Default rejects connection.');
  writeln('  -s --strict-clientid Validate Client IDs to ensure they contain only');
  writeln('                       letters and numbers.  Default accepts any character.');
  writeln('  -h --help            Displays this help message');
end;

procedure TMQTTServerCLI.LoadCommandLineOptions;
var
  S: String;
  I: Integer;
begin
  if HasOption('i','interface') then
    TCP.Host := GetOptionValue('i','interface');
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
  if HasOption('v','verbose') then
    LogDispatcher.Filter := [mtInfo,mtDebug,mtWarning,mtError]
  else
    LogDispatcher.Filter := [mtInfo,mtWarning,mtError];
  if HasOption('a','authenticate') then
    Server.RequireAuthentication := True;
  if HasOption('n','null-clientid') then
    Server.AllowNullClientIDs := True;
  if HasOption('s','strict-clientid') then
    Server.StrictClientIDValidation := True;
end;

procedure TMQTTServerCLI.LoadConfiguration(Filename: String);
var
  Ini: TInifile;
  I: Integer;
  S: String;
begin
  Ini := TInifile.Create(Filename,[ifoStripComments,ifoStripInvalid,ifoFormatSettingsActive]);
  try
    Server.RequireAuthentication := Ini.ReadBool('Server','RequireAuthentication',False);
    Server.AllowNullClientIDs := Ini.ReadBool('Server','AllowNullClientIDs',True);
    Server.StrictClientIDValidation := Ini.ReadBool('Server','StrictClientIDValidation',False);

    I := Ini.ReadInteger('Server','MaximumQOS',2);
    if I < 0 then
      I := 0;
    if I > 2 then
      I := 2;
    Server.MaximumQOS := TMQTTQOSType(I);
    TCP.Host := Ini.ReadString('Server','Host','0.0.0.0');
    I := Ini.ReadInteger('Server','Port',1883);
        if I < 81 then
      I := 81;
    if I > 65535 then
      I := 65535;
    TCP.Port := I;
  finally
    Ini.Free;
  end;
end;

procedure TMQTTServerCLI.TCPAccept(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
begin
  Conn := Server.StartConnection;
  LogDispatcher.Send(mtInfo,'TCP connection accepted from %s on port %d',[aSocket.PeerAddress,aSocket.PeerPort]);
  aSocket.UserData := Conn;
  Conn.Socket := aSocket;
end;

procedure TMQTTServerCLI.ServerAccepted(AConnection: TMQTTServerConnection);
var
  Username: String;
begin
  if AConnection.Socket is TLSocket then
    begin
      Username := AConnection.Username;
      LogDispatcher.Send(mtInfo,'MQTT session started client=%s user=%s',[AConnection.Session.ClientID,Username]);
    end;
end;

procedure TMQTTServerCLI.TCPDisconnect(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
begin
  if Assigned(ASocket) then
    begin
      Conn := TMQTTServerConnection(aSocket.UserData);
      if Assigned(Conn) then
        Conn.Socket := nil;
      aSocket.UserData := nil;
      if Assigned(Conn) then
        if Conn.State <> ssDisconnected then
          Conn.Disconnected;
    end;
end;

procedure TMQTTServerCLI.ServerDisconnect(AConnection: TMQTTServerConnection);
var
  Socket: TLSocket;
begin
  Socket := AConnection.Socket as TLSocket;
  if Assigned(Socket) and (Socket.ConnectionStatus = scConnected) then
    Socket.Disconnect(False);
end;

procedure TMQTTServerCLI.ServerDisconnected(AConnection: TMQTTServerConnection);
var
  Socket: TLSocket;
begin
  Socket := AConnection.Socket as TLSocket;
  if Assigned(Socket) and (Socket.ConnectionStatus = scConnected) then
    Socket.Disconnect(False);
end;

procedure TMQTTServerCLI.TCPError(const msg: string; aSocket: TLSocket);
begin
  LogDispatcher.Send(mtError,Msg);
end;

procedure TMQTTServerCLI.ServerError(AConnection: TMQTTServerConnection; ErrCode: Word; ErrMsg: String);
begin
  LogDispatcher.Send(mtError,ErrMsg);
end;

procedure TMQTTServerCLI.TCPReceive(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
  Data: Pointer;
  Size: Integer;
begin
  Conn := TMQTTServerConnection(aSocket.UserData);
  if Assigned(Conn) and (Conn is TMQTTServerConnection) then
    begin
      GetMem(Data,32);
      try
        repeat
          Size := aSocket.Get(Data^,32);
          if Size > 0 then
            Conn.RecvBuffer.Write(Data,Size);
        until Size < 32;
        Conn.DataAvailable(Conn.RecvBuffer);
      finally
        FreeMem(Data,32);
      end;
    end;
end;

procedure TMQTTServerCLI.ServerSendData(AConnection: TMQTTServerConnection);
begin
  TCPCanSend(AConnection.Socket as TLSocket);
end;

procedure TMQTTServerCLI.ServerConnectionDestroy(
  AConnection: TMQTTServerConnection);
begin
  if (AConnection.Socket is TLSocket) then
    begin
      (AConnection.Socket as TLSocket).UserData := nil;
      (AConnection.Socket as TLSocket).Disconnect;
    end;
end;

procedure TMQTTServerCLI.TCPCanSend(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
  Data: Pointer;
  Sent,Size: Integer;
begin
  Conn := TMQTTServerConnection(aSocket.UserData);
  if Assigned(Conn) then
    begin
      Size := Conn.SendBuffer.Size;
      GetMem(Data,Size);
      try
        Conn.SendBuffer.Peek(Data,Size);
        Sent := aSocket.Send(Data^,Size);
        Conn.SendBuffer.Read(Data,Sent);
      finally
        FreeMem(Data,Size);
      end;
    end;
end;
{var
  Conn: TMQTTServerConnection;
  Data: Pointer;
  Size,Sent: Integer;
begin
  Conn := TMQTTServerConnection(aSocket.UserData);
  if Conn.SendBuffer.Size > 0 then
    begin
      GetMem(Data,32);
      try
        repeat
          Size := Conn.SendBuffer.Peek(Data,32);
          if Size > 0 then
            begin
              Sent := TCP.Send(Data^,Size);
              if Sent > 0 then
                Conn.SendBuffer.Read(Data,Sent);
            end;
        until (Size = 0) or (Sent < Size) or (Size < 32);
      finally
        FreeMem(Data,32);
      end;
    end;
end;}

var
  Application: TMQTTServerCLI;

begin
  Application:=TMQTTServerCLI.Create(nil);
  Application.Title:='MQTT Server';
  Application.Run;
  Application.Free;
end.


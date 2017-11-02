unit serverfm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, Menus, ExtCtrls, StdCtrls, Buffers, Logging, LNet, LNetComponents,
  MQTTConsts, MQTTSubscriptions, MQTTServer, MQTTMessages;

const
  LISTEN_RETRY_DELAY    = 15000;
  MQTT_DEFAULT_CONFIG_FILENAME1           = '/etc/mqtt/mqtt.ini';
  MQTT_DEFAULT_CONFIG_FILENAME2           = 'mqttserver.ini';

type
  TDebugMessage = record
    MessageType: TLogMessageType;
    Module: String;
    Message: String;
  end;
  PDebugMessage = ^TDebugMessage;

  { TServerForm }

  TServerForm = class(TForm)
    CBEnabled: TCheckBox;
    CBFiltered: TCheckBox;
    ClearBtn: TButton;
    FilterText: TEdit;
    LogGrid: TStringGrid;
    LogToolbarPanel: TPanel;
    PasswordManagerItm: TMenuItem;
    RefreshRetainedMessagesItm: TMenuItem;
    RetainedMessagesGridMenu: TPopupMenu;
    RetainedMessagesGrid: TStringGrid;
    RetainedMessagesTab: TTabSheet;
    TabSheet1: TTabSheet;
    TCP: TLTCPComponent;
    SessionsGrid: TStringGrid;
    MainMenu: TMainMenu;
    LoadConfigurationItm: TMenuItem;
    MenuDividerItem1: TMenuItem;
    ConnectionsGridMenu: TPopupMenu;
    RefreshSessionsItm: TMenuItem;
    SessionsGridMenu: TPopupMenu;
    Server: TMQTTServer;
    RefreshSubscriptionsItm: TMenuItem;
    SubscriptionsGridMenu: TPopupMenu;
    RefreshConnectionsItm: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveConfigurationItm: TMenuItem;
    SaveDialog: TSaveDialog;
    ServerMenu: TMenuItem;
    PropertiesItm: TMenuItem;
    ExitItm: TMenuItem;
    PageControl: TPageControl;
    ConnectionsTab: TTabSheet;
    SubscriptionsGrid: TStringGrid;
    ConnectionsGrid: TStringGrid;
    SubscriptionsTab: TTabSheet;
    SessionsTab: TTabSheet;
    ListenTimer: TTimer;
    procedure CBEnabledChange(Sender: TObject);
    procedure CBFilteredChange(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure ExitItmClick(Sender: TObject);
    procedure FilterTextExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListenTimerTimer(Sender: TObject);
    procedure LoadConfigurationItmClick(Sender: TObject);
    procedure PasswordManagerItmClick(Sender: TObject);
    procedure PropertiesItmClick(Sender: TObject);
    procedure RefreshConnectionsItmClick(Sender: TObject);
    procedure RefreshRetainedMessagesItmClick(Sender: TObject);
    procedure RefreshSessionsItmClick(Sender: TObject);
    procedure RefreshSubscriptionsItmClick(Sender: TObject);
    procedure SaveConfigurationItmClick(Sender: TObject);
    procedure ServerAccepted(AConnection: TMQTTServerConnection);
    procedure ServerDisconnect(AConnection: TMQTTServerConnection);
    procedure ServerDisconnected(AConnection: TMQTTServerConnection);
    procedure ServerError(AConnection: TMQTTServerConnection; ErrCode: Word; ErrMsg: String);
    procedure ServerSendData(AConnection: TMQTTServerConnection);
    procedure TCPAccept(aSocket: TLSocket);
    procedure TCPCanSend(aSocket: TLSocket);
    procedure TCPDisconnect(aSocket: TLSocket);
    procedure TCPError(const msg: string; aSocket: TLSocket);
    procedure TCPReceive(aSocket: TLSocket);
  private
    FConfigFilename : String;
    FListener       : TLogListener;
    FRecords        : TList;
    procedure ClearRecords;
    procedure DisplayHelp;
    procedure HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
    procedure LoadCommandLineOptions;
    function PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
    procedure Filter(Filter: String);

    function CheckConfigWriteAccess(Filename: String): Boolean;
    procedure LoadConfiguration(Filename: String);
    procedure RefreshAll;
    procedure SaveConfiguration(Filename: String);
  public
    Log: TLogDispatcher;
    Crt: TLogCrtListener;
  end;

var
  ServerForm: TServerForm;
  C: Integer = 0;

implementation

{$R *.lfm}

uses
  IniFiles, HelpFM, PasswordManFM, ServerPropertiesFM;

{ TServerForm }

procedure TServerForm.FormCreate(Sender: TObject);
var
  S: String;
begin
  Log := TLogDispatcher.Create('ServerForm');
  CRT := TLogCRTListener.Create;
  FRecords := TList.Create;
  FListener := TLogListener.Create;
  FListener.OnMessage := @HandleMessage;

  S := Application.CheckOptions('i:p:dansc:','interface: port: disabled authenticate null-clientid strict-clientid help config:');
  if S > '' then
    begin
      Log.Send(mtError,S);
      DisplayHelp;
    end;
  S := GetCurrentDir;
  Log.Send(mtInfo,'Current working directory is "%s"',[S]);
  if Application.HasOption('c','config') then
    FConfigFilename := Application.GetOptionValue('c','config')
  else
    if FileExists(MQTT_DEFAULT_CONFIG_FILENAME1) then
      FConfigFilename := MQTT_DEFAULT_CONFIG_FILENAME1
    else
      FConfigFilename := MQTT_DEFAULT_CONFIG_FILENAME2;

  FConfigFilename := ExpandFilename(FConfigFilename);
  Log.Send(mtInfo,'DefaultINIFilename="%s"',[FConfigFilename]);
  OpenDialog.InitialDir := GetCurrentDir;
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  OpenDialog.Filename := FConfigFilename;
  SaveDialog.Filename := FConfigFilename;
  if FileExists(FConfigFilename) then
    LoadConfiguration(FConfigFilename)
  else
    Log.Send(mtWarning,'Config file %s not found. Using default values.',[FConfigFilename]);
  LoadCommandLineOptions;
  Log.Send(mtDebug,'RequireAuthentication=%s',[BoolToStr(Server.RequireAuthentication,'True','False')]);
  Log.Send(mtDebug,'AllowNullClientIDs=%s',[BoolToStr(Server.AllowNullClientIDs,'True','False')]);
  Log.Send(mtDebug,'StrictClientIDValidation=%s',[BoolToStr(Server.StrictClientIDValidation,'true','false')]);
  if not Server.Enabled then
    Log.Send(mtWarning,'The server is being started in a disabled state');
  SaveConfigurationItm.Enabled := CheckConfigWriteAccess(FConfigFilename);
  PageControl.ActivePage := ConnectionsTab;
end;

procedure TServerForm.FormDestroy(Sender: TObject);
begin
  TCP.Disconnect;
  if SaveConfigurationItm.Enabled then
    try
      SaveConfiguration(FConfigFilename);
    except
    end;
  //Server.Free;
  CRT.Free;
  Log.Free;
  FListener.Destroy;
  ClearRecords;
  FRecords.Destroy;
end;

procedure TServerForm.ExitItmClick(Sender: TObject);
begin
  Close;
end;

procedure TServerForm.ListenTimerTimer(Sender: TObject);
begin
  ListenTimer.Enabled := False;
  if Server.Enabled then
    if TCP.Listen then
      begin
        Log.Send(mtInfo,'Server listening on port %d',[TCP.Port]);
        ConnectionsGrid.Enabled := True;
        SubscriptionsGrid.Enabled := True;
        SessionsGrid.Enabled := True;
        RetainedMessagesGrid.Enabled := True;
        RefreshAll;
      end
    else
      begin
        ListenTimer.Interval := LISTEN_RETRY_DELAY;
        Log.Send(mtError,'Server could not enter listening state.  Trying again in %d seconds.',[ListenTimer.Interval div 1000]);
        ListenTimer.Enabled := True;
      end;
end;

procedure TServerForm.LoadCommandLineOptions;
var
  S: String;
  I: Integer;
begin
  if Application.HasOption('i','interface') then
    TCP.Host := Application.GetOptionValue('i','interface');
  if Application.HasOption('help') then
    DisplayHelp;
  if Application.HasOption('p','port') then
    begin
      S := Application.GetOptionValue('p','port');
      if TryStrToInt(S,I) and (I > 80) and (I < 65535) then
        TCP.Port := I;
    end;
  if Application.HasOption('a','authenticate') then
    Server.RequireAuthentication := True;
  if Application.HasOption('n','null-clientid') then
    Server.AllowNullClientIDs := True;
  if Application.HasOption('s','strict-clientid') then
    Server.StrictClientIDValidation := True;
  if Application.HasOption('d','disabled') then
    Server.Enabled := False;
end;

procedure TServerForm.DisplayHelp;
begin
  if Application.ConsoleApplication then
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
      writeln('  -i --interface       Sets the IP address of the interface to listen on.');
      writeln('                       Default is all interfaces.');
      writeln('  -p --port            Sets the TCP port number to listen on.  Default 1883.');
      writeln('  -a --authenticate    When specified, clients are authenticated against the');
      writeln('                       password database.  Default is no authentication.');
      writeln('  -n --null-clientid   When no ClientID is provided by a client, generate a');
      writeln('                       unique one automatically.  Default rejects connection.');
      writeln('  -s --strict-clientid Validate Client IDs to ensure they contain only');
      writeln('                       letters and numbers.  Default accepts any character.');
      writeln('  -d --disabled        Starts the server in disabled state.  Use to create a');
      writeln('                       config ini file using the GUI.');
      writeln('     --help            Displays this help message');
    end
  else
    begin
      HelpForm.ShowModal;
      Application.Terminate;
    end;
end;

function TServerForm.CheckConfigWriteAccess(Filename: String): Boolean;
var
  H: THandle;
begin
  if FileExists(Filename) then
    begin
      H := FileOpen(Filename,fmOpenReadWrite);
      Result := H <> feInvalidHandle;
      if Result then
        FileClose(H);
    end
  else
    begin
      H := FileCreate(Filename);
      Result := H <> feInvalidHandle;
      if Result then
        FileClose(H);
    end;
end;

procedure TServerForm.PropertiesItmClick(Sender: TObject);
begin
  if ServerPropertiesDlg(Server,TCP) then
    begin
      SaveConfiguration(SaveDialog.Filename);
      RefreshAll;
    end;
end;

procedure TServerForm.LoadConfigurationItmClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    begin
      LoadConfiguration(OpenDialog.Filename);
      RefreshAll;
    end;
end;

procedure TServerForm.SaveConfigurationItmClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    SaveConfiguration(SaveDialog.Filename);
end;

procedure TServerForm.LoadConfiguration(Filename: String);
var
  Ini: TInifile;
  I: Integer;
  S: String;
begin
  Ini := TInifile.Create(Filename,[ifoStripComments,ifoStripInvalid,ifoFormatSettingsActive]);
  try
    Server.Enabled := Ini.ReadBool('Server','Enabled',True);
    Server.RequireAuthentication := Ini.ReadBool('Server','RequireAuthentication',False);
    Server.AllowNullClientIDs := Ini.ReadBool('Server','AllowNullClientIDs',True);
    Server.StrictClientIDValidation := Ini.ReadBool('Server','StrictClientIDValidation',False);
    Server.SystemClock := Ini.ReadBool('Server','UseSystemClock',True);

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
    S := Ini.ReadString('Server','Passwords','');
    if S > '' then
      Server.Passwords.AsBase64 := S;
  finally
    Ini.Free;
  end;
end;

procedure TServerForm.SaveConfiguration(Filename: String);
var
  Ini: TInifile;
  I: Integer;
begin
  Ini := TInifile.Create(Filename,[ifoStripComments,ifoStripInvalid,ifoFormatSettingsActive]);
  try
    Ini.WriteBool('Server','Enabled',Server.Enabled);
    Ini.WriteBool('Server','RequireAuthentication',Server.RequireAuthentication);
    Ini.WriteBool('Server','AllowNullClientIDs',Server.AllowNullClientIDs);
    Ini.WriteBool('Server','StrictClientIDValidation',Server.StrictClientIDValidation);
    Ini.WriteBool('Server','UseSystemClock',Server.SystemClock);
    I := ord(Server.MaximumQOS);
    Ini.WriteInteger('Server','MaximumQOS',I);
    Ini.WriteString('Server','Host',TCP.Host);
    Ini.WriteInteger('Server','Port',TCP.Port);
    Ini.WriteString('Server','Passwords',Server.Passwords.AsBase64);
  finally
    Ini.Free;
  end;
end;

procedure TServerForm.PasswordManagerItmClick(Sender: TObject);
begin
  PassManForm.PassMan := Server.Passwords;
  PassManForm.PassmanToGrid;
  if PassManForm.ShowModal = mrOK then
    PassManForm.GridToPassman;
end;

procedure TServerForm.TCPAccept(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
begin
  Conn := Server.StartConnection;
  Log.Send(mtInfo,'TCP connection accepted from %s on port %d',[aSocket.PeerAddress,aSocket.PeerPort]);
  aSocket.UserData := Conn;
  Conn.Socket := aSocket;
end;

procedure TServerForm.ServerAccepted(AConnection: TMQTTServerConnection);
var
  Username: String;
begin
  if AConnection.Socket is TLSocket then
    begin
      if Assigned(AConnection.User) then
        Username := AConnection.User.Username
      else
        Username := '(N/A)';
      Log.Send(mtInfo,'MQTT session started client=%s user=%s',[AConnection.Session.ClientID,Username]);
    end;
end;

procedure TServerForm.TCPDisconnect(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
begin
  if Assigned(ASocket) then
    begin
      Conn := TMQTTServerConnection(aSocket.UserData);
      aSocket.UserData := nil;
      if Assigned(Conn) then
        begin
          Conn.Socket := nil;
          if Assigned(Conn) then
              if Conn.State <> ssDisconnected then
                Conn.Disconnected;
        end;
    end;
end;

procedure TServerForm.ServerDisconnect(AConnection: TMQTTServerConnection);
var
  Socket: TLSocket;
begin
  Socket := AConnection.Socket as TLSocket;
  if Assigned(Socket) and (Socket.ConnectionStatus = scConnected) then
    Socket.Disconnect(False);
  RefreshConnectionsItmClick(nil);
end;

procedure TServerForm.ServerDisconnected(AConnection: TMQTTServerConnection);
var
  Socket: TLSocket;
begin
  Socket := AConnection.Socket as TLSocket;
  if Assigned(Socket) and (Socket.ConnectionStatus = scConnected) then
    Socket.Disconnect(False);
  RefreshConnectionsItmClick(nil);
end;

procedure TServerForm.TCPError(const msg: string; aSocket: TLSocket);
begin
  Log.Send(mtError,Msg);
end;

procedure TServerForm.ServerError(AConnection: TMQTTServerConnection; ErrCode: Word; ErrMsg: String);
begin
  Log.Send(mtError,ErrMsg);
end;

procedure TServerForm.TCPReceive(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
  Data: Pointer;
  Size: Integer;
begin
  if Assigned(aSocket) and (aSocket.Connected) then
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
end;

procedure TServerForm.ServerSendData(AConnection: TMQTTServerConnection);
begin
  TCPCanSend(AConnection.Socket as TLSocket);
end;

procedure TServerForm.TCPCanSend(aSocket: TLSocket);
var
  Conn: TMQTTServerConnection;
  Data: Pointer;
  Sent,Size: Integer;
begin
  Conn := TMQTTServerConnection(aSocket.UserData);
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

procedure TServerForm.RefreshAll;
begin
  RefreshConnectionsItmClick(nil);
  RefreshSessionsItmClick(nil);
  RefreshSubscriptionsItmClick(nil);
  RefreshRetainedMessagesItmClick(nil);
end;

function BoolToGridStr(B: Boolean): String;
begin
  if B then Result := '1' else Result := '0';
end;

procedure TServerForm.RefreshConnectionsItmClick(Sender: TObject);
var
  I: Integer;
  S: TMQTTServerConnection;
begin
  ConnectionsGrid.RowCount := Server.Connections.Count + 1;
  for I := 0 to Server.Connections.Count - 1 do
    begin
      S := Server.Connections[I];
      ConnectionsGrid.Objects[0,I+1] := S;
      ConnectionsGrid.Cells[0,I+1] := SERVER_CONNECTION_STATE_NAMES[S.State];
      if (S.Socket is TLSocket) then
        ConnectionsGrid.Cells[1,I+1] := (S.Socket as TLSocket).PeerAddress
      else
        ConnectionsGrid.Cells[1,I+1] := '(N/A)';
      if Assigned(S.User) then
        begin
          ConnectionsGrid.Cells[2,I+1]   := S.User.Username;
          ConnectionsGrid.Objects[2,I+1] := S.User;
        end
      else
        begin
          ConnectionsGrid.Cells[2,I+1] := '(N/A)';
          ConnectionsGrid.Objects[2,I+1] := nil;
        end;
      ConnectionsGrid.Cells[3,I+1] := S.WillMessage.DisplayText;
      ConnectionsGrid.Objects[3,I+1] := S.WillMessage;
    end;
end;

procedure TServerForm.RefreshSessionsItmClick(Sender: TObject);
var
  I: Integer;
  S: TMQTTSession;
begin
  SessionsGrid.RowCount := Server.Sessions.Count + 1;
  for I := 0 to Server.Sessions.Count - 1 do
    begin
      S := Server.Sessions[I];
      SessionsGrid.Objects[0,I+1] := S;
      SessionsGrid.Cells[0,I+1] := S.ClientID;
      SessionsGrid.Cells[1,I+1] := S.Description;
      SessionsGrid.Cells[2,I+1] := MQTTQOSTypeNames[S.MaximumQOS];
    end;
end;

procedure TServerForm.RefreshSubscriptionsItmClick(Sender: TObject);
var
  I,J,X: Integer;
  C: TMQTTServerConnection;
  S: TMQTTSubscription;
begin
  J := 1;
  for I := 0 to Server.Connections.Count - 1 do
    begin
      C := Server.Connections[I];
      if Assigned(C) and Assigned(C.Session) then
        begin
          for X := 0 to C.Session.Subscriptions.Count - 1 do
            begin
              S := C.Session.Subscriptions[X];
              if J >= SubscriptionsGrid.RowCount then
                SubscriptionsGrid.RowCount     := SubscriptionsGrid.RowCount + 1;
              SubscriptionsGrid.Cells[0,J] := C.Session.ClientID;
              SubscriptionsGrid.Cells[1,J] := S.Filter;
              SubscriptionsGrid.Cells[2,J] := MQTTQOSTypeNames[S.QOS];
              SubscriptionsGrid.Cells[3,J] := IntToStr(S.Age);
              inc(J);
            end;
        end;
    end;
  SubscriptionsGrid.RowCount := J;
end;

procedure TServerForm.RefreshRetainedMessagesItmClick(Sender: TObject);
var
  I: Integer;
  M: TMQTTMessage;
begin
  RetainedMessagesGrid.RowCount := Server.RetainedMessages.Count + 1;
  for I := 0 to Server.RetainedMessages.Count - 1 do
    begin
      M := Server.RetainedMessages[I];
      RetainedMessagesGrid.Objects[0,I+1] := M;
      RetainedMessagesGrid.Cells[0,I+1] := M.ClientID;
      RetainedMessagesGrid.Cells[1,I+1] := M.Topic;
      RetainedMessagesGrid.Cells[2,I+1] := M.Data;
      RetainedMessagesGrid.Cells[3,I+1] := MQTTQOSTypeNames[M.QOS];
    end;
end;

procedure TServerForm.ClearRecords;
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

procedure TServerForm.FilterTextExit(Sender: TObject);
begin
  if CBFiltered.Checked then
    begin
      LogGrid.RowCount := 1;
      Filter(FilterText.Text);
    end;
end;

procedure TServerForm.ClearBtnClick(Sender: TObject);
begin
  ClearRecords;
  LogGrid.RowCount := 1;
end;

procedure TServerForm.HandleMessage(Dispatcher: TLogDispatcher; MessageType: TLogMessageType; Message: String);
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

procedure TServerForm.CBFilteredChange(Sender: TObject);
begin
  if CBFiltered.Checked then
    Filter(FilterText.Text)
  else
    Filter('');
end;

procedure TServerForm.CBEnabledChange(Sender: TObject);
begin
  FListener.Enabled := CBEnabled.Checked;
end;

function TServerForm.PassesFilter(Filter: String; Rec: PDebugMessage): Boolean;
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

procedure TServerForm.Filter(Filter: string);
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


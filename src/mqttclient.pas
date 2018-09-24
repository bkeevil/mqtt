unit mqttclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buffers, Logging, MQTTTokenizer,
  MQTTConsts, MQTTPackets, MQTTPacketDefs, MQTTSubscriptions;

type
  TMQTTClient = class;
  TMQTTClientSubscription = class;
  TMQTTClientSubscriptions = class;

  TMQTTClientSendDataEvent = procedure (AClient: TMQTTClient) of object;
  TMQTTClientReceiveMessageEvent = procedure (AClient: TMQTTClient; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean) of object;
  TMQTTClientErrorEvent = procedure (AClient: TMQTTClient; ErrCode: Word; ErrMsg: String) of object;
  TMQTTClientSubscriptionReceiveMessageEvent = procedure (Subscription: TMQTTClientSubscription; Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retained: Boolean) of object;

  { TMQTTClientThread }

  TMQTTClientThread = class(TThread)
    private
      FClient: TMQTTClient;
      FConnect: Integer;
      procedure OnTimer;
      procedure OnConnectTimer;
    protected
      procedure Execute; override;
  end;

  { TMQTTClient }

  TMQTTClient = class(TComponent)
    private
      FSocket              : TObject;
      FState               : TMQTTConnectionState;
      FPacketIDManager     : TMQTTPacketIDManager;
      FSubscriptions       : TMQTTSubscriptionList;
      FClientSubscriptions : TList;
      FSendBuffer          : TBuffer;
      FRecvBuffer          : TBuffer;
      FResendPacketTimeout : Word;
      FMaxResendAttempts   : Byte;
      // Packet Queues
      FWaitingForAck       : TMQTTPacketQueue; // QoS 1 and QoS 2 messages which have been sent to the Server, but have not been completely acknowledged.
      FPendingReceive      : TMQTTPacketQueue; // QoS 2 messages which have been received from the Server, but have not been completely acknowledged.       // Attributes
      FWillMessage         : TMQTTWillMessage;
      FCleanSession        : Boolean;
      FClientID            : UTF8String;
      FPassword            : AnsiString;
      FUsername            : UTF8String;
      FKeepAlive           : Word;
      FPingInterval        : Word;
      FPingCount           : Byte;
      FPingIntRemaining    : Word;
      FThread              : TMQTTClientThread;
      // State Fields
      FInsufficientData    : Byte;
      //
      FOnInitSession          : TNotifyEvent;
      FOnConnected            : TNotifyEvent;
      FOnDisconnect           : TNotifyEvent;
      FOnDisconnected         : TNotifyEvent;
      FOnError                : TMQTTClientErrorEvent;
      FOnSendData             : TMQTTClientSendDataEvent;
      FOnReceiveMessage       : TMQTTClientReceiveMessageEvent;
      FOnSubscriptionsChanged : TNotifyEvent;
      // Timer Methods
      procedure HandleTimer;
      procedure HandleConnectTimer;
      procedure MergeSubscriptions(Subscriptions: TMQTTSubscriptionList; ReturnCodes: TBuffer);
      procedure ProcessPingIntervals;
      procedure ProcessAckQueueIntervals;
      //procedure SendPendingMessages;
      // Methods to handle incomming packets
      procedure HandleCONNACKPacket(APacket: TMQTTCONNACKPacket);
      procedure HandlePINGRESPPacket;
      procedure HandleSUBACKPacket(APacket: TMQTTSUBACKPacket);
      procedure HandleUNSUBACKPacket(APacket: TMQTTUNSUBACKPacket);
      procedure HandlePUBLISHPacket(APacket: TMQTTPUBLISHPacket);
      procedure HandlePUBLISHPacket1(APacket: TMQTTPUBLISHPacket);
      procedure HandlePUBLISHPacket2(APacket: TMQTTPUBLISHPacket);
      procedure HandlePUBACKPacket(APacket: TMQTTPUBACKPacket);
      procedure HandlePUBRECPacket(APacket: TMQTTPUBRECPacket);
      procedure HandlePUBRELPacket(APacket: TMQTTPUBRELPacket);
      procedure HandlePUBCOMPPacket(APacket: TMQTTPUBCOMPPacket);
      procedure ProcessReturnCodes(AList: TMQTTSubscriptionList; ReturnCodes: TBuffer);
      procedure SetClientID(AValue: UTF8String);
      procedure SetKeepAlive(AValue: Word);
      // Property access methods
      procedure SetPingInterval(AValue: Word);
      // Methods to send packets
      procedure Ping;
      procedure SetWillMessage(AValue: TMQTTWillMessage);
    protected
      // Event Handlers
      procedure Connected; virtual;
      //
      procedure Loaded; override;
      procedure InitSession; virtual;
      procedure SendData; virtual;
      procedure ReceiveMessage(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retained: Boolean); virtual;
      property PacketIDManager: TMQTTPacketIDManager read FPacketIDManager;
    public
      Log: TLogDispatcher;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      // Methods
      procedure Reset;
      function Connect: Boolean;
      procedure Disconnect;
      procedure Publish(Topic: UTF8String; Data: String; QOS: TMQTTQOSType = qtAT_MOST_ONCE; Retain: Boolean = False; Duplicate: Boolean = False);
      procedure Disconnected; virtual;
      procedure Bail(ErrCode: Word);
      function Subscribe(Subscriptions: TMQTTSubscriptionList): Boolean;
      function Unsubscribe(Subscriptions: TMQTTSubscriptionList): Boolean;
      // Event Handlers
      procedure DataAvailable; virtual;
      // Properties
      property SendBuffer: TBuffer read FSendBuffer;
      property RecvBuffer: TBuffer read FRecvBuffer;
      property Socket: TObject read FSocket write FSocket;
      property State: TMQTTConnectionState read FState;
      property Subscriptions: TMQTTSubscriptionList read FSubscriptions;
    published
      property ResendPacketTimeout: Word read FResendPacketTimeout write FResendPacketTimeout default 2; // Seconds
      property MaxResendAttmpts: Byte read FMaxResendAttempts write FMaxResendAttempts default 3;
      property ClientID: UTF8String read FClientID write SetClientID;
      property Username: UTF8String read FUsername write FUsername;
      property Password: AnsiString read FPassword write FPassword;
      property WillMessage: TMQTTWillMessage read FWillMessage write SetWillMessage;
      property CleanSession: Boolean read FCleanSession write FCleanSession default True;
      property KeepAlive: Word read FKeepAlive write SetKeepAlive default 30;
      property PingInterval: Word read FPingInterval write SetPingInterval default 15;
      // Events
      property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
      property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
      property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
      property OnInitSession: TNotifyEvent read FOnInitSession write FOnInitSession;
      property OnError: TMQTTClientErrorEvent read FOnError write FOnError;
      property OnSendData: TMQTTClientSendDataEvent read FOnSendData write FOnSendData;
      property OnReceiveMessage: TMQTTClientReceiveMessageEvent read FOnReceiveMessage write FOnReceiveMessage;
      property OnSubscriptionsChanged: TNotifyEvent read FOnSubscriptionsChanged write FOnSubscriptionsChanged;
  end;

  { TMQTTClientSubscription }

  TMQTTClientSubscription = class(TCollectionItem)
    private
      FFilter: UTF8String;
      FTokenizer: TMQTTTokenizer;
      FQOS: TMQTTQOSType;
      FOnMessage: TMQTTClientSubscriptionReceiveMessageEvent;
      function GetClient: TMQTTClient;
      procedure SetFilter(AValue: UTF8String);
    protected
      function GetDisplayName: String; override;
      procedure SetDisplayName(const Value: String); override;
      //procedure HandleMessage(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retained: Boolean); virtual;
    public
      constructor Create(ACollection: TCollection); override;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
    published
      property Filter: UTF8String read FFilter write SetFilter;
      property QOS: TMQTTQOSType read FQOS write FQOS default qtAT_MOST_ONCE;
      property Client: TMQTTClient read GetClient;
      property Tokens: TMQTTTokenizer read FTokenizer;
      //
      property OnMessage: TMQTTClientSubscriptionReceiveMessageEvent read FOnMessage write FOnMessage;
  end;

  { TMQTTClientSubscriptions }

  TMQTTClientSubscriptions = class(TCollection)
    private
      FClient: TMQTTClient;
      function GetItems(Index: Integer): TMQTTClientSubscription;
      procedure SetItems(Index: Integer; AValue: TMQTTClientSubscription);
    public
      constructor Create(AClient: TMQTTClient);
      function GetOwner: TPersistent; override;
      function Add: TMQTTClientSubscription;
      function Insert(Index: Integer): TMQTTClientSubscription;
      function FindItemID(ID: Integer): TMQTTClientSubscription;
      property Client: TMQTTClient read FClient;
      property Items[Index: Integer]: TMQTTClientSubscription read GetItems write SetItems; default;
  end;

implementation

{ TMQTTClientThread }

procedure TMQTTClientThread.OnTimer;
begin
  FClient.HandleTimer;
end;

procedure TMQTTClientThread.OnConnectTimer;
begin
  FClient.HandleConnectTimer;
end;

procedure TMQTTClientThread.Execute;
begin
  while not Terminated do
    begin
      Sleep(1000);
      if Assigned(FClient) then
        if FClient.State = csConnecting then
          begin
            inc(FConnect);
            if (FConnect >= 2) then
              begin
                Synchronize(@OnConnectTimer);
                FConnect := 0;
              end;
          end
        else
          if FClient.State = csConnected then
            Synchronize(@OnTimer);
    end;
end;

{ TMQTTClient }

constructor TMQTTClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Log                  := TLogDispatcher.Create('Client');
  FResendPacketTimeout := 2;
  FMaxResendAttempts   := 3;
  FSendBuffer          := TBuffer.Create;
  FRecvBuffer          := TBuffer.Create;
  FPacketIDManager     := TMQTTPacketIDManager.Create;
  FSubscriptions       := TMQTTSubscriptionList.Create;
  FClientSubscriptions := TList.Create;
  FWaitingForAck       := TMQTTPacketQueue.Create;
  FPendingReceive      := TMQTTPacketQueue.Create;
  FWillMessage         := TMQTTWillMessage.Create;
  FKeepAlive           := 30;
  FPingInterval        := 15;
  FPingIntRemaining    := FPingInterval;
  FCleanSession        := True;

  FThread                 := TMQTTClientThread.Create(False);
  FThread.FreeOnTerminate := True;
  FThread.FClient         := Self;
end;

destructor TMQTTClient.Destroy;
begin
  FThread.FClient := nil;
  FThread.Terminate;
  FWillMessage.Free;
  FWaitingForAck.Free;
  FPendingReceive.Free;
  FClientSubscriptions.Free;
  FSubscriptions.Free;
  FPacketIDManager.Free;
  FSendBuffer.Free;
  FRecvBuffer.Free;
  Log.Free;
  FThread.Free;
  inherited Destroy;
end;

procedure TMQTTClient.Reset;
begin
  FPendingReceive.Clear;
  FWaitingForAck.Clear;
  FPacketIDManager.Reset;
  FWillMessage.Clear;
  FState := csNew;
  FUsername := '';
  FPassword := '';
  FClientID := '';
  Log.Name := 'Client';
  FCleanSession := False;
  FPingIntRemaining := FPingInterval;
  FPingCount := 0;
  Log.Send(mtInfo,'Client has been reset');
end;

procedure TMQTTClient.HandleTimer;
begin
  if Assigned(Self) and (State = csConnected) then
    begin
      ProcessPingIntervals;
      ProcessAckQueueIntervals;
    end;
end;

procedure TMQTTClient.HandleConnectTimer;
begin
  if (State = csConnecting) then
    begin
      Reset;
      FThread.FConnect := 0;
      if Assigned(FOnError) then
        FOnError(Self,MQTT_ERROR_CONNECT_TIMEOUT,GetMQTTErrorMessage(MQTT_ERROR_CONNECT_TIMEOUT));
    end;
end;

procedure TMQTTClient.ProcessAckQueueIntervals;
var
  I: Integer;
  Packet: TMQTTQueuedPacket;
begin
  if (State = csConnected) then
    for I := FWaitingForAck.Count - 1 downto 0 do
      begin
        Packet := FWaitingForAck[I];
        if Packet.SecondsInQueue >= FResendPacketTimeout then
          begin
            if Packet.ResendCount < FMaxResendAttempts then
              begin
                if Packet.PacketType = ptPUBLISH then
                  (Packet as TMQTTPUBLISHPacket).Duplicate := True;
                Packet.WriteToBuffer(SendBuffer);
                Log.Send(mtWarning,'Resending %s packet',[Packet.PacketTypeName]);
                SendData;
                Packet.SecondsInQueue := 0;
                Packet.ResendCount := Packet.ResendCount + 1;
              end
            else
              begin
                FWaitingForAck.Delete(I);
                if Packet.PacketType in [ptSUBACK,ptUNSUBACK,ptPUBACK,ptPUBCOMP] then
                  PacketIDManager.ReleaseID(Packet.PacketID);
                Log.Send(mtWarning,'A %s packet went unacknowledged by the server',[Packet.PacketTypeName]);
                Packet.Free;
              end;
          end;
      end;
end;

procedure TMQTTClient.SetWillMessage(AValue: TMQTTWillMessage);
begin
  FWillMessage.Assign(AValue);
end;

function TMQTTClient.Connect: Boolean;
var
  Packet: TMQTTCONNECTPacket;
begin
  Assert(State = csNew);
  if (State = csNew) then
    begin
      FThread.FConnect := 0;  // Reset connect timer counter
      // Check preconditions
      if ((FUsername > '') and (FPassword = '')) then
        begin
          Result := False;
          if Assigned(OnError) then
            OnError(Self,MQTT_ERROR_BAD_USERNAME_PASSWORD,GetMQTTErrorMessage(MQTT_ERROR_BAD_USERNAME_PASSWORD));
          Exit;
        end;
      if (FClientID = '') then
        begin
          Result := False;
          if Assigned(OnError) then
            OnError(Self,MQTT_ERROR_NO_CLIENTID,GetMQTTErrorMessage(MQTT_ERROR_NO_CLIENTID));
          Exit;
        end;
      if (FWillMessage.Enabled) and ((FWillMessage.Topic = '') or (FWillMessage.Message = '')) then
        begin
          Result := False;
          if Assigned(OnError) then
            OnError(Self,MQTT_ERROR_WILLMESSAGE_INVALID,GetMQTTErrorMessage(MQTT_ERROR_WILLMESSAGE_INVALID));
          Exit;
        end;
      Result := True;

      // Construct a CONNECT packet object
      Packet := TMQTTCONNECTPacket.Create;
      try
        Packet.ClientID     := FClientID;
        Packet.Username     := FUsername;
        Packet.UsernameFlag := FUsername > '';
        Packet.Password     := FPassword;
        Packet.PasswordFlag := FPassword > '';
        Packet.CleanSession := FCleanSession;
        Packet.KeepAlive    := FKeepAlive;
        Packet.WillMessage  := FWillMessage;
        // Use the CONNECT object to write packet data to a buffer and send the buffer
        Packet.WriteToBuffer(SendBuffer);
        FState := csConnecting;
        Log.Send(mtDebug,'Sending CONNECT');
        SendData;
      finally
        Packet.Destroy;
      end;
    end;
end;

procedure TMQTTClient.HandleCONNACKPacket(APacket: TMQTTCONNACKPacket);
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received CONNACK ('+IntToStr(APacket.ReturnCode)+')');
      if APacket.ReturnCode = MQTT_CONNACK_SUCCESS then
        begin
          // Start the ping interval timer
          FPingIntRemaining := FPingInterval;
          FThread.FConnect := 0;
          Connected;
          if not APacket.SessionPresent then
            InitSession;
        end
      else
        case APacket.ReturnCode of
          MQTT_CONNACK_UNACCEPTABLE_PROTOCOL : Bail(MQTT_ERROR_UNACCEPTABLE_PROTOCOL);
          MQTT_CONNACK_CLIENTID_REJECTED     : Bail(MQTT_ERROR_CLIENTID_REJECTED);
          MQTT_CONNACK_SERVER_UNAVAILABLE    : Bail(MQTT_ERROR_SERVER_UNAVAILABLE);
          MQTT_CONNACK_BAD_USERNAME_PASSWORD : Bail(MQTT_ERROR_BAD_USERNAME_PASSWORD);
          MQTT_CONNACK_NOT_AUTHORIZED        : Bail(MQTT_ERROR_NOT_AUTHORIZED);
        end;
    end;
end;

procedure TMQTTClient.Connected;
begin
  Assert(State = csConnecting);
  if (State = csConnecting) then
    begin
      Log.Send(mtInfo,'Client has connected');
      FState := csConnected;
      if Assigned(FOnConnected) then
        FOnConnected(Self);
    end;
end;

procedure TMQTTClient.Loaded;
begin
  inherited Loaded;
  Log.Name := Name;
end;

procedure TMQTTClient.InitSession;
begin
  FSubscriptions.Clear;
  FWaitingForAck.Clear;
  FPendingReceive.Clear;
  if Assigned(FOnInitSession) then
    FOnInitSession(Self);
end;

procedure TMQTTClient.Disconnect;
var
  Packet: TMQTTDISCONNECTPacket;
begin
  Assert(State = csConnected);
  if FState <> csDisconnected then
    begin
      Packet := TMQTTDISCONNECTPacket.Create;
      try
        Packet.WriteToBuffer(SendBuffer);
        Log.Send(mtDebug,'Sending DISCONNECT');
        SendData;
        FState := csDisconnecting;
      finally
        Packet.Destroy;
      end;
    end;
end;

procedure TMQTTClient.Disconnected;
begin
  Assert(State <> csDisconnected);
  if FState <> csDisconnected then
    begin
      Log.Send(mtInfo,'The server terminated the connection');
      FState := csDisconnected;
      FPingIntRemaining := FPingInterval;
      if Assigned(FOnDisconnected) then
        FOnDisconnected(Self);
      FState := csNew;
    end;
end;

procedure TMQTTClient.Bail(ErrCode: Word);
var
  Msg: String;
begin
  Msg := GetMQTTErrorMessage(ErrCode);
  Log.Send(mtError,'Bail: '+Msg);
  FState := csDisconnecting;
  FPingIntRemaining := FPingInterval;
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
  FState := csDisconnected;
  if Assigned(FOnError) then
    FOnError(Self,ErrCode,Msg);
  Reset;
end;

procedure TMQTTClient.SendData;
begin
  FPingIntRemaining := FPingInterval;
  if Assigned(FOnSendData) then
    FOnSendData(Self);
end;

procedure TMQTTClient.DataAvailable;
var
  DestroyPacket: Boolean;
  Packet: TMQTTPacket;
  ErrCode: Word;
begin
  DestroyPacket := True;
  Packet := nil;
  ErrCode := ReadMQTTPacketFromBuffer(FRecvBuffer,Packet,State = csConnected);
  try
    if ErrCode = MQTT_ERROR_NONE then
      begin
        FInsufficientData := 0;
        if State = csConnected then
          begin
            case Packet.PacketType of
              ptPINGRESP : HandlePINGRESPPacket;
              ptSUBACK   : HandleSUBACKPacket(Packet as TMQTTSUBACKPacket);
              ptUNSUBACK : HandleUNSUBACKPacket(Packet as TMQTTUNSUBACKPacket);
              ptPUBLISH  : HandlePUBLISHPacket(Packet as TMQTTPUBLISHPacket);
              ptPUBACK   : HandlePUBACKPacket(Packet as TMQTTPUBACKPacket);
              ptPUBREC   : HandlePUBRECPacket(Packet as TMQTTPUBRECPacket);
              ptPUBREL   : HandlePUBRELPacket(Packet as TMQTTPUBRELPacket);
              ptPUBCOMP  : HandlePUBCOMPPacket(Packet as TMQTTPUBCOMPPacket);
            else
              Bail(MQTT_ERROR_UNHANDLED_PACKETTYPE);
            end;
            if (Packet.PacketType = ptPUBLISH) and ((Packet as TMQTTPUBLISHPacket).QOS = qtEXACTLY_ONCE) then
              DestroyPacket := False;
          end
        else
          if (State = csDisconnecting) then
            // Ignore any packets while disconnecting
          else
            if (State = csConnecting) and (Packet is TMQTTCONNACKPacket) then
              HandleCONNACKPacket(Packet as TMQTTCONNACKPacket)
            else
              Bail(MQTT_ERROR_NOT_CONNECTED)
      end
    else
      if ErrCode = MQTT_ERROR_INSUFFICIENT_DATA then
        if FInsufficientData >= 2 then
          Bail(ErrCode)
        else
          inc(FInsufficientData)
      else
        Bail(ErrCode);
    // More data?
    if (ErrCode = MQTT_ERROR_NONE) and (FRecvBuffer.Size > 0) then
      DataAvailable;
  finally
    if Assigned(Packet) and DestroyPacket then
      FreeAndNil(Packet);
  end;
end;

procedure TMQTTClient.ProcessPingIntervals;
begin
  if FPingIntRemaining = 1 then
    begin
      if FPingCount >= 2 then
        begin
          FPingIntRemaining := 0;
          FPingCount := 0;
          Bail(MQTT_ERROR_NO_PING_RESPONSE);
          Exit;
        end;
      inc(FPingCount);
      FPingIntRemaining := 6;
      Ping;
    end
  else
    if FPingIntRemaining > 1 then
      dec(FPingIntRemaining);
end;

procedure TMQTTClient.SetPingInterval(AValue: Word);
begin
  if (AValue >= FKeepAlive) and (FKeepAlive > 2) then
    AValue := FKeepAlive - 2;
  if FPingInterval=AValue then Exit;
  Log.Send(mtDebug,'Ping interval changed from %d to %d',[FPingInterval,AValue]);
  FPingInterval:=AValue;
  FPingIntRemaining := FPingInterval;
end;

procedure TMQTTClient.Ping;
var
  Packet: TMQTTPINGREQPacket;
begin
  Assert(State = csConnected);
  if (State = csConnected) then
    begin
      Packet := TMQTTPINGREQPacket.Create;
      try
        Packet.WriteToBuffer(SendBuffer);
        Log.Send(mtDebug,'Sending PINGREQ');
        SendData;
      finally
        Packet.Free;
      end;
    end;
end;

procedure TMQTTClient.HandlePINGRESPPacket;
begin
  Log.Send(mtDebug,'Received PINGRESP');
  FPingCount := 0;
  FPingIntRemaining := FPingInterval;
end;

function TMQTTClient.Subscribe(Subscriptions: TMQTTSubscriptionList): Boolean;
var
  Packet: TMQTTSUBSCRIBEPacket;
begin
  Assert(Assigned(Subscriptions) and (State = csConnected));
  if Assigned(Subscriptions) and (State = csConnected) then
    begin
      Result := Assigned(Subscriptions) and (Subscriptions.Count > 0);
      if not Result then Exit;
      Packet := TMQTTSUBSCRIBEPacket.Create;
      Packet.Subscriptions.Assign(Subscriptions);
      Packet.PacketID := PacketIDManager.GenerateID;
      FWaitingForAck.Add(Packet);
      Packet.WriteToBuffer(SendBuffer);
      Log.Send(mtDebug,'Sending SUBSCRIBE');
      SendData;
    end;
end;

procedure TMQTTClient.HandleSUBACKPacket(APacket: TMQTTSUBACKPacket);
var
  I: Integer;
  Packet: TMQTTQueuedPacket;
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received SUBACK (PacketID='+IntToStr(APacket.PacketID)+')');
      // Remove from Ack Queue
      for I := FWaitingForAck.Count - 1 downto 0 do
        begin
          Packet := FWaitingForAck[I];
          if (Packet.PacketType = ptSUBSCRIBE) and (Packet.PacketID = APacket.PacketID) then
            begin
              FWaitingForAck.Delete(I);
              ProcessReturnCodes((Packet as TMQTTSUBSCRIBEPacket).Subscriptions,APacket.ReturnCodes);
              Subscriptions.MergeList((Packet as TMQTTSUBSCRIBEPacket).Subscriptions);
              if Assigned(FOnSubscriptionsChanged) then
                FOnSubscriptionsChanged(Self);
              PacketIDManager.ReleaseID(Packet.PacketID);
              Packet.Free;
            end;
        end;
    end;
end;

procedure TMQTTClient.ProcessReturnCodes(AList: TMQTTSubscriptionList; ReturnCodes: TBuffer);
var
  X: Integer;
  S: TMQTTSubscription;
  Q: TMQTTQOSType;
  B: Byte;
begin
  Assert(Assigned(AList) and Assigned(ReturnCodes));
  if Assigned(AList) and Assigned(ReturnCodes) then
    begin
      if (AList.Count <> ReturnCodes.Size) then
        Bail(MQTT_ERROR_INVALID_RETURN_CODES);
      for X := AList.Count - 1 downto 0 do
        begin
          S := AList[X];
          ReturnCodes.Read(@B,1);
          if B = $80 then
            begin
              Log.Send(mtWarning,'The server rejected the subscription "%s"',[S.Filter]);
              S.Free;
              AList.Delete(X);
            end
          else
            begin
              Q := TMQTTQOSType(B);
              if ord(Q) < ord(S.QOS) then
                begin
                  S.QOS := Q;
                  Log.Send(mtDebug,'Downgrade QoS of subscription "%s"',[S.Filter]);
                end;
            end;
        end;
    end;
end;

procedure TMQTTClient.SetClientID(AValue: UTF8String);
begin
  if FClientID=AValue then Exit;
  FClientID:=AValue;
  if AValue = '' then
    Log.Name := 'Client'
  else
    Log.Name := AValue;
end;

procedure TMQTTClient.SetKeepAlive(AValue: Word);
begin
  if FKeepAlive=AValue then Exit;
  FKeepAlive:=AValue;
  if (FPingInterval >= FKeepAlive) and (FKeepAlive > 2) then
    FPingInterval := FKeepAlive - 2;
end;

procedure TMQTTClient.MergeSubscriptions(Subscriptions: TMQTTSubscriptionList; ReturnCodes: TBuffer);
var
  I: Integer;
  S, Subscription: TMQTTSubscription;
  QOS: TMQTTQOSType;
begin
  Assert(Subscriptions.Count = ReturnCodes.Size);
  if (Subscriptions.Count = ReturnCodes.Size) then
    for I := 0 to Subscriptions.Count - 1 do
      begin
        Subscription := Subscriptions[I];
        ReturnCodes.Read(@QOS,1);
        Subscription.QOS := QOS;
        S := Subscriptions.Find(Subscription.Filter);
        if Assigned(S) then
          S.QOS := QOS
        else
          begin
            S := TMQTTSubscription.Create;
            S.Assign(Subscription);
            Subscriptions.Update(S);
          end;
      end;
end;

function TMQTTClient.Unsubscribe(Subscriptions: TMQTTSubscriptionList): Boolean;
var
  Packet: TMQTTUNSUBSCRIBEPacket;
begin
  Assert(State = csConnected);
  if (State = csConnected) then
    begin
      Result := Assigned(Subscriptions) and (Subscriptions.Count > 0);
      if not Result then Exit;
      Packet := TMQTTUNSUBSCRIBEPacket.Create;
      Packet.Subscriptions.Assign(Subscriptions);
      Packet.PacketID := PacketIDManager.GenerateID;
      FWaitingForAck.Add(Packet);
      Packet.WriteToBuffer(SendBuffer);
      Log.Send(mtDebug,'Sending UNSUBSCRIBE');
      SendData;
    end;
end;

procedure TMQTTClient.HandleUNSUBACKPacket(APacket: TMQTTUNSUBACKPacket);
var
  I: Integer;
  Packet: TMQTTQueuedPacket;
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received UNSUBACK ('+IntToStr(APacket.PacketID)+')');
      // Remove from Ack Queue
      for I := FWaitingForAck.Count - 1 downto 0 do
        begin
          Packet := FWaitingForAck[I];
          if (Packet.PacketType = ptUNSUBSCRIBE) and (Packet.PacketID = APacket.PacketID) then
            begin
              FWaitingForAck.Delete(I);
              Subscriptions.DeleteList((Packet as TMQTTUNSUBSCRIBEPacket).Subscriptions);
              if Assigned(FOnSubscriptionsChanged) then
                FOnSubscriptionsChanged(Self);
              PacketIDManager.ReleaseID(Packet.PacketID);
              Packet.Free;
              Break;
            end;
        end;
    end;
end;

procedure TMQTTClient.ReceiveMessage(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retained: Boolean);
var
  Tokens: TMQTTTokenizer;
  I: Integer;
  Subscription: TMQTTClientSubscription;
begin
  if FClientSubscriptions.Count > 0 then
    begin
      Tokens := TMQTTTokenizer.Create(Topic,False);
      try
        for I := 0 to FClientSubscriptions.Count - 1 do
          begin
            Subscription := TMQTTClientSubscription(FClientSubscriptions[I]);
            if CheckTopicMatchesFilter(Tokens,Subscription.Tokens) then
              if Assigned(Subscription.OnMessage) then
                Subscription.OnMessage(Subscription,Topic,Data,QOS,Retained);
          end;
      finally
        Tokens.Free;
      end;
    end;
  if Assigned(FOnReceiveMessage) then
    FOnReceiveMessage(Self,Topic,Data,QOS,Retained);
end;

procedure TMQTTClient.Publish(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean; Duplicate: Boolean);
var
  Packet: TMQTTPUBLISHPacket;
begin
  Assert(State = csConnected);
  if (State = csConnected) then
    begin
      Packet := TMQTTPUBLISHPacket.Create;
      try
        Packet.QOS := QOS;
        Packet.Duplicate := Duplicate;
        Packet.Retain := Retain;
        Packet.Topic := Topic;
        Packet.Data := Data;
        if QOS in [qtAT_LEAST_ONCE, qtEXACTLY_ONCE] then
          begin
            Packet.PacketID := PacketIDManager.GenerateID;
            FWaitingForAck.Add(Packet);
          end;
          Packet.WriteToBuffer(SendBuffer);
          Log.Send(mtDebug,'Sending PUBLISH (%d)',[Packet.PacketID]);
          SendData;
      finally
        if QOS = qtAT_MOST_ONCE then
          Packet.Free;
      end;
    end;
end;

procedure TMQTTClient.HandlePUBLISHPacket(APacket: TMQTTPUBLISHPacket);
begin
  Assert(Assigned(APacket) and (State = csConnected));
  if Assigned(APacket) and (State = csConnected) then
    begin
      Log.Send(mtDebug,'Received PUBLISH (PacketID=%d,QOS=%s)',[APacket.PacketID,GetQOSTypeName(APacket.QOS)]);
      case APacket.QOS of
        qtAT_MOST_ONCE  : ReceiveMessage(APacket.Topic,APacket.Data,APacket.QOS,APacket.Retain);
        qtAT_LEAST_ONCE : HandlePUBLISHPacket1(APacket);
        qtEXACTLY_ONCE  : HandlePUBLISHPacket2(APacket);
      end;
      {if APacket.QOS in [qtAT_LEAST_ONCE,qtEXACTLY_ONCE] then
        SendPendingMessages;}
    end;
end;

procedure TMQTTClient.HandlePUBLISHPacket1(APacket: TMQTTPUBLISHPacket);
var
  Reply: TMQTTPUBACKPacket;
begin
  Reply := TMQTTPUBACKPacket.Create;
  try
    Reply.PacketID := APacket.PacketID;
    Reply.WriteToBuffer(SendBuffer);
    Log.Send(mtDebug,'Sending PUBACK (%d)',[Reply.PacketID]);
    SendData;
    ReceiveMessage(APacket.Topic,APacket.Data,APacket.QOS,APacket.Retain);
  finally
    Reply.Free;
  end;
end;

procedure TMQTTClient.HandlePUBLISHPacket2(APacket: TMQTTPUBLISHPacket);
var
  Pkt    : TMQTTPUBLISHPacket;
  Reply  : TMQTTPUBRECPacket;
begin
  // If the duplicate flag is set, ensure we don't add the packet to the pending dispatch list twice
  if APacket.Duplicate then
    Pkt := FPendingReceive.Find(ptPublish,APacket.PacketID) as TMQTTPUBLISHPacket
  else
    Pkt := nil;
  if not Assigned(Pkt) then
    begin
      Pkt := APacket;
      FPendingReceive.Add(APacket);
    end;

  Reply := TMQTTPUBRECPacket.Create;
  Reply.PacketID := Pkt.PacketID;
  FWaitingForAck.Add(Reply);
  Reply.WriteToBuffer(SendBuffer);
  Log.Send(mtDebug,'Sending PUBREC (%d)',[Reply.PacketID]);
  SendData;
end;

procedure TMQTTClient.HandlePUBACKPacket(APacket: TMQTTPUBACKPacket);
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received PUBACK (%d)',[APacket.PacketID]);
      FPacketIDManager.ReleaseID(APacket.PacketID);
      FWaitingForAck.Remove(ptPUBLISH,APacket.PacketID);
    end;
end;

procedure TMQTTClient.HandlePUBRECPacket(APacket: TMQTTPUBRECPacket);
var
  Reply: TMQTTPUBRELPacket;
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received PUBREC (%d)',[APacket.PacketID]);
      FWaitingForAck.Remove(ptPublish,APacket.PacketID);
      Reply := TMQTTPUBRELPacket.Create;
      Reply.PacketID := APacket.PacketID;
      FWaitingForAck.Add(Reply);
      Reply.WriteToBuffer(SendBuffer);
      Log.Send(mtDebug,'Sending PUBREL (%d)',[Reply.PacketID]);
      SendData;
    end;
end;

procedure TMQTTClient.HandlePUBRELPacket(APacket: TMQTTPUBRELPacket);
var
  Pkt: TMQTTPUBLISHPacket;
  Reply: TMQTTPUBCOMPPacket;
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received PUBREL (%d)',[APacket.PacketID]);
      FWaitingForAck.Remove(ptPUBREC,APacket.PacketID);
      Pkt := FPendingReceive.Find(ptPUBLISH,APacket.PacketID) as TMQTTPUBLISHPacket;
      Assert(Assigned(Pkt));
      if Assigned(Pkt) then
        begin
          ReceiveMessage(Pkt.Topic,Pkt.Data,Pkt.QOS,Pkt.Retain);
          FPendingReceive.Remove(Pkt);
          Pkt.Free;
          Reply := TMQTTPUBCOMPPacket.Create;
          try
            Reply.PacketID := APacket.PacketID;
            Reply.WriteToBuffer(SendBuffer);
            Log.Send(mtDebug,'Sending PUBCOMP (%d)',[Reply.PacketID]);
            SendData;
          finally
            Reply.Free;
          end;
        end;
    end;
end;

procedure TMQTTClient.HandlePUBCOMPPacket(APacket: TMQTTPUBCOMPPacket);
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received PUBCOMP (PacketID='+IntToStr(APacket.PacketID)+')');
      FPacketIDManager.ReleaseID(APacket.PacketID);
      FWaitingForAck.Remove(ptPUBREL,APacket.PacketID);
    end;
end;

{ TMQTTClientSubscription }

constructor TMQTTClientSubscription.Create(ACollection: TCollection);
begin
  if Assigned(ACollection) then
    inherited Create(ACollection);
  FQOS := qtAT_MOST_ONCE;
end;

destructor TMQTTClientSubscription.Destroy;
begin
  if Assigned(FTokenizer) then
    FreeAndNil(FTokenizer);
  inherited Destroy;
end;

procedure TMQTTClientSubscription.Assign(Source: TPersistent);
begin
  if (Source is TMQTTClientSubscription) then
    begin
      SetFilter((Source as TMQTTClientSubscription).Filter);
      FQOS := (Source as TMQTTClientSubscription).QOS;
      FOnMessage := (Source as TMQTTClientSubscription).OnMessage;
    end
  else
    inherited Assign(Source);
end;

procedure TMQTTClientSubscription.SetFilter(AValue: UTF8String);
begin
  if Assigned(FTokenizer) then
    FreeAndNil(FTokenizer);
  FFilter := AValue;
  if FFilter > '' then
    FTokenizer := TMQTTTokenizer.Create(AValue,True);
end;

function TMQTTClientSubscription.GetDisplayName: String;
begin
  Result := FFilter;
  end;

procedure TMQTTClientSubscription.SetDisplayName(const Value: String);
begin
  FFilter := Value;
end;

function TMQTTClientSubscription.GetClient: TMQTTClient;
begin
  Result := (Collection as TMQTTClientSubscriptions).Client;
end;

{procedure TMQTTClientSubscription.HandleMessage(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retained: Boolean);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self,Topic,Data,QOS,Retained);
end; }

{ TMQTTClientSubscriptions }

constructor TMQTTClientSubscriptions.Create(AClient: TMQTTClient);
begin
  inherited Create(TMQTTClientSubscription);
  FClient := AClient;
  PropName := 'Subscriptions';
end;

function TMQTTClientSubscriptions.GetOwner: TPersistent;
begin
  Result := FClient;
end;

function TMQTTClientSubscriptions.Add: TMQTTClientSubscription;
begin
  Result := inherited Add as TMQTTClientSubscription;
end;

function TMQTTClientSubscriptions.Insert(Index: Integer): TMQTTClientSubscription;
begin
  Result := inherited Insert(Index) as TMQTTClientSubscription;
end;

function TMQTTClientSubscriptions.FindItemID(ID: Integer): TMQTTClientSubscription;
begin
  Result := inherited FindItemID(ID) as TMQTTClientSubscription;
end;

function TMQTTClientSubscriptions.GetItems(Index: Integer): TMQTTClientSubscription;
begin
  Result := TMQTTClientSubscription(inherited Items[Index]);
end;

procedure TMQTTClientSubscriptions.SetItems(Index: Integer; AValue: TMQTTClientSubscription);
begin
  Items[Index].Assign(AValue);
end;

end.


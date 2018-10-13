unit mqttclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {Contnrs, }Buffers, Logging, MQTTTokenizer,
  MQTTConsts, MQTTPackets, MQTTPacketDefs, MQTTSubscriptions;

const
  MQTT_DEFAULT_PING_INTERVAL         = 15; // Number of seconds between PINGREQ packets being sent to the server
  MQTT_DEFAULT_KEEPALIVE             = 30; // If the server has not been heard from in this number of seconds, assume the connection has been lost and disconnect.
  MQTT_CONNECT_TIMEOUT               = 2;  // Number of seconds until a connection attempts times out.
  MQTT_DEFAULT_RESEND_PACKET_TIMEOUT = 2;  // Default number of seconds until a packet is deemed lost and resent.
  MQTT_DEFAULT_MAX_RESEND_ATTEMPTS   = 3;  // Default maximum number of times to resend a packet before giving up.

type
  TMQTTClient = class;
  TMQTTClientSubscription = class;
  TMQTTClientSubscriptionList = class;
  TMQTTClientPublisher = class;
  TMQTTClientPublisherList = class;

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
      FPublishers          : TMQTTClientPublisherList;
      FSubscriptions       : TMQTTClientSubscriptionList;
      FSendBuffer          : TBuffer;
      FRecvBuffer          : TBuffer;
      FResendPacketTimeout : Word;
      FMaxResendAttempts   : Byte;
      // Packet Queues
      FWaitingForAck       : TMQTTPacketQueue; // QoS 1 and QoS 2 messages which have been sent to the Server, but have not been completely acknowledged.
      FPendingReceive      : TMQTTPacketQueue; // QoS 2 messages which have been received from the Server, but have not been completely acknowledged.       // Attributes
      // Properties
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
      // Events
      FOnInitSession       : TNotifyEvent;
      FOnConnected         : TNotifyEvent;
      FOnDisconnect        : TNotifyEvent;
      FOnDisconnected      : TNotifyEvent;
      FOnError             : TMQTTClientErrorEvent;
      FOnSendData          : TMQTTClientSendDataEvent;
      FOnReceiveMessage    : TMQTTClientReceiveMessageEvent;
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
      property Publishers: TMQTTClientPublisherList read FPublishers;
      property Subscriptions: TMQTTClientSubscriptionList read FSubscriptions;
      property SendBuffer: TBuffer read FSendBuffer;
      property RecvBuffer: TBuffer read FRecvBuffer;
      property Socket: TObject read FSocket write FSocket;
      property State: TMQTTConnectionState read FState;
    published
      property ResendPacketTimeout: Word read FResendPacketTimeout write FResendPacketTimeout default MQTT_DEFAULT_RESEND_PACKET_TIMEOUT;
      property MaxResendAttmpts: Byte read FMaxResendAttempts write FMaxResendAttempts default MQTT_DEFAULT_MAX_RESEND_ATTEMPTS;
      property ClientID: UTF8String read FClientID write SetClientID;
      property Username: UTF8String read FUsername write FUsername;
      property Password: AnsiString read FPassword write FPassword;
      property WillMessage: TMQTTWillMessage read FWillMessage write SetWillMessage;
      property CleanSession: Boolean read FCleanSession write FCleanSession default True;
      property KeepAlive: Word read FKeepAlive write SetKeepAlive default MQTT_DEFAULT_KEEPALIVE;
      property PingInterval: Word read FPingInterval write SetPingInterval default MQTT_DEFAULT_PING_INTERVAL;
      // Events
      property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
      property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
      property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
      property OnInitSession: TNotifyEvent read FOnInitSession write FOnInitSession;
      property OnError: TMQTTClientErrorEvent read FOnError write FOnError;
      property OnSendData: TMQTTClientSendDataEvent read FOnSendData write FOnSendData;
      property OnReceiveMessage: TMQTTClientReceiveMessageEvent read FOnReceiveMessage write FOnReceiveMessage;
  end;

  { TMQTTClientSubscription }

  TMQTTClientSubscription = class(TComponent)
    private
      FClient             : TMQTTClient;
      FTokens             : TMQTTTokenizer;
      FQOS                : TMQTTQOSType;
      FModified           : Boolean;
      FEnabled            : Boolean;
      FOnMessage          : TMQTTClientSubscriptionReceiveMessageEvent;
      FOnChanged          : TNotifyEvent;
      FOnSendSubscription : TNotifyEvent;
      function GetClient: TMQTTClient;
      procedure SetClient(AValue: TMQTTClient);
      function GetFilter: UTF8String;
      procedure SetFilter(AValue: UTF8String);
      procedure SetQOS(AValue: TMQTTQOSType);
    protected
      procedure HandleMessage(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retained: Boolean); virtual;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Changed; virtual;
      procedure SendSubscription; virtual;
      property Tokens: TMQTTTokenizer read FTokens;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure Clear;
      procedure Subscribe;
      procedure Unsubscribe;
      property Modified: Boolean read FModified write FModified;
    published
      property Enabled: Boolean read FEnabled write FEnabled default true;
      property Filter: UTF8String read GetFilter write SetFilter;
      property QOS: TMQTTQOSType read FQOS write SetQOS default qtAT_MOST_ONCE;
      property Client: TMQTTClient read GetClient write SetClient;
      //
      property OnMessage: TMQTTClientSubscriptionReceiveMessageEvent read FOnMessage write FOnMessage;
      property OnSendSubscription: TNotifyEvent read FOnSendSubscription write FOnSendSubscription;
      property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TMQTTClientSubscriptionList }

  TMQTTClientSubscriptionList = class(TObject)
    private
      FList: TList;
      function GetCount: Integer;
      function GetItem(Index: Integer): TMQTTClientSubscription;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(AItem: TMQTTClientSubscription);
      procedure Remove(AItem: TMQTTClientSubscription);
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TMQTTClientSubscription read GetItem; default;
  end;

  { TMQTTClientPublisher }

  TMQTTClientPublisher = class(TComponent)
    private
      FEnabled     : Boolean;
      FAutoPublish : Boolean;
      FClient      : TMQTTClient;
      FModified    : Boolean;
      FTokens      : TMQTTTokenizer;
      FData        : String;
      FQOS         : TMQTTQOSType;
      FRetain      : Boolean;
      FOnChanged   : TNotifyEvent;
      FOnPublish   : TNotifyEvent;
      function GetTopic: String;
      procedure SetClient(AValue: TMQTTClient);
      procedure SetData(AValue: String);
      procedure SetQOS(AValue: TMQTTQOSType);
      procedure SetRetained(AValue: Boolean);
      procedure SetTopic(AValue: String);
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Changed; virtual;
      property Tokens: TMQTTTokenizer read FTokens;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure Publish;
      procedure Clear;
      property Modified: Boolean read FModified write FModified;
    published
      property Enabled: Boolean read FEnabled write FEnabled default True;
      property AutoPublish: Boolean read FAutoPublish write FAutoPublish default False;
      property Client: TMQTTClient read FClient write SetClient;
      property Topic: String read GetTopic write SetTopic;
      property Data: String read FData write SetData;
      property QOS: TMQTTQOSType read FQOS write SetQOS default qtAT_LEAST_ONCE;
      property Retain: Boolean read FRetain write SetRetained default False;
      property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
      property OnPublish: TNotifyEvent read FOnPublish write FOnPublish;
  end;

  { TMQTTClientPublisherList }

  TMQTTClientPublisherList = class(TObject)
    private
      FList: TList;
      function GetCount: Integer;
      function GetItem(Index: Integer): TMQTTClientPublisher;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(AItem: TMQTTClientPublisher);
      procedure Remove(AItem: TMQTTClientPublisher);
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TMQTTClientPublisher read GetItem; default;
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
            if (FConnect > MQTT_CONNECT_TIMEOUT) then
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
  Log                     := TLogDispatcher.Create('Client');
  Log.Filter              := ALL_LOG_MESSAGE_TYPES;
  FResendPacketTimeout    := MQTT_DEFAULT_RESEND_PACKET_TIMEOUT;
  FMaxResendAttempts      := MQTT_DEFAULT_MAX_RESEND_ATTEMPTS;
  FSendBuffer             := TBuffer.Create;
  FRecvBuffer             := TBuffer.Create;
  FPacketIDManager        := TMQTTPacketIDManager.Create;
  FSubscriptions          := TMQTTClientSubscriptionList.Create;
  FPublishers             := TMQTTClientPublisherList.Create;
  FWaitingForAck          := TMQTTPacketQueue.Create;
  FPendingReceive         := TMQTTPacketQueue.Create;
  FWillMessage            := TMQTTWillMessage.Create;
  FKeepAlive              := MQTT_DEFAULT_KEEPALIVE;
  FPingInterval           := MQTT_DEFAULT_PING_INTERVAL;
  FPingIntRemaining       := FPingInterval;
  FCleanSession           := True;
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
  FPublishers.Free;
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
  Log.Send(mtInfo,GetMQTTLogMessage(LM_CLIENT_RESET));
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
                Log.Send(mtWarning,GetMQTTLogMessage(LM_RESENDING_PACKET),[Packet.PacketTypeName]);
                SendData;
                Packet.SecondsInQueue := 0;
                Packet.ResendCount := Packet.ResendCount + 1;
              end
            else
              begin
                FWaitingForAck.Delete(I);
                if Packet.PacketType in [ptSUBACK,ptUNSUBACK,ptPUBACK,ptPUBCOMP] then
                  PacketIDManager.ReleaseID(Packet.PacketID);
                Log.Send(mtWarning,GetMQTTLogMessage(LM_PACKET_UNACKNOWLEDGED_BY_SERVER),[Packet.PacketTypeName]);
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
        Log.Send(mtDebug,'Sending CONNECT (%s)',[Packet.AsString]);
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
      Log.Send(mtDebug,'Received CONNACK (%s)',[APacket.AsString]);
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
      Log.Send(mtInfo,GetMQTTLogMessage(LM_CLIENT_HAS_CONNECTED));
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
var
  X,N: Integer;
  S: TMQTTClientSubscription;
  L: TMQTTSubscriptionList;
begin
  FWaitingForAck.Clear;
  FPendingReceive.Clear;
  if Assigned(FOnInitSession) then
    FOnInitSession(Self);
  if Subscriptions.Count > 0 then
    begin
      L := TMQTTSubscriptionList.Create;
      try
        for X := 0 to Subscriptions.Count - 1 do
          begin
            S := Subscriptions[X];
            if Assigned(S) and S.Enabled then
              L.New(S.Filter,S.QOS);
          end;
        // Invalid subscriptions must be removed before duplicate subscriptions
        N := L.RemoveInvalidSubscriptions;
        if N > 0 then
          Log.Send(mtWarning,GetMQTTLogMessage(LM_INVALID_SUBSCRIPTION_REMOVED),[N]);
        N := L.RemoveDuplicates;
        if N > 0 then
          Log.Send(mtDebug,GetMQTTLogMessage(LM_DUPLICATE_SUBSCRIPTION_REMOVED),[N]);
        if not Subscribe(L) then
          Log.Send(mtError,GetMQTTLogMessage(LM_COULD_NOT_REGISTER_SUBSCRIPTIONS));
      finally
        L.Free;
      end;
    end;
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
        //Log.Send(mtDebug,'Sending PINGREQ');
        SendData;
      finally
        Packet.Free;
      end;
    end;
end;

procedure TMQTTClient.HandlePINGRESPPacket;
begin
  //Log.Send(mtDebug,'Received PINGRESP');
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
      Log.Send(mtDebug,'Sending SUBSCRIBE (%s)',[Packet.AsString]);
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
      Log.Send(mtDebug,'Received SUBACK (%s)',[APacket.AsString]);
      // Remove from Ack Queue
      for I := FWaitingForAck.Count - 1 downto 0 do
        begin
          Packet := FWaitingForAck[I];
          if (Packet.PacketType = ptSUBSCRIBE) and (Packet.PacketID = APacket.PacketID) then
            begin
              FWaitingForAck.Delete(I);
              ProcessReturnCodes((Packet as TMQTTSUBSCRIBEPacket).Subscriptions,APacket.ReturnCodes);
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
      Log.Send(mtDebug,'Sending UNSUBSCRIBE (%s)',[Packet.AsString]);
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
      Log.Send(mtDebug,'Received UNSUBACK (%s)',[APacket.AsString]);
      // Remove from Ack Queue
      for I := FWaitingForAck.Count - 1 downto 0 do
        begin
          Packet := FWaitingForAck[I];
          if (Packet.PacketType = ptUNSUBSCRIBE) and (Packet.PacketID = APacket.PacketID) then
            begin
              FWaitingForAck.Delete(I);
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
  if FSubscriptions.Count > 0 then
    begin
      Tokens := TMQTTTokenizer.Create(Topic,False);
      try
        for I := 0 to FSubscriptions.Count - 1 do
          begin
            Subscription := TMQTTClientSubscription(FSubscriptions[I]);
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
          Log.Send(mtDebug,'Sending PUBLISH (%s)',[Packet.AsString]);
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
      Log.Send(mtDebug,'Received PUBLISH (%s)',[APacket.AsString]);
      case APacket.QOS of
        qtAT_MOST_ONCE  : ReceiveMessage(APacket.Topic,APacket.Data,APacket.QOS,APacket.Retain);
        qtAT_LEAST_ONCE : HandlePUBLISHPacket1(APacket);
        qtEXACTLY_ONCE  : HandlePUBLISHPacket2(APacket);
      end;
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
    Log.Send(mtDebug,'Sending PUBACK (%s)',[Reply.AsString]);
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
  Log.Send(mtDebug,'Sending PUBREC (%s)',[Reply.AsString]);
  SendData;
end;

procedure TMQTTClient.HandlePUBACKPacket(APacket: TMQTTPUBACKPacket);
begin
  Assert(Assigned(APacket));
  if Assigned(APacket) then
    begin
      Log.Send(mtDebug,'Received PUBACK (%s)',[APacket.AsString]);
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
      Log.Send(mtDebug,'Received PUBREC (%s)',[APacket.AsString]);
      FWaitingForAck.Remove(ptPublish,APacket.PacketID);
      Reply := TMQTTPUBRELPacket.Create;
      Reply.PacketID := APacket.PacketID;
      FWaitingForAck.Add(Reply);
      Reply.WriteToBuffer(SendBuffer);
      Log.Send(mtDebug,'Sending PUBREL (%s)',[Reply.AsString]);
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
      Log.Send(mtDebug,'Received PUBREL (%s)',[APacket.AsString]);
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
            Log.Send(mtDebug,'Sending PUBCOMP (%s)',[Reply.AsString]);
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
      Log.Send(mtDebug,'Received PUBCOMP (%s)',[APacket.AsString]);
      FPacketIDManager.ReleaseID(APacket.PacketID);
      FWaitingForAck.Remove(ptPUBREL,APacket.PacketID);
    end;
end;

{ TMQTTClientSubscriptionList }

constructor TMQTTClientSubscriptionList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMQTTClientSubscriptionList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TMQTTClientSubscriptionList.GetItem(Index: Integer): TMQTTClientSubscription;
begin
  Result := TMQTTClientSubscription(FList[Index]);
end;

function TMQTTClientSubscriptionList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TMQTTClientSubscriptionList.Add(AItem: TMQTTClientSubscription);
begin
  // Ensure this is the only copy of this object in the list
  if FList.IndexOf(AItem) = -1 then
    FList.Add(AItem);
end;

procedure TMQTTClientSubscriptionList.Remove(AItem: TMQTTClientSubscription);
begin
  FList.Remove(AItem);
end;

{ TMQTTClientSubscription }

constructor TMQTTClientSubscription.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FQOS := qtAT_LEAST_ONCE;
end;

destructor TMQTTClientSubscription.Destroy;
begin
  Client := nil;
  if Assigned(FTokens) then
    FreeAndNil(FTokens);
  inherited Destroy;
end;

procedure TMQTTClientSubscription.Assign(Source: TPersistent);
begin
  if (Source is TMQTTClientSubscription) then
    begin
      Enabled            := (Source as TMQTTClientSubscription).Enabled;
      Client             := (Source as TMQTTClientSubscription).Client;
      Filter             := (Source as TMQTTClientSubscription).Filter;
      QOS                := (Source as TMQTTClientSubscription).QOS;
      OnMessage          := (Source as TMQTTClientSubscription).OnMessage;
      OnChanged          := (Source as TMQTTClientSubscription).OnChanged;
      OnSendSubscription := (Source as TMQTTClientSubscription).OnSendSubscription;
      Changed;
    end
  else
    inherited Assign(Source);
end;

procedure TMQTTClientSubscription.Clear;
begin
  if Assigned(FTokens) then
    FreeAndNil(FTokens);
  FEnabled := True;
  FQOS := qtAT_LEAST_ONCE;
  FModified := False;
end;

procedure TMQTTClientSubscription.Subscribe;
var
  L: TMQTTSubscriptionList;
begin
  if Assigned(FClient) then
    begin
      L := TMQTTSubscriptionList.Create;
      try
        L.New(Filter,QOS);
        FClient.Subscribe(L);
      finally
        L.Free;
      end;
    end;
end;

procedure TMQTTClientSubscription.Unsubscribe;
var
  L: TMQTTSubscriptionList;
begin
  if Assigned(FClient) then
    begin
      L := TMQTTSubscriptionList.Create;
      try
        L.New(Filter,QOS);
        FClient.Unsubscribe(L);
      finally
        L.Free;
      end;
    end;
end;

procedure TMQTTClientSubscription.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FClient) then
    begin
      Client := nil;
      Changed;
    end;
  inherited Notification(AComponent, Operation);
end;

procedure TMQTTClientSubscription.Changed;
begin
  FModified := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMQTTClientSubscription.SendSubscription;
begin
  if Enabled and Assigned(FOnSendSubscription) then
    FOnSendSubscription(Self);
end;

procedure TMQTTClientSubscription.HandleMessage(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retained: Boolean);
begin
  if Enabled and Assigned(FOnMessage) then
    FOnMessage(Self,Topic,Data,QOS,Retained);
end;

function TMQTTClientSubscription.GetFilter: UTF8String;
begin
  if Assigned(FTokens) then
    Result := FTokens.AsString
  else
    Result := '';
end;

procedure TMQTTClientSubscription.SetFilter(AValue: UTF8String);
begin
  if GetFilter = AValue then Exit;
  if Assigned(FTokens) then
    FreeAndNil(FTokens);
  if AValue > '' then
    FTokens := TMQTTTokenizer.Create(AValue,True);
  Changed;
end;

procedure TMQTTClientSubscription.SetQOS(AValue: TMQTTQOSType);
begin
  if FQOS=AValue then Exit;
  FQOS:=AValue;
  Changed;
end;

function TMQTTClientSubscription.GetClient: TMQTTClient;
begin
  Result := FClient as TMQTTClient;
end;

procedure TMQTTClientSubscription.SetClient(AValue: TMQTTClient);
begin
  if AValue = FClient then Exit;
  if Assigned(FClient) then
    begin
      FClient.Subscriptions.Remove(Self);
      FClient.RemoveFreeNotification(Self);
    end;
  FClient := AValue;
  if Assigned(FClient) then
    begin
      FClient.FSubscriptions.Add(Self);
      FClient.FreeNotification(Self);
    end;
  Changed;
end;

{ TMQTTClientPublisherList }

constructor TMQTTClientPublisherList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMQTTClientPublisherList.Destroy;
begin
  // This list should already have been cleared by the FreeNotification system
  FList.Free;
  inherited Destroy;
end;

function TMQTTClientPublisherList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMQTTClientPublisherList.GetItem(Index: Integer): TMQTTClientPublisher;
begin
  Result := TMQTTClientPublisher(FList[Index]);
end;

procedure TMQTTClientPublisherList.Add(AItem: TMQTTClientPublisher);
begin
  // Prevent duplicates
  if FList.IndexOf(AItem) = -1 then
    FList.Add(AItem);
end;

procedure TMQTTClientPublisherList.Remove(AItem: TMQTTClientPublisher);
begin
  FList.Remove(AItem);
end;

{ TMQTTClientPublisher }

constructor TMQTTClientPublisher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FQOS := qtAT_LEAST_ONCE;
end;

destructor TMQTTClientPublisher.Destroy;
begin
  Client := nil; // Remove self from client's FreeNotification and Publishers lists
  FreeAndNil(FTokens);
  inherited Destroy;
end;

procedure TMQTTClientPublisher.Assign(Source: TPersistent);
begin
  if Source is TMQTTClientPublisher then
    begin
      FAutoPublish := (Source as TMQTTClientPublisher).AutoPublish;
      FData := (Source as TMQTTClientPublisher).Data;
      FQOS := (Source as TMQTTClientPublisher).QOS;
      FRetain := (Source as TMQTTClientPublisher).Retain;
      Client := (Source as TMQTTClientPublisher).Client;
      Topic := (Source as TMQTTClientPublisher).Topic;
      Changed;
    end
  else
    inherited Assign(Source);
end;

procedure TMQTTClientPublisher.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FClient) and (Operation = opRemove) then
    FClient := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TMQTTClientPublisher.Publish;
begin
  try
    if Enabled and Assigned(FOnPublish) then
      FOnPublish(Self);
    if Enabled and Assigned(FClient) then
      begin
        if AutoPublish then
          FClient.Log.Send(mtDebug,'Topic %s automatically published due to change in data');
        FClient.Publish(Topic,Data,QOS,Retain);
      end;
  except
    on E: EAbort do begin
      // Suppress the exception
    end;
  end;
end;

procedure TMQTTClientPublisher.Clear;
begin
  FAutoPublish := False;
  if Assigned(FTokens) then
    FreeAndNil(FTokens);
  FData := '';
  FQOS := qtAT_LEAST_ONCE;
  FRetain := False;
  FModified := False;
end;

procedure TMQTTClientPublisher.SetClient(AValue: TMQTTClient);
begin
  if FClient = AValue then Exit;
  if Assigned(FClient) then
    begin
      FClient.Publishers.Remove(Self);
      FClient.RemoveFreeNotification(Self);
    end;
  FClient := AValue;
  if Assigned(FClient) then
    begin
      FClient.Publishers.Add(Self);
      FClient.FreeNotification(Self);
    end;
end;

procedure TMQTTClientPublisher.SetData(AValue: String);
begin
  if FData = AValue then Exit;
  FData := AValue;
  Changed;
  if AutoPublish then
    Publish;
end;

procedure TMQTTClientPublisher.Changed;
begin
  FModified := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMQTTClientPublisher.SetQOS(AValue: TMQTTQOSType);
begin
  if FQOS = AValue then Exit;
  FQOS := AValue;
  Changed;
end;

procedure TMQTTClientPublisher.SetRetained(AValue: Boolean);
begin
  if FRetain = AValue then Exit;
  FRetain := AValue;
  Changed;
end;

function TMQTTClientPublisher.GetTopic: String;
begin
  if Assigned(FTokens) then
    Result := FTokens.AsString
  else
    Result := '';
end;

procedure TMQTTClientPublisher.SetTopic(AValue: String);
begin
  if AValue = FTokens.AsString then Exit;
  FreeAndNil(FTokens);
  if AValue > '' then
    FTokens := TMQTTTokenizer.Create(AValue,False);
  Changed;
end;

end.


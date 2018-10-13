unit mqttpacketdefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Buffers, Logging, MQTTConsts, MQTTPackets, MQTTSubscriptions;

type

  { TMQTTWillMessage }

  TMQTTWillMessage = class(TPersistent)
    private
      FRetain: Boolean;
      FQOS: TMQTTQOSType;
      FEnabled: Boolean;
      FTopic: UTF8String;
      FMessage: UTF8String;
    public
      procedure Clear;
      procedure Assign(ASource: TPersistent); override;
      function DisplayText: String;
      function AsString: String;
      property Retain: Boolean read FRetain write FRetain;
      property QOS: TMQTTQOSType read FQOS write FQOS;
      property Enabled: Boolean read FEnabled write FEnabled;
      property Topic: UTF8String read FTopic write FTopic;
      property Message: UTF8String read FMessage write FMessage;
  end;

  { TMQTTCONNECTPacket }

  TMQTTCONNECTPacket = class(TMQTTPacket)
    private
      FReturnCode: Byte;
      FUsernameFlag: Boolean;
      FPasswordFlag: Boolean;
      FWillMessage: TMQTTWillMessage;
      FCleanSession: Boolean;
      FKeepAlive: Word;
      FClientID: UTF8String;
      FUsername: UTF8String;
      FPassword: AnsiString;
      function GetFlagsByte: Byte;
      function ParsePayload(ABuffer: TBuffer): Boolean;
      function ParseProtocolName(ABuffer: TBuffer): Boolean;
      function ParseVarHeader(ABuffer: TBuffer): Boolean;
      function SetConnectFlags(B: Byte): Boolean;
      procedure SetWillMessage(AValue: TMQTTWillMessage);
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      constructor Create;
      destructor Destroy; override;
      function AsString: String; override;
      procedure WriteToBuffer(ABuffer: TBuffer); override;
      property UsernameFlag: Boolean read FUsernameFlag write FUsernameFlag;
      property PasswordFlag: Boolean read FPasswordFlag write FPasswordFlag;
      property CleanSession: Boolean read FCleanSession write FCleanSession;
      property KeepAlive: Word read FKeepAlive write FKeepAlive;
      property ClientID: UTF8String read FClientID write FClientID;
      property Username: UTF8String read FUsername write FUsername;
      property Password: AnsiString read FPassword write FPassword;
      property WillMessage: TMQTTWillMessage read FWillMessage write SetWillMessage;
      property ReturnCode: Byte read FReturnCode;
  end;

  { TMQTTCONNACKPacket }

  TMQTTCONNACKPacket = class(TMQTTPacket)
    private
      FReturnCode: Byte;
      FSessionPresent: Boolean;
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      function AsString: String; override;
      procedure WriteToBuffer(ABuffer: TBuffer); override;
      property ReturnCode: Byte read FReturnCode write FReturnCode;
      property SessionPresent: Boolean read FSessionPresent write FSessionPresent;
  end;

  { TMQTTPUBLISHPacket }

  TMQTTPUBLISHPacket = class(TMQTTQueuedPacket)
    private
      FQOS: TMQTTQOSType;
      FDuplicate: Boolean;
      FRetain: Boolean;
      FTopic: UTF8String;
      FData: String;
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      destructor Destroy; override;
      function AsString: String; override;
      procedure WriteToBuffer(ABuffer: TBuffer); override;
      property Duplicate: Boolean read FDuplicate write FDuplicate;
      property Retain: Boolean read FRetain write FRetain;
      property Topic: UTF8String read FTopic write FTopic;
      property Data: String read FData write FData;
      property QOS: TMQTTQOSType read FQOS write FQOS;
  end;

  { TMQTTPUBACKPacket }

  TMQTTPUBACKPacket = class(TMQTTPacketIDPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

  { TMQTTPUBRECPacket }

  TMQTTPUBRECPacket = class(TMQTTQueuedPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

  { TMQTTPUBRELPacket }

  TMQTTPUBRELPacket = class(TMQTTQueuedPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

  { TMQTTPUBCOMPPacket }

  TMQTTPUBCOMPPacket = class(TMQTTPacketIDPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

  { TMQTTSUBSCRIBEPacket }

  TMQTTSUBSCRIBEPacket = class(TMQTTQueuedPacket)
    private
      FSubscriptions: TMQTTSubscriptionList;
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      constructor Create;
      destructor Destroy; override;
      function AsString: String; override;
      procedure WriteToBuffer(ABuffer: TBuffer); override;
      property Subscriptions: TMQTTSubscriptionList read FSubscriptions;
  end;

  { TMQTTSUBACKPacket }

  TMQTTSUBACKPacket = class(TMQTTPacketIDPacket)
    private
      FReturnCodes: TBuffer;
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      constructor Create;
      destructor Destroy; override;
      function AsString: String; override;
      procedure WriteToBuffer(ABuffer: TBuffer); override;
      property ReturnCodes: TBuffer read FReturnCodes;
  end;

  { TMQTTUNSUBSCRIBEPacket }

  TMQTTUNSUBSCRIBEPacket = class(TMQTTQueuedPacket)
    private
      FSubscriptions: TMQTTSubscriptionList;
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      constructor Create;
      destructor Destroy; override;
      function AsString: String; override;
      procedure WriteToBuffer(ABuffer: TBuffer); override;
      property Subscriptions: TMQTTSubscriptionList read FSubscriptions;
  end;

  { TMQTTUNSUBACKPacket }

  TMQTTUNSUBACKPacket = class(TMQTTPacketIDPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

  { TMQTTPINGREQPacket }

  TMQTTPINGREQPacket = class(TMQTTPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      //
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

  { TMQTTPINGRESPPacket }

  TMQTTPINGRESPPacket = class(TMQTTPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function Validate: Boolean; override;
      function GetPacketType: TMQTTPacketType; override;
    public
      //
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

  { TMQTTDISCONNECTPacket }

  TMQTTDISCONNECTPacket = class(TMQTTPacket)
    protected
      function ReadFromBuffer(ABuffer: TBuffer): Word; override;
      function GetPacketType: TMQTTPacketType; override;
      function Validate: Boolean; override;
    public
      //
      procedure WriteToBuffer(ABuffer: TBuffer); override;
  end;

implementation

uses
  LazUTF8;

{ TMQTTWillMessage }

procedure TMQTTWillMessage.Clear;
begin
  FQOS     := qtAT_MOST_ONCE;
  FRetain  := False;
  FEnabled := False;
  FTopic   := '';
  FMessage := '';
end;

procedure TMQTTWillMessage.Assign(ASource: TPersistent);
var
  WM: TMQTTWillMessage;
begin
  if ASource is TMQTTWillMessage then
    begin
      WM := ASource as TMQTTWillMessage;
      FEnabled := WM.FEnabled;
      FTopic   := WM.FTopic;
      FMessage := WM.FMessage;
      FQOS     := WM.FQOS;
      FRetain  := WM.FRetain;
    end
  else
    inherited Assign(ASource);
end;

function TMQTTWillMessage.DisplayText: String;
begin
  if FEnabled then
    Result := FTopic + '=' + FMessage
  else
    Result := '(Disabled)';
end;

function TMQTTWillMessage.AsString: String;
begin
  Result := 'Enabled: ' + BoolToStr(Enabled,true) + ', QOS: ' + GetQOSTypeName(QOS) +
    ', Retain: ' + BoolToStr(Retain,true) + ', Topic: ' + Topic + ', Message: ' + Message;
end;

{ TMQTTDISCONNECTPacket }

function TMQTTDISCONNECTPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptDisconnect;
end;

function TMQTTDISCONNECTPacket.Validate: Boolean;
begin
  Result:=inherited Validate and (RemainingLength = 0);
end;

function TMQTTDISCONNECTPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  Result := MQTT_ERROR_NONE;
end;

procedure TMQTTDISCONNECTPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := $E0;
  ABuffer.Write(@B,1);
  B := 0;
  ABuffer.Write(@B,1);
end;

{ TMQTTPINGRESPPacket }

function TMQTTPINGRESPPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  Result := MQTT_ERROR_NONE;
end;

function TMQTTPINGRESPPacket.Validate: Boolean;
begin
  Result := inherited Validate and (RemainingLength = 0);
end;

function TMQTTPINGRESPPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptPINGRESP;
end;

procedure TMQTTPINGRESPPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := 13 shl 4;
  ABuffer.Write(@B,1);
  B := 0;
  ABuffer.Write(@B,1);
end;

{ TMQTTPINGREQPacket }

function TMQTTPINGREQPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  Result := MQTT_ERROR_NONE;
end;

function TMQTTPINGREQPacket.Validate: Boolean;
begin
  Result := inherited Validate and (RemainingLength = 0);
end;

function TMQTTPINGREQPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptPINGREQ;
end;

procedure TMQTTPINGREQPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := 12 shl 4;
  ABuffer.Write(@B,1);
  B := 0;
  ABuffer.Write(@B,1);
end;

{ TMQTTUNSUBACKPacket }

function TMQTTUNSUBACKPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
var
  W: Word;
begin
  if ReadWordFromBuffer(ABuffer,W) then
    begin
      Result   := MQTT_ERROR_NONE;
      PacketID := W;
    end
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

function TMQTTUNSUBACKPacket.Validate: Boolean;
begin
  Result := inherited Validate and (RemainingLength = 2);
end;

function TMQTTUNSUBACKPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptUNSUBACK;
end;

procedure TMQTTUNSUBACKPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := $B0;
  ABuffer.Write(@B,1);
  WriteRemainingLengthToBuffer(ABuffer,2);
  WriteWordToBuffer(ABuffer,PacketID);
end;


{ TMQTTUNSUBSCRIBEPacket }

function TMQTTUNSUBSCRIBEPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
var
  Str: UTF8String;
  Len: DWORD;
  Sub: TMQTTSubscription;
  W: Word;
begin
  Result := MQTT_ERROR_NONE;

  if Assigned(FSubscriptions) then
    FSubscriptions.Clear
  else
    FSubscriptions := TMQTTSubscriptionList.Create;

  if ReadWordFromBuffer(ABuffer,W) then
    begin
      PacketID := W;
      Len := RemainingLength - 2;
      while Len >= 2 do
        if ReadUTF8StringFromBuffer(ABuffer,Str) then
          begin
            Sub := TMQTTSubscription.Create(Str);
            Subscriptions.Update(Sub);
            Len := Len - 2 - Length(Str);
          end
        else
          begin
            Result := MQTT_ERROR_PAYLOAD_INVALID;
            Exit;
          end;
    end
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

function TMQTTUNSUBSCRIBEPacket.Validate: Boolean;
begin
  Result := inherited Validate and Assigned(Subscriptions) and (Subscriptions.Count > 0);
end;

function TMQTTUNSUBSCRIBEPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptUNSUBSCRIBE;
end;

constructor TMQTTUNSUBSCRIBEPacket.Create;
begin
  inherited Create;
  FSubscriptions := TMQTTSubscriptionList.Create;
end;

destructor TMQTTUNSUBSCRIBEPacket.Destroy;
begin
  FSubscriptions.Free;
  inherited Destroy;
end;

function TMQTTUNSUBSCRIBEPacket.AsString: String;
var
  I: Integer;
  S: TMQTTSubscription;
begin
  Result := inherited AsString;
  for I := 0 to FSubscriptions.Count - 1 do
    begin
      S := FSubscriptions[I];
      Result := Result + ', Subscription[' + IntToStr(I) + ']: (Filter: ' + S.Filter +
        ', QOS: ' + GetQOSTypeName(S.QOS) + ', Age: ' + IntToStr(S.Age)+')';
    end;
end;

procedure TMQTTUNSUBSCRIBEPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
  I: Integer;
  S: TMQTTSubscription;
  LBuffer: TBuffer;
begin
  Assert(Assigned(Subscriptions));
  B := $A2;
  ABuffer.Write(@B,1);
  LBuffer := TBuffer.Create;
  try
    for I := 0 to Subscriptions.Count - 1 do
      begin
        S := Subscriptions[I];
        WriteUTF8StringToBuffer(LBuffer,S.Filter);
      end;
    WriteRemainingLengthToBuffer(ABuffer,LBuffer.Size+2);
    WriteWordToBuffer(ABuffer,PacketID);
    ABuffer.WriteBuffer(LBuffer);
  finally
    LBuffer.Free;
  end;
end;


{ TMQTTSUBACKPacket }

constructor TMQTTSUBACKPacket.Create;
begin
  inherited Create;
  FReturnCodes := TBuffer.Create;
end;

destructor TMQTTSUBACKPacket.Destroy;
begin
  FReturnCodes.Free;
  inherited Destroy;
end;

function TMQTTSUBACKPacket.AsString: String;
var
  I: Integer;
  RC: Byte;
  Buf: TBuffer;
begin
  Result := inherited AsString;
  if ReturnCodes.Size = 0 then Exit;
  Buf := TBuffer.Create;
  try
    Buf.CopyFrom(ReturnCodes);
    for I := 1 to Buf.Size do
      begin
        Buf.Read(@RC,1);
        if RC = $80 then
          Result := Result + ', ReturnCode[' + IntToStr(I) + ']=Error'
        else
          Result := Result + ', ReturnCode[' + IntToStr(I) + ']=' + GetQOSTypeName(TMQTTQOSType(RC));
      end;
  finally
    Buf.Free;
  end;
end;

function TMQTTSUBACKPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
var
  Data: Pointer;
  Len: Integer;
  W: Word;
begin
  Result := MQTT_ERROR_NONE;
  if ReadWordFromBuffer(ABuffer,W) then
    begin
      PacketID := W;
      Len := RemainingLength - 2;
      if Len > 0 then
        begin
          GetMem(Data,Len);
          try
            ABuffer.Read(Data,Len);
            FReturnCodes.Write(Data,Len);
          finally
            FreeMem(Data);
          end;
        end
      else
        Result := MQTT_ERROR_PAYLOAD_INVALID;
    end
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

function TMQTTSUBACKPacket.Validate: Boolean;
begin
  Result := inherited Validate and (RemainingLength > 2) and (ReturnCodes.Size > 0);
end;

function TMQTTSUBACKPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptSUBACK;
end;

procedure TMQTTSUBACKPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := $90;
  ABuffer.Write(@B,1);
  WriteRemainingLengthToBuffer(ABuffer,2+FReturnCodes.Size);
  WriteWordToBuffer(ABuffer,PacketID);
  ABuffer.WriteBuffer(FReturnCodes);
end;

{ TMQTTSUBSCRIBEPacket }

constructor TMQTTSUBSCRIBEPacket.Create;
begin
  inherited Create;
  FSubscriptions := TMQTTSubscriptionList.Create;
end;

destructor TMQTTSUBSCRIBEPacket.Destroy;
begin
  FSubscriptions.Free;
  inherited Destroy;
end;

function TMQTTSUBSCRIBEPacket.AsString: String;
var
  I: Integer;
  S: TMQTTSubscription;
begin
  Result := inherited AsString;
  for I := 0 to FSubscriptions.Count - 1 do
    begin
      S := FSubscriptions[I];
      Result := Result + ', Subscription[' + IntToStr(I) + ']: (Filter: ' + S.Filter +
        ', QOS: ' + GetQOSTypeName(S.QOS) + ', Age: ' + IntToStr(S.Age)+')'
    end;
end;

function TMQTTSUBSCRIBEPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
var
  Str: UTF8String;
  Len: DWORD;
  QOS: TMQTTQOSType;
  Sub: TMQTTSubscription;
  W: Word;
  B: Byte;
begin
  Result := MQTT_ERROR_NONE;

  if Assigned(FSubscriptions) then
    FSubscriptions.Clear
  else
    FSubscriptions := TMQTTSubscriptionList.Create;

  if ReadWordFromBuffer(ABuffer,W) then
    begin
      PacketID := W;
      Len := RemainingLength - 2;
      while Len >= 2 do
        if ReadUTF8StringFromBuffer(ABuffer,Str) then
          begin
            ABuffer.Read(@B,1);
            QOS := TMQTTQOSType(B);
            Sub := TMQTTSubscription.Create(Str,QOS);
            Subscriptions.Update(Sub);
            Len := Len - 3 - Length(Str);
          end
        else
          begin
            Result := MQTT_ERROR_PAYLOAD_INVALID;
            Exit;
          end;
    end
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

function TMQTTSUBSCRIBEPacket.Validate: Boolean;
begin
  Result := inherited Validate and Assigned(Subscriptions) and (Subscriptions.Count > 0);
end;

function TMQTTSUBSCRIBEPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptSUBSCRIBE;
end;

procedure TMQTTSUBSCRIBEPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
  I: Integer;
  S: TMQTTSubscription;
  LBuffer: TBuffer;
begin
  Assert(Assigned(Subscriptions) and (Subscriptions.Count > 0));
  B := $82;
  ABuffer.Write(@B,1);
  LBuffer := TBuffer.Create;
  try
    for I := 0 to Subscriptions.Count - 1 do
      begin
        S := Subscriptions[I];
        WriteUTF8StringToBuffer(LBuffer,S.Filter);
        B := ord(S.QOS);
        LBuffer.Write(@B,1);
      end;
    if LBuffer.Size = 0 then
      raise Exception.Create(GetMQTTErrorMessage(MQTT_ERROR_NOSUBSCRIPTIONSINSUBSCRIBE));
    WriteRemainingLengthToBuffer(ABuffer,LBuffer.Size+2);
    WriteWordToBuffer(ABuffer,PacketID);
    ABuffer.WriteBuffer(LBuffer);
  finally
    LBuffer.Free;
  end;
end;

{ TMQTTPUBCOMPPacket }

function TMQTTPUBCOMPPacket.Validate: Boolean;
begin
  Result := inherited Validate and (PacketID > 0);
end;

function TMQTTPUBCOMPPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptPUBCOMP;
end;

function TMQTTPUBCOMPPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  if ReadWordFromBuffer(ABuffer,FPacketID) then
    Result := MQTT_ERROR_NONE
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

procedure TMQTTPUBCOMPPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := $70;
  ABuffer.Write(@B,1);
  B := 2;
  ABuffer.Write(@B,1);
  WriteWordToBuffer(ABuffer,PacketID);
end;

{ TMQTTPUBRELPacket }

function TMQTTPUBRELPacket.Validate: Boolean;
begin
  Result := inherited Validate and (PacketID > 0);
end;

function TMQTTPUBRELPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptPUBREL;
end;

function TMQTTPUBRELPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  if ReadWordFromBuffer(ABuffer,FPacketID) then
    Result := MQTT_ERROR_NONE
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

procedure TMQTTPUBRELPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := $62;
  ABuffer.Write(@B,1);
  B := 2;
  ABuffer.Write(@B,1);
  WriteWordToBuffer(ABuffer,PacketID);
end;

{ TMQTTPUBRECPacket }

function TMQTTPUBRECPacket.Validate: Boolean;
begin
  Result := inherited Validate and (PacketID > 0);
end;

function TMQTTPUBRECPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptPUBREC;
end;

function TMQTTPUBRECPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  if ReadWordFromBuffer(ABuffer,FPacketID) then
    Result := MQTT_ERROR_NONE
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

procedure TMQTTPUBRECPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := $50;
  ABuffer.Write(@B,1);
  B := 2;
  ABuffer.Write(@B,1);
  WriteWordToBuffer(ABuffer,PacketID);
end;

{ TMQTTPUBACKPacket }

function TMQTTPUBACKPacket.Validate: Boolean;
begin
  Result := inherited Validate and (PacketID > 0);
end;

function TMQTTPUBACKPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptPUBACK;
end;

function TMQTTPUBACKPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  if ReadWordFromBuffer(ABuffer,FPacketID) then
    Result := MQTT_ERROR_NONE
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

procedure TMQTTPUBACKPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := $40;
  ABuffer.Write(@B,1);
  B := 2;
  ABuffer.Write(@B,1);
  WriteWordToBuffer(ABuffer,PacketID);
end;

{ TMQTTCONNACKPacket }

function TMQTTCONNACKPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
var
  B: Byte;
begin
  ReturnCode := MQTT_CONNACK_SUCCESS;    // Default return code is success
  ABuffer.Read(@B,1);
  SessionPresent := (B = 1);
  if ((B and $FE) = 0) and Validate then
    Result := MQTT_ERROR_NONE
  else
    Result := MQTT_ERROR_PACKET_INVALID;
  ABuffer.Read(@B,1);
  ReturnCode := B;
end;

function TMQTTCONNACKPacket.Validate: Boolean;
begin
  Result := inherited Validate;
  if (ReturnCode > 0) and (SessionPresent) then
    Result := False;
  if (RemainingLength <> 2) then
    Result := False;
end;

function TMQTTCONNACKPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptCONNACK;
end;

function TMQTTCONNACKPacket.AsString: String;
begin
  Result := inherited AsString +
    ', ReturnCode: ' + IntToStr(ReturnCode) +
    ', SessionPresent: ' + BoolToStr(SessionPresent,true);
end;

procedure TMQTTCONNACKPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
begin
  B := 32;
  ABuffer.Write(@B,1);
  B := 2;
  ABuffer.Write(@B,1);
  if (SessionPresent) then
    B := 1
  else
    B := 0;
  ABuffer.Write(@B,1);
  B := ReturnCode;
  ABuffer.Write(@B,1);
end;

{ TMQTTCONNECTPacket }

constructor TMQTTCONNECTPacket.Create;
begin
  inherited Create;
  FWillMessage := TMQTTWillMessage.Create;
  FKeepAlive := 30;
end;

destructor TMQTTCONNECTPacket.Destroy;
begin
  FWillMessage.Free;
  inherited Destroy;
end;

function TMQTTCONNECTPacket.AsString: String;
begin
  Result := inherited AsString + ', ClientID: ' + FClientID + ', Username: ' + FUsername +
    ', UsernameFlag: ' + BoolToStr(UsernameFlag,true) + ', PasswordFlag: ' + BoolToStr(PasswordFlag,true) +
    ', KeepAlive: ' + IntToStr(KeepAlive) + ', CleanSession: ' + BoolToStr(CleanSession,true) +
    ', WillMessage: (' + WillMessage.AsString + '), ReturnCode: ' + IntToStr(ReturnCode);
end;

function TMQTTCONNECTPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptCONNECT;
end;

function TMQTTCONNECTPacket.ParseProtocolName(ABuffer: TBuffer): Boolean;
var
  Str: UTF8String;
begin
  Result := False;
  if ReadUTF8StringFromBuffer(ABuffer,Str) then
    Result := UTF8CompareStr('MQTT') = 0;
  if not Result then
    FReturnCode := MQTT_CONNACK_UNACCEPTABLE_PROTOCOL;
end;
{var
  B: array [1..6] of Char;
begin
  if ABuffer.Read(@B,6) = 6 then
    begin
      Result := (B[1] = #0) and (B[2] = #4) and (B[3] = 'M') and (B[4] = 'Q')
                and (B[5] = 'T') and (B[6] = 'T');
    end
  else
    Result := False;}

function TMQTTCONNECTPacket.SetConnectFlags(B: Byte): Boolean;
begin
  FUsernameFlag        := (B and 128) > 0;
  FPasswordFlag        := (B and 64) > 0;
  FWillMessage.Retain  := (B and 32) > 0;
  FWillMessage.QOS     := TMQTTQOSType((B shr 3) and 3);
  FWillMessage.Enabled := (B and 4) > 0;
  FCleanSession        := (B and 2) > 0;
  Result               := (B and 1) = 0;
end;

procedure TMQTTCONNECTPacket.SetWillMessage(AValue: TMQTTWillMessage);
begin
  FWillMessage.Assign(AValue);
end;

function TMQTTCONNECTPacket.ParseVarHeader(ABuffer: TBuffer): Boolean;
var
  ProtocolLevel, ConnectFlags, B: Byte;
begin
  Result := False;
  ParseProtocolName(ABuffer); // Ignore the return value.  Its stored in ReturnCode instead.
  ABuffer.Read(@ProtocolLevel,1);
  if ProtocolLevel = $04 then
    begin
      ABuffer.Read(@ConnectFlags,1);
      if SetConnectFlags(ConnectFlags) then
        begin
          ABuffer.Read(@B,1);
          FKeepAlive := B * 256;
          ABuffer.Read(@B,1);
          FKeepAlive := FKeepAlive + B;
          Result := True;
        end
      else
        { The Server MUST validate that the reserved flag in the CONNECT Control
        Packet is set to zero and disconnect the Client if it is not zero [MQTT-3.1.2-3] }
        Bail(MQTT_ERROR_INVALID_PACKET_FLAGS);
    end
  else
    FReturnCode := MQTT_ERROR_UNACCEPTABLE_PROTOCOL;
end;

function TMQTTCONNECTPacket.ParsePayload(ABuffer: TBuffer): Boolean;
begin
  Result := False;
  if not ReadUTF8StringFromBuffer(ABuffer,FClientID) then Exit;
  if WillMessage.Enabled then
    begin
      if not ReadUTF8StringFromBuffer(ABuffer,WillMessage.FTopic) then Exit;
      if not ReadUTF8StringFromBuffer(ABuffer,WillMessage.FMessage) then Exit;
    end;
  // If the username and password are malformed then a CONNACK needs to be sent
  // with the appropriate error code.  Therefore continue parsing the packet.
  Result := True;
  if (FUsernameFlag and (not FPasswordFlag)) or (FPasswordFlag and (not FUsernameFlag)) then
    FReturnCode := MQTT_CONNACK_BAD_USERNAME_PASSWORD;
  if FUsernameFlag and (not ReadUTF8StringFromBuffer(ABuffer,FUsername)) then
    FReturnCode := MQTT_CONNACK_BAD_USERNAME_PASSWORD;
  if FPasswordFlag and (not ReadBinaryDataFromBuffer(ABuffer,FPassword)) then
    FReturnCode := MQTT_CONNACK_BAD_USERNAME_PASSWORD;
end;

function TMQTTCONNECTPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
begin
  FReturnCode := MQTT_CONNACK_SUCCESS;    // Default return code is success
  if ParseVarHeader(ABuffer) then
    if ParsePayload(ABuffer) then
      if Validate then
        Result := MQTT_ERROR_NONE
      else
        Result := MQTT_ERROR_PACKET_INVALID
    else
      Result := MQTT_ERROR_PAYLOAD_INVALID
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

function TMQTTCONNECTPacket.GetFlagsByte: Byte;
begin
  if UsernameFlag then
    Result := 128
  else
    Result := 0;
  if PasswordFlag then
    Result := Result or 64;
  if WillMessage.Retain then
    Result := Result or 32;
  Result := Result or (ord(WillMessage.QOS) shl 3);
  if WillMessage.Enabled then
    Result := Result or 4;
  if CleanSession then
    Result := Result or 2;
end;

procedure TMQTTCONNECTPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
  LBuffer: TBuffer;
  PROTOCOL_NAME: array[1..7] of byte = (0,4,ord('M'),ord('Q'),ord('T'),ord('T'),4);
begin
  B := $10;
  ABuffer.Write(@B,SizeOf(B));
  LBuffer := TBuffer.Create;
  try
    // Variable Header
    LBuffer.Write(@PROTOCOL_NAME,7);
    B := GetFlagsByte;
    LBuffer.Write(@B,1);
    B := KeepAlive shr 8;
    LBuffer.Write(@B,1);
    B := KeepAlive and 255;
    LBuffer.Write(@B,1);
    // Paload
    WriteUTF8StringToBuffer(LBuffer,FClientID);
    if WillMessage.Enabled then
      begin
        WriteUTF8StringToBuffer(LBuffer,WillMessage.Topic);
        WriteUTF8StringToBuffer(LBuffer,WillMessage.Message);
      end;
    if UsernameFlag then
      WriteUTF8StringToBuffer(LBuffer,Username);
    if PasswordFlag then
      WriteBinaryDataToBuffer(LBuffer,Password);
    WriteRemainingLengthToBuffer(ABuffer,LBuffer.Size);
    ABuffer.WriteBuffer(LBuffer);
  finally
    LBuffer.Free;
  end;
end;

{ TMQTTPUBLISHPacket }

function TMQTTPUBLISHPacket.Validate: Boolean;
begin
  Result := inherited Validate and ((QOS = qtAT_MOST_ONCE) or (PacketID > 0));
end;

function TMQTTPUBLISHPacket.GetPacketType: TMQTTPacketType;
begin
  Result := ptPUBLISH;
end;

destructor TMQTTPUBLISHPacket.Destroy;
begin
  inherited Destroy;
end;

function TMQTTPUBLISHPacket.AsString: String;
begin
  Result := inherited AsString + ', QOS: ' + GetQOSTypeName(FQOS) +
    ', Retain: ' + BoolToStr(FRetain,true) +
    ', Duplicate: ' + BoolToStr(FDuplicate,true) +
    ', Topic: ' + FTopic +
    ', Data: ' + FData;
end;

function TMQTTPUBLISHPacket.ReadFromBuffer(ABuffer: TBuffer): Word;
var
  Success: Boolean;
  Len: Integer;
begin
  Result := MQTT_ERROR_NONE;
  Len := ABuffer.Size;
  ReadUTF8StringFromBuffer(ABuffer,FTopic);
  if QOS in [qtAT_LEAST_ONCE, qtEXACTLY_ONCE] then
    Success := ReadWordFromBuffer(ABuffer,FPacketID)
  else
    Success := True;
  if Success then
    begin
      Len := Len - ABuffer.Size;
      Len := RemainingLength - Len;
      SetLength(FData,Len);
      if (Len > 0) then
        ABuffer.Read(PChar(FData),Len);
    end
  else
    Result := MQTT_ERROR_VARHEADER_INVALID;
end;

procedure TMQTTPUBLISHPacket.WriteToBuffer(ABuffer: TBuffer);
var
  B: Byte;
  LBuffer: TBuffer;
begin
  B := $30;
  if Duplicate then
    B := B or 8;
  B := B or (ord(QOS) shl 1);
  if Retain then
    B := B or 1;
  ABuffer.Write(@B,1);
  LBuffer := TBuffer.Create;
  try
    WriteUTF8StringToBuffer(LBuffer,Topic);
    if QOS in [qtAT_LEAST_ONCE, qtEXACTLY_ONCE] then
      WriteWordToBuffer(LBuffer,PacketID);
    LBuffer.Write(PChar(Data),Length(Data));
    WriteRemainingLengthToBuffer(ABuffer,LBuffer.Size);
    ABuffer.WriteBuffer(LBuffer);
  finally
    LBuffer.Free;
  end;
end;

end.

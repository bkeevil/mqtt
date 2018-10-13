unit mqttmessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MQTTConsts, MQTTTokenizer;

type
  TMQTTMessageList = class;

  { TMQTTMessage }

  TMQTTMessage = class(TObject)
    private
      FClientID : UTF8String;
      FTokens   : TMQTTTokenizer;
      FTopic    : UTF8String;
      FData     : String;
      FQOS      : TMQTTQOSType;
      FRetain   : Boolean;
      function GetTokens: TMQTTTokenizer;
      procedure SetTopic(AValue: UTF8String);
    public
      constructor Create;
      destructor Destroy; override;
      //
      function Clone: TMQTTMessage;
      procedure LoadFromStream(Stream: TStream);
      procedure SaveToStream(Stream: TStream);
      //
      property ClientID: UTF8String read FClientID write FClientID;
      property Tokens: TMQTTTokenizer read GetTokens;
      property Topic: UTF8String read FTopic write SetTopic;
      property Data: String read FData write FData;
      property QOS: TMQTTQOSType read FQOS write FQOS;
      property Retain: Boolean read FRetain write FRetain;
  end;

  { TMQTTMessageList }

  TMQTTMessageList = class(TObject)
    private
      FList: TList;
      function GetCount: Integer;
      function GetItem(Index: Integer): TMQTTMessage;
    public
      constructor Create;
      destructor Destroy; override;
      //
      procedure Clear;
      function New(ATopic: UTF8String): TMQTTMessage;
      procedure Add(AMessage: TMQTTMessage);
      procedure Update(AMessage: TMQTTMessage);
      //function Find(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean): TMQTTMessage;
      procedure Remove(AMessage: TMQTTMessage);
      procedure Delete(Index: Integer);
      procedure DeleteByTopic(Topic: UTF8String);
      procedure LoadFromStream(Stream: TStream);
      procedure SaveToStream(Stream: TStream);
      //
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TMQTTMessage read GetItem; default;
  end;

implementation

uses
  StreamUtils;

{ TMQTTMessageList }

constructor TMQTTMessageList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMQTTMessageList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TMQTTMessageList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMQTTMessageList.GetItem(Index: Integer): TMQTTMessage;
begin
  Result := TMQTTMessage(FList[Index]);
end;

procedure TMQTTMessageList.Clear;
var
  X: Integer;
begin
  for X := Count - 1 downto 0 do
    Items[X].Destroy;
  FList.Clear;
end;

function TMQTTMessageList.New(ATopic: UTF8String): TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FTopic := ATopic;
  Add(Result);
end;

procedure TMQTTMessageList.Add(AMessage: TMQTTMessage);
begin
  FList.Add(AMessage);
end;

procedure TMQTTMessageList.Update(AMessage: TMQTTMessage);
var
  X: Integer;
  O: TMQTTMessage;
begin
  for X := 0 to Count - 1 do
    begin
      O := Items[X];
      if (O.Topic = AMessage.Topic) then
        begin
          O.Free;
          FList.Items[X] := AMessage;
          Exit;
        end;
    end;
  FList.Add(AMessage);
end;

{function TMQTTMessageList.Find(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean): TMQTTMessage;
var
  X: Integer;
begin
  for X := 0 to Count - 1 do
    begin
      Result := Items[X];
      if (Result.Topic = Topic) and (Result.Data = Data) and (Result.QOS = QOS) and (Result.Retain = Retain) then
        Exit;
    end;
  Result := nil;
end;}

procedure TMQTTMessageList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TMQTTMessageList.DeleteByTopic(Topic: UTF8String);
var
  X: Integer;
  Msg: TMQTTMessage;
begin
  for X := Count - 1 downto 0 do
    begin
      Msg := Items[X];
      if (Msg.Topic = Topic) then
        begin
          Msg.Free;
          FList.Delete(X);
        end;
    end;
end;

procedure TMQTTMessageList.LoadFromStream(Stream: TStream);
var
  I,C: Integer;
  M: TMQTTMessage;
begin
  if Assigned(Stream) then
    begin
      Clear;
      Stream.Read(C,SizeOf(C));
      for I := 0 to C - 1 do
        begin
          M := TMQTTMessage.Create;
          M.LoadFromStream(Stream);
          FList.Add(M);
        end;
    end;
end;

procedure TMQTTMessageList.SaveToStream(Stream: TStream);
var
  I,C: Integer;
  M: TMQTTMessage;
begin
  if Assigned(Stream) then
    begin
      C := Count;
      Stream.Write(C,SizeOf(C));
      for I := 0 to C - 1 do
        begin
          M := Items[I];
          M.SaveToStream(Stream);
        end;
    end;
end;

procedure TMQTTMessageList.Remove(AMessage: TMQTTMessage);
begin
  FList.Remove(AMessage);
end;

{ TMQTTMessage }

constructor TMQTTMessage.Create;
begin
  inherited Create;
  FQOS   := qtAT_LEAST_ONCE;
end;

destructor TMQTTMessage.Destroy;
begin
  if Assigned(FTokens) then
    FTokens.Free;
  inherited Destroy;
end;

function TMQTTMessage.Clone: TMQTTMessage;
begin
  Result := TMQTTMessage.Create;
  Result.FTopic := FTopic;
  Result.FData := FData;
  Result.FQOS := FQOS;
  Result.FRetain := FRetain;
end;

procedure TMQTTMessage.LoadFromStream(Stream: TStream);
begin
  if Assigned(Stream) then
    begin
      if Assigned(FTokens) then
        FreeAndNil(FTokens);
      FClientID := LoadStringFromStreamWord(Stream);
      FTopic := LoadStringFromStreamWord(Stream);
      FData := LoadStringFromStreamWord(Stream);
      Stream.Read(FQOS,SizeOf(FQOS));
      Stream.Read(FRetain,SizeOf(FRetain));
    end;
end;

procedure TMQTTMessage.SaveToStream(Stream: TStream);
begin
  if Assigned(Stream) then
    begin
      SaveStringToStreamWord(FClientID,Stream);
      SaveStringToStreamWord(FTopic,Stream);
      SaveStringToStreamWord(FData,Stream);
      Stream.Write(FQOS,SizeOf(FQOS));
      Stream.Write(FRetain,SizeOf(FRetain));
    end;
end;

function TMQTTMessage.GetTokens: TMQTTTokenizer;
begin
  if not Assigned(FTokens) then
    FTokens := TMQTTTokenizer.Create(FTopic,False);
  Result := FTokens;
end;

procedure TMQTTMessage.SetTopic(AValue: UTF8String);
begin
  if AValue = FTopic then Exit;
  FTopic := AValue;
  if Assigned(FTokens) then
    FreeAndNil(FTokens);
end;

end.


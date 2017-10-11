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
      function Find(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean): TMQTTMessage;
      procedure Remove(AMessage: TMQTTMessage);
      procedure Delete(Index: Integer);
      //
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TMQTTMessage read GetItem; default;
  end;

implementation

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

function TMQTTMessageList.Find(Topic: UTF8String; Data: String; QOS: TMQTTQOSType; Retain: Boolean): TMQTTMessage;
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
end;

procedure TMQTTMessageList.Delete(Index: Integer);
begin
  FList.Delete(Index);
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


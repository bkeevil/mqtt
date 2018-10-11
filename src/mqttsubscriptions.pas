unit mqttsubscriptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MQTTConsts, MQTTTokenizer;

type

  { TMQTTSubscription }

  TMQTTSubscription = class(TObject)
    private
      FFilter     : UTF8String;
      FQOS        : TMQTTQOSType;
      FTokens     : TMQTTTokenizer;
      FAge        : Integer;
      function GetTokens: TMQTTTokenizer;
      procedure SetFilter(AValue: UTF8String);
    public
      constructor Create(AFilter: UTF8String = ''; AQOS: TMQTTQOSType = qtAT_MOST_ONCE);
      destructor Destroy; override;
      procedure Assign(ASource: TMQTTSubscription);
      property QOS: TMQTTQOSType read FQOS write FQOS;
      property Filter: UTF8String read FFilter write SetFilter;
      property Tokens: TMQTTTokenizer read GetTokens;
      property Age: Integer read FAge write FAge;
  end;

  { TMQTTSubscriptionList }

  TMQTTSubscriptionList = class(TObject)
    private
      FList     : TList;
      function GetCount: Integer;
      function GetItem(Index: Integer): TMQTTSubscription;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(ASource: TMQTTSubscriptionList);
      procedure Clear;
      function New(AFilter: UTF8String; AQOS: TMQTTQOSType = qtAT_MOST_ONCE): TMQTTSubscription;
      procedure Update(ASubscription: TMQTTSubscription);
      function Find(AFilter: UTF8String): TMQTTSubscription;
      procedure Remove(ASubscription: TMQTTSubscription);
      procedure Delete(Index: Integer);
      procedure MergeList(AList: TMQTTSubscriptionList);
      procedure DeleteList(AList: TMQTTSubscriptionList);
      function RemoveDuplicates: Integer;
      function RemoveInvalidSubscriptions: Integer;
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TMQTTSubscription read GetItem; default;
  end;

implementation

{ TMQTTSubscription }

constructor TMQTTSubscription.Create(AFilter: UTF8String; AQOS: TMQTTQOSType);
begin
  inherited Create;
  FFilter := AFilter;
  FQOS    := AQOS;
end;

destructor TMQTTSubscription.Destroy;
begin
  if Assigned(FTokens) then
    FreeAndNil(FTokens);
  inherited Destroy;
end;

procedure TMQTTSubscription.SetFilter(AValue: UTF8String);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
  if Assigned(FTokens) then
    FreeAndNil(FTokens);
end;

function TMQTTSubscription.GetTokens: TMQTTTokenizer;
begin
  if not Assigned(FTokens) then
    FTokens := TMQTTTokenizer.Create(FFilter,True);
  Result := FTokens;
end;

procedure TMQTTSubscription.Assign(ASource: TMQTTSubscription);
begin
  if Assigned(ASource) then
    begin
      FQOS := ASource.FQOS;
      FFilter := ASource.FFilter;
      FAge := 0;
      ASource.FAge := 0;
      if Assigned(FTokens) then
        FreeAndNil(FTokens);
    end;
end;

{ TMQTTSubscriptionList }

constructor TMQTTSubscriptionList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMQTTSubscriptionList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TMQTTSubscriptionList.Clear;
var
  I: Integer;
  O: TMQTTSubscription;
begin
  for I := Count - 1 downto 0 do
    begin
      O := Items[I];
      O.Destroy;
    end;
  FList.Clear;
end;

procedure TMQTTSubscriptionList.Assign(ASource: TMQTTSubscriptionList);
var
  I: Integer;
  O: TMQTTSubscription;
begin
  if Assigned(ASource) then
    begin
      Clear;
      for I := 0 to ASource.Count - 1 do
        begin
          O := TMQTTSubscription.Create;
          O.Assign(ASource[I]);
          O.Age := 0;
          FList.Add(O);
        end;
    end;
end;

function TMQTTSubscriptionList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMQTTSubscriptionList.GetItem(Index: Integer): TMQTTSubscription;
begin
  Result := TMQTTSubscription(FList[Index]);
end;

function TMQTTSubscriptionList.New(AFilter: UTF8String; AQOS: TMQTTQOSType): TMQTTSubscription;
begin
  Result := Find(AFilter);
  if Assigned(Result) then
    begin
      Result.FAge := 0;
      Result.FQOS := AQOS;
    end
  else
    begin
      Result := TMQTTSubscription.Create(AFilter,AQOS);
      FList.Add(Result);
    end;
end;

procedure TMQTTSubscriptionList.Update(ASubscription: TMQTTSubscription);
var
  O: TMQTTSubscription;
begin
  O := Find(ASubscription.Filter);
  if Assigned(O) then
    Remove(O);
  FList.Add(ASubscription);
  ASubscription.FAge := 0;
end;

function TMQTTSubscriptionList.Find(AFilter: UTF8String): TMQTTSubscription;
var
  I: Integer;
begin
  Assert(AFilter > '');
  for I := 0 to Count - 1 do
    begin
      Result := Items[I];
      if Result.Filter = AFilter then
        begin
          Result.Age := 0;
          Exit;
        end;
    end;
  Result := nil;
end;

procedure TMQTTSubscriptionList.Remove(ASubscription: TMQTTSubscription);
begin
  Assert(Assigned(ASubscription));
  if Assigned(ASubscription) then
    begin
      FList.Remove(ASubscription);
      ASubscription.Free;
    end;
end;

procedure TMQTTSubscriptionList.Delete(Index: Integer);
var
  O: TMQTTSubscription;
begin
  O := Items[Index];
  if Assigned(O) then
    O.Free;
  FList.Delete(Index);
end;

procedure TMQTTSubscriptionList.MergeList(AList: TMQTTSubscriptionList);
var
  I: Integer;
  S1,S2: TMQTTSubscription;
begin
  Assert(Assigned(AList));
  if Assigned(AList) then
    begin
      for I := 0 to AList.Count - 1 do
        begin
          S1 := AList[I];
          S2 := Find(S1.Filter);
          if not Assigned(S2) then
            S2 := New(S1.Filter,S1.QOS)
          else
            S1.Assign(S2);
        end;
    end;
end;

procedure TMQTTSubscriptionList.DeleteList(AList: TMQTTSubscriptionList);
var
  I,J: Integer;
  A,B: TMQTTSubscription;
begin
  Assert(Assigned(AList));
  if Assigned(AList) then
    for I := 0 to AList.Count - 1 do
      begin
        A := AList[I];
        for J := FList.Count - 1 downto 0 do
          begin
            B := Items[J];
            Assert(Assigned(B));
            if Assigned(B) then
              if A.Filter = B.Filter then
                begin
                  FList.Delete(J);
                  B.Free;
                end;
          end
      end;
end;

function TMQTTSubscriptionList.RemoveDuplicates: Integer;
var
  X,Y,N: Integer;
  S,R: TMQTTSubscription;
begin
  N := 0;
  for X := Count - 1 downto 1 do
    begin
      S := Items[X];
      for Y := X - 1 downto 0 do
        begin
          R := Items[Y];
          if (S.Filter = R.Filter) then
            begin
              if ord(S.QOS) > ord(R.QOS) then
                R.QOS := S.QOS;
              S.Free;
              Delete(X);
              inc(N);
              Break;
            end;
        end;
    end;
  Result := N;
end;

function TMQTTSubscriptionList.RemoveInvalidSubscriptions: Integer;
var
  X: Integer;
  S: TMQTTSubscription;
begin
  Result := 0;
  for X := Count - 1 downto 0 do
    begin
      S := Items[X];
      if not S.Tokens.Valid then
        begin
          inc(Result);
          FList.Delete(X);
          S.Free;
        end;
    end;
end;

end.


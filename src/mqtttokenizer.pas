unit mqtttokenizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Logging, MQTTConsts;

type

  TMQTTTokenKind = (tkInvalid,tkValid,tkMultiLevel,tkSingleLevel);

  { TMQTTToken }

  TMQTTToken = class(TObject)
    private
      FText: UTF8String;
      FKind: TMQTTTokenKind;
      function ValidTopicName: Boolean;
      function ValidateTopicFilter(IsLast: Boolean): Boolean;
    public
      property Kind: TMQTTTokenKind read FKind;
      property Text: UTF8String read FText;
  end;

  { TMQTTTokenizer }

  TMQTTTokenizer = class(TObject)
    private
      FList  : TList;
      FValid : Boolean;
      procedure Clear;
      function GetCount: Integer;
      function GetItem(Index: Integer): TMQTTToken;
      procedure _Tokenize(var Str: UTF8String);
      procedure Add(Text: UTF8String);
      function ValidateTopicName: Boolean;
      function ValidateTopicFilter: Boolean;
    public
      constructor Create(Str: UTF8String; Filter: Boolean = True);
      destructor Destroy; override;
      function AsString: String;
      property Valid: Boolean read FValid;
      property Count: Integer read GetCount;
      property Items[Index: Integer]: TMQTTToken read GetItem; default;
  end;

function CheckTopicMatchesFilter(Topic: TMQTTTokenizer; Filter: TMQTTTokenizer): Boolean;

implementation

function CheckTopicMatchesFilter(Topic: TMQTTTokenizer; Filter: TMQTTTokenizer): Boolean;
var
  I: Integer;
  FilterToken: TMQTTToken;
  TopicToken: TMQTTToken;
begin
  Result := False;
  for I := 0 to Filter.Count - 1 do
    begin
      FilterToken := Filter[I];
      if I >= Topic.Count then
        begin
          Result := Result and (FilterToken.Kind = tkMultiLevel);
          Exit;
        end;
      TopicToken  := Topic[I];
      if FilterToken.Kind = tkInvalid then
        begin
          Result := False;
          Exit;
        end
      else
      if FilterToken.Kind = tkValid then
        begin
          Result := FilterToken.Text = TopicToken.Text;
          if not Result then Exit;
        end
      else
      if FilterToken.Kind = tkMultilevel then
        begin
          Result := True;
          Exit;
        end
      else
      if FilterToken.Kind = tkSingleLevel then
        Result := True;
    end;
  if Filter.Count < Topic.Count then
    Result := Result and (Filter[Filter.Count-1].Kind = tkMultiLevel);
end;

{ TMQTTToken }

function TMQTTToken.ValidTopicName: Boolean;
begin
  // Empty string is valid
  // Any token containing a hash or a plus is invalid
  Result := (System.Length(FText) = 0) or ((Pos('#',FText) = 0) and (Pos('+',FText) = 0));
  if Result then
    FKind := tkValid
  else
    FKind := tkInvalid;
end;

function TMQTTToken.ValidateTopicFilter(IsLast: Boolean): Boolean;
var
  FLength, FHashPos, FPlusPos: SizeInt;
begin
  Result := True;
  FKind := tkValid;
  FLength := System.Length(FText);
  // Empty string is valid
  if FLength = 0 then Exit;
  FHashPos := Pos('#',FText);
  FPlusPos := Pos('+',FText);

  // Any token not containing a special char is valid
  if (FHashPos = 0) and (FPlusPos = 0) then Exit;

  Result := False;
  FKind := tkInvalid;
  // The hash character must only appear on its own
  if (FHashPos > 0) and (FLength <> 1) then Exit;
  // The hash character must only be in the last in the list of tokens
  if (FHashPos > 0) and (not IsLast) then Exit;
  // The plus character must only appear on its own
  if (FPlusPos > 0) and (FLength <> 1) then Exit;

  Result := True;
  if FHashPos = 1 then
    FKind := tkMultiLevel
  else
    if FPlusPos = 1 then
      FKind := tkSingleLevel;
end;

{ TMQTTTokenizer }

constructor TMQTTTokenizer.Create(Str: UTF8String; Filter: Boolean);
begin
  inherited Create;
  FList := TList.Create;
  if Str > '' then
    _Tokenize(Str);
  if Filter then
    FValid := ValidateTopicFilter
  else
    FValid := ValidateTopicName;
end;

destructor TMQTTTokenizer.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TMQTTTokenizer.AsString: String;
var
  I: Integer;
  T: TMQTTToken;
  S: String;
begin
  S := '';
  for I := 0 to FList.Count - 1 do
    begin
      T := Items[I];
      case T.Kind of
        tkSingleLevel: S := S + '+';
        tkMultiLevel: S := S + '#';
        tkValid: S := S + T.Text;
      end;
      if I < FList.Count - 1 then
        S := S + '/';
    end;
  Result := S;
end;

function TMQTTTokenizer.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMQTTTokenizer.GetItem(Index: Integer): TMQTTToken;
begin
  Result := TMQTTToken(FList[Index]);
end;

function TMQTTTokenizer.ValidateTopicName: Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count = 0 then Exit;
  for I := 0 to Count - 1 do
    if not Items[I].ValidTopicName then
      Exit;
  Result := True;
end;

function TMQTTTokenizer.ValidateTopicFilter: Boolean;
var
  I: Integer;
begin
  Result := False;
  if Count = 0 then Exit;
  for I := 0 to Count - 1 do
    if not Items[I].ValidateTopicFilter(I=Count-1) then
      Exit;
  Result := True;
end;

procedure TMQTTTokenizer._Tokenize(var Str: UTF8String);
var      // Recursive.  Call Tokenize to invoke
  P: SizeInt;
  Token: UTF8String;
begin
  P := Pos('/',Str);
  if P = 0 then
    begin
      Token := Str;
      Str := '';
    end
  else
    begin
      Token := copy(Str,1,P-1);
      Str := copy(Str,P+1,Length(Str)-P);
    end;
   Add(Token);
   if (P > 0) or (Str > '') then
     _Tokenize(Str);
end;

procedure TMQTTTokenizer.Clear;
var
  I: Integer;
  O: TMQTTToken;
begin
  for I := FList.Count -1 downto 0 do
    begin
      O := Items[I];
      if Assigned(O) then
        O.Free;
    end;
  FList.Clear;
end;

procedure TMQTTTokenizer.Add(Text: UTF8String);
var
  O: TMQTTToken;
begin
  O := TMQTTToken.Create;
  O.FText := Text;
  FList.Add(O);
end;

end.


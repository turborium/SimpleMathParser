// copyright 2021-2021 crazzzypeter
// license GNU 3.0

unit MathParser;

{$IFDEF FPC}{$MODE DELPHIUNICODE}{$ENDIF}

{$SCOPEDENUMS ON}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  EParserError = class(Exception)
  private
    FPosition: Integer;
  public
    constructor Create(const Position: Integer; const Message: string);
    property Position: Integer read FPosition;
  end;

  // Plus := '+'
  // Minus := '-'
  // Multiply := '*'
  // Divide := '/'

  // <Number> := <'0'..'9'>[.]<'0'..'9'>[e<['0'..'9']>] ...
  // <Primitive> := <'('><AddAndSub><')'> | <Number>
  // <MulAndDiv> := <Primitive> [<Multiply> | <Divide>] <Primitive> ...
  // <AddAndSub> := <MulAndDiv> [<Plus> | <Minus>] <MulAndDiv> ...

  TLogEvent = procedure (sender: TObject; str: string) of object;

  TTokenType = (Number, Plus, Minus, Multiply, Divide, Power, LeftBracket, RightBracket, &Function, Variable,
    Terminal);

  TMathCall = function(): Double of object;

  TMathParser = class sealed
  private
    FExpression: string;
    FOnLog: TLogEvent;
    procedure SetExpression(const Value: string);
  private
    FContants: TDictionary<string, Double>;
    Data: PChar;
    Position: Integer;
    PrevPosition: Integer;
    Token: TTokenType;
    Value: Double;
    Identifier: string;
    StackLevel: Integer;
    procedure NextToken;
    procedure SkipSpaces;
    function Primitive: Double;
    function AddAndSub: Double;
    function MulAndDiv: Double;
    function ExecuteFunction(const X: Double; FunctionName: string; const FunctionPosition: Integer): Double;
    procedure Log(str: string);
    function Pow: Double;
    function Call(const Func: TMathCall): Double;
    function GetConstant(const Name: string): Double;
    procedure SetConstant(const Name: string; const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    function Calculate: Double;
    property Expression: string read FExpression write SetExpression;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property Constants[const Name: string]: Double read GetConstant write SetConstant;
  end;

implementation

uses
  Math;

const
  MaxStackLevel = 32;

  sClosingParenthesisExpected = 'Closing parenthesis expected';
  sPrimitiveExpected = 'Primitive expected';
  sMissingOperator = 'Missing operator';
  sUnmatchedRightParenthesis = 'Unmatched right parenthesis';
  sUnexpectedSymbol = 'Unexpected symbol';
  sBadNumber = 'Bad number';
  sDivisionByZero = 'Division by zero';
  sBadFunctionArgument = 'Bad function argument';
  sBadFunction = 'Bad function';
  sOverflow = 'Overflow';
  sInternalError = 'Internal error!';
  sStackOverflow = 'Stack overflow';
  sContantNotFound = 'Constant not found';

{ TMathParser }

procedure TMathParser.NextToken;
var
  TokenString: string;
begin
  SkipSpaces;

  PrevPosition := Position;

  case Data[Position] of
    '0'..'9':
    begin
      // for ex: 12.34e+56
      // 12
      Token := TTokenType.Number;
      while CharInSet(Data[Position], ['0'..'9']) do
      begin
        TokenString := TokenString + Data[Position];
        Position := Position + 1;
      end;
      // .
      if Data[Position] = '.' then
      begin
        TokenString := TokenString + Data[Position];
        Position := Position + 1;
      end;
      // 34
      while CharInSet(Data[Position], ['0'..'9']) do
      begin
        TokenString := TokenString + Data[Position];
        Position := Position + 1;
      end;
      // e
      if CharInSet(Data[Position], ['e', 'E']) then
      begin
        TokenString := TokenString + Data[Position];
        Position := Position + 1;
        // +/-
        if CharInSet(Data[Position], ['-', '+']) then
        begin
          TokenString := TokenString + Data[Position];
          Position := Position + 1;
        end;
        // 56
        if not CharInSet(Data[Position], ['0'..'9']) then
          raise Exception.Create(sBadNumber);// error
        while CharInSet(Data[Position], ['0'..'9']) do
        begin
          TokenString := TokenString + Data[Position];
          Position := Position + 1;
        end;
      end;

      if not TryStrToFloat(TokenString, Value,
        {$IFNDEF FPC}TFormatSettings.Invariant{$ELSE}DefaultFormatSettings{$ENDIF}) then
        raise EParserError.Create(PrevPosition, sBadNumber);// error

      Log('NextToken: TokenType = Number, Value = ' +
        FloatToStr(Value, {$IFNDEF FPC}TFormatSettings.Invariant{$ELSE}DefaultFormatSettings{$ENDIF}));
    end;
    '+':
    begin
      Log('NextToken: TokenType = Plus');
      Token := TTokenType.Plus;
      Position := Position + 1;
    end;
    '-':
    begin
      Log('NextToken: TokenType = Minus');
      Token := TTokenType.Minus;
      Position := Position + 1;
    end;
    '*':
    begin
      Log('NextToken: TokenType = Multiply');
      Token := TTokenType.Multiply;
      Position := Position + 1;
    end;
    '/':
    begin
      Log('NextToken: TokenType = Divide');
      Token := TTokenType.Divide;
      Position := Position + 1;
    end;
    '^':
    begin
      Log('NextToken: TokenType = Power');
      Token := TTokenType.Power;
      Position := Position + 1;
    end;
    '(':
    begin
      Log('NextToken: TokenType = LeftBracket');
      Token := TTokenType.LeftBracket;
      Position := Position + 1;
    end;
    ')':
    begin
      Log('NextToken: TokenType = RightBracket');
      Token := TTokenType.RightBracket;
      Position := Position + 1;
    end;
    'a'..'z', 'A'..'Z':
    begin
      Identifier := '';
      // abc
      while CharInSet(Data[Position], ['a'..'z', 'A'..'Z', '0'..'9']) do
      begin
        Identifier := Identifier + Data[Position];
        Position := Position + 1;
      end;
      // SkipSpaces;
      // (
      if Data[Position] = '(' then
      begin
        Token := TTokenType.&Function;
        Position := Position + 1;
      end else
        Token := TTokenType.Variable;

      //raise EParserError.Create(PrevPosition, sBadFunction);// error

      Log('NextToken: TokenType = Function, Value = ' + Identifier);
    end;
    #0:
    begin
      Log('NextToken: TokenType = Terminal');
      Token := TTokenType.Terminal;
    end;
    else
      raise EParserError.Create(Position, sUnexpectedSymbol);// error
  end;
end;

function TMathParser.Primitive: Double;
var
  FunctionName: string;
  FunctionPos: Integer;
begin
  NextToken;
  case Token of
    // unary operators +/-
    TTokenType.Plus:
    begin
      Result := Call(Primitive);
    end;
    TTokenType.Minus:
    begin
      Result := -Call(Primitive);
    end;
    // primitives
    TTokenType.Number:
    begin
      Result := Value;
      NextToken;
    end;
    TTokenType.LeftBracket:
    begin
      Result := Call(AddAndSub);
      if Token <> TTokenType.RightBracket then
        raise EParserError.Create(Position, sClosingParenthesisExpected);// error
      NextToken;
    end;
    TTokenType.&Function:
    begin
      FunctionName := UpperCase(Identifier);// hmmm...
      FunctionPos := PrevPosition;
      Result := Call(AddAndSub);
      if Token <> TTokenType.RightBracket then
        raise EParserError.Create(Position, sClosingParenthesisExpected);// error
      Result := ExecuteFunction(Result, FunctionName, FunctionPos);
      NextToken;
    end;
    TTokenType.Variable:
    begin
      if FContants.ContainsKey(UpperCase(Identifier)) then
        Result := FContants[UpperCase(Identifier)]
      else
        raise EParserError.Create(PrevPosition, sContantNotFound);// error
      NextToken;
    end
    else
      raise EParserError.Create(PrevPosition, sPrimitiveExpected);// error
  end;

  if Token in [TTokenType.Number, TTokenType.LeftBracket, TTokenType.&Function] then
    raise EParserError.Create(PrevPosition, sMissingOperator);// error
end;

function TMathParser.Pow: Double;
begin
  Result := Primitive;

  while True do
  begin
    case Token of
      // ^
      TTokenType.Power:
      begin
        Result := Power(Result, Call(Pow));
      end;
      else
        break;
    end;
  end;
end;

function TMathParser.MulAndDiv: Double;
var
  RightValue: Double;
begin
  Result := Pow;

  while True do
  begin
    case Token of
      // *
      TTokenType.Multiply:
      begin
        Result := Result * Pow;
      end;
      // /
      TTokenType.Divide:
      begin
        RightValue := Pow;
        if RightValue = 0.0 then
          raise EParserError.Create(Position, sDivisionByZero);
        Result := Result / RightValue;
      end;
      else
        break;
    end;
  end;
end;

function TMathParser.AddAndSub: Double;
begin
  Result := MulAndDiv;

  while True do
  begin
    case Token of
      // +
      TTokenType.Plus:
      begin
        Result := Result + MulAndDiv;
      end;
      // -
      TTokenType.Minus:
      begin
        Result := Result - MulAndDiv;
      end;
      else
        break;
    end;
  end;
end;

function TMathParser.Calculate: Double;
var
  Mask: {$IFNDEF FPC}TArithmeticExceptionMask{$ELSE}TFPUExceptionMask{$ENDIF};
begin
  Position := 0;
  StackLevel := 0;

  Mask := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  try
    Result := AddAndSub;

    if Token = TTokenType.RightBracket then
      raise EParserError.Create(PrevPosition, sUnmatchedRightParenthesis);// error

    if Token <> TTokenType.Terminal then
      raise EParserError.Create(Position, sInternalError);// error

    if IsInfinite(Result) then
      raise EParserError.Create(0, sOverflow);// error
  finally
    SetExceptionMask(Mask);
  end;
end;

function TMathParser.Call(const Func: TMathCall): Double;
begin
  StackLevel := StackLevel + 1;
  if StackLevel > MaxStackLevel then
    raise EParserError.Create(PrevPosition, sStackOverflow);
  Result := Func();
  StackLevel := StackLevel - 1;
end;

constructor TMathParser.Create;
begin
  FContants := TDictionary<string, Double>.Create;
  FContants.Add('PI', System.Pi);
  Data := PChar(FExpression);
end;

destructor TMathParser.Destroy;
begin
  FContants.Free;
  inherited;
end;

function TMathParser.ExecuteFunction(const X: Double; FunctionName: string; const FunctionPosition: Integer): Double;
begin
  if IsNan(X) then
    Exit(NaN);

  if FunctionName = 'SQRT' then
  begin
    if X < 0 then
      raise EParserError.Create(FunctionPosition, sBadFunctionArgument);
    Result := Sqrt(X);
  end
  else if FunctionName = 'SIN' then
    Result := Sin(X)
  else if FunctionName = 'COS' then
    Result := Cos(X)
  else if FunctionName = 'TAN' then
    Result := Tan(X)
  else if FunctionName = 'ARCSIN' then
  begin
    if (X < -1) or (X > 1) then
      raise EParserError.Create(FunctionPosition, sBadFunctionArgument);
    Result := ArcSin(X);
  end
  else if FunctionName = 'ARCCOS' then
  begin
    if (X < -1) or (X > 1) then
      raise EParserError.Create(FunctionPosition, sBadFunctionArgument);
    Result := ArcCos(X);
  end
  else if FunctionName = 'ARCTAN' then
  begin
    Result := ArcTan(X);
  end
  else if FunctionName = 'LOG' then
  begin
    if (X <= 0) then
      raise EParserError.Create(FunctionPosition, sBadFunctionArgument);
    Result := Log10(X);
  end
  else if FunctionName = 'EXP' then
  begin
    Result := Exp(X);
  end else
    raise EParserError.Create(FunctionPosition, sBadFunction);
end;

function TMathParser.GetConstant(const Name: string): Double;
begin
  Result := FContants[UpperCase(Name)];
end;

procedure TMathParser.Log(str: string);
begin
  if Assigned(FOnLog) then
    FOnLog(self, str);
end;

procedure TMathParser.SetConstant(const Name: string; const Value: Double);
begin
  FContants.AddOrSetValue(UpperCase(Name), Value);
end;

procedure TMathParser.SetExpression(const Value: string);
begin
  FExpression := Value;
  Data := PChar(FExpression);
end;

procedure TMathParser.SkipSpaces;
begin
  while CharInSet(Data[Position], [#9, ' ']) do
  begin
    Position := Position + 1;
  end;
end;

{ EPerserError }

constructor EParserError.Create(const Position: Integer; const Message: string);
begin
  inherited Create('Error: ' + Message + ' at ' + IntToStr(Position));
  FPosition := Position;
end;

end.

program ConsoleCalc;

{$MODE DELPHIUNICODE}

uses
  MathParser, SysUtils;

var
  parser: TMathParser;
  s: string;
  ans: Double;
begin
  parser := TMathParser.Create;
  try
    WriteLn('Enter expression:');
    ReadLn(s);
    parser.Expression := s;
    try
      ans := parser.Calculate;
      WriteLn('Ans = ', FloatToStr(ans, DefaultFormatSettings));
    except
      on E: EParserError do
        WriteLn(E.Message);
    end;
  finally
    parser.Free;
  end;
  WriteLn('Press Enter key...');
  ReadLn;
end.

unit MathParserTest;

interface

uses
  DUnitX.TestFramework, MathParser, SysUtils, StrUtils;

type
  [TestFixture]
  TMathParserTest = class
  private
    Parser: TMathParser;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Test with TestCase Attribute to supply parameters.

    [Test]
    [TestCase('Plus1','1+1;2',';')]
    [TestCase('Plus2','1000+0;1000',';')]
    [TestCase('Plus3','1+10;11',';')]
    [TestCase('Plus4','1+1+1;3',';')]
    [TestCase('PlusMinus','10-100+50-40-2;-82',';')]
    [TestCase('--','5--10-12;3',';')]
    [TestCase('++','5++++2;7',';')]
    [TestCase('Mul1','100*10;1000',';')]
    [TestCase('Mul2','10-10*10;-90',';')]
    [TestCase('Mul3','10*2*3;60',';')]
    [TestCase('div1','1000/10;100',';')]
    [TestCase('div2','10+100/10;20',';')]
    [TestCase('div3','1000/10/2;50',';')]
    [TestCase('bracket1','(10-5)*2;10',';')]
    [TestCase('bracket2','2*((10-5)*2);20',';')]
    [TestCase('bracket3','2*(10-5);10',';')]
    [TestCase('bracket4','10-(10+10);-10',';')]
    [TestCase('bracket5','(3*-(-((-2))))-(0)--9+++++9;12',';')]
    [TestCase('functions1','log(log(8)/log(2))/1/(-(-(log(log(8)/log(2)))));1',';')]
    [TestCase('Power','2^3^4;2417851639229258349412352',';')]
    procedure TestBase(const Expression: string; const Ans: Double);

    [Test]
    [TestCase('empty expression',';0',';')]
    [TestCase('unexpected end 1','1 +  1 +  ;10',';')]
    [TestCase('unexpected end 2','1  +  ;6',';')]
    [TestCase('unexpected operation','  +  ;5',';')]
    [TestCase('expected number','3  *     *    9;9',';')]
    [TestCase('* _','   *    9;3',';')]
    [TestCase('no op','543253 345 345 34;7',';')]
    [TestCase('no op2','1 -2 -3 4;8',';')]
    [TestCase('brackets1','(3(+2));2',';')]
    [TestCase('brackets2',')(;0',';')]
    [TestCase('brackets3','();1',';')]
    [TestCase('brackets4','((23)-(23)3)-(33));10',';')]
    [TestCase('brackets5','(3));3',';')]
    [TestCase('brackets6','((3);4',';')]
    procedure TestExceptions(const Expression: string; const ErrorPosionon: Integer);
  end;

implementation

procedure TMathParserTest.Setup;
begin
  Parser := TMathParser.Create;
end;

procedure TMathParserTest.TearDown;
begin
  Parser.Free;
end;

procedure TMathParserTest.TestBase(const Expression: string; const Ans: Double);
begin
  Parser.Expression := Expression;
  Assert.AreEqual(Ans, Parser.Calculate);
end;

procedure TMathParserTest.TestExceptions(const Expression: string; const ErrorPosionon: Integer);
var
  Visualize: string;
begin
  Parser.Expression := Expression;
  try
    Parser.Calculate;
  except
    on E: EParserError do
    begin
      Visualize := '"' + Parser.Expression + '_"' + #13;
      Visualize :=  Visualize + ' ' + DupeString(' ', E.Position) + '^';


      Assert.AreEqual(ErrorPosionon, E.Position, {'Expected: ' + IntToStr(ErrorPosionon) +
         ', actual: ' + IntToStr(E.Position) + ', message: ' +} E.Message + #13 + visualize);
      Exit;
    end;
  end;
  Assert.Fail('Error in "' + Expression + '" not found!');
end;

initialization
  TDUnitX.RegisterTestFixture(TMathParserTest);

end.





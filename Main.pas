unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, MathParser;

type
  TFormMain = class(TForm)
    EditExpression: TEdit;
    ButtonExecute: TButton;
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
  private
    Parser: TMathParser;
    procedure ParserLog(sender: TObject; str: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  StrUtils, Math;

{$R *.dfm}

procedure TFormMain.ButtonExecuteClick(Sender: TObject);
var
  Ans: Double;
begin
  //Caption := Math.Tan(000000).ToString;
  //EditExpression.Text := StrUtils.DupeString('-', 80000) + '9';

  Parser.Expression := EditExpression.Text;
  MemoLog.Clear;
  try
    Ans := Parser.Calculate;
    ParserLog(self, 'Ans: ' + FloatToStr(Ans, TFormatSettings.Invariant));
  except
    on E: EParserError do
    begin
      ParserLog(Self, ' 0123456789012345678901234567890123456789');
      ParserLog(Self, '"' + Parser.Expression + '_"');
      ParserLog(Self, ' ' + DupeString(' ', E.Position) + '^');
      ParserLog(Self, E.Message);
    end;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Parser := TMathParser.Create;
  Parser.OnLog := ParserLog;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Parser.Free;
end;

procedure TFormMain.ParserLog(sender: TObject; str: string);
begin
  MemoLog.Lines.Add(str);
end;

end.

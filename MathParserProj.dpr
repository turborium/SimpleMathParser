program MathParserProj;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormMain},
  MathParser in 'MathParser.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

program Ascii_Text_Generator;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  FIGlet in 'FIGlet.pas',
  Common in '..\_Delphi\units\Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

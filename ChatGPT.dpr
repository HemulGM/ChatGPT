program ChatGPT;

uses
  System.StartUpCopy,
  FMX.Forms,
  ChatGPT.Main in 'ChatGPT.Main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

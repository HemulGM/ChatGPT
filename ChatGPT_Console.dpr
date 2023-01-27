program ChatGPT_Console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  OpenAI.API.Params in 'DelphiOpenAI\OpenAI.API.Params.pas',
  OpenAI.API in 'DelphiOpenAI\OpenAI.API.pas',
  OpenAI.Completions in 'DelphiOpenAI\OpenAI.Completions.pas',
  OpenAI.Edits in 'DelphiOpenAI\OpenAI.Edits.pas',
  OpenAI.Embeddings in 'DelphiOpenAI\OpenAI.Embeddings.pas',
  OpenAI.Engines in 'DelphiOpenAI\OpenAI.Engines.pas',
  OpenAI.Errors in 'DelphiOpenAI\OpenAI.Errors.pas',
  OpenAI.Files in 'DelphiOpenAI\OpenAI.Files.pas',
  OpenAI.FineTunes in 'DelphiOpenAI\OpenAI.FineTunes.pas',
  OpenAI.Images in 'DelphiOpenAI\OpenAI.Images.pas',
  OpenAI.Models in 'DelphiOpenAI\OpenAI.Models.pas',
  OpenAI.Moderations in 'DelphiOpenAI\OpenAI.Moderations.pas',
  OpenAI in 'DelphiOpenAI\OpenAI.pas';

begin
  try
    var OpenAI := TOpenAI.Create({$include MY_TOKEN.txt});
    var Buf := TStringList.Create;
    Writeln('ChatGPT is ready');
    var Prompt: string := '';
    repeat
      Write('Human: ');
      Readln(Prompt);
      if Prompt.IsEmpty then
        Break;
      Buf.Add('Human: ' + Prompt);
      try
        var Completions := OpenAI.Completion.Create(
          procedure(Params: TCompletionParams)
          begin
            Params.Prompt(Buf.Text);
            Params.MaxTokens(1024);
          end);
        try
          for var Choise in Completions.Choices do
          begin
            Buf.Add(Choise.Text.Trim([#13, #10, ' ']));
            Writeln(Choise.Text.Trim([#13, #10, ' ']));
          end;
        finally
          Completions.Free;
        end;
      except
        on E: Exception do
          Writeln('Error: ', E.Message);
      end;
    until False;
    Buf.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


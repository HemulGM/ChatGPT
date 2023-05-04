program ChatGPT_Console;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  OpenAI.Chat,
  OpenAI.Utils.ChatHistory,
  OpenAI;

var
   History: TChatHistory;

begin
  try
    var OpenAI := TOpenAI.Create({$include MY_TOKEN.txt});
    History := TChatHistory.Create;
    Writeln('ChatGPT is ready');
    var Prompt: string := '';
    repeat
      Write('Human: ');
      Readln(Prompt);
      if Prompt.IsEmpty then
        Break;
      History.New(TMessageRole.User, Prompt, '');
      try    {
        OpenAI.Chat.CreateStream(
          procedure(Params: TChatParams)
          begin
            Params.Messages(History.ToArray);
            Params.MaxTokens(1024);
            Params.Stream;
          end,
          procedure(Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
          begin
            if (not IsDone) and Assigned(Chat) then
              Writeln(Chat.Choices[0].Delta.Content)
            else if IsDone then
              Writeln('DONE!');
            Writeln('-------');
            Sleep(80);
          end);   }

        var Chat := OpenAI.Chat.Create(
          procedure(Params: TChatParams)
          begin
            Params.Messages(History.ToArray);
            Params.MaxTokens(1024);
          end);
        try
          for var Choise in Chat.Choices do
          begin
            History.New(TMessageRole.Assistant, Choise.Message.Content, '');
            Writeln(Choise.Message.Content.Trim([#13, #10, ' ']));
          end;
        finally
          Chat.Free;
        end;
      except
        on E: Exception do
          Writeln('Error: ', E.Message);
      end;
    until False;
    History.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.


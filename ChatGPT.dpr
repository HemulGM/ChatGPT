program ChatGPT;

uses
  System.StartUpCopy,
  FMX.Forms,
  Skia.FMX,
  ChatGPT.Main in 'ChatGPT.Main.pas' {FormMain},
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
  OpenAI in 'DelphiOpenAI\OpenAI.pas',
  ChatGPT.FrameChat in 'ChatGPT.FrameChat.pas' {FrameChat: TFrame},
  ChatGPT.FrameMessage in 'ChatGPT.FrameMessage.pas' {FrameMessage: TFrame},
  ChatGPT.Translate in 'ChatGPT.Translate.pas',
  OpenAI.Chat in 'DelphiOpenAI\OpenAI.Chat.pas',
  OpenAI.Audio in 'DelphiOpenAI\OpenAI.Audio.pas',
  OpenAI.Utils.ChatHistory in 'DelphiOpenAI\OpenAI.Utils.ChatHistory.pas',
  ChatGPT.FrameImagePreview in 'ChatGPT.FrameImagePreview.pas',
  ChatGPT.FrameImage in 'ChatGPT.FrameImage.pas',
  ChatGPT.FrameCode in 'ChatGPT.FrameCode.pas' {FrameCode: TFrame},
  ChatGPT.Classes in 'ChatGPT.Classes.pas',
  ChatGPT.FrameSVG in 'ChatGPT.FrameSVG.pas' {FrameSVG: TFrame},
  ChatGPT.FramePlainText in 'ChatGPT.FramePlainText.pas' {FrameText},
  ChatGPT.ChatSettings in 'ChatGPT.ChatSettings.pas' {FrameChatSettings: TFrame},
  DarkModeApi.Consts in 'WindowDarkMode\DarkModeApi.Consts.pas',
  DarkModeApi.FMX in 'WindowDarkMode\DarkModeApi.FMX.pas',
  DarkModeApi in 'WindowDarkMode\DarkModeApi.pas',
  DarkModeApi.Types in 'WindowDarkMode\DarkModeApi.Types.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

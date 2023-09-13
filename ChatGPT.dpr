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
  DarkModeApi.Consts in 'WindowDarkMode\DarkModeApi.Consts.pas',
  DarkModeApi.FMX in 'WindowDarkMode\DarkModeApi.FMX.pas',
  DarkModeApi in 'WindowDarkMode\DarkModeApi.pas',
  DarkModeApi.Types in 'WindowDarkMode\DarkModeApi.Types.pas',
  ChatGPT.Overlay in 'ChatGPT.Overlay.pas' {FrameOveraly: TFrame},
  ChatGPT.Settings in 'ChatGPT.Settings.pas' {FrameSettings: TFrame},
  ChatGPT.ChatSettings in 'ChatGPT.ChatSettings.pas' {FrameChatSettings: TFrame},
  ChatGPT.FrameUIMessage in 'ChatGPT.FrameUIMessage.pas' {FrameUIMessage: TFrame},
  ChatGPT.Code.Pascal in 'Code\ChatGPT.Code.Pascal.pas',
  ChatGPT.Code in 'ChatGPT.Code.pas',
  ChatGPT.Code.Python in 'Code\ChatGPT.Code.Python.pas',
  ChatGPT.Code.MarkDown in 'Code\ChatGPT.Code.MarkDown.pas',
  HGM.FMX.Image in 'HGM.FMX.Image.pas',
  HGM.FMX.Ani in 'HGM.FMX.Ani.pas',
  ChatGPT.Code.JSON in 'Code\ChatGPT.Code.JSON.pas',
  ChatGPT.About in 'ChatGPT.About.pas' {FrameAbout: TFrame},
  ChatGPT.SoundRecorder in 'ChatGPT.SoundRecorder.pas',
  ChatGPT.Android in 'ChatGPT.Android.pas',
  OpenAI.Chat.Functions in 'DelphiOpenAI\OpenAI.Chat.Functions.pas',
  OpenAI.Chat.Functions.Samples in 'DelphiOpenAI\OpenAI.Chat.Functions.Samples.pas',
  ChatGPT.Functions in 'ChatGPT.Functions.pas',
  ChatGPT.Code.SQL in 'Code\ChatGPT.Code.SQL.pas',
  ChatGPT.Functions.External in 'ChatGPT.Functions.External.pas',
  ChatGPT.Functions.External.Intf in 'ChatGPT.Functions.External.Intf.pas',
  ChatGPT.LoadedFunctions in 'ChatGPT.LoadedFunctions.pas' {FrameLoadedFunctions: TFrame},
  ChatGPT.TextEditor in 'ChatGPT.TextEditor.pas' {FrameTextEditor},
  ChatGPT.ImportExport in 'ChatGPT.ImportExport.pas' {FrameImportExport: TFrame},
  OpenAI.FineTuning in 'DelphiOpenAI\OpenAI.FineTuning.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.


unit ChatGPT.Classes;

interface

uses
  System.SysUtils, OpenAI.Chat.Functions;

{$SCOPEDENUMS ON}

type
  TWindowMode = (Compact, Full);

  TPartType = (Text, Code, SVG);

  TMessageKind = (User, Assistant, System, Error, Func);

  TMessageKindHelper = record helper for TMessageKind
    function ToString: string;
    class function FromString(const Value: string): TMessageKind; static;
  end;

  TMessageFuncState = (Wait, Success, Error);

  TPart = record
    PartType: TPartType;
    Content: string;
    Language: string;
  end;

  TUnderMouse = record
    WordStart: Int64;
    WordLength: Int64;
    WordLine: Int64;
    Text: string;
  end;

  TOnFuncExecute = procedure(Sender: TObject; const FuncName, FuncArgs: string; Callback: TProc<Boolean, string>) of object;

  TOnNeedFuncList = procedure(Sender: TObject; out Items: TArray<IChatFunction>) of object;

const
  MaxMessageWidth = 850;

procedure OpenUrl(const URL: string);

implementation

uses
  {$IFDEF ANDROID}
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.NET,
  {$ENDIF}
  {$IFDEF IOS OR IOS64}
  MacApi.Helpers, iOSApi.Foundation, FMX.Helpers.iOS,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}
  {$IFDEF MSWINDOWS}
  ShellAPI, DarkModeApi.FMX, FMX.Platform.Win,
  {$ENDIF}
  FMX.Platform;

procedure OpenUrl(const URL: string);
begin
  {$IFDEF ANDROID}
  TAndroidHelper.Context.startActivity(TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW, StrToJURI(URL)));
  {$ENDIF}
  {$IFDEF IOS OR IOS64}
  SharedApplication.OpenURL(StrToNSUrl(URL));
  {$ENDIF}
  {$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(URL)));
  {$ENDIF POSIX}
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(URL), nil, nil, 1);
  {$ENDIF}
end;

{ TMessageKindHelper }

class function TMessageKindHelper.FromString(const Value: string): TMessageKind;
begin
  if Value = 'system' then
    Exit(TMessageKind.System)
  else if Value = 'user' then
    Exit(TMessageKind.User)
  else if Value = 'assistant' then
    Exit(TMessageKind.Assistant)
  else if Value = 'error' then
    Exit(TMessageKind.Error)
  else if Value = 'function' then
    Exit(TMessageKind.Func)
  else
    Result := TMessageKind.User;
end;

function TMessageKindHelper.ToString: string;
begin
  case Self of
    TMessageKind.System:
      Result := 'system';
    TMessageKind.User:
      Result := 'user';
    TMessageKind.Assistant:
      Result := 'assistant';
    TMessageKind.Error:
      Result := 'error';
    TMessageKind.Func:
      Result := 'function';
  end;
end;

end.


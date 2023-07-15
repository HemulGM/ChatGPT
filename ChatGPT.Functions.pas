unit ChatGPT.Functions;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils, System.Generics.Collections, OpenAI.Chat.Functions,
  ChatGPT.Functions.external, ChatGPT.Functions.external.Intf;

type
  TGPTFunctionsFunc = function: TArray<IChatFunctionExternal>;

function LoadExternalFunctions: TArray<IChatFunction>;

var
  FLoadedLibs: TList<NativeInt>;

implementation

uses
  System.IOUtils;

function LoadExternalFunctions: TArray<IChatFunction>;
begin
  if TDirectory.Exists('funcs') then
  begin
    for var Lib in TDirectory.GetFiles('funcs', '*.dll') do
    try
      var LibHandle := LoadLibrary(PChar(Lib));
      try
        var Func: TGPTFunctionsFunc := GetProcAddress(LibHandle, 'gptfunctions');
        if @Func <> nil then
        begin
          for var Item in Func() do
          begin
            SetLength(Result, Length(Result) + 1);
            Result[Length(Result) - 1] := TChatFunctionExternal.Create(Item);
          end;
          FLoadedLibs.Add(LibHandle);
        end;
      except
        FreeLibrary(LibHandle);
      end;
    except
      //
    end;
  end;
end;

initialization
  FLoadedLibs := TList<NativeInt>.Create;

finalization
  for var Lib in FLoadedLibs do
    FreeLibrary(Lib);
  FLoadedLibs.Free;

end.


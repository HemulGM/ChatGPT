library gpt_function_cmd;

uses
  System.SysUtils,
  ChatGPT.Functions.External.Intf in '..\ChatGPT.Functions.External.Intf.pas',
  func_cmd in 'func_cmd.pas',
  Process in '..\TProcessDelphi\Process.pas',
  Pipes in '..\TProcessDelphi\Pipes.pas';

function gptfunctions: TArray<IChatFunctionExternal>;
begin
  Result := [TChatFunctionCMD.Create];
end;

exports
  gptfunctions;

begin
end.


library gpt_function_owm;

uses
  System.SysUtils,
  ChatGPT.Functions.External.Intf in '..\ChatGPT.Functions.External.Intf.pas',
  OWM.API in '..\OWM_API\OWM.API.pas',
  OWM.Classes in '..\OWM_API\OWM.Classes.pas',
  func_weather in 'func_weather.pas';

function gptfunctions: TArray<IChatFunctionExternal>;
begin
  Result := [TChatFunctionWeather.Create];
end;

exports
  gptfunctions;

begin
end.


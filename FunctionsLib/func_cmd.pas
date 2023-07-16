unit func_cmd;

interface

uses
  System.SysUtils, System.JSON, ChatGPT.Functions.External.Intf, Process;

type
  TChatFunctionCMD = class(TInterfacedObject, IChatFunctionExternal)
  protected
    function GetDescription: WideString;
    function GetName: WideString;
    function GetParameters: WideString;
  public
    function Execute(const Args: WideString): WideString;
  end;

implementation

{ TChatFunctionCMD }

function TChatFunctionCMD.Execute(const Args: WideString): WideString;
var
  JSON: TJSONObject;
  Command: string;
  Params: TArray<string>;
begin
  Result := '';
  // Parse arguments
  try
    JSON := TJSONObject.ParseJSONValue(Args) as TJSONObject;
    if Assigned(JSON) then
    try
      Command := JSON.GetValue('command', '');
      Params := ['/c', Command] + JSON.GetValue<TArray<string>>('params', ['']);
    finally
      JSON.Free;
    end;
  except
    JSON := nil;
  end;
  // Invalid arguments
  if (not Assigned(JSON)) or Command.IsEmpty then
    Exit;

  // Generate response
  JSON := TJSONObject.Create;
  try
    var Output: AnsiString;
    if RunCommand('cmd', Params, Output, [poNoConsole]) then
      JSON.AddPair('result', string(Output))
    else
      JSON.AddPair('error', 'Unknown error');
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

function TChatFunctionCMD.GetDescription: WideString;
begin
  Result := 'Run command via cmd';
end;

function TChatFunctionCMD.GetName: WideString;
begin
  Result := 'execute_command';
end;

function TChatFunctionCMD.GetParameters: WideString;
begin
  Result :=
    '{' +
    '  "type": "object",' +
    '  "properties": {' +
    '    "command": {"type": "string", "description": "Command to execute"},' +
    '    "params": {"type": "array", "items": {"type": "string"}, "description": "Command Options"}' +
    '  },' +
    '  "required": ["command"]' +
    '}';
end;

end.



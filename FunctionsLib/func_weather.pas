unit func_weather;

interface

uses
  System.SysUtils, System.JSON, ChatGPT.Functions.External.Intf, OWM.API,
  OWM.Classes;

type
  TChatFunctionWeather = class(TInterfacedObject, IChatFunctionExternal)
    const
      OWM_APPID = '36994c7b370d2e4c0753e34696105d7c'; //I do not mind)
  protected
    function GetDescription: WideString;
    function GetName: WideString;
    function GetParameters: WideString;
  public
    function Execute(const Args: WideString): WideString;
  end;

implementation

{ TChatFunctionWeather }

function TChatFunctionWeather.Execute(const Args: WideString): WideString;
var
  JSON: TJSONObject;
  Location: string;
  UnitKind: string;
begin
  Result := '';
  // Parse arguments
  try
    JSON := TJSONObject.ParseJSONValue(Args) as TJSONObject;
    if Assigned(JSON) then
    try
      Location := JSON.GetValue('location', '');
      UnitKind := JSON.GetValue('unit', '');
    finally
      JSON.Free;
    end;
  except
    JSON := nil;
  end;
  // Invalid arguments
  if (not Assigned(JSON)) or Location.IsEmpty then
    Exit;

  // Generate response
  JSON := TJSONObject.Create;
  try
    var OWM := TOWMAPI.Create(nil, OWM_APPID);
    try
      var OWMCurrent: TOWMCurrent;
      var Units: TOWMUnit;
      if UnitKind = 'celsius' then
        Units := TOWMUnit.Metric
      else if UnitKind = 'fahrenheit' then
        Units := TOWMUnit.Imperial
      else
        Units := TOWMUnit.Metric;

      if OWM.Current(OWMCurrent, Location, Units) then
      try
        JSON.AddPair('location', OWMCurrent.Name);
        JSON.AddPair('unit', UnitKind);

        JSON.AddPair('temperature', TJSONNumber.Create(Round(OWMCurrent.Main.Temp)));
        JSON.AddPair('pressure', TJSONNumber.Create(OWMCurrent.Main.Pressure));
        JSON.AddPair('humidity', TJSONNumber.Create(OWMCurrent.Main.Humidity));

        Result := JSON.ToJSON;
      finally
        OWMCurrent.Free;
      end;
    finally
      OWM.Free;
    end;
  finally
    JSON.Free;
  end;
end;

function TChatFunctionWeather.GetDescription: WideString;
begin
  Result := 'Get the current weather in a given location';
end;

function TChatFunctionWeather.GetName: WideString;
begin
  Result := 'get_current_weather';
end;

function TChatFunctionWeather.GetParameters: WideString;
begin
  Result :=
    '{' +
    '  "type": "object",' +
    '  "properties": {' +
    '    "location": {"type": "string", "description": "The city and state, e.g. San Francisco, CA"},' +
    '    "unit": {"type": "string", "enum": ["celsius", "fahrenheit"]}' +
    '  },' +
    '  "required": ["location"]' +
    '}';
end;

end.


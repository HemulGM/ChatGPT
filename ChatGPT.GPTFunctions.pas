unit ChatGPT.GPTFunctions;

interface

uses
  OpenAI.Chat.Functions;

type
  TChatFunctionWeather = class(TChatFunction)
    const
      OWM_APPID = '36994c7b370d2e4c0753e34696105d7c'; //I do not mind)
  protected
    function GetDescription: string; override;
    function GetName: string; override;
    function GetParameters: string; override;
  public
    constructor Create; override;
    function Execute(const Args: string): string; override;
  end;

implementation

uses
  System.JSON, System.SysUtils, OWM.API, OWM.Classes;

{ TChatFunctionWeather }

constructor TChatFunctionWeather.Create;
begin
  inherited;
end;

function TChatFunctionWeather.Execute(const Args: string): string;
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

function TChatFunctionWeather.GetDescription: string;
begin
  Result := 'Get the current weather in a given location';
end;

function TChatFunctionWeather.GetName: string;
begin
  Result := 'get_current_weather';
end;

function TChatFunctionWeather.GetParameters: string;
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


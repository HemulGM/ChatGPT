unit ChatGPT.Functions.external.Intf;

interface

type
  IChatFunctionExternal = interface
    ['{B2DACD74-B773-44C2-838D-43BF99CB5405}']
    function GetDescription: WideString;                  //Example: 'Get the current weather in a given location'
    function GetName: WideString;                         //Example: 'get_current_weather'
    function GetParameters: WideString;                   //JSON Scheme
    function Execute(const Args: WideString): WideString; //Args - json with params, result - json with info
  end;

implementation

end.


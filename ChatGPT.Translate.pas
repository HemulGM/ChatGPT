unit ChatGPT.Translate;

interface

function TranslateGoogle(const Text, LangSrc, LangTarget: string): string;

implementation

uses
  System.Net.HttpClient, System.JSON, System.Net.URLClient, System.SysUtils,
  System.Classes;

function TranslateGoogle(const Text, LangSrc, LangTarget: string): string;
var
  HTTP: THTTPClient;
  Response: TStringStream;
begin
  Result := '';
  if Text.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  Response := TStringStream.Create;

  try
    var URI := TURI.Create('https://translate.googleapis.com/translate_a/single');
    URI.AddParameter('client', 'gtx');
    URI.AddParameter('sl', LangSrc);
    URI.AddParameter('tl', LangTarget);
    URI.AddParameter('hl', LangTarget);
    URI.AddParameter('dt', 't');
    URI.AddParameter('dt', 'bd');
    URI.AddParameter('dj', '1');
    URI.AddParameter('ie', 'UTF-8');
    URI.AddParameter('source', 'icon');
    URI.AddParameter('tk', '467103.467103');
    URI.AddParameter('q', Text.Replace('...', '', [rfReplaceAll]).Replace(#13#10, ' ', [rfReplaceAll]));
    try
      if HTTP.Get(URI.Encode, Response).StatusCode = 200 then
      begin
        var JSON := TJSONObject.ParseJSONValue(UTF8ToString(RawByteString(Response.DataString)));
        if Assigned(JSON) then
        try
          Result := JSON.GetValue('sentences[0].trans', '');
        finally
          JSON.Free;
        end;
      end;
    except
      Result := '';
    end;
  finally
    HTTP.Free;
    Response.Free;
  end;
end;

end.


unit ChatGPT.Classes;

interface

type
  TWindowMode = (wmCompact, wmFull);

  TPartType = (ptText, ptCode, ptSVG);

  TMessageKind = (User, Assistant, System, Error);

  TMessageKindHelper = record helper for TMessageKind
    function ToString: string;
    class function FromString(const Value: string): TMessageKind; static;
  end;

  TPart = record
    PartType: TPartType;
    Content: string;
    Language: string;
  end;

const
  MaxMessageWidth = 850;

implementation

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
  end;
end;

end.


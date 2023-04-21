unit ChatGPT.Classes;

interface

type
  TWindowMode = (wmCompact, wmFull);

  TPartType = (ptText, ptCode);

  TPart = record
    PartType: TPartType;
    Content: string;
    Language: string;
  end;

implementation

end.


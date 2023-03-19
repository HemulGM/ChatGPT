unit ChatGPT.Classes;

interface

type
  TPartType = (ptText, ptCode);

  TPart = record
    PartType: TPartType;
    Content: string;
    Language: string;
  end;

implementation

end.


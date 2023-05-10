unit ChatGPT.Classes;

interface

type
  TWindowMode = (wmCompact, wmFull);

  TPartType = (ptText, ptCode, ptSVG);

  TPart = record
    PartType: TPartType;
    Content: string;
    Language: string;
  end;

const
  MaxMessageWidth = 850;

implementation

end.


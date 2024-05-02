unit ChatGPT.FrameSVG;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Skia, FMX.Skia;

type
  TFrameSVG = class(TFrame)
    SvgImage: TSkSvg;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; SvgData: string); reintroduce;
  end;

implementation

{$R *.fmx}

{ TFrameSVG }

constructor TFrameSVG.Create(AOwner: TComponent; SvgData: string);
begin
  inherited Create(AOwner);
  Name := '';
  SvgImage.Svg.Source := SvgData;
end;

end.


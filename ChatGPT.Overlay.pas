unit ChatGPT.Overlay;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Classes, FMX.Objects, System.Generics.Collections;

type
  TFrameOveraly = class(TFrame)
    RectangleBG: TRectangle;
  private
    FMode: TWindowMode;
  protected
    procedure SetMode(const Value: TWindowMode); virtual;
  public
    property Mode: TWindowMode read FMode write SetMode;
    procedure Cancel; virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Overlays: TList<TFrameOveraly>;

implementation

{$R *.fmx}

{ TFrameOveraly }

constructor TFrameOveraly.Create(AOwner: TComponent);
begin
  inherited;
  Overlays.Add(Self);
  Name := '';
  SetFocus;
end;

destructor TFrameOveraly.Destroy;
begin
  Overlays.Remove(Self);
  inherited;
end;

procedure TFrameOveraly.SetMode(const Value: TWindowMode);
begin
  FMode := Value;
end;

initialization
  Overlays := TList<TFrameOveraly>.Create;

finalization
  Overlays.Free;

end.


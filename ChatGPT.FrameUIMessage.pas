unit ChatGPT.FrameUIMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Controls.Presentation;

type
  TFrameUIMessage = class(TFrame)
    LayoutContent: TLayout;
    RectangleBG: TRectangle;
    LabelText: TLabel;
    TimerClose: TTimer;
    procedure LabelTextResize(Sender: TObject);
    procedure TimerCloseTimer(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure ShowUIMessage(const Text: string);

implementation

{$R *.fmx}

procedure ShowUIMessage(const Text: string);
begin
  with TFrameUIMessage.Create(Application) do
  begin
    LabelText.Text := Text;
    Align := TAlignLayout.Client;
    Parent := Application.MainForm;
    BringToFront;
  end;
end;

constructor TFrameUIMessage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
end;

procedure TFrameUIMessage.LabelTextResize(Sender: TObject);
begin
  RectangleBG.Width := LabelText.Width + 20;
end;

procedure TFrameUIMessage.TimerCloseTimer(Sender: TObject);
begin
  Release;
end;

end.


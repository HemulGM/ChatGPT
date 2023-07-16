unit ChatGPT.TextEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Overlay, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TFrameTextEditor = class(TFrameOveraly)
    LayoutClient: TLayout;
    RectangleFrame: TRectangle;
    LayoutActions: TLayout;
    ButtonOk: TButton;
    LabelCaption: TLabel;
    Rectangle1: TRectangle;
    MemoText: TMemo;
    ButtonCancel: TButton;
    procedure FrameResize(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FProcCallback: TProc<TFrameTextEditor, Boolean>;
    FLayoutClientWidth, FLayoutClientHeight: Single;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure Execute(AParent: TControl; ProcSet: TProc<TFrameTextEditor>; ProcExecuted: TProc<TFrameTextEditor, Boolean>);
  end;

var
  FrameTextEditor: TFrameTextEditor;

implementation

uses
  System.Math, ChatGPT.Main;

{$R *.fmx}

procedure TFrameTextEditor.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, False);
  Release;
end;

procedure TFrameTextEditor.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, True);
  Release;
end;

constructor TFrameTextEditor.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  FLayoutClientWidth := LayoutClient.Width;
  FLayoutClientHeight := LayoutClient.Height;
end;

class procedure TFrameTextEditor.Execute(AParent: TControl; ProcSet: TProc<TFrameTextEditor>; ProcExecuted: TProc<TFrameTextEditor, Boolean>);
begin
  var Frame := TFrameTextEditor.Create(AParent);
  Frame.Parent := AParent;
  Frame.FProcCallback := ProcExecuted;
  Frame.Align := TAlignLayout.Contents;
  Frame.BringToFront;
  if Assigned(ProcSet) then
    ProcSet(Frame);
end;

procedure TFrameTextEditor.FrameResize(Sender: TObject);
begin
  LayoutClient.Width := Min(FLayoutClientWidth, Width);
  LayoutClient.Height := Min(FLayoutClientHeight, Height);
end;

end.


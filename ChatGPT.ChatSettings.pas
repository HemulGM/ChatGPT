unit ChatGPT.ChatSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Overlay, FMX.Objects, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  ChatGPT.Classes;

type
  TFrameChatSettings = class(TFrameOveraly)
    LayoutClient: TLayout;
    RectangleFrame: TRectangle;
    Label1: TLabel;
    EditLangSrc: TEdit;
    ClearEditButton1: TClearEditButton;
    Path4: TPath;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Layout2: TLayout;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    Layout3: TLayout;
    TrackBarTemp: TTrackBar;
    LabelTemp: TLabel;
    procedure TrackBarTempTracking(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure RectangleBGClick(Sender: TObject);
  private
    FProcCallback: TProc<TFrameChatSettings, Boolean>;
  protected
    procedure SetMode(const Value: TWindowMode); override;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure Execute(AParent: TControl; ProcSet: TProc<TFrameChatSettings>; ProcExecuted: TProc<TFrameChatSettings, Boolean>);
  end;

var
  FrameChatSettings: TFrameChatSettings;

implementation

uses
  System.Math, FMX.Ani;

{$R *.fmx}

{ TFrameChatSettings }

procedure TFrameChatSettings.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, False);
  TThread.ForceQueue(nil, Free);
end;

procedure TFrameChatSettings.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, True);
  TThread.ForceQueue(nil, Free);
end;

constructor TFrameChatSettings.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
end;

class procedure TFrameChatSettings.Execute(AParent: TControl; ProcSet: TProc<TFrameChatSettings>; ProcExecuted: TProc<TFrameChatSettings, Boolean>);
begin
  var Frame := TFrameChatSettings.Create(AParent);
  Frame.Parent := AParent;
  Frame.FProcCallback := ProcExecuted;
  Frame.Align := TAlignLayout.Contents;
  Frame.BringToFront;
  if Assigned(ProcSet) then
    ProcSet(Frame);
  Frame.TrackBarTempTracking(nil);
end;

procedure TFrameChatSettings.FrameResize(Sender: TObject);
begin
  LayoutClient.Width := Min(320, Width);
  LayoutClient.Height := Min(380, Height);
end;

procedure TFrameChatSettings.RectangleBGClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWait(LayoutClient, 'RotationAngle', 2, 0.2, TAnimationType.out, TInterpolationType.Elastic);
  TAnimator.AnimateFloatWait(LayoutClient, 'RotationAngle', 0, 0.2, TAnimationType.out, TInterpolationType.Back);
end;

procedure TFrameChatSettings.SetMode(const Value: TWindowMode);
begin
  inherited;
  if Mode = TWindowMode.wmCompact then
    LayoutClient.Align := TAlignLayout.Client
  else
    LayoutClient.Align := TAlignLayout.Center;
  FrameResize(nil);
end;

procedure TFrameChatSettings.TrackBarTempTracking(Sender: TObject);
begin
  LabelTemp.Text := FormatFloat('0.0', TrackBarTemp.Value / 10);
end;

end.


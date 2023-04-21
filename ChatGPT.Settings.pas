unit ChatGPT.Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Overlay, FMX.Edit, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts,
  ChatGPT.Classes;

type
  TFrameSettings = class(TFrameOveraly)
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
    EditToken: TEdit;
    ClearEditButton2: TClearEditButton;
    Path1: TPath;
    Label5: TLabel;
    Label6: TLabel;
    Layout4: TLayout;
    ButtonGetToken: TButton;
    VertScrollBoxContent: TVertScrollBox;
    procedure TrackBarTempTracking(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonGetTokenClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure RectangleBGClick(Sender: TObject);
  private
    FProcCallback: TProc<TFrameSettings, Boolean>;
  protected
    procedure SetMode(const Value: TWindowMode); override;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure Execute(AParent: TControl; ProcSet: TProc<TFrameSettings>; ProcExecuted: TProc<TFrameSettings, Boolean>);
  end;

var
  FrameSettings: TFrameSettings;

implementation

uses
  ChatGPT.Main, System.Math, FMX.Ani;

{$R *.fmx}

{ TFrameSettings }

procedure TFrameSettings.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, False);
  TThread.ForceQueue(nil, Free);
end;

procedure TFrameSettings.ButtonGetTokenClick(Sender: TObject);
begin
  OpenUrl('https://platform.openai.com/account/api-keys');
end;

procedure TFrameSettings.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, True);
  TThread.ForceQueue(nil, Free);
end;

constructor TFrameSettings.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  VertScrollBoxContent.AniCalculations.Animation := True;
end;

class procedure TFrameSettings.Execute(AParent: TControl; ProcSet: TProc<TFrameSettings>; ProcExecuted: TProc<TFrameSettings, Boolean>);
begin
  var Frame := TFrameSettings.Create(AParent);
  Frame.Parent := AParent;
  Frame.FProcCallback := ProcExecuted;
  Frame.Align := TAlignLayout.Contents;
  Frame.BringToFront;
  if Assigned(ProcSet) then
    ProcSet(Frame);
  Frame.TrackBarTempTracking(nil);
end;

procedure TFrameSettings.FrameResize(Sender: TObject);
begin
  LayoutClient.Width := Min(440, Width);
  LayoutClient.Height := Min(450, Height);
end;

procedure TFrameSettings.RectangleBGClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWait(LayoutClient, 'RotationAngle', 2, 0.2, TAnimationType.Out, TInterpolationType.Elastic);
  TAnimator.AnimateFloatWait(LayoutClient, 'RotationAngle', 0, 0.2, TAnimationType.Out, TInterpolationType.Back);
end;

procedure TFrameSettings.SetMode(const Value: TWindowMode);
begin
  inherited;
  if Mode = TWindowMode.wmCompact then
    LayoutClient.Align := TAlignLayout.Client
  else
    LayoutClient.Align := TAlignLayout.Center;
  FrameResize(nil);
end;

procedure TFrameSettings.TrackBarTempTracking(Sender: TObject);
begin
  LabelTemp.Text := FormatFloat('0.0', TrackBarTemp.Value / 10);
end;

end.


unit ChatGPT.ChatSettings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Overlay, FMX.Objects, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  ChatGPT.Classes, FMX.ComboEdit;

type
  TFrameChatSettings = class(TFrameOveraly)
    LayoutClient: TLayout;
    RectangleFrame: TRectangle;
    LayoutActs: TLayout;
    ButtonCancel: TButton;
    ButtonOk: TButton;
    VertScrollBox: TVertScrollBox;
    ComboEditModel: TComboEdit;
    EditLangSrc: TEdit;
    ClearEditButton1: TClearEditButton;
    Path4: TPath;
    EditMaxTokens: TEdit;
    ClearEditButton3: TClearEditButton;
    Path2: TPath;
    EditQueryMaxToken: TEdit;
    ClearEditButton4: TClearEditButton;
    Path3: TPath;
    Label1: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Layout3: TLayout;
    TrackBarTemp: TTrackBar;
    LabelTemp: TLabel;
    Layout1: TLayout;
    TrackBarPP: TTrackBar;
    LabelPP: TLabel;
    Label12: TLabel;
    Label11: TLabel;
    Label15: TLabel;
    Layout5: TLayout;
    TrackBarFP: TTrackBar;
    LabelFP: TLabel;
    Label13: TLabel;
    Label28: TLabel;
    Layout6: TLayout;
    TrackBarTopP: TTrackBar;
    LabelTopP: TLabel;
    Label30: TLabel;
    LabelAppearance: TLabel;
    Layout7: TLayout;
    SwitchUseFunctions: TSwitch;
    Label34: TLabel;
    Label35: TLabel;
    Layout9: TLayout;
    SwitchAutoExecFuncs: TSwitch;
    Label37: TLabel;
    Layout2: TLayout;
    procedure TrackBarTempTracking(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure RectangleBGClick(Sender: TObject);
    procedure TrackBarPPTracking(Sender: TObject);
    procedure TrackBarFPTracking(Sender: TObject);
    procedure TrackBarTopPTracking(Sender: TObject);
  private
    FProcCallback: TProc<TFrameChatSettings, Boolean>;
    FLayoutClientWidth: Single;
    FLayoutClientHeight: Single;
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
  System.Math, FMX.Ani, HGM.FMX.Ani;

{$R *.fmx}

{ TFrameChatSettings }

procedure TFrameChatSettings.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, False);
  Release;
end;

procedure TFrameChatSettings.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, True);
  Release;
end;

constructor TFrameChatSettings.Create(AOwner: TComponent);
begin
  inherited;
  FLayoutClientWidth := LayoutClient.Width;
  FLayoutClientHeight := LayoutClient.Height;
  VertScrollBox.AniCalculations.Animation := True;
  VertScrollBox.AniCalculations.Interval := 1;
  VertScrollBox.AniCalculations.Averaging := True;
  VertScrollBox.ViewportPosition := TPoint.Zero;
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
  Frame.TrackBarPPTracking(nil);
  Frame.TrackBarFPTracking(nil);
  Frame.TrackBarTopPTracking(nil);
end;

procedure TFrameChatSettings.FrameResize(Sender: TObject);
begin
  LayoutClient.Width := Min(FLayoutClientWidth, Width);
  LayoutClient.Height := Min(FLayoutClientHeight, Height);
end;

procedure TFrameChatSettings.RectangleBGClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWithFinish(LayoutClient, 'RotationAngle', 2,
    procedure
    begin
      TAnimator.AnimateFloat(LayoutClient, 'RotationAngle', 0, 0.2, TAnimationType.out, TInterpolationType.Back);
    end,
    0.2, TAnimationType.out, TInterpolationType.Elastic);
end;

procedure TFrameChatSettings.SetMode(const Value: TWindowMode);
begin
  inherited;
  if Mode = TWindowMode.Compact then
  begin
    LayoutClient.Align := TAlignLayout.Client;
    RectangleFrame.Corners := [];
  end
  else
  begin
    LayoutClient.Align := TAlignLayout.Center;
    RectangleFrame.Corners := AllCorners;
  end;
  FrameResize(nil);
end;

procedure TFrameChatSettings.TrackBarFPTracking(Sender: TObject);
begin
  LabelFP.Text := FormatFloat('0.0', TrackBarFP.Value / 10);
end;

procedure TFrameChatSettings.TrackBarPPTracking(Sender: TObject);
begin
  LabelPP.Text := FormatFloat('0.0', TrackBarPP.Value / 10);
end;

procedure TFrameChatSettings.TrackBarTempTracking(Sender: TObject);
begin
  LabelTemp.Text := FormatFloat('0.0', TrackBarTemp.Value / 10);
end;

procedure TFrameChatSettings.TrackBarTopPTracking(Sender: TObject);
begin
  LabelTopP.Text := FormatFloat('0.0', TrackBarTopP.Value / 10);
end;

end.


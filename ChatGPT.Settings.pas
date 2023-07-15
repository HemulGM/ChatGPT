unit ChatGPT.Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Overlay, FMX.Edit, FMX.Objects, FMX.Layouts, ChatGPT.Classes,
  FMX.ComboEdit, FMX.ListBox, FMX.Controls.Presentation, FMX.ComboEdit.Style,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TStyledComboEdit = class(FMX.ComboEdit.Style.TStyledComboEdit)
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
  end;

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
    Label7: TLabel;
    Label8: TLabel;
    ComboEditModel: TComboEdit;
    Label9: TLabel;
    EditMaxTokens: TEdit;
    ClearEditButton3: TClearEditButton;
    Path2: TPath;
    Layout1: TLayout;
    TrackBarPP: TTrackBar;
    LabelPP: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Layout5: TLayout;
    TrackBarFP: TTrackBar;
    LabelFP: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    EditQueryMaxToken: TEdit;
    ClearEditButton4: TClearEditButton;
    Path3: TPath;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    LabelAppearance: TLabel;
    Label14: TLabel;
    EditOrg: TEdit;
    ClearEditButton5: TClearEditButton;
    Path5: TPath;
    Label20: TLabel;
    EditBaseUrl: TEdit;
    ClearEditButton6: TClearEditButton;
    Path6: TPath;
    Label21: TLabel;
    Label22: TLabel;
    LayoutOnTop: TLayout;
    SwitchOnTop: TSwitch;
    Label23: TLabel;
    Label10: TLabel;
    Label24: TLabel;
    EditProxyServer: TEdit;
    ClearEditButton7: TClearEditButton;
    Path7: TPath;
    Label25: TLabel;
    EditProxyPort: TEdit;
    ClearEditButton8: TClearEditButton;
    Path8: TPath;
    EditProxyUsername: TEdit;
    ClearEditButton9: TClearEditButton;
    Path9: TPath;
    Label26: TLabel;
    EditProxyPassword: TEdit;
    ClearEditButton10: TClearEditButton;
    Path10: TPath;
    Label27: TLabel;
    Label28: TLabel;
    Layout6: TLayout;
    TrackBarTopP: TTrackBar;
    LabelTopP: TLabel;
    Label30: TLabel;
    Label29: TLabel;
    LabelVersion: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    MemoCustomHeaders: TMemo;
    Label33: TLabel;
    Layout7: TLayout;
    SwitchUseFunctions: TSwitch;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Layout8: TLayout;
    ButtonLoadedFunctions: TButton;
    procedure TrackBarTempTracking(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonGetTokenClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure RectangleBGClick(Sender: TObject);
    procedure TrackBarPPTracking(Sender: TObject);
    procedure TrackBarFPTracking(Sender: TObject);
    procedure TrackBarTopPTracking(Sender: TObject);
    procedure ButtonLoadedFunctionsClick(Sender: TObject);
  private
    FProcCallback: TProc<TFrameSettings, Boolean>;
    FLayoutClientWidth, FLayoutClientHeight: Single;
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
  ChatGPT.Main, System.Math, FMX.Ani, FMX.Presentation.Style,
  FMX.Presentation.Factory, HGM.FMX.Ani, ChatGPT.LoadedFunctions;

{$R *.fmx}

{ TFrameSettings }

procedure TFrameSettings.ButtonCancelClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, False);
  Release;
end;

procedure TFrameSettings.ButtonGetTokenClick(Sender: TObject);
begin
  OpenUrl('https://platform.openai.com/account/api-keys');
end;

procedure TFrameSettings.ButtonLoadedFunctionsClick(Sender: TObject);
begin
  TFrameLoadedFunctions.Execute(TControl(Parent));
end;

procedure TFrameSettings.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, True);
  Release;
end;

constructor TFrameSettings.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF ANDROID OR IOS OR IOS64}
  LabelAppearance.Visible := False;
  LayoutOnTop.Visible := False;
  {$ENDIF}
  Name := '';
  MemoCustomHeaders.Text := '';
  MemoCustomHeaders.Lines.NameValueSeparator := ':';
  FLayoutClientWidth := LayoutClient.Width;
  FLayoutClientHeight := LayoutClient.Height;
  VertScrollBoxContent.AniCalculations.Animation := True;
  VertScrollBoxContent.ViewportPosition := TPoint.Zero;
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
  Frame.TrackBarPPTracking(nil);
  Frame.TrackBarFPTracking(nil);
  Frame.TrackBarTopPTracking(nil);
end;

procedure TFrameSettings.FrameResize(Sender: TObject);
begin
  LayoutClient.Width := Min(FLayoutClientWidth, Width);
  LayoutClient.Height := Min(FLayoutClientHeight, Height);
end;

procedure TFrameSettings.RectangleBGClick(Sender: TObject);
begin
  TAnimator.AnimateFloatWithFinish(LayoutClient, 'RotationAngle', 2,
    procedure
    begin
      TAnimator.AnimateFloat(LayoutClient, 'RotationAngle', 0, 0.2, TAnimationType.out, TInterpolationType.Back);
    end,
    0.2, TAnimationType.out, TInterpolationType.Elastic);
end;

procedure TFrameSettings.SetMode(const Value: TWindowMode);
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

procedure TFrameSettings.TrackBarFPTracking(Sender: TObject);
begin
  LabelFP.Text := FormatFloat('0.0', TrackBarFP.Value / 10);
end;

procedure TFrameSettings.TrackBarPPTracking(Sender: TObject);
begin
  LabelPP.Text := FormatFloat('0.0', TrackBarPP.Value / 10);
end;

procedure TFrameSettings.TrackBarTempTracking(Sender: TObject);
begin
  LabelTemp.Text := FormatFloat('0.0', TrackBarTemp.Value / 10);
end;

procedure TFrameSettings.TrackBarTopPTracking(Sender: TObject);
begin
  LabelTopP.Text := FormatFloat('0.0', TrackBarTopP.Value / 10);
end;

{ TStyledComboEdit }

procedure TStyledComboEdit.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  Handled := False;
end;

initialization
  TPresentationProxyFactory.Current.Unregister(TComboEdit, TControlType.Styled, TStyledPresentationProxy<FMX.ComboEdit.Style.TStyledComboEdit>);
  TPresentationProxyFactory.Current.Register(TComboEdit, TControlType.Styled, TStyledPresentationProxy<TStyledComboEdit>);

finalization
  TPresentationProxyFactory.Current.Unregister(TComboEdit, TControlType.Styled, TStyledPresentationProxy<TStyledComboEdit>);

end.


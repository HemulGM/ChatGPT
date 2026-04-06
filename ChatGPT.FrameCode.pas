unit ChatGPT.FrameCode;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Edit.Style, FMX.Memo, FMX.Layouts, FMX.Memo.Style, ChatGPT.Classes,
  FMX.TextLayout, ChatGPT.Code, FMX.Gestures, FMX.RichEdit.Style;

{$IF DEFINED(ANDROID) OR DEFINED(IOS) OR DEFINED(IOS64)}
  {$DEFINE MOBILE}
{$ENDIF}

type
  TMemo = class(FMX.Memo.TMemo)
  protected
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
  end;

  TFrameCode = class(TFrame)
    RectangleHead: TRectangle;
    RectangleClient: TRectangle;
    MemoCode: TMemo;
    LabelLanguage: TLabel;
    LayoutCopyCode: TLayout;
    PathCopy: TPath;
    LabelCopy: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure LayoutCopyCodeClick(Sender: TObject);
    procedure MemoCodeMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure LabelCopyResize(Sender: TObject);
    procedure RectangleClientMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure RectangleClientMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure RectangleClientMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure TimerMouseOverTimer(Sender: TObject);
    procedure MemoCodePresentationNameChoosing(Sender: TObject; var PresenterName: string);
  private
    FOnWheel: TMouseWheelEvent;
    FStyledMemo: TRichEditStyled;
    FMouseDown: TPointF;
    procedure FOnStyleLookup(Sender: TObject);
    procedure SetOnWheel(const Value: TMouseWheelEvent);
    function IsJson(const Value: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetContentHeight: Single;
    procedure Fill(Data: TPart);
    property OnWheel: TMouseWheelEvent read FOnWheel write SetOnWheel;
  end;

implementation

uses
  System.Math, FMX.Clipboard, System.JSON, FMX.Platform, ChatGPT.FrameUIMessage,
  System.Net.URLClient;

{$R *.fmx}

{ TFrameCode }

constructor TFrameCode.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  MemoCode.DisableDisappear := True;
  FStyledMemo := (MemoCode.Presentation as TRichEditStyled);
  {$IFDEF MOBILE}
  MemoCode.HitTest := False;
  {$ENDIF}
  MemoCode.OnApplyStyleLookup := FOnStyleLookup;
end;

destructor TFrameCode.Destroy;
begin
  inherited;
end;

function TFrameCode.IsJson(const Value: string): Boolean;
begin
  Result := False;
  try
    var JSON := TJSONObject.ParseJSONValue(Value);
    if Assigned(JSON) then
    begin
      JSON.Free;
      Result := True;
    end;
  except
    Result := False;
  end;
end;

procedure TFrameCode.Fill(Data: TPart);
begin
  if Data.Language.IsEmpty then
    if IsJson(Data.Content) then
      Data.Language := 'json';
  if not Data.Language.IsEmpty then
    //FCodeSyntax := TCodeSyntax.FindSyntax(Data.Language, MemoCode.Font, MemoCode.FontColor);
    FStyledMemo.SetCodeSyntaxName(Data.Language, MemoCode.Font, MemoCode.FontColor)
  else
    FStyledMemo.SetCodeSyntaxName('md', MemoCode.Font, MemoCode.FontColor);

  MemoCode.Text := Data.Content;
  if Data.Language.IsEmpty then
    LabelLanguage.Text := ''
  else
    LabelLanguage.Text := Data.Language;
  FrameResize(nil);
end;

procedure TFrameCode.FOnStyleLookup(Sender: TObject);
begin
  (MemoCode.Presentation as TRichEditStyled).RoundedSelection := True;
  (MemoCode.Presentation as TRichEditStyled).ShowGutter := True;
  (MemoCode.Presentation as TRichEditStyled).GutterRightMargin := 4;
  (MemoCode.Presentation as TRichEditStyled).UseSelectedTextColor := True;
  (MemoCode.Presentation as TRichEditStyled).SelectedTextColor := TAlphaColorRec.White;
  FrameResize(nil);
end;

procedure TFrameCode.FrameResize(Sender: TObject);
begin
  Height := GetContentHeight;
end;

function TFrameCode.GetContentHeight: Single;
begin
  Result := Max(MemoCode.ContentBounds.Height + 20, 30) +
    MemoCode.Margins.Top +
    MemoCode.Margins.Bottom +
    RectangleHead.Height;
end;

procedure TFrameCode.LabelCopyResize(Sender: TObject);
begin
  LayoutCopyCode.Width := LabelCopy.Width + PathCopy.Width + 8;
end;

procedure TFrameCode.LayoutCopyCodeClick(Sender: TObject);
begin
  var ClipBoard: IFMXClipboardService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipBoard) then
  begin
    ClipBoard.SetClipboard(MemoCode.Text);
    ShowUIMessage('Coppied');
  end
  else
    ShowUIMessage('Clipboard error');
end;

procedure TFrameCode.MemoCodeMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if (MemoCode.SelLength > 0) and (Root.Captured = IControl(FStyledMemo)) then
  begin
    Handled := True;
    if Assigned(FOnWheel) then
      FOnWheel(Sender, Shift, WheelDelta, Handled);
  end;
end;

procedure TFrameCode.MemoCodePresentationNameChoosing(Sender: TObject; var PresenterName: string);
begin
  PresenterName := 'RichEditStyled';
end;

procedure TFrameCode.RectangleClientMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseDown := TPointF.Create(X, Y);
  Root.Captured := RectangleClient;
end;

procedure TFrameCode.RectangleClientMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
  begin
    MemoCode.ViewportPosition := TPointF.Create(MemoCode.ViewportPosition.X + (FMouseDown.X - X), MemoCode.ViewportPosition.Y);
    FMouseDown := TPointF.Create(X, Y);
  end;
end;

procedure TFrameCode.RectangleClientMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Root.Captured := nil;
end;

procedure TFrameCode.SetOnWheel(const Value: TMouseWheelEvent);
begin
  FOnWheel := Value;
end;

procedure TFrameCode.TimerMouseOverTimer(Sender: TObject);
begin

end;

{ TMemo }

procedure TMemo.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
end;

end.


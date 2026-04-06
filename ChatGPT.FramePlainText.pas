unit ChatGPT.FramePlainText;

interface

{$IF DEFINED(ANDROID) OR DEFINED(IOS) OR DEFINED(IOS64)}
  {$DEFINE MOBILE}
{$ENDIF}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.Memo.Style, ChatGPT.Classes, FMX.TextLayout,
  ChatGPT.Code, FMX.RichEdit.Style;

type
  TFrameText = class(TFrame)
    MemoText: TMemo;
    procedure FrameResize(Sender: TObject);
    procedure MemoTextMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure MemoTextPresentationNameChoosing(Sender: TObject; var PresenterName: string);
    procedure MemoTextApplyStyleLookup(Sender: TObject);
  private
    FOnWheel: TMouseWheelEvent;
    FStyledMemo: TRichEditStyled;
    procedure SetOnWheel(const Value: TMouseWheelEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetContentHeight: Single;
    procedure Fill(Data: TPart);
    property OnWheel: TMouseWheelEvent read FOnWheel write SetOnWheel;
  end;

implementation

uses
  System.Math, System.Net.URLClient;

{$R *.fmx}

{ TFrameText }

constructor TFrameText.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  MemoText.DisableDisappear := True;
  {$IFDEF MOBILE}
  MemoText.HitTest := False;
  {$ENDIF}
  MemoText.TextSettings.VertAlign := TTextAlign.Center;
  MemoText.TextSettings.WordWrap := True;
end;

destructor TFrameText.Destroy;
begin
  inherited;
end;

procedure TFrameText.Fill(Data: TPart);
begin
  FStyledMemo := (MemoText.Presentation as TRichEditStyled);
  MemoText.ApplyStyleLookup;
  MemoText.Text := Data.Content;
  FrameResize(nil);
end;

procedure TFrameText.FrameResize(Sender: TObject);
begin
  var H := GetContentHeight;
  if H <> Height then
    Height := H;
end;

function TFrameText.GetContentHeight: Single;
begin
  if Assigned(FStyledMemo) then
    FStyledMemo.RecalcSize;
  var ContentH := MemoText.ContentBounds.Height;
  if (ContentH + 5) < 30 then
    MemoText.Margins.Top := 25 - ContentH
  else
    MemoText.Margins.Top := 0;
  Result := Max(ContentH + 5, 30) +
    MemoText.Margins.Top +
    MemoText.Margins.Bottom;
end;

procedure TFrameText.MemoTextApplyStyleLookup(Sender: TObject);
begin
  FStyledMemo.SetCodeSyntaxName('md', MemoText.Font, MemoText.FontColor);
  FStyledMemo.SelectedTextColor := TAlphaColorRec.White;
  FStyledMemo.RoundedSelection := True;
  FStyledMemo.UseSelectedTextColor := True;
  FrameResize(nil);
end;

procedure TFrameText.MemoTextMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if (MemoText.SelLength > 0) and (Root.Captured = IControl(FStyledMemo)) then
  begin
    Handled := True;
    if Assigned(FOnWheel) then
      FOnWheel(Sender, Shift, WheelDelta, Handled);
  end;
end;

procedure TFrameText.MemoTextPresentationNameChoosing(Sender: TObject; var PresenterName: string);
begin
  PresenterName := 'RichEditStyled';
end;

procedure TFrameText.SetOnWheel(const Value: TMouseWheelEvent);
begin
  FOnWheel := Value;
end;

end.


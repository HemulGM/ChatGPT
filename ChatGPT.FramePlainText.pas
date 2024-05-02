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
    TimerMouseOver: TTimer;
    procedure FrameResize(Sender: TObject);
    procedure MemoTextMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure MemoTextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MemoTextMouseLeave(Sender: TObject);
    procedure MemoTextClick(Sender: TObject);
    procedure TimerMouseOverTimer(Sender: TObject);
    procedure MemoTextPresentationNameChoosing(Sender: TObject;
      var PresenterName: string);
  private
    FOnWheel: TMouseWheelEvent;
    FCodeSyntax: TCodeSyntax;
    FStyledMemo: TRichEditStyled;
    FMouseMemo: TPointF;
    FUnderMouse: TUnderMouse;
    FUnderMouseAttr: TTextAttribute;
    procedure SetOnWheel(const Value: TMouseWheelEvent);
  private
    {$IFDEF NEW_MEMO}
    procedure UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
    {$ENDIF}
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
  FCodeSyntax := nil;
  FUnderMouseAttr.Color := MemoText.TextSettings.FontColor; // $FF006CE8;
  FUnderMouseAttr.Font := TFont.Create;
  FUnderMouseAttr.Font.Assign(MemoText.TextSettings.Font);
  FUnderMouseAttr.Font.Style := [TFontStyle.fsUnderline];
  FStyledMemo := (MemoText.Presentation as TRichEditStyled);
  {$IFDEF MOBILE}
  MemoText.HitTest := False;
  {$ENDIF}
  MemoText.TextSettings.VertAlign := TTextAlign.Center;
  {$IFDEF NEW_MEMO}
  //FStyledMemo.OnUpdateLayoutParams := UpdateLayout;
  {$IFDEF MOBILE}
  //FStyledMemo.NeedSelectorPoints := True;
  {$ENDIF}
  {$ENDIF}
end;

destructor TFrameText.Destroy;
begin
  FUnderMouseAttr.Font.Free;
  FCodeSyntax.Free;
  inherited;
end;

procedure TFrameText.Fill(Data: TPart);
begin
  {$IFDEF NEW_MEMO}
  if Assigned(FCodeSyntax) then
  begin
    FCodeSyntax.Free;
    FCodeSyntax := nil;
  end;
  //FCodeSyntax := TCodeSyntax.FindSyntax('md', MemoText.Font, MemoText.FontColor);
  FStyledMemo.SetCodeSyntaxName('md', MemoText.Font, MemoText.FontColor);
  {$ENDIF}
  MemoText.Text := Data.Content;
  MemoText.TextSettings.WordWrap := True;
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

procedure TFrameText.MemoTextClick(Sender: TObject);
begin
  if (FUnderMouse.WordLine <> -1) and (not FUnderMouse.Text.IsEmpty) and (MemoText.SelLength = 0) then
    OpenUrl(FUnderMouse.Text);
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

procedure TFrameText.MemoTextPresentationNameChoosing(Sender: TObject;
  var PresenterName: string);
begin
  PresenterName := 'RichEditStyled';
end;

procedure TFrameText.SetOnWheel(const Value: TMouseWheelEvent);
begin
  FOnWheel := Value;
end;

procedure TFrameText.MemoTextMouseLeave(Sender: TObject);
begin
  {$IFDEF NEW_MEMO}
  TimerMouseOver.Enabled := False;
  FUnderMouse.WordLine := -1;
  FStyledMemo.UpdateVisibleLayoutParams;
  FStyledMemo.Repaint;
  {$ENDIF}
end;

procedure TFrameText.MemoTextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF NEW_MEMO}
  TimerMouseOver.Enabled := False;
  TimerMouseOver.Enabled := True;
  FMouseMemo := TPointF.Create(X, Y);
  {$ENDIF}
end;

procedure TFrameText.TimerMouseOverTimer(Sender: TObject);
begin
  TimerMouseOver.Enabled := False;
  {$IFDEF NEW_MEMO}    {
  var BeginWord: Int64;
  var Line: Int64;
  var Str := FStyledMemo.GetWordAtPos(FMouseMemo.X, FMouseMemo.Y, BeginWord, Line);
  if (not Str.IsEmpty) and (Str.ToLower.StartsWith('http')) then
  try
    TURI.Create(Str);
    MemoText.Cursor := crHandPoint;
  except
    MemoText.Cursor := crDefault;
    Line := -1;
  end
  else
  begin
    MemoText.Cursor := crDefault;
    Line := -1;
  end;
  FUnderMouse.WordStart := BeginWord;
  FUnderMouse.WordLength := Str.Length;
  FUnderMouse.WordLine := Line;
  FUnderMouse.Text := Str;
  FStyledMemo.UpdateVisibleLayoutParams;
  MemoText.Repaint;   }
  {$ENDIF}
end;

{$IFDEF NEW_MEMO}
procedure TFrameText.UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
begin
  if not Assigned(Layout) then
    Exit;
  Layout.BeginUpdate;
  try
    Layout.ClearAttributes;
    Layout.Padding.Top := 1;
    Layout.Padding.Bottom := 1;
    if Assigned(FCodeSyntax) then
      for var Attr in FCodeSyntax.GetAttributesForLine(MemoText.Lines[Index], Index) do
        Layout.AddAttribute(Attr.Range, Attr.Attribute);
    if Index = FUnderMouse.WordLine then
      Layout.AddAttribute(TTextRange.Create(FUnderMouse.WordStart, FUnderMouse.WordLength), FUnderMouseAttr);
  finally
    Layout.EndUpdate;
  end;
end;
{$ENDIF}

end.


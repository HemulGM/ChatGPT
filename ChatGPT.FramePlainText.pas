unit ChatGPT.FramePlainText;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.Memo.Style, ChatGPT.Classes, FMX.TextLayout,
  ChatGPT.Code;

type
  TFrameText = class(TFrame)
    MemoText: TMemo;
    procedure FrameResize(Sender: TObject);
    procedure MemoTextApplyStyleLookup(Sender: TObject);
    procedure MemoTextMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    FOnWheel: TMouseWheelEvent;
    FCodeSyntax: TCodeSyntax;
    procedure SetOnWheel(const Value: TMouseWheelEvent);
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
  System.Math;

{$R *.fmx}

{ TFrameText }

constructor TFrameText.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  FCodeSyntax := nil;
  {$IFDEF ANDROID OR IOS OR IOS64}
  MemoText.HitTest := False;
  {$ENDIF}
  MemoText.TextSettings.VertAlign := TTextAlign.Center;
  {$IFDEF NEW_MEMO}
  (MemoText.Presentation as TStyledMemo).OnUpdateLayoutParams := UpdateLayout;
  {$IFDEF ANDROID OR IOS OR IOS64}
  (MemoText.Presentation as TStyledMemo).NeedSelectorPoints := True;
  {$ENDIF}
  {$ENDIF}
end;

destructor TFrameText.Destroy;
begin
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
  FCodeSyntax := TCodeSyntax.FindSyntax('md', MemoText.Font, MemoText.FontColor);
  {$ENDIF}
  MemoText.Text := Data.Content;
  MemoText.TextSettings.WordWrap := True;
  FrameResize(nil);
end;

procedure TFrameText.FrameResize(Sender: TObject);
begin
  Height := GetContentHeight;
end;

function TFrameText.GetContentHeight: Single;
begin
  {$IFDEF NEW_MEMO}
  (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
  (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  //var ContentH := (MemoText.Presentation as TStyledMemo).LinesLayout.ContentSize.Height;
  var ContentH := MemoText.ContentBounds.Height;
  {$ELSE}
  (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
  (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  var ContentH := MemoText.ContentBounds.Height;
  {$ENDIF}
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
  FrameResize(nil);
end;

procedure TFrameText.MemoTextMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if (MemoText.SelLength > 0) and (Root.Captured = IControl((MemoText.Presentation as TStyledMemo))) then
  begin
    Handled := True;
    if Assigned(FOnWheel) then
      FOnWheel(Sender, Shift, WheelDelta, Handled);
  end;
end;

procedure TFrameText.SetOnWheel(const Value: TMouseWheelEvent);
begin
  FOnWheel := Value;
end;

{$IFDEF NEW_MEMO}
procedure TFrameText.UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
begin
  if not Assigned(Layout) then
    Exit;
  Layout.ClearAttributes;
  Layout.Padding.Top := 1;
  Layout.Padding.Bottom := 1;
  if Assigned(FCodeSyntax) then
    for var Attr in FCodeSyntax.GetAttributesForLine(MemoText.Lines[Index]) do
      Layout.AddAttribute(Attr.Range, Attr.Attribute);
end;
{$ENDIF}

end.


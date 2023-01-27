unit ChatGPT.FrameMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TFrameMessage = class(TFrame)
    RectangleBG: TRectangle;
    MemoText: TMemo;
    Layout1: TLayout;
    RectangleUser: TRectangle;
    Path1: TPath;
    RectangleBot: TRectangle;
    Path2: TPath;
    LayoutContent: TLayout;
    procedure MemoTextChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FIsUser: Boolean;
    FText: string;
    FIsError: Boolean;
    procedure UpdateContentSize;
    procedure SetIsUser(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetIsError(const Value: Boolean);
  public
    property Text: string read FText write SetText;
    property IsUser: Boolean read FIsUser write SetIsUser;
    property IsError: Boolean read FIsError write SetIsError;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  System.Math, FMX.Memo.Style;

{$R *.fmx}

procedure TFrameMessage.UpdateContentSize;
begin
  var H := Padding.Top + Padding.Bottom;
  H := H + Max(MemoText.ContentBounds.Height, 30);
  if Height <> H then
    Height := H;
end;

constructor TFrameMessage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  {$IFDEF ANDROID}
  MemoText.HitTest := False;
  {$ENDIF}
end;

procedure TFrameMessage.FrameResize(Sender: TObject);
begin
  LayoutContent.Width := Min(Width - (Padding.Left + Padding.Right), 650);
  UpdateContentSize;
end;

procedure TFrameMessage.MemoTextChange(Sender: TObject);
begin
  UpdateContentSize;
end;

procedure TFrameMessage.SetIsError(const Value: Boolean);
begin
  FIsError := Value;
  MemoText.FontColor := $FFEF4444;
end;

procedure TFrameMessage.SetIsUser(const Value: Boolean);
begin
  FIsUser := Value;
  RectangleUser.Visible := FIsUser;
  RectangleBot.Visible := not FIsUser;

  if FIsUser then
  begin
    RectangleBG.Fill.Color := $00FFFFFF;
    MemoText.FontColor := $FFECECF1;
  end
  else
  begin
    RectangleBG.Fill.Color := $14FFFFFF;
    MemoText.FontColor := $FFD1D5E3;
  end;
end;

procedure TFrameMessage.SetText(const Value: string);
begin
  if not Value.IsEmpty then
    FText := Value
  else
    FText := 'пусто';
  MemoText.Text := FText.Trim([' ', #13, #10]);
  (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
  (MemoText.Presentation as TStyledMemo).PrepareForPaint;
end;

end.


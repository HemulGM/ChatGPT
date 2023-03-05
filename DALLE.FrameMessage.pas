unit DALLE.FrameMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TFrameMessageImage = class(TFrame)
    RectangleBG: TRectangle;
    Layout1: TLayout;
    RectangleUser: TRectangle;
    Path1: TPath;
    RectangleBot: TRectangle;
    Path2: TPath;
    LayoutContent: TLayout;
    LayoutContentItems: TLayout;
    FlowLayoutImages: TFlowLayout;
    MemoText: TMemo;
    procedure MemoTextChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FIsUser: Boolean;
    FText: string;
    FIsError: Boolean;
    FImages: TArray<string>;
    procedure UpdateContentSize;
    procedure SetIsUser(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetIsError(const Value: Boolean);
    procedure SetImages(const Value: TArray<string>);
  public
    property Text: string read FText write SetText;
    property Images: TArray<string> read FImages write SetImages;
    property IsUser: Boolean read FIsUser write SetIsUser;
    property IsError: Boolean read FIsError write SetIsError;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  System.Math, FMX.Memo.Style, DALLE.FrameImage;

{$R *.fmx}

procedure TFrameMessageImage.UpdateContentSize;
begin
  //Memo
  MemoText.Height := Max(MemoText.ContentBounds.Height, 30);

  //Flow
  var ItemW := Min(256, Max(Trunc(FlowLayoutImages.Width / FlowLayoutImages.ControlsCount), 48));
  if ItemW = 48 then
    ItemW := Trunc(FlowLayoutImages.Width / Trunc(FlowLayoutImages.Width / 48));
  var H: Single := 0;
  for var Control in FlowLayoutImages.Controls do
  begin
    Control.Size.Size := TSizeF.Create(ItemW, ItemW);
    H := Max(Control.Position.Y + Control.Height, H);
  end;
  if FlowLayoutImages.Height <> H then
    FlowLayoutImages.Height := H;

  //Frame
  H := Padding.Top + Padding.Bottom;
  for var Control in LayoutContentItems.Controls do
    if Control.Visible then
      H := H + Control.Height + Control.Margins.Top + Control.Margins.Bottom;
  if Height <> H then
    Height := H;
end;

constructor TFrameMessageImage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  {$IFDEF ANDROID}
  MemoText.HitTest := False;
  {$ENDIF}
  MemoText.Visible := False;
end;

procedure TFrameMessageImage.FrameResize(Sender: TObject);
begin
  LayoutContent.Width := Min(Width - (Padding.Left + Padding.Right), 650);
  UpdateContentSize;
end;

procedure TFrameMessageImage.MemoTextChange(Sender: TObject);
begin
  UpdateContentSize;
end;

procedure TFrameMessageImage.SetImages(const Value: TArray<string>);
begin
  FImages := Value;
  while FlowLayoutImages.ControlsCount > 0 do
    FlowLayoutImages.Controls[0].Free;
  for var Item in FImages do
  begin
    var Frame := TFrameImage.Create(FlowLayoutImages);
    Frame.Parent := FlowLayoutImages;
    Frame.Image := Item;
  end;
  UpdateContentSize;
end;

procedure TFrameMessageImage.SetIsError(const Value: Boolean);
begin
  FIsError := Value;
  MemoText.FontColor := $FFEF4444;
end;

procedure TFrameMessageImage.SetIsUser(const Value: Boolean);
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

procedure TFrameMessageImage.SetText(const Value: string);
begin
  MemoText.Visible := not Value.IsEmpty;
  if not Value.IsEmpty then
    FText := Value
  else
    FText := '';
  MemoText.Text := FText.Trim([' ', #13, #10]);
  (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
  (MemoText.Presentation as TStyledMemo).PrepareForPaint;
end;

end.


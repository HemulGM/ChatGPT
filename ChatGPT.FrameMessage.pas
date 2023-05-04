unit ChatGPT.FrameMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, System.Generics.Collections, FMX.BehaviorManager,
  ChatGPT.FrameImage, ChatGPT.Classes;

type
  TFrameMessage = class(TFrame)
    RectangleBG: TRectangle;
    LayoutInfo: TLayout;
    RectangleUser: TRectangle;
    Path1: TPath;
    RectangleBot: TRectangle;
    Path2: TPath;
    LayoutContent: TLayout;
    LayoutContentText: TLayout;
    LayoutAudio: TLayout;
    RectangleAudio: TRectangle;
    Path3: TPath;
    FlowLayoutImages: TFlowLayout;
    LayoutClient: TLayout;
    LayoutActions: TLayout;
    ButtonCopy: TButton;
    PathCopy: TPath;
    TimerRestoreCopy: TTimer;
    Line1: TLine;
    LayoutCompact: TLayout;
    ButtonCopyCompact: TButton;
    Path5: TPath;
    RectangleAudioCompact: TRectangle;
    Path6: TPath;
    procedure MemoTextChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure TimerRestoreCopyTimer(Sender: TObject);
  private
    FIsUser: Boolean;
    FText: string;
    FIsError: Boolean;
    FIsAudio: Boolean;
    FImages: TArray<string>;
    FId: string;
    FMode: TWindowMode;
    procedure SetIsUser(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetIsError(const Value: Boolean);
    procedure ParseText(const Value: string);
    procedure SetIsAudio(const Value: Boolean);
    procedure BuildContent(Parts: TList<TPart>);
    procedure SetImages(const Value: TArray<string>);
    procedure SetId(const Value: string);
    procedure FOnTextWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure UpdateMode;
  public
    procedure UpdateContentSize;
    property Id: string read FId write SetId;
    property Text: string read FText write SetText;
    property Images: TArray<string> read FImages write SetImages;
    property IsUser: Boolean read FIsUser write SetIsUser;
    property IsAudio: Boolean read FIsAudio write SetIsAudio;
    property IsError: Boolean read FIsError write SetIsError;
    procedure SetMode(const Value: TWindowMode);
    procedure StartAnimate;
    constructor Create(AOwner: TComponent); override;
  end;

//const
//  PathCopy: string = {$INCLUDE PathCopy.txt};   }

implementation

uses
  System.Math, FMX.Platform, FMX.Memo.Style, FMX.Ani, ChatGPT.FrameCode,
  ChatGPT.FrameSVG, ChatGPT.FramePlainText, ChatGPT.FrameUIMessage, System.JSON;

{$R *.fmx}

procedure TFrameMessage.UpdateContentSize;
begin
  //Flow
  if FlowLayoutImages.Visible then
  begin
    var ItemW := Min(256, Max(Trunc(FlowLayoutImages.Width / FlowLayoutImages.ControlsCount), 48));
    if ItemW = 48 then
      ItemW := Trunc(FlowLayoutImages.Width / Trunc(FlowLayoutImages.Width / 48));
    var FH: Single := 0;
    for var Control in FlowLayoutImages.Controls do
    begin
      Control.Size.Size := TSizeF.Create(ItemW, ItemW);
      FH := Max(Control.Position.Y + Control.Height, FH);
    end;
    if FlowLayoutImages.Height <> FH then
      FlowLayoutImages.Height := FH;
  end;

  //Frames
  var H := LayoutClient.Padding.Top + LayoutClient.Padding.Bottom;
  for var Control in LayoutContentText.Controls do
    if Control.Visible then
      H := H + Control.Height + Control.Margins.Top + Control.Margins.Bottom;

  if LayoutCompact.Visible then
    H := H + LayoutCompact.Height;
  if Height <> H then
    Height := H;
end;

procedure TFrameMessage.UpdateMode;
begin
  case FMode of
    wmCompact:
      begin
        LayoutCompact.Visible := not FText.IsEmpty;
        LayoutActions.Visible := False;
        LayoutAudio.Visible := False;
      end;
    wmFull:
      begin
        LayoutCompact.Visible := False;
        LayoutActions.Visible := not FText.IsEmpty;
      end;
  end;
end;

procedure TFrameMessage.ButtonCopyClick(Sender: TObject);
begin
  var ClipBoard: IFMXClipboardService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipBoard) then
  begin
    ClipBoard.SetClipboard(Text);
    ShowUIMessage('Coppied');
  end
  else
    ShowUIMessage('Clipboard error');
end;

constructor TFrameMessage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  LayoutActions.Visible := False;
  IsAudio := False;
  FlowLayoutImages.Visible := False;
end;

procedure TFrameMessage.StartAnimate;
begin
  LayoutClient.Margins.Top := 50;
  LayoutClient.Opacity := 0;
  TAnimator.AnimateFloat(LayoutClient, 'Margins.Top', 0);
  TAnimator.AnimateFloat(LayoutClient, 'Opacity', 1);
end;

procedure TFrameMessage.TimerRestoreCopyTimer(Sender: TObject);
begin
  TimerRestoreCopy.Enabled := False;
end;

procedure TFrameMessage.FOnTextWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if ssTouch in Shift then
  begin
    if ParentControl.ParentControl is TCustomScrollBox then
      (ParentControl.ParentControl as TCustomScrollBox).ViewportPosition :=
        TPointF.Create((ParentControl.ParentControl as TCustomScrollBox).ViewportPosition.X,
        (ParentControl.ParentControl as TCustomScrollBox).ViewportPosition.Y + WheelDelta);
  end
  else if ParentControl.ParentControl is TCustomScrollBox then
    (ParentControl.ParentControl as TCustomScrollBox).AniCalculations.MouseWheel(0, -WheelDelta);
end;

procedure TFrameMessage.FrameResize(Sender: TObject);
begin
  LayoutContent.Width := Min(Width - (LayoutClient.Padding.Left + LayoutClient.Padding.Right), 650);
  UpdateContentSize;
end;

procedure TFrameMessage.MemoTextChange(Sender: TObject);
begin
  UpdateContentSize;
end;

procedure TFrameMessage.SetId(const Value: string);
begin
  FId := Value;
end;

procedure TFrameMessage.SetImages(const Value: TArray<string>);
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
  FlowLayoutImages.Visible := FlowLayoutImages.ControlsCount > 0;
  UpdateContentSize;
end;

procedure TFrameMessage.SetIsAudio(const Value: Boolean);
begin
  FIsAudio := Value;
  LayoutAudio.Visible := FIsAudio;
  RectangleAudioCompact.Visible := FIsAudio;
end;

procedure TFrameMessage.SetIsError(const Value: Boolean);
begin
  FIsError := Value;
  for var Control in LayoutContentText.Controls do
    if Control is TFrameText then
      (Control as TFrameText).MemoText.FontColor := $FFEF4444;
end;

procedure TFrameMessage.SetIsUser(const Value: Boolean);
begin
  FIsUser := Value;
  RectangleUser.Visible := FIsUser;
  RectangleBot.Visible := not FIsUser;

  if FIsUser then
    RectangleBG.Fill.Color := $00FFFFFF
  else
    RectangleBG.Fill.Color := $14FFFFFF;
end;

procedure TFrameMessage.SetMode(const Value: TWindowMode);
begin
  FMode := Value;
  UpdateMode;
end;

procedure TFrameMessage.ParseText(const Value: string);

  function CreatePart(AType: TPartType; AContent: string): TPart;
  begin
    Result.PartType := AType;
    if (AType = ptCode) and (not (AContent.StartsWith(#13) or AContent.StartsWith(' '))) then
    begin
      var Len := AContent.IndexOfAny([#13, #10, ' ']);
      if Len >= 0 then
      begin
        Result.Language := AContent.Substring(0, Len);
        Result.Content := AContent.Remove(0, Len).Trim([#13, #10, ' ']);
      end
      else
        Result.Content := AContent.Trim([#13, #10, ' ']);
    end
    else
      Result.Content := AContent.Trim([#13, #10, ' ']);
  end;

var
  Parts: TList<TPart>;
  CodePairs: Integer;
  IsCodeParse: Boolean;
  Buf: string;
begin
  Parts := TList<TPart>.Create;
  try
    try
      var JSON := TJSONObject.ParseJSONValue(Value);
      if Assigned(JSON) then
      try
        var Part: TPart;
        Part.PartType := ptCode;
        Part.Content := Value;
        Part.Language := 'json';
        Parts.Add(Part);
        Exit;
      finally
        JSON.Free;
      end;
    except
      //
    end;
    CodePairs := 0;
    Buf := '';
    IsCodeParse := False;
    for var C in Value do
    begin
      if C = '`' then
      begin
        Inc(CodePairs);
        Buf := Buf + C;
        if CodePairs = 3 then
        begin
          if IsCodeParse then
          begin
            if not Buf.Trim([' ', '`']).IsEmpty then
              Parts.Add(CreatePart(ptCode, Buf.Trim(['`'])));
            IsCodeParse := False;
          end
          else
          begin
            if not Buf.Trim([' ', '`']).IsEmpty then
              Parts.Add(CreatePart(ptText, Buf.Trim([' ', '`'])));
            IsCodeParse := True;
          end;
          CodePairs := 0;
          Buf := '';
        end;
      end
      else
      begin
        CodePairs := 0;
        Buf := Buf + C;
      end;
    end;
    if IsCodeParse then
    begin
      if not Buf.Trim([' ', '`']).IsEmpty then
        Parts.Add(CreatePart(ptCode, Buf.Trim(['`'])));
    end
    else
    begin
      if not Buf.Trim([' ']).IsEmpty then
        Parts.Add(CreatePart(ptText, Buf));
    end;
  finally
    BuildContent(Parts);
    Parts.Free;
  end;
  UpdateContentSize;
end;

procedure TFrameMessage.BuildContent(Parts: TList<TPart>);
begin
  for var Part in Parts do
  begin
    //SVG
    var PosS := Part.Content.IndexOf('<svg');
    if PosS >= 0 then
    begin
      var PosE := Part.Content.IndexOf('</svg>');
      if PosE >= 0 then
      begin
        var SvgText := Part.Content.Substring(PosS, PosE - PosS + 6);
        if not SvgText.IsEmpty then
        try
          var Frame := TFrameSVG.Create(LayoutContentText, SvgText);
          Frame.Parent := LayoutContentText;
          Frame.Align := TAlignLayout.None;
          Frame.Position.Y := 10000;
          Frame.Align := TAlignLayout.Top;
        except
          // not insert
        end;
      end;
    end;

    // Code
    if Part.PartType = ptCode then
    begin
      var Frame := TFrameCode.Create(LayoutContentText);
      Frame.Parent := LayoutContentText;
      Frame.Fill(Part);
      Frame.Align := TAlignLayout.None;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.OnWheel := FOnTextWheel;
      Continue;
    end;

    // Text
    if Part.PartType = ptText then
    begin
      var Frame := TFrameText.Create(LayoutContentText);
      Frame.Parent := LayoutContentText;
      Frame.Fill(Part);
      Frame.Align := TAlignLayout.None;
      Frame.Position.Y := 10000;
      Frame.Align := TAlignLayout.Top;
      Frame.OnWheel := FOnTextWheel;
      if IsError then
        Frame.MemoText.FontColor := $FFEF4444
      else if FIsUser then
        Frame.MemoText.FontColor := $FFECECF1
      else
        Frame.MemoText.FontColor := $FFD1D5E3;
      Continue;
    end;
  end;
end;

procedure TFrameMessage.SetText(const Value: string);
begin
  if not Value.IsEmpty then
    FText := Value
  else
    FText := 'empty';
  FText := FText.Trim([' ', #13, #10]);
  UpdateMode;
  ParseText(FText);
end;

end.


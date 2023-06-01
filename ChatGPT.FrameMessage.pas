unit ChatGPT.FrameMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, System.Generics.Collections, FMX.BehaviorManager,
  ChatGPT.FrameImage, ChatGPT.Classes, FMX.Menus, OpenAI.Chat, System.JSON;

{$SCOPEDENUMS ON}

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
    Line1: TLine;
    LayoutCompact: TLayout;
    ButtonCopyCompact: TButton;
    Path5: TPath;
    RectangleAudioCompact: TRectangle;
    Path6: TPath;
    ButtonActions: TButton;
    Path4: TPath;
    RectangleSystem: TRectangle;
    Path7: TPath;
    ButtonDeleteCompact: TButton;
    Path8: TPath;
    PopupMenuActions: TPopupMenu;
    MenuItemDelete: TMenuItem;
    MenuItemEdit: TMenuItem;
    RectangleError: TRectangle;
    Path9: TPath;
    procedure FrameResize(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonActionsClick(Sender: TObject);
  private
    FText: string;
    FIsError: Boolean;
    FIsAudio: Boolean;
    FImages: TArray<string>;
    FId: string;
    FMode: TWindowMode;
    FOnDelete: TNotifyEvent;
    FMessageRole: TMessageKind;
    procedure SetText(const Value: string);
    procedure SetIsError(const Value: Boolean);
    procedure ParseText(const Value: string);
    procedure SetIsAudio(const Value: Boolean);
    procedure BuildContent(Parts: TList<TPart>);
    procedure SetImages(const Value: TArray<string>);
    procedure SetId(const Value: string);
    procedure FOnTextWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure UpdateMode;
    procedure CreateCodePart(Part: TPart);
    procedure CreatePartText(Part: TPart);
    procedure CreatePartSVG(Part: TPart);
    procedure SetOnDelete(const Value: TNotifyEvent);
    procedure UpdateMessageRole;
    procedure SetMessageRole(const Value: TMessageKind);
  public
    procedure UpdateContentSize;
    property Id: string read FId write SetId;
    property Text: string read FText write SetText;
    property Images: TArray<string> read FImages write SetImages;
    property IsAudio: Boolean read FIsAudio write SetIsAudio;
    property IsError: Boolean read FIsError write SetIsError;
    property MessageRole: TMessageKind read FMessageRole write SetMessageRole;
    procedure SetMode(const Value: TWindowMode);
    procedure StartAnimate;
    constructor Create(AOwner: TComponent); override;
    property OnDelete: TNotifyEvent read FOnDelete write SetOnDelete;
    function ToJsonObject: TJSONObject;
  end;

const
  ColorError = $FFEF4444;
  ColorUser = $FFECECF1;
  ColorBot = $FFD1D5E3;
  BGColorUser = $00FFFFFF;
  BGColorBot = $14FFFFFF;

implementation

uses
  System.Math, FMX.Platform, FMX.Memo.Style, FMX.Ani, ChatGPT.FrameCode,
  ChatGPT.FrameSVG, ChatGPT.FramePlainText, ChatGPT.FrameUIMessage;

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
    H := Ceil(H + LayoutCompact.Height);
  H := Max(H, 64);
  if Height <> H then
    Height := H;
end;

procedure TFrameMessage.UpdateMode;
begin
  case FMode of
    wmCompact:
      begin
        LayoutCompact.Visible := True;
        LayoutActions.Visible := False;
        LayoutAudio.Visible := False;
      end;
    wmFull:
      begin
        LayoutCompact.Visible := False;
        LayoutAudio.Visible := FIsAudio;
        LayoutActions.Visible := True;
      end;
  end;
end;

procedure TFrameMessage.ButtonActionsClick(Sender: TObject);
begin
  PopupMenuActions.PopupComponent := ButtonActions;
  var Pt := Application.MainForm.ClientToScreen(ButtonActions.AbsoluteRect.TopLeft);
  PopupMenuActions.Popup(Pt.X, Pt.Y + ButtonActions.Height);
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

procedure TFrameMessage.ButtonDeleteClick(Sender: TObject);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self);
  Release;
end;

constructor TFrameMessage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  LayoutActions.Visible := False;
  IsAudio := False;
  FlowLayoutImages.Visible := False;
  ButtonCopy.Enabled := False;
  MenuItemEdit.Enabled := False;
end;

procedure TFrameMessage.StartAnimate;
begin
  LayoutClient.Margins.Top := 50;
  LayoutClient.Opacity := 0;
  TAnimator.AnimateFloat(LayoutClient, 'Margins.Top', 0);
  TAnimator.AnimateFloat(LayoutClient, 'Opacity', 1);
end;

function TFrameMessage.ToJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('id', FId);
    Result.AddPair('role', FMessageRole.ToString);
    Result.AddPair('content', FText);
    if Length(FImages) > 0 then
    begin
      var JImages := TJSONArray.Create;
      for var Img in FImages do
        JImages.Add(Img);
      Result.AddPair('images', JImages);
    end;
    Result.AddPair('is_audio', FIsAudio);
  except
    //
  end;
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
  LayoutContent.Width := Min(Width - (LayoutClient.Padding.Left + LayoutClient.Padding.Right), MaxMessageWidth);
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
  UpdateMessageRole;
end;

procedure TFrameMessage.SetMessageRole(const Value: TMessageKind);
begin
  FMessageRole := Value;
  UpdateMessageRole;
end;

procedure TFrameMessage.SetMode(const Value: TWindowMode);
begin
  FMode := Value;
  UpdateMode;
end;

procedure TFrameMessage.SetOnDelete(const Value: TNotifyEvent);
begin
  FOnDelete := Value;
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
  try
    if Value.IsEmpty then
      Exit;
    Parts := TList<TPart>.Create;
    try
      if Value.StartsWith('{') then
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
      for var i := 1 to Value.Length do
      begin
        if Value[i] = '`' then
        begin
          Inc(CodePairs);
          Buf := Buf + Value[i];
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
          Buf := Buf + Value[i];
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
  finally
    UpdateContentSize;
  end;
end;

procedure TFrameMessage.CreateCodePart(Part: TPart);
begin
  var Frame := TFrameCode.Create(LayoutContentText);
  Frame.Parent := LayoutContentText;
  Frame.Fill(Part);
  Frame.Align := TAlignLayout.None;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.OnWheel := FOnTextWheel;
end;

procedure TFrameMessage.UpdateMessageRole;
begin
  RectangleUser.Visible := FMessageRole = TMessageKind.User;
  RectangleBot.Visible := FMessageRole = TMessageKind.Assistant;
  RectangleSystem.Visible := FMessageRole = TMessageKind.System;
  RectangleError.Visible := FMessageRole = TMessageKind.Error;

  if FMessageRole = TMessageKind.Assistant then
    RectangleBG.Fill.Color := BGColorBot
  else
    RectangleBG.Fill.Color := BGColorUser;
end;

procedure TFrameMessage.CreatePartText(Part: TPart);
begin
  var Frame := TFrameText.Create(LayoutContentText);
  Frame.Parent := LayoutContentText;
  Frame.Fill(Part);
  Frame.Align := TAlignLayout.None;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
  Frame.OnWheel := FOnTextWheel;
  if IsError then
    Frame.MemoText.FontColor := ColorError
  else if FMessageRole = TMessageKind.Assistant then
    Frame.MemoText.FontColor := ColorBot
  else
    Frame.MemoText.FontColor := ColorUser;
end;

procedure TFrameMessage.CreatePartSVG(Part: TPart);
begin
  var Frame := TFrameSVG.Create(LayoutContentText, Part.Content);
  Frame.Parent := LayoutContentText;
  Frame.Align := TAlignLayout.None;
  Frame.Position.Y := 10000;
  Frame.Align := TAlignLayout.Top;
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
      CreateCodePart(Part);
      Continue;
    end;

    // Text
    if Part.PartType = ptText then
    begin
      CreatePartText(Part);
      Continue;
    end;

    // Text
    if Part.PartType = ptSVG then
    begin
      CreatePartSVG(Part);
      Continue;
    end;
  end;
end;

procedure TFrameMessage.SetText(const Value: string);
begin
  if not Value.IsEmpty then
    FText := Value
  else
    FText := '';
  FText := FText.Trim([' ', #13, #10]);

  ButtonCopy.Enabled := not FText.IsEmpty;
  MenuItemEdit.Enabled := not FText.IsEmpty;
  UpdateMode;
  ParseText(FText);
end;

end.


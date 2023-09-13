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
    RectangleFunc: TRectangle;
    Path10: TPath;
    LayoutFunc: TLayout;
    ButtonExecuteFunc: TButton;
    LabelGPTFunc: TLabel;
    AniIndicatorFunc: TAniIndicator;
    LayoutFuncState: TLayout;
    PathSuccess: TPath;
    PathError: TPath;
    PathWait: TPath;
    ButtonEditCompact: TButton;
    Path11: TPath;
    procedure FrameResize(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonActionsClick(Sender: TObject);
    procedure ButtonExecuteFuncClick(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
  private
    FText: string;
    FIsError: Boolean;
    FIsAudio: Boolean;
    FImages: TArray<string>;
    FId: string;
    FMode: TWindowMode;
    FOnDelete: TNotifyEvent;
    FMessageRole: TMessageKind;
    FFuncName: string;
    FFuncArgs: string;
    FOnFuncExecute: TOnFuncExecute;
    FFuncState: TMessageFuncState;
    FOnTextUpdated: TOnTextUpdated;
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
    procedure SetOnDelete(const Value: TNotifyEvent);
    procedure UpdateMessageRole;
    procedure SetMessageRole(const Value: TMessageKind);
    procedure SetFuncArgs(const Value: string);
    procedure SetFuncName(const Value: string);
    procedure SetOnFuncExecute(const Value: TOnFuncExecute);
    procedure AfterExecuteFunc(Result: Boolean; Error: string);
    procedure SetFuncState(const Value: TMessageFuncState);
    procedure UpdateBufferText(const Id, Text: string);
    procedure SetOnTextUpdated(const Value: TOnTextUpdated);
  public
    procedure UpdateContentSize;
    property Id: string read FId write SetId;
    property Text: string read FText write SetText;
    property Images: TArray<string> read FImages write SetImages;
    property IsAudio: Boolean read FIsAudio write SetIsAudio;
    property IsError: Boolean read FIsError write SetIsError;
    property MessageRole: TMessageKind read FMessageRole write SetMessageRole;
    property FuncName: string read FFuncName write SetFuncName;
    property FuncArgs: string read FFuncArgs write SetFuncArgs;
    property FuncState: TMessageFuncState read FFuncState write SetFuncState;
    procedure SetMode(const Value: TWindowMode);
    procedure StartAnimate;
    procedure ExecuteFunc;
    constructor Create(AOwner: TComponent); override;
    property OnDelete: TNotifyEvent read FOnDelete write SetOnDelete;
    property OnFuncExecute: TOnFuncExecute read FOnFuncExecute write SetOnFuncExecute;
    property OnTextUpdated: TOnTextUpdated read FOnTextUpdated write SetOnTextUpdated;
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
  ChatGPT.FrameSVG, ChatGPT.FramePlainText, ChatGPT.FrameUIMessage,
  ChatGPT.TextEditor, ChatGPT.Main;

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
    TWindowMode.Compact:
      begin
        LayoutCompact.Visible := True;
        LayoutActions.Visible := False;
        LayoutAudio.Visible := False;
      end;
    TWindowMode.Full:
      begin
        LayoutCompact.Visible := False;
        LayoutAudio.Visible := FIsAudio;
        LayoutActions.Visible := True;
      end;
  end;
end;

procedure TFrameMessage.AfterExecuteFunc(Result: Boolean; Error: string);
begin
  LayoutFuncState.Hint := '';
  LayoutFuncState.HitTest := False;
  if Result then
    FuncState := TMessageFuncState.Success
  else
  begin
    FuncState := TMessageFuncState.Error;
    LayoutFuncState.Hint := Error;
    LayoutFuncState.HitTest := True;
  end;
end;

procedure TFrameMessage.ExecuteFunc;
begin
  if Assigned(FOnFuncExecute) then
  begin
    FuncState := TMessageFuncState.Executing;
    FOnFuncExecute(Self, FFuncName, FFuncArgs, AfterExecuteFunc);
  end;
end;

procedure TFrameMessage.ButtonExecuteFuncClick(Sender: TObject);
begin
  ExecuteFunc;
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
    if not Text.IsEmpty then
      ClipBoard.SetClipboard(Text)
    else if Length(Images) > 0 then
      ClipBoard.SetClipboard(string.Join(','#13#10, Images))
    else if not FFuncName.IsEmpty then
      ClipBoard.SetClipboard(FFuncName + #13#10 + FFuncArgs);
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
  MenuItemEdit.Enabled := False;
  LayoutFunc.Visible := False;
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
    Result.AddPair('func_name', FFuncName);
    Result.AddPair('func_args', FFuncArgs);
    Result.AddPair('func_state', Ord(FFuncState));
  except
    //
  end;
end;

procedure TFrameMessage.FOnTextWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if ssTouch in Shift then
  begin
    if ParentControl.ParentControl is TCustomScrollBox then
      TCustomScrollBox(ParentControl.ParentControl).ViewportPosition :=
        TPointF.Create(TCustomScrollBox(ParentControl.ParentControl).ViewportPosition.X,
        TCustomScrollBox(ParentControl.ParentControl).ViewportPosition.Y + WheelDelta);
  end
  else if ParentControl.ParentControl is TCustomScrollBox then
    TCustomScrollBox(ParentControl.ParentControl).AniCalculations.MouseWheel(0, -WheelDelta);
end;

procedure TFrameMessage.FrameResize(Sender: TObject);
begin
  LayoutContent.Width := Min(Width - (LayoutClient.Padding.Left + LayoutClient.Padding.Right), MaxMessageWidth);
  UpdateContentSize;
end;

procedure TFrameMessage.UpdateBufferText(const Id, Text: string);
begin
  if Assigned(FOnTextUpdated) then
    FOnTextUpdated(Self, Id, Text);
end;

procedure TFrameMessage.MenuItemEditClick(Sender: TObject);
begin
  FormMain.LayoutOverlay.BringToFront;
  TFrameTextEditor.Execute(FormMain.LayoutOverlay,
    procedure(Frame: TFrameTextEditor)
    begin
      Frame.LabelCaption.Text := 'Message edit';
      Frame.MemoText.Text := Text;
    end,
    procedure(Frame: TFrameTextEditor; Success: Boolean)
    begin
      if Success then
      begin
        Text := Frame.MemoText.Text;
        UpdateBufferText(Id, Text);
      end;
    end);
end;

procedure TFrameMessage.SetFuncArgs(const Value: string);
begin
  FFuncArgs := Value;
  LabelGPTFunc.Hint := 'Arsg: '#13#10 + Value;
end;

procedure TFrameMessage.SetFuncName(const Value: string);
begin
  FFuncName := Value;
  LayoutFunc.Visible := not FFuncName.IsEmpty;
  LabelGPTFunc.Text := Value;
end;

procedure TFrameMessage.SetFuncState(const Value: TMessageFuncState);
begin
  FFuncState := Value;
  AniIndicatorFunc.Visible := Value = TMessageFuncState.Executing;
  AniIndicatorFunc.Enabled := AniIndicatorFunc.Visible;
  ButtonExecuteFunc.Enabled := Value in [TMessageFuncState.Error, TMessageFuncState.Wait];
  PathSuccess.Visible := Value = TMessageFuncState.Success;
  PathError.Visible := Value = TMessageFuncState.Error;
  PathWait.Visible := Value = TMessageFuncState.Wait;
end;

procedure TFrameMessage.SetId(const Value: string);
begin
  FId := Value;
end;

procedure TFrameMessage.SetImages(const Value: TArray<string>);
begin
  FImages := Value;
  FlowLayoutImages.BeginUpdate;
  try
    while FlowLayoutImages.ControlsCount > 0 do
      FlowLayoutImages.Controls[0].Free;
    for var Item in FImages do
    begin
      var Frame := TFrameImage.Create(FlowLayoutImages);
      Frame.Parent := FlowLayoutImages;
      Frame.Image := Item;
    end;
  finally
    FlowLayoutImages.EndUpdate;
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
      TFrameText(Control).MemoText.FontColor := $FFEF4444;
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

procedure TFrameMessage.SetOnFuncExecute(const Value: TOnFuncExecute);
begin
  FOnFuncExecute := Value;
end;

procedure TFrameMessage.SetOnTextUpdated(const Value: TOnTextUpdated);
begin
  FOnTextUpdated := Value;
end;

procedure TFrameMessage.ParseText(const Value: string);

  function CreatePart(AType: TPartType; AContent: string): TPart;
  begin
    Result.PartType := AType;
    if (AType = TPartType.Code) and (not (AContent.StartsWith(#13) or AContent.StartsWith(' '))) then
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
          Part.PartType := TPartType.Code;
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
                Parts.Add(CreatePart(TPartType.Code, Buf.Trim(['`'])));
              IsCodeParse := False;
            end
            else
            begin
              if not Buf.Trim([' ', '`']).IsEmpty then
                Parts.Add(CreatePart(TPartType.Text, Buf.Trim([' ', '`'])));
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
          Parts.Add(CreatePart(TPartType.Code, Buf.Trim(['`'])));
      end
      else
      begin
        if not Buf.Trim([' ']).IsEmpty then
          Parts.Add(CreatePart(TPartType.Text, Buf));
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
  RectangleFunc.Visible := FMessageRole = TMessageKind.Func;

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

procedure TFrameMessage.BuildContent(Parts: TList<TPart>);
begin
  LayoutContentText.BeginUpdate;
  try
    FlowLayoutImages.Parent := nil;
    LayoutFunc.Parent := nil;
    while LayoutContentText.ControlsCount > 0 do
      LayoutContentText.Controls[0].Free;
    FlowLayoutImages.Parent := LayoutContentText;
    LayoutFunc.Parent := LayoutContentText;
    for var Part in Parts do
    begin
      //SVG check
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
      if Part.PartType = TPartType.Code then
      begin
        CreateCodePart(Part);
        Continue;
      end;

      // Text
      if Part.PartType = TPartType.Text then
      begin
        CreatePartText(Part);
        Continue;
      end;
    end;
  finally
    LayoutContentText.EndUpdate;
  end;
end;

procedure TFrameMessage.SetText(const Value: string);
begin
  FText := Value.Trim([' ', #13, #10]);
  MenuItemEdit.Enabled := not FText.IsEmpty;
  UpdateMode;
  ParseText(FText);
end;

end.


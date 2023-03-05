unit ChatGPT.FrameChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, OpenAI, OpenAI.Completions, ChatGPT.FrameMessage,
  System.Threading, FMX.Edit, FMX.ImgList, OpenAI.Chat,
  System.Generics.Collections, OpenAI.Audio, OpenAI.Utils.ChatHistory,
  OpenAI.Images, DALLE.FrameMessage;

type
  TWindowMode = (wmCompact, wmFull);

  TButton = class(FMX.StdCtrls.TButton)
  public
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  end;

  TLabel = class(FMX.StdCtrls.TLabel)
  public
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  end;

  TFrameChat = class(TFrame)
    VertScrollBoxChat: TVertScrollBox;
    LayoutSend: TLayout;
    RectangleSendBG: TRectangle;
    MemoQuery: TMemo;
    LayoutQuery: TLayout;
    Rectangle2: TRectangle;
    Layout1: TLayout;
    LayoutTyping: TLayout;
    TimerTyping: TTimer;
    LayoutTypingContent: TLayout;
    Layout3: TLayout;
    RectangleBot: TRectangle;
    Path3: TPath;
    Layout4: TLayout;
    RectangleIndicate: TRectangle;
    LabelTyping: TLabel;
    LineBorder: TLine;
    LayoutTranslate: TLayout;
    ButtonTranslate: TButton;
    Path2: TPath;
    LayoutTranslateSet: TLayout;
    EditLangSrc: TEdit;
    ClearEditButton1: TClearEditButton;
    Path4: TPath;
    Label1: TLabel;
    Rectangle3: TRectangle;
    LayoutWelcome: TLayout;
    RectangleBG: TRectangle;
    Label11: TLabel;
    FlowLayoutWelcome: TFlowLayout;
    LayoutExampleTitle: TLayout;
    Label2: TLabel;
    Path6: TPath;
    ButtonExample3: TButton;
    ButtonExample2: TButton;
    ButtonExample1: TButton;
    LayoutCapabilitiesTitle: TLayout;
    Label3: TLabel;
    Path5: TPath;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    LayoutLimitationsTitle: TLayout;
    Label4: TLabel;
    Path7: TPath;
    Label8: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Layout2: TLayout;
    ButtonAudio: TButton;
    Path8: TPath;
    ButtonSend: TButton;
    Path1: TPath;
    OpenDialogAudio: TOpenDialog;
    ButtonImage: TButton;
    PathImage: TPath;
    RectangleImageMode: TRectangle;
    procedure LayoutSendResize(Sender: TObject);
    procedure MemoQueryChange(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure TimerTypingTimer(Sender: TObject);
    procedure LayoutTypingResize(Sender: TObject);
    procedure MemoQueryKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ButtonTranslateClick(Sender: TObject);
    procedure EditLangSrcChangeTracking(Sender: TObject);
    procedure LayoutWelcomeResize(Sender: TObject);
    procedure FlowLayoutWelcomeResize(Sender: TObject);
    procedure ButtonExample1Click(Sender: TObject);
    procedure ButtonExample2Click(Sender: TObject);
    procedure ButtonExample3Click(Sender: TObject);
    procedure MemoQueryResize(Sender: TObject);
    procedure ButtonAudioClick(Sender: TObject);
    procedure ButtonImageClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FAPI: IOpenAI;
    FChatId: string;
    FPool: TThreadPool;
    FTitle: string;
    FMode: TWindowMode;
    FLangSrc: string;
    FIsTyping: Boolean;
    FBuffer: TChatHistory;
    FIsImageMode: Boolean;
    function NewMessage(const Text: string; IsUser: Boolean; UseBuffer: Boolean = True; IsAudio: Boolean = False): TFrameMessage;
    procedure ClearChat;
    procedure SetTyping(const Value: Boolean);
    procedure SetAPI(const Value: IOpenAI);
    procedure SetChatId(const Value: string);
    procedure ShowError(const Text: string);
    procedure AppendMessages(Response: TChat); overload;
    procedure AppendMessages(Response: TImageGenerations); overload;
    procedure ScrollDown;
    procedure SetTitle(const Value: string);
    procedure SetMode(const Value: TWindowMode);
    function ProcText(const Text: string; FromUser: Boolean): string;
    procedure SetLangSrc(const Value: string);
    procedure AppendAudio(Response: TAudioText);
    procedure SetIsImageMode(const Value: Boolean);
    procedure SendRequestImage;
    procedure SendRequestPrompt;
    function NewMessageImage(const Text: string; IsUser: Boolean; Images: TArray<string>): TFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property API: IOpenAI read FAPI write SetAPI;
    property ChatId: string read FChatId write SetChatId;
    property Title: string read FTitle write SetTitle;
    property Mode: TWindowMode read FMode write SetMode;
    property LangSrc: string read FLangSrc write SetLangSrc;
    property IsImageMode: Boolean read FIsImageMode write SetIsImageMode;
  end;

const
  MAX_TOKENS = 1024;
  MODEL_TOKENS_LIMIT = 4096;

implementation

uses
  FMX.Ani, System.Math, OpenAI.API, ChatGPT.Translate, System.IOUtils;

{$R *.fmx}

procedure TFrameChat.ShowError(const Text: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      var Frame := NewMessage(Text, False);
      Frame.IsError := True;
    end);
end;

procedure TFrameChat.AppendMessages(Response: TImageGenerations);
begin
  try
    var Images: TArray<string>;
    SetLength(Images, Length(Response.Data));
    for var i := 0 to High(Response.Data) do
      Images[i] := Response.Data[i].Url;
    NewMessageImage('', False, Images);
  finally
    Response.Free;
  end;
end;

function TFrameChat.NewMessageImage(const Text: string; IsUser: Boolean; Images: TArray<string>): TFrame;
begin
  LayoutWelcome.Visible := False;
  Result := TFrameMessageImage.Create(VertScrollBoxChat);
  Result.Position.Y := VertScrollBoxChat.ContentBounds.Height;
  Result.Parent := VertScrollBoxChat;
  Result.Align := TAlignLayout.MostTop;
  TFrameMessageImage(Result).Text := Text;
  TFrameMessageImage(Result).IsUser := IsUser;
  TFrameMessageImage(Result).Images := Images;
end;

procedure TFrameChat.AppendMessages(Response: TChat);
begin
  try
    for var Item in Response.Choices do
      NewMessage(Item.Message.Content, False);
  finally
    Response.Free;
  end;
end;

procedure TFrameChat.AppendAudio(Response: TAudioText);
begin
  try
    NewMessage(Response.Text, False, True, True);
  finally
    Response.Free;
  end;
end;

procedure TFrameChat.ScrollDown;
begin
  VertScrollBoxChat.ViewportPosition := TPointF.Create(0, VertScrollBoxChat.ContentBounds.Height);
end;

procedure TFrameChat.ButtonAudioClick(Sender: TObject);
begin
  if FIsTyping then
    Exit;
  if not OpenDialogAudio.Execute then
    Exit;
  var AudioFile := OpenDialogAudio.FileName;
  MemoQuery.Text := '';
  NewMessage(TPath.GetFileName(AudioFile), True, False, True);
  SetTyping(True);
  ScrollDown;
  TTask.Run(
    procedure
    begin
      try
        var Audio := API.Audio.CreateTranscription(
          procedure(Params: TAudioTranscription)
          begin
            Params.&File(AudioFile);
            Params.Language('ru');
          end);
        TThread.Queue(nil,
          procedure
          begin
            AppendAudio(Audio);
          end);
      except
        on E: OpenAIException do
          ShowError(E.Message);
        on E: Exception do
          ShowError('Error: ' + E.Message);
      end;
      TThread.Queue(nil,
        procedure
        begin
          SetTyping(False);
        end);
    end, FPool);
end;

procedure TFrameChat.ButtonExample1Click(Sender: TObject);
begin
  MemoQuery.Text := 'Explain quantum computing in simple terms';
end;

procedure TFrameChat.ButtonExample2Click(Sender: TObject);
begin
  MemoQuery.Text := 'Got any creative ideas for a 10 year old’s birthday?';
end;

procedure TFrameChat.ButtonExample3Click(Sender: TObject);
begin
  MemoQuery.Text := 'How do I make an HTTP request in Javascript?';
end;

procedure TFrameChat.ButtonImageClick(Sender: TObject);
begin
  IsImageMode := not IsImageMode;
end;

procedure TFrameChat.SendRequestImage;
begin
  if FIsTyping then
    Exit;
  var Prompt := MemoQuery.Text;
  if Prompt.IsEmpty then
    Exit;
  MemoQuery.Text := '';
  NewMessage(Prompt, True);
  SetTyping(True);
  ScrollDown;
  TTask.Run(
    procedure
    begin
      try
        var Images := API.Image.Create(
          procedure(Params: TImageCreateParams)
          begin
            Params.Prompt(ProcText(Prompt, True));
            Params.ResponseFormat(TImageResponseFormat.Url);
            Params.N(4);
            Params.Size(TImageSize.x512);
            Params.User(FChatId);
          end);
        TThread.Queue(nil,
          procedure
          begin
            AppendMessages(Images);
          end);
      except
        on E: OpenAIException do
          ShowError(E.Message);
        on E: Exception do
          ShowError('Error: ' + E.Message);
      end;
      TThread.Queue(nil,
        procedure
        begin
          SetTyping(False);
        end);
    end, FPool);
end;

procedure TFrameChat.SendRequestPrompt;
begin
  if FIsTyping then
    Exit;
  var Prompt := MemoQuery.Text;
  if Prompt.IsEmpty then
    Exit;
  MemoQuery.Text := '';
  NewMessage(Prompt, True);
  SetTyping(True);
  ScrollDown;
  TTask.Run(
    procedure
    begin
      try
        var Completions := API.Chat.Create(
          procedure(Params: TChatParams)
          begin
            Params.Messages(FBuffer.ToArray);
            Params.MaxTokens(MAX_TOKENS);
            //Params.Temperature(0.5);
            Params.User(FChatId);
          end);
        TThread.Queue(nil,
          procedure
          begin
            AppendMessages(Completions);
          end);
      except
        on E: OpenAIException do
          ShowError(E.Message);
        on E: Exception do
          ShowError('Error: ' + E.Message);
      end;
      TThread.Queue(nil,
        procedure
        begin
          SetTyping(False);
        end);
    end, FPool);
end;

procedure TFrameChat.ButtonSendClick(Sender: TObject);
begin
  if IsImageMode then
    SendRequestImage
  else
    SendRequestPrompt;
end;

procedure TFrameChat.ButtonTranslateClick(Sender: TObject);
begin
  if LayoutTranslateSet.Position.Y >= 0 then
  begin
    LayoutTranslateSet.Opacity := 0;
    TAnimator.AnimateFloat(LayoutTranslateSet, 'Position.Y', -70);
    TAnimator.AnimateFloat(LayoutTranslateSet, 'Opacity', 1, 0.4);
  end
  else
  begin
    LayoutTranslateSet.Opacity := 1;
    TAnimator.AnimateFloat(LayoutTranslateSet, 'Position.Y', 0);
    TAnimator.AnimateFloat(LayoutTranslateSet, 'Opacity', 0, 0.1);
  end;
end;

procedure TFrameChat.ClearChat;
begin
  LayoutTyping.Parent := nil;
  LayoutWelcome.Parent := nil;
  while VertScrollBoxChat.Content.ControlsCount > 0 do
    VertScrollBoxChat.Content.Controls[0].Free;
  LayoutTyping.Parent := VertScrollBoxChat;
  LayoutWelcome.Parent := VertScrollBoxChat;
end;

constructor TFrameChat.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TChatHistory.Create;
  FPool := TThreadPool.Create;
  LangSrc := '';
  Name := '';
  LayoutTranslateSet.Opacity := 0;
  LayoutTranslateSet.Position.Y := 0;
  VertScrollBoxChat.AniCalculations.Animation := True;
  SetTyping(False);
  ClearChat;
  IsImageMode := False;
end;

destructor TFrameChat.Destroy;
begin
  FPool.Free;
  FBuffer.Free;
  inherited;
end;

procedure TFrameChat.EditLangSrcChangeTracking(Sender: TObject);
begin
  LangSrc := EditLangSrc.Text;
end;

procedure TFrameChat.FlowLayoutWelcomeResize(Sender: TObject);
begin
  var W: Single := 0;
  case Mode of
    wmCompact:
      W := FlowLayoutWelcome.Width;
    wmFull:
      W := Trunc(FlowLayoutWelcome.Width / FlowLayoutWelcome.ControlsCount);
  end;
  for var Control in FlowLayoutWelcome.Controls do
    Control.Width := W;

  var B: Single := 0;
  for var Control in LayoutExampleTitle.Controls do
    B := Max(B, Control.Position.Y + Control.Height + Control.Margins.Bottom);
  if LayoutExampleTitle.Height <> B then
    LayoutExampleTitle.Height := B;

  B := 0;
  for var Control in LayoutCapabilitiesTitle.Controls do
    B := Max(B, Control.Position.Y + Control.Height + Control.Margins.Bottom);
  if LayoutCapabilitiesTitle.Height <> B then
    LayoutCapabilitiesTitle.Height := B;

  B := 0;
  for var Control in LayoutLimitationsTitle.Controls do
    B := Max(B, Control.Position.Y + Control.Height + Control.Margins.Bottom);
  if LayoutLimitationsTitle.Height <> B then
    LayoutLimitationsTitle.Height := B;

  B := 0;
  for var Control in FlowLayoutWelcome.Controls do
    B := Max(B, Control.Position.Y + Control.Height);

  B := B + FlowLayoutWelcome.Position.Y;
  if LayoutWelcome.Height <> B then
    LayoutWelcome.Height := B;
end;

procedure TFrameChat.FrameResize(Sender: TObject);
begin
  {$IFDEF ANDROID}
  LayoutTranslateSet.Width := LayoutQuery.Width;
  {$ELSE}
  if Mode = TWindowMode.wmCompact then
    LayoutTranslateSet.Width := LayoutQuery.Width;
  {$ENDIF}
end;

procedure TFrameChat.LayoutSendResize(Sender: TObject);
begin
  LayoutQuery.Width := Min(768, LayoutSend.Width - 48);
  VertScrollBoxChat.Padding.Bottom := LayoutSend.Height;
end;

procedure TFrameChat.LayoutTypingResize(Sender: TObject);
begin
  LayoutTypingContent.Width := Min(LayoutTyping.Width - (LayoutTyping.Padding.Left + LayoutTyping.Padding.Right), 650);
end;

procedure TFrameChat.LayoutWelcomeResize(Sender: TObject);
begin
  FlowLayoutWelcome.Width := Min(720, LayoutWelcome.Width);
end;

procedure TFrameChat.MemoQueryChange(Sender: TObject);
begin
  var H: Single :=
    LayoutSend.Padding.Top + LayoutSend.Padding.Bottom +
    MemoQuery.ContentBounds.Height +
    LayoutQuery.Padding.Top + LayoutQuery.Padding.Bottom;
  LayoutSend.Height := Max(LayoutSend.TagFloat, Min(H, 400));
end;

procedure TFrameChat.MemoQueryKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) and not (ssCtrl in Shift) then
  begin
    Key := 0;
    KeyChar := #0;
    ButtonSendClick(nil);
  end;
end;

procedure TFrameChat.MemoQueryResize(Sender: TObject);
begin
  MemoQueryChange(Sender);
end;

function TFrameChat.NewMessage(const Text: string; IsUser: Boolean; UseBuffer: Boolean; IsAudio: Boolean): TFrameMessage;
begin
  if UseBuffer and (not IsImageMode) then
  begin
    if IsUser then
    begin
      if Text.StartsWith('/system ') then
      begin
        var AText := Text.Replace('/system ', '', []);
        FBuffer.New(TMessageRole.System, ProcText(AText, IsUser));
      end;
      FBuffer.New(TMessageRole.User, ProcText(Text, IsUser));
    end
    else
      FBuffer.New(TMessageRole.Assistant, ProcText(Text, IsUser));
  end;
  LayoutWelcome.Visible := False;
  Result := TFrameMessage.Create(VertScrollBoxChat);
  Result.Position.Y := VertScrollBoxChat.ContentBounds.Height;
  Result.Parent := VertScrollBoxChat;
  Result.Align := TAlignLayout.MostTop;
  Result.Text := Text;
  Result.IsUser := IsUser;
  Result.IsAudio := IsAudio;
  Result.UpdateContentSize;
end;

function TFrameChat.ProcText(const Text: string; FromUser: Boolean): string;
begin
  if LangSrc.IsEmpty then
    Exit(Text);
  var Translate := '';
  if FromUser then
    Translate := TranslateGoogle(Text, LangSrc, 'en')
  else
    Translate := TranslateGoogle(Text, 'en', LangSrc);
  if Translate.IsEmpty then
    Result := Text
  else
    Result := Translate;
end;

procedure TFrameChat.SetAPI(const Value: IOpenAI);
begin
  FAPI := Value;
end;

procedure TFrameChat.SetChatId(const Value: string);
begin
  FChatId := Value;
end;

procedure TFrameChat.SetIsImageMode(const Value: Boolean);
begin
  FIsImageMode := Value;
  if FIsImageMode then
  begin
    PathImage.Fill.Color := $FFDDDDE4;
    RectangleImageMode.Visible := True;
  end
  else
  begin
    PathImage.Fill.Color := $FFACACBE;
    RectangleImageMode.Visible := False;
  end;
end;

procedure TFrameChat.SetLangSrc(const Value: string);
begin
  FLangSrc := Value;
end;

procedure TFrameChat.SetMode(const Value: TWindowMode);
begin
  FMode := Value;
  case FMode of
    wmCompact:
      begin
        LayoutSend.TagFloat := 100;
        VertScrollBoxChat.Padding.Bottom := 100;
        LayoutSend.Height := 100;
        LineBorder.Visible := True;
        LayoutSend.Padding.Rect := TRectF.Create(0, 10, 0, 40);
        RectangleSendBG.Fill.Kind := TBrushKind.Solid;
        RectangleSendBG.Fill.Color := $FF343541;
        LayoutTranslateSet.Width := LayoutQuery.Width;
      end;
    wmFull:
      begin
        LayoutSend.TagFloat := 170;
        VertScrollBoxChat.Padding.Bottom := 170;
        LayoutSend.Height := 170;
        LineBorder.Visible := False;
        LayoutSend.Padding.Rect := TRectF.Create(0, 80, 0, 40);
        RectangleSendBG.Fill.Kind := TBrushKind.Gradient;
        LayoutTranslateSet.Width := 153;
      end;
  end;
  FlowLayoutWelcomeResize(nil);
end;

procedure TFrameChat.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TFrameChat.SetTyping(const Value: Boolean);
begin
  FIsTyping := Value;
  ButtonSend.Enabled := not Value;
  TimerTyping.Enabled := Value;
  LayoutTyping.Visible := Value;
  LabelTyping.Visible := Value;
end;

procedure TFrameChat.TimerTypingTimer(Sender: TObject);
begin
  RectangleIndicate.Visible := not RectangleIndicate.Visible;
  if LabelTyping.Text.Length > 2 then
    LabelTyping.Text := '.'
  else
    LabelTyping.Text := LabelTyping.Text + '.';
end;

{ TButton }

procedure TButton.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  if Assigned(Canvas) and (Tag = 1) then
  begin
    var H := TRectF.Create(0, 0, Width - 20, 10000);
    Canvas.Font.Size := Font.Size;
    Canvas.MeasureText(H, Text, WordWrap, [], TextAlign, VertTextAlign);
    if AHeight <> H.Height + 24 then
      Height := H.Height + 24;
  end;
end;

{ TLabel }

procedure TLabel.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  if Assigned(Canvas) and (Tag = 1) then
  begin
    var H := TRectF.Create(0, 0, Width - 20, 10000);
    Canvas.Font.Size := Font.Size;
    Canvas.MeasureText(H, Text, WordWrap, [], TextAlign, VertTextAlign);
    if AHeight <> H.Height + 24 then
      Height := H.Height + 24;
  end;
end;

end.


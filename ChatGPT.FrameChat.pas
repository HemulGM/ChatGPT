unit ChatGPT.FrameChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, OpenAI, OpenAI.Completions, ChatGPT.FrameMessage,
  System.Threading, FMX.Edit, FMX.ImgList, OpenAI.Chat,
  System.Generics.Collections, OpenAI.Audio, OpenAI.Utils.ChatHistory,
  OpenAI.Images, ChatGPT.ChatSettings, System.JSON, FMX.Effects;

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
    RectangleTypeBG: TRectangle;
    ButtonSettings: TButton;
    Path9: TPath;
    LayoutRetry: TLayout;
    ButtonRetry: TButton;
    ShadowEffect1: TShadowEffect;
    procedure LayoutSendResize(Sender: TObject);
    procedure MemoQueryChange(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure TimerTypingTimer(Sender: TObject);
    procedure LayoutTypingResize(Sender: TObject);
    procedure MemoQueryKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure LayoutWelcomeResize(Sender: TObject);
    procedure FlowLayoutWelcomeResize(Sender: TObject);
    procedure ButtonExample1Click(Sender: TObject);
    procedure ButtonExample2Click(Sender: TObject);
    procedure ButtonExample3Click(Sender: TObject);
    procedure MemoQueryResize(Sender: TObject);
    procedure ButtonAudioClick(Sender: TObject);
    procedure ButtonImageClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure ButtonRetryClick(Sender: TObject);
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
    FTemperature: Single;
    FLastRequest: TProc;
    function NewMessage(const Text: string; IsUser: Boolean; UseBuffer: Boolean = True; IsAudio: Boolean = False): TFrameMessage;
    function NewMessageImage(IsUser: Boolean; Images: TArray<string>): TFrameMessage;
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
    procedure SetTemperature(const Value: Single);
    procedure RequestPrompt;
    procedure RequestImage(const Prompt: string);
    procedure RequestAudio(const AudioFile: string);
    procedure SetLastRequest(const Value: TProc);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MakeContentScreenshot: TBitmap;
    function SaveAsJson: TJSONObject;
    procedure LoadFromJson(JSON: TJSONObject);
    property API: IOpenAI read FAPI write SetAPI;
    property ChatId: string read FChatId write SetChatId;
    property Title: string read FTitle write SetTitle;
    property Mode: TWindowMode read FMode write SetMode;
    property LangSrc: string read FLangSrc write SetLangSrc;
    property Temperature: Single read FTemperature write SetTemperature;
    property IsImageMode: Boolean read FIsImageMode write SetIsImageMode;
    property LastRequest: TProc read FLastRequest write SetLastRequest;
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
      var Frame := NewMessage(Text, False, False);
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
    NewMessageImage(False, Images);
  finally
    Response.Free;
  end;
end;

function TFrameChat.NewMessageImage(IsUser: Boolean; Images: TArray<string>): TFrameMessage;
begin
  LayoutWelcome.Visible := False;
  Result := TFrameMessage.Create(VertScrollBoxChat);
  Result.Position.Y := VertScrollBoxChat.ContentBounds.Height;
  Result.Parent := VertScrollBoxChat;
  Result.Align := TAlignLayout.MostTop;
  TFrameMessage(Result).IsUser := IsUser;
  TFrameMessage(Result).Images := Images;
  Result.StartAnimate;
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

function TFrameChat.SaveAsJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    var JArray := TJSONArray.Create;
    Result.AddPair('chat_id', FChatId);
    Result.AddPair('temperature', TJSONNumber.Create(FTemperature));
    Result.AddPair('user_lang', FLangSrc);
    Result.AddPair('title', FTitle);
    Result.AddPair('items', JArray);
    for var Item in FBuffer do
    begin
      var JItem := TJSONObject.Create;
      JItem.AddPair('role', Item.Role.ToString);
      JItem.AddPair('content', Item.Content);
      JArray.Add(JItem);
    end;
  except
    //
  end;
end;

procedure TFrameChat.LoadFromJson(JSON: TJSONObject);
begin
  FChatId := JSON.GetValue('chat_id', TGUID.NewGuid.ToString);
  FTemperature := JSON.GetValue('chat_id', 0.0);
  FLangSrc := JSON.GetValue('user_lang', '');
  FTitle := JSON.GetValue('title', '');
  var JArray: TJSONArray;
  if JSON.TryGetValue<TJSONArray>('items', JArray) then
    for var JItem in JArray do
    begin
      var Item: TChatMessageBuild;
      Item.Role := TMessageRole.FromString(JItem.GetValue('role', 'user'));
      Item.Content := JItem.GetValue('content', '');
      //FBuffer.Add(Item);
      var Frame := TFrameMessage.Create(VertScrollBoxChat);
      Frame.Position.Y := VertScrollBoxChat.ContentBounds.Height;
      Frame.Parent := VertScrollBoxChat;
      Frame.Align := TAlignLayout.MostTop;
      Frame.IsUser := Item.Role = TMessageRole.User;
      Frame.IsAudio := False;
      Frame.Text := Item.Content;
      Frame.UpdateContentSize;
      Frame.StartAnimate;
    end;
end;

procedure TFrameChat.ScrollDown;
begin
  VertScrollBoxChat.ViewportPosition := TPointF.Create(0, VertScrollBoxChat.ContentBounds.Height);
end;

function TFrameChat.MakeContentScreenshot: TBitmap;

  function GetMaxBitmapRect: TRectF;
  var
    MaxDimensionSize: Integer;
  begin
    MaxDimensionSize := TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize);
    Result := TRectF.Create(0, 0, MaxDimensionSize, MaxDimensionSize);
  end;

var
  SceneScale: Single;
  BitmapRect: TRectF;
begin
  if Scene <> nil then
    SceneScale := Scene.GetSceneScale
  else
    SceneScale := 1;

  // TBitmap has limitation of size. It's a max texture size. If we takes screenshot of control, which exceeds this
  // limitation, we get "Bitmap size to big" exception. So we normalize size for avoiding it.
  BitmapRect := TRectF.Create(0, 0, VertScrollBoxChat.Content.ScrollBox.ContentBounds.Width * SceneScale, VertScrollBoxChat.Content.ScrollBox.ContentBounds.Height * SceneScale);
  BitmapRect := BitmapRect.PlaceInto(GetMaxBitmapRect);

  Result := TBitmap.Create(Round(BitmapRect.Width), Round(BitmapRect.Height));
  Result.BitmapScale := SceneScale;
  Result.Clear(0);
  if Result.Canvas.BeginScene then
  try
    VertScrollBoxChat.Content.PaintTo(Result.Canvas, TRectF.Create(0, 0, Result.Width / SceneScale, Result.Height / SceneScale));
  finally
    Result.Canvas.EndScene;
  end;
end;

procedure TFrameChat.ButtonSettingsClick(Sender: TObject);
begin
  TFrameChatSettings.Execute(Self,
    procedure(Frame: TFrameChatSettings)
    begin
      Frame.EditLangSrc.Text := LangSrc;
      Frame.TrackBarTemp.Value := Temperature * 10;
    end,
    procedure(Frame: TFrameChatSettings; Success: Boolean)
    begin
      MemoQuery.SetFocus;
      if not Success then
        Exit;
      LangSrc := Frame.EditLangSrc.Text;
      Temperature := Frame.TrackBarTemp.Value / 10;
    end);
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
  RequestAudio(AudioFile);
end;

procedure TFrameChat.RequestAudio(const AudioFile: string);
begin
  SetTyping(True);
  ScrollDown;
  LastRequest := nil;
  TTask.Run(
    procedure
    begin
      try
        var Audio := API.Audio.CreateTranscription(
          procedure(Params: TAudioTranscription)
          begin
            Params.&File(AudioFile);
            Params.Temperature(Temperature / 2);
            Params.Language('ru');
          end);
        TThread.Queue(nil,
          procedure
          begin
            AppendAudio(Audio);
          end);
      except
        on E: OpenAIException do
        begin
          ShowError(E.Message);
          LastRequest :=
            procedure
            begin
              RequestAudio(AudioFile);
            end;
        end;
        on E: Exception do
        begin
          ShowError(E.Message);
          LastRequest :=
            procedure
            begin
              RequestAudio(AudioFile);
            end;
        end;
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

procedure TFrameChat.ButtonRetryClick(Sender: TObject);
begin
  if Assigned(FLastRequest) then
    FLastRequest;
end;

procedure TFrameChat.SendRequestImage;
begin
  if FIsTyping then
    Exit;
  var Prompt := MemoQuery.Text;
  if Prompt.IsEmpty then
    Exit;
  MemoQuery.Text := '';
  NewMessage(Prompt, True, False);
  RequestImage(Prompt);
end;

procedure TFrameChat.RequestImage(const Prompt: string);
begin
  SetTyping(True);
  ScrollDown;
  LastRequest := nil;
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
        begin
          ShowError(E.Message);
          LastRequest :=
            procedure
            begin
              RequestImage(Prompt);
            end;
        end;
        on E: Exception do
        begin
          ShowError(E.Message);
          LastRequest :=
            procedure
            begin
              RequestImage(Prompt);
            end;
        end;
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
  if not Prompt.StartsWith('/system ') then
    RequestPrompt;
end;

procedure TFrameChat.RequestPrompt;
begin
  SetTyping(True);
  ScrollDown;
  LastRequest := nil;
  TTask.Run(
    procedure
    begin
      try
        var Completions := API.Chat.Create(
          procedure(Params: TChatParams)
          begin
            //Params.Model('gpt-4');
            Params.Messages(FBuffer.ToArray);
            Params.MaxTokens(MAX_TOKENS);
            Params.Temperature(Temperature);
            Params.User(FChatId);
          end);
        TThread.Queue(nil,
          procedure
          begin
            AppendMessages(Completions);
          end);
      except
        on E: OpenAIException do
        begin
          ShowError(E.Message);
          LastRequest := RequestPrompt;
        end;
        on E: Exception do
        begin
          ShowError(E.Message);
          LastRequest := RequestPrompt;
        end;
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
  LastRequest := nil;
  FBuffer := TChatHistory.Create;
  FPool := TThreadPool.Create;
  LangSrc := '';
  Temperature := 0.2;
  Name := '';
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
  TThread.ForceQueue(nil,
    procedure
    begin
      var H: Single :=
        LayoutSend.Padding.Top + LayoutSend.Padding.Bottom +
        MemoQuery.ContentBounds.Height +
        LayoutQuery.Padding.Top + LayoutQuery.Padding.Bottom;
      LayoutSend.Height := Max(LayoutSend.TagFloat, Min(H, 400));
    end);
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
  if UseBuffer then
  begin
    var MessageTag := TGUID.NewGuid.ToString;
    if IsUser then
    begin
      if Text.StartsWith('/system ') then
      begin
        var AText := Text.Replace('/system ', '', []);
        if not AText.IsEmpty then
          FBuffer.New(TMessageRole.System, ProcText(AText, IsUser), MessageTag);
      end;
      FBuffer.New(TMessageRole.User, ProcText(Text, IsUser), MessageTag);
    end
    else
      FBuffer.New(TMessageRole.Assistant, ProcText(Text, IsUser), MessageTag);
  end;
  LayoutWelcome.Visible := False;
  Result := TFrameMessage.Create(VertScrollBoxChat);
  Result.Position.Y := VertScrollBoxChat.ContentBounds.Height;
  Result.Parent := VertScrollBoxChat;
  Result.Align := TAlignLayout.MostTop;
  Result.IsUser := IsUser;
  Result.IsAudio := IsAudio;
  Result.Text := Text;
  Result.UpdateContentSize;
  Result.StartAnimate;
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

procedure TFrameChat.SetLastRequest(const Value: TProc);
begin
  FLastRequest := Value;
  LayoutRetry.Visible := Assigned(FLastRequest);
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
      end;
    wmFull:
      begin
        LayoutSend.TagFloat := 170;
        VertScrollBoxChat.Padding.Bottom := 170;
        LayoutSend.Height := 170;
        LineBorder.Visible := False;
        LayoutSend.Padding.Rect := TRectF.Create(0, 80, 0, 40);
        RectangleSendBG.Fill.Kind := TBrushKind.Gradient;
      end;
  end;
  FlowLayoutWelcomeResize(nil);
end;

procedure TFrameChat.SetTemperature(const Value: Single);
begin
  FTemperature := Value;
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


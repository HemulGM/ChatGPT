unit ChatGPT.FrameChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.Memo.Style, FMX.ScrollBox, FMX.Memo, OpenAI, OpenAI.Completions,
  ChatGPT.FrameMessage, ChatGPT.Classes, System.Threading, FMX.Edit, FMX.ImgList,
  OpenAI.Chat, System.Generics.Collections, OpenAI.Audio,
  OpenAI.Utils.ChatHistory, OpenAI.Images, ChatGPT.ChatSettings, System.JSON,
  FMX.Effects, FMX.ListBox;

type
  TButton = class(FMX.StdCtrls.TButton)
  public
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  end;

  TLabel = class(FMX.StdCtrls.TLabel)
  public
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  end;

  TVertScrollBox = class(FMX.Layouts.TVertScrollBox)
  private
    FViewPositionY: Single;
    procedure SetViewPositionY(const Value: Single);
  published
    property ViewPositionY: Single read FViewPositionY write SetViewPositionY;
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
    LabelWelcomeTitle: TLabel;
    FlowLayoutWelcome: TFlowLayout;
    LayoutExampleTitle: TLayout;
    PathExaFull: TPath;
    ButtonExample3: TButton;
    ButtonExample2: TButton;
    ButtonExample1: TButton;
    LayoutCapabilitiesTitle: TLayout;
    PathCapFull: TPath;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    LayoutLimitationsTitle: TLayout;
    PathLimFull: TPath;
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
    LayoutRetry: TLayout;
    ButtonRetry: TButton;
    ShadowEffect1: TShadowEffect;
    TimerUpdateTextSize: TTimer;
    LabelSendTip: TLabel;
    Layout5: TLayout;
    Label2: TLabel;
    PathExaCompact: TPath;
    Layout6: TLayout;
    Label1: TLabel;
    PathCapCompact: TPath;
    Layout7: TLayout;
    Label11: TLabel;
    PathLimCompact: TPath;
    Layout8: TLayout;
    ButtonSettings: TButton;
    Path9: TPath;
    LayoutScrollDown: TLayout;
    ButtonScrollDown: TButton;
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
    procedure TimerUpdateTextSizeTimer(Sender: TObject);
    procedure ButtonScrollDownClick(Sender: TObject);
    procedure VertScrollBoxChatViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
  private
    class var
      FChatIdCount: Integer;
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
    FMenuItem: TListBoxItem;
    FIsFirstMessage: Boolean;
    FPresencePenalty: Single;
    FModel: string;
    FMaxTokens: Integer;
    FMaxTokensQuery: Integer;
    FFrequencyPenalty: Single;
    function NewMessage(const Text: string; IsUser: Boolean; UseBuffer: Boolean = True; IsAudio: Boolean = False): TFrameMessage;
    function NewMessageImage(IsUser: Boolean; Images: TArray<string>): TFrameMessage;
    procedure ClearChat;
    procedure SetTyping(const Value: Boolean);
    procedure SetAPI(const Value: IOpenAI);
    procedure SetChatId(const Value: string);
    procedure ShowError(const Text: string);
    procedure AppendMessages(Response: TChat); overload;
    procedure AppendMessages(Response: TImageGenerations); overload;
    procedure ScrollDown(Animate: Boolean = False);
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
    procedure SetMenuItem(const Value: TListBoxItem);
    procedure UpdateMenuTitle(const Text: string);
    class function NextChatId: Integer; static;
    procedure SetFrequencyPenalty(const Value: Single);
    procedure SetMaxTokens(const Value: Integer);
    procedure SetMaxTokensQuery(const Value: Integer);
    procedure SetModel(const Value: string);
    procedure SetPresencePenalty(const Value: Single);
    procedure ChatToUp;
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
    property MaxTokens: Integer read FMaxTokens write SetMaxTokens;
    property MaxTokensQuery: Integer read FMaxTokensQuery write SetMaxTokensQuery;
    property PresencePenalty: Single read FPresencePenalty write SetPresencePenalty;
    property FrequencyPenalty: Single read FFrequencyPenalty write SetFrequencyPenalty;
    property Model: string read FModel write SetModel;
    property IsImageMode: Boolean read FIsImageMode write SetIsImageMode;
    property LastRequest: TProc read FLastRequest write SetLastRequest;
    property MenuItem: TListBoxItem read FMenuItem write SetMenuItem;
    property IsFirstMessage: Boolean read FIsFirstMessage write FIsFirstMessage;
  end;

const
  MAX_TOKENS = 1024;
  MODEL_TOKENS_LIMIT = 4096;

implementation

uses
  FMX.Ani, System.Math, OpenAI.API, ChatGPT.Translate, System.IOUtils,
  ChatGPT.Overlay, FMX.BehaviorManager;

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
  ChatToUp;
  LayoutWelcome.Visible := False;
  Result := TFrameMessage.Create(VertScrollBoxChat);
  Result.SetMode(FMode);
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

    Result.AddPair('frequency_penalty', TJSONNumber.Create(FrequencyPenalty));
    Result.AddPair('presence_penalty', TJSONNumber.Create(PresencePenalty));
    Result.AddPair('max_tokens', TJSONNumber.Create(MaxTokens));
    Result.AddPair('max_tokens_query', TJSONNumber.Create(MaxTokensQuery));
    Result.AddPair('model', Model);

    for var Item in FBuffer do
    begin
      var JItem := TJSONObject.Create;
      JItem.AddPair('id', Item.Tag);
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
  var ItemCount: Integer := 0;
  var LastRoleIsUser: Boolean := False;
  FChatId := JSON.GetValue('chat_id', TGUID.NewGuid.ToString);
  FTemperature := JSON.GetValue('temperature', 0.0);
  FLangSrc := JSON.GetValue('user_lang', '');
  FTitle := JSON.GetValue('title', '');
  FrequencyPenalty := JSON.GetValue<Single>('frequency_penalty', 0.0);
  PresencePenalty := JSON.GetValue<Single>('presence_penalty', 0.0);
  Model := JSON.GetValue('model', '');
  MaxTokens := JSON.GetValue<Integer>('max_tokens', 0);
  MaxTokensQuery := JSON.GetValue<Integer>('max_tokens_query', 0);
  var JArray: TJSONArray;
  if JSON.TryGetValue<TJSONArray>('items', JArray) then
    for var JItem in JArray do
    begin
      var Item: TChatMessageBuild;
      Item.Role := TMessageRole.FromString(JItem.GetValue('role', 'user'));
      Item.Content := JItem.GetValue('content', '');
      Item.Tag := JItem.GetValue('id', TGUID.NewGuid.ToString);
      FBuffer.Add(Item);
      var Frame := TFrameMessage.Create(VertScrollBoxChat);
      Frame.Position.Y := VertScrollBoxChat.ContentBounds.Height;
      VertScrollBoxChat.AddObject(Frame);
      Frame.Align := TAlignLayout.MostTop;
      Frame.IsUser := Item.Role = TMessageRole.User;
      LastRoleIsUser := Frame.IsUser;
      Frame.IsAudio := False;
      Frame.Text := Item.Content;
      Frame.SetMode(FMode);
      Frame.UpdateContentSize;
      Inc(ItemCount);
    end;
  if ItemCount > 0 then
  begin
    LayoutWelcome.Visible := False;
    IsFirstMessage := False;
    ScrollDown;
    if LastRoleIsUser then
      LastRequest := RequestPrompt;
  end;
end;

procedure TFrameChat.ScrollDown(Animate: Boolean = False);
begin
  VertScrollBoxChat.RecalcSize;
  if Animate then
    TAnimator.AnimateFloat(VertScrollBoxChat, 'ViewPositionY', VertScrollBoxChat.ContentBounds.Height - VertScrollBoxChat.Height, 1, TAnimationType.out, TInterpolationType.Circular)
  else
    VertScrollBoxChat.ViewportPosition := TPointF.Create(0, VertScrollBoxChat.ContentBounds.Height - VertScrollBoxChat.Height);
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
      Frame.Mode := FMode;
      Frame.EditLangSrc.Text := LangSrc;
      Frame.TrackBarTemp.Value := Temperature * 10;
      Frame.TrackBarPP.Value := PresencePenalty * 10;
      Frame.TrackBarFP.Value := FrequencyPenalty * 10;
      Frame.EditMaxTokens.Text := MaxTokens.ToString;
      Frame.EditQueryMaxToken.Text := MaxTokensQuery.ToString;
      Frame.ComboEditModel.Text := Model;
    end,
    procedure(Frame: TFrameChatSettings; Success: Boolean)
    begin
      MemoQuery.SetFocus;
      if not Success then
        Exit;
      LangSrc := Frame.EditLangSrc.Text;
      Temperature := Frame.TrackBarTemp.Value / 10;
      PresencePenalty := Frame.TrackBarPP.Value / 10;
      FrequencyPenalty := Frame.TrackBarFP.Value / 10;
      MaxTokens := StrToIntDef(Frame.EditMaxTokens.Text, 0);
      MaxTokensQuery := StrToIntDef(Frame.EditQueryMaxToken.Text, 0);
      Model := Frame.ComboEditModel.Text;
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
  if (not Prompt.StartsWith('/system ')) and (not Prompt.StartsWith('/assistant ')) and (not Prompt.StartsWith('/user ')) then
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
            if not Model.IsEmpty then
              Params.Model(Model);
            if PresencePenalty <> 0 then
              Params.PresencePenalty(PresencePenalty);
            if FrequencyPenalty <> 0 then
              Params.FrequencyPenalty(FrequencyPenalty);
            Params.Messages(FBuffer.ToArray);
            Params.MaxTokens(FBuffer.MaxTokensForQuery);
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

procedure TFrameChat.ButtonScrollDownClick(Sender: TObject);
begin
  ScrollDown(True);
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

class function TFrameChat.NextChatId: Integer;
begin
  Inc(FChatIdCount);
  Result := FChatIdCount;
end;

constructor TFrameChat.Create(AOwner: TComponent);
begin
  inherited;
  ChatId := TGUID.NewGuid.ToString;
  Title := 'New chat ' + NextChatId.ToString;
  FIsFirstMessage := True;
  LastRequest := nil;
  FBuffer := TChatHistory.Create;
  FBuffer.MaxTokensForQuery := MAX_TOKENS;
  FBuffer.MaxTokensOfModel := MODEL_TOKENS_LIMIT;
  FPool := TThreadPool.Create;
  LangSrc := '';
  Temperature := 0.2;
  Name := '';
  VertScrollBoxChat.AniCalculations.Animation := True;
  MemoQuery.ScrollAnimation := TBehaviorBoolean.True;
  SetTyping(False);
  ClearChat;
  IsImageMode := False;
end;

destructor TFrameChat.Destroy;
begin
  FMenuItem := nil;
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
  LabelSendTip.Visible := MemoQuery.Text.IsEmpty;
  TimerUpdateTextSize.Enabled := False;
  TimerUpdateTextSize.Enabled := True;
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

procedure TFrameChat.UpdateMenuTitle(const Text: string);
begin
  if not Assigned(FMenuItem) then
    Exit;
  if not FMenuItem.StylesData['changed_title'].AsBoolean then
  begin
    FTitle := Text.Substring(0, 50);
    FMenuItem.Text := FTitle;
  end;
end;

procedure TFrameChat.VertScrollBoxChatViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  VertScrollBoxChat.FViewPositionY := NewViewportPosition.Y;
  LayoutScrollDown.Visible := NewViewportPosition.Y < VertScrollBoxChat.ContentBounds.Height - VertScrollBoxChat.Height - 100;
end;

procedure TFrameChat.ChatToUp;
begin
  if not Assigned(FMenuItem) then
    Exit;
  FMenuItem.Index := 0;
  FMenuItem.IsSelected := True;
end;

function TFrameChat.NewMessage(const Text: string; IsUser: Boolean; UseBuffer: Boolean; IsAudio: Boolean): TFrameMessage;
begin
  ChatToUp;
  if IsUser and IsFirstMessage then
  begin
    IsFirstMessage := False;
    UpdateMenuTitle(Text);
  end;

  var AppendText := Text;
  if UseBuffer then
  begin
    var MessageTag := TGUID.NewGuid.ToString;
    if IsUser then
    begin
      if AppendText.StartsWith('/system ') then
      begin
        AppendText := AppendText.Replace('/system ', '', []);
        AppendText := ProcText(AppendText, IsUser);
        FBuffer.New(TMessageRole.System, AppendText, MessageTag);
      end
      else if AppendText.StartsWith('/user ') then
      begin
        AppendText := AppendText.Replace('/user ', '', []);
        AppendText := ProcText(AppendText, IsUser);
        FBuffer.New(TMessageRole.User, AppendText, MessageTag);
      end
      else if AppendText.StartsWith('/assistant ') then
      begin
        AppendText := AppendText.Replace('/assistant ', '', []);
        AppendText := ProcText(AppendText, IsUser);
        FBuffer.New(TMessageRole.Assistant, AppendText, MessageTag);
      end
      else
      begin
        AppendText := ProcText(AppendText, IsUser);
        FBuffer.New(TMessageRole.User, AppendText, MessageTag);
      end;
    end
    else
    begin
      AppendText := ProcText(AppendText, IsUser);
      FBuffer.New(TMessageRole.Assistant, AppendText, MessageTag);
    end;
  end;
  LayoutWelcome.Visible := False;
  Result := TFrameMessage.Create(VertScrollBoxChat);
  Result.Position.Y := VertScrollBoxChat.ContentBounds.Height;
  Result.Parent := VertScrollBoxChat;
  Result.Align := TAlignLayout.MostTop;
  Result.IsUser := IsUser;
  Result.IsAudio := IsAudio;
  Result.Text := AppendText;
  Result.SetMode(FMode);
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

procedure TFrameChat.SetFrequencyPenalty(const Value: Single);
begin
  FFrequencyPenalty := Value;
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

procedure TFrameChat.SetMaxTokens(const Value: Integer);
begin
  FMaxTokens := Value;
  FBuffer.MaxTokensOfModel := FMaxTokens;
end;

procedure TFrameChat.SetMaxTokensQuery(const Value: Integer);
begin
  FMaxTokensQuery := Value;
  FBuffer.MaxTokensForQuery := FMaxTokensQuery;
end;

procedure TFrameChat.SetMenuItem(const Value: TListBoxItem);
begin
  FMenuItem := Value;
end;

procedure TFrameChat.SetMode(const Value: TWindowMode);
begin
  FMode := Value;
  for var Control in Controls do
    if Control is TFrameOveraly then
    begin
      var Frame := Control as TFrameOveraly;
      Frame.Mode := FMode;
    end;
  for var Control in VertScrollBoxChat do
    if Control is TFrameMessage then
    begin
      var Frame := Control as TFrameMessage;
      Frame.SetMode(FMode);
    end;

  case FMode of
    wmCompact:
      begin
        {$IFNDEF ANDROID OR IOS OR IOS64}
        LayoutSend.Margins.Right := 11;
        {$ELSE}
        LayoutSend.Margins.Right := 0;
        {$ENDIF}
        LabelWelcomeTitle.Margins.Top := 20;
        LayoutSend.TagFloat := 100;
        VertScrollBoxChat.Padding.Bottom := 100;
        LayoutSend.Height := 100;
        LayoutRetry.Margins.Bottom := 0;
        LayoutScrollDown.Margins.Bottom := 0;
        LineBorder.Visible := True;
        PathExaCompact.Visible := True;
        PathExaFull.Visible := False;
        PathCapCompact.Visible := True;
        PathCapFull.Visible := False;
        PathLimCompact.Visible := True;
        PathLimFull.Visible := False;
        LayoutSend.Padding.Rect := TRectF.Create(0, 10, 0, 40);
        RectangleSendBG.Fill.Kind := TBrushKind.Solid;
        RectangleSendBG.Fill.Color := $FF343541;
      end;
    wmFull:
      begin
        {$IFNDEF ANDROID OR IOS OR IOS64}
        LayoutSend.Margins.Right := 11;
        {$ELSE}
        LayoutSend.Margins.Right := 0;
        {$ENDIF}
        LabelWelcomeTitle.Margins.Top := 188;
        LayoutSend.TagFloat := 170;
        LayoutRetry.Margins.Bottom := -70;
        LayoutScrollDown.Margins.Bottom := -70;
        VertScrollBoxChat.Padding.Bottom := 170;
        LayoutSend.Height := 170;
        LineBorder.Visible := False;
        PathExaCompact.Visible := False;
        PathExaFull.Visible := True;
        PathCapCompact.Visible := False;
        PathCapFull.Visible := True;
        PathLimCompact.Visible := False;
        PathLimFull.Visible := True;
        LayoutSend.Padding.Rect := TRectF.Create(0, 80, 0, 40);
        RectangleSendBG.Fill.Kind := TBrushKind.Gradient;
      end;
  end;
  FlowLayoutWelcomeResize(nil);
end;

procedure TFrameChat.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TFrameChat.SetPresencePenalty(const Value: Single);
begin
  FPresencePenalty := Value;
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

procedure TFrameChat.TimerUpdateTextSizeTimer(Sender: TObject);
begin
  TimerUpdateTextSize.Enabled := False;
  var H: Single :=
    LayoutSend.Padding.Top + LayoutSend.Padding.Bottom +
    MemoQuery.ContentBounds.Height +
    LayoutQuery.Padding.Top + LayoutQuery.Padding.Bottom;
  LayoutSend.Height := Max(LayoutSend.TagFloat, Min(H, 400));
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

{ TVertScrollBox }

procedure TVertScrollBox.SetViewPositionY(const Value: Single);
begin
  FViewPositionY := Value;
  ViewportPosition := TPointF.Create(ViewportPosition.X, FViewPositionY);
end;

end.


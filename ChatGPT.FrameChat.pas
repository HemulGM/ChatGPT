unit ChatGPT.FrameChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, OpenAI, OpenAI.Completions, System.Threading,
  FMX.Edit, FMX.ImgList;

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
    ButtonSend: TButton;
    Path1: TPath;
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
    Layout2: TLayout;
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
  private
    FAPI: TOpenAI;
    FChatId: string;
    FPool: TThreadPool;
    FTitle: string;
    FMode: TWindowMode;
    FLangSrc: string;
    FIsTyping: Boolean;
    procedure NewMessage(const Text: string; IsUser: Boolean);
    procedure ClearChat;
    procedure SetTyping(const Value: Boolean);
    procedure SetAPI(const Value: TOpenAI);
    procedure SetChatId(const Value: string);
    procedure ShowError(const Text: string);
    procedure AppendMessages(Response: TCompletions);
    procedure ScrollDown;
    procedure SetTitle(const Value: string);
    procedure SetMode(const Value: TWindowMode);
    function ProcText(const Text: string; FromUser: Boolean): string;
    procedure SetLangSrc(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property API: TOpenAI read FAPI write SetAPI;
    property ChatId: string read FChatId write SetChatId;
    property Title: string read FTitle write SetTitle;
    property Mode: TWindowMode read FMode write SetMode;
    property LangSrc: string read FLangSrc write SetLangSrc;
  end;

implementation

uses
  FMX.Ani, System.Math, ChatGPT.FrameMessage, OpenAI.API, ChatGPT.Translate,
  ChatGPT.Main;

{$R *.fmx}

procedure TFrameChat.ShowError(const Text: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      ShowMessage(Text);
    end);
end;

procedure TFrameChat.AppendMessages(Response: TCompletions);
begin
  try
    for var Item in Response.Choices do
      NewMessage(Item.Text, False);
  finally
    Response.Free;
  end;
end;

procedure TFrameChat.ScrollDown;
begin
  VertScrollBoxChat.ViewportPosition := TPointF.Create(0, VertScrollBoxChat.ContentBounds.Height);
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

procedure TFrameChat.ButtonSendClick(Sender: TObject);
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
        var Completions := API.Completion.Create(
          procedure(Params: TCompletionParams)
          begin
            Params.Prompt(ProcText(Prompt, True));
            Params.MaxTokens(1024);
            Params.User(FChatId);
          end);
        if not LangSrc.IsEmpty then
          for var Item in Completions.Choices do
            Item.Text := ProcText(Item.Text, False);
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
  FPool := TThreadPool.Create;
  LangSrc := '';
  Name := '';
  LayoutTranslateSet.Opacity := 0;
  LayoutTranslateSet.Position.Y := 0;
  VertScrollBoxChat.AniCalculations.Animation := True;
  SetTyping(False);
  ClearChat;
  {$IFDEF ANDROID}
  ButtonTranslate.Visible := False;
  {$ENDIF}
end;

destructor TFrameChat.Destroy;
begin
  FPool.Free;
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
      begin
        W := FlowLayoutWelcome.Width;
      end;
    wmFull:
      begin
        W := Trunc(FlowLayoutWelcome.Width / FlowLayoutWelcome.ControlsCount);
      end;
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

procedure TFrameChat.NewMessage(const Text: string; IsUser: Boolean);
begin
  LayoutWelcome.Visible := False;
  var Frame := TFrameMessage.Create(VertScrollBoxChat);
  Frame.Position.Y := VertScrollBoxChat.ContentBounds.Height;
  Frame.Parent := VertScrollBoxChat;
  Frame.Align := TAlignLayout.MostTop;
  Frame.Text := Text;
  Frame.IsUser := IsUser;
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

procedure TFrameChat.SetAPI(const Value: TOpenAI);
begin
  FAPI := Value;
end;

procedure TFrameChat.SetChatId(const Value: string);
begin
  FChatId := Value;
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


unit ChatGPT.FrameChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, OpenAI, OpenAI.Completions, System.Threading,
  FMX.Edit;

type
  TWindowMode = (wmCompact, wmFull);

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
    LayoutWelcome: TLayout;
    Layout2: TLayout;
    ButtonTranslate: TButton;
    Path2: TPath;
    LayoutTranslateSet: TLayout;
    EditLangSrc: TEdit;
    ClearEditButton1: TClearEditButton;
    Path4: TPath;
    Label1: TLabel;
    Rectangle3: TRectangle;
    procedure LayoutSendResize(Sender: TObject);
    procedure MemoQueryChange(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure TimerTypingTimer(Sender: TObject);
    procedure LayoutTypingResize(Sender: TObject);
    procedure MemoQueryKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ButtonTranslateClick(Sender: TObject);
    procedure EditLangSrcChangeTracking(Sender: TObject);
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
  FMX.Ani, System.Math, ChatGPT.FrameMessage, OpenAI.API, ChatGPT.Translate;

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
  while VertScrollBoxChat.Content.ControlsCount > 0 do
    VertScrollBoxChat.Content.Controls[0].Free;
  LayoutTyping.Parent := VertScrollBoxChat;
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

procedure TFrameChat.LayoutSendResize(Sender: TObject);
begin
  LayoutQuery.Width := Min(768, LayoutSend.Width - 48);
end;

procedure TFrameChat.LayoutTypingResize(Sender: TObject);
begin
  LayoutTypingContent.Width := Min(LayoutTyping.Width - (LayoutTyping.Padding.Left + LayoutTyping.Padding.Right), 650);
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

end.


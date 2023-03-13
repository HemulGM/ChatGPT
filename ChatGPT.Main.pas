unit ChatGPT.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, OpenAI,
  FMX.Objects, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, ChatGPT.FrameChat,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Effects, FMX.Filter.Effects,
  FMX.Edit;

type
  TFormMain = class(TForm)
    LayoutChats: TLayout;
    Rectangle1: TRectangle;
    StyleBook: TStyleBook;
    SVGIconImageList: TSVGIconImageList;
    LayoutHead: TLayout;
    ButtonMenu: TButton;
    ButtonNewChatCompact: TButton;
    LabelChatName: TLabel;
    LayoutMenuContent: TLayout;
    RectangleMenu: TRectangle;
    LayoutMenu: TLayout;
    ButtonNewChat: TButton;
    ListBoxChatList: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    RectangleMenuBG: TRectangle;
    ButtonCloseMenu: TButton;
    LayoutMenuContainer: TLayout;
    Layout1: TLayout;
    ButtonDiscord: TButton;
    Line1: TLine;
    ButtonClear: TButton;
    ButtonFAQ: TButton;
    LayoutChatsBox: TLayout;
    Layout2: TLayout;
    ButtonClearConfirm: TButton;
    ButtonClearCancel: TButton;
    procedure ButtonNewChatClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonCloseMenuClick(Sender: TObject);
    procedure ButtonNewChatCompactClick(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Single);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonClearConfirmClick(Sender: TObject);
    procedure ButtonClearCancelClick(Sender: TObject);
    procedure ButtonDiscordClick(Sender: TObject);
    procedure ButtonFAQClick(Sender: TObject);
  private
    FOpenAI: TOpenAIComponent;
    FMode: TWindowMode;
    FChatIdCount: Integer;
    procedure SetMode(const Value: TWindowMode);
    procedure UpdateMode;
    function NextChatId: Integer;
    procedure SelectChat(const ChatId: string);
    procedure FOnChatItemClick(Sender: TObject);
    {$HINTS OFF}
    procedure FOnChatItemTap(Sender: TObject; const Point: TPointF);
    {$HINTS ON}
    function CreateChat: string;
    procedure CloseMenu;
    procedure Clear;
    procedure ShowClearConfirm;
    procedure HideClearConfirm;
    procedure FOnChatEditClick(Sender: TObject);
  public
    property OpenAI: TOpenAIComponent read FOpenAI;
    constructor Create(AOwner: TComponent); override;
    property Mode: TWindowMode read FMode write SetMode;
  end;

const
  API_TOKEN = {$include MY_TOKEN.txt};
  URL_DISCORD = 'https://discord.com/invite/openai';
  URL_FAQ = 'https://help.openai.com/en/collections/3742473-chatgpt';

var
  FormMain: TFormMain;

const
  AniInterpolation = TInterpolationType.Quadratic;

implementation

uses
  {$IFDEF ANDROID}
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ShellAPI,
  {$ENDIF}
  FMX.Ani, System.Math, System.Rtti, FMX.Utils, FMX.DialogService;

{$R *.fmx}

{$IFDEF ANDROID}
procedure OpenUrl(const URL: string);
begin
  TAndroidHelper.Context.startActivity(
    TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW, StrToJURI(URL)));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

procedure OpenUrl(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), nil, nil, 0);
end;
{$ENDIF}

{ TFormMain }

procedure TFormMain.ShowClearConfirm;
begin
  ButtonClear.Text := 'Confirm clear';
  ButtonClearConfirm.Visible := True;
  ButtonClearCancel.Visible := True;
end;

procedure TFormMain.HideClearConfirm;
begin
  ButtonClear.Text := 'Clear conversations';
  ButtonClearConfirm.Visible := False;
  ButtonClearCancel.Visible := False;
end;

procedure TFormMain.ButtonClearCancelClick(Sender: TObject);
begin
  HideClearConfirm;
end;

procedure TFormMain.ButtonClearClick(Sender: TObject);
begin
  ShowClearConfirm;
end;

procedure TFormMain.ButtonClearConfirmClick(Sender: TObject);
begin
  Clear;
end;

procedure TFormMain.ButtonCloseMenuClick(Sender: TObject);
begin
  CloseMenu;
end;

procedure TFormMain.ButtonDiscordClick(Sender: TObject);
begin
  OpenUrl(URL_DISCORD);
end;

procedure TFormMain.ButtonFAQClick(Sender: TObject);
begin
  OpenUrl(URL_FAQ);
end;

procedure TFormMain.ButtonMenuClick(Sender: TObject);
begin
  RectangleMenuBG.Opacity := 0;
  LayoutMenuContainer.Opacity := 0;
  LayoutMenuContainer.Margins.Left := -(LayoutMenuContainer.Width - 45);
  ButtonCloseMenu.Opacity := 0;
  LayoutMenuContent.Visible := True;
  TAnimator.AnimateFloat(RectangleMenuBG, 'Opacity', 1, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloat(ButtonCloseMenu, 'Opacity', 1, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloat(LayoutMenuContainer, 'Opacity', 1, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloat(LayoutMenuContainer, 'Margins.Left', 0, 0.2, TAnimationType.InOut, AniInterpolation);
end;

function TFormMain.NextChatId: Integer;
begin
  Inc(FChatIdCount);
  Result := FChatIdCount;
end;

procedure TFormMain.CloseMenu;
begin
  TAnimator.AnimateFloat(RectangleMenuBG, 'Opacity', 0, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloat(ButtonCloseMenu, 'Opacity', 0, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloat(LayoutMenuContainer, 'Opacity', 0, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloatWait(LayoutMenuContainer, 'Margins.Left', -(LayoutMenuContainer.Width - 45), 0.2, TAnimationType.InOut, AniInterpolation);
  LayoutMenuContent.Visible := False;
end;

function TFormMain.CreateChat: string;
begin
  Result := TGUID.NewGuid.ToString;
  var ChatTitle := 'New chat ' + NextChatId.ToString;

  var Frame := TFrameChat.Create(LayoutChatsBox);
  Frame.Align := TAlignLayout.Client;
  Frame.Parent := LayoutChatsBox;
  Frame.API := OpenAI;
  Frame.ChatId := Result;
  Frame.Title := ChatTitle;
  Frame.Mode := Mode;

  var ItemList := TListBoxItem.Create(ListBoxChatList);
  ItemList.HitTest := True;
  {$IFDEF ANDROID}
  ItemList.OnTap := FOnChatItemTap;
  {$ELSE}
  ItemList.OnClick := FOnChatItemClick;
  {$ENDIF}
  ItemList.Margins.Bottom := 8;
  ItemList.Text := ChatTitle;
  ItemList.TagString := Result;
  ItemList.ImageIndex := 1;
  ItemList.DisableDisappear := True;
  ItemList.StylesData['edit.OnClick'] := TValue.From<TNotifyEvent>(FOnChatEditClick);
  ListBoxChatList.AddObject(ItemList);
  ItemList.ApplyStyleLookup;
end;

procedure TFormMain.ButtonNewChatClick(Sender: TObject);
begin
  SelectChat(CreateChat);
  if Mode = wmCompact then
    CloseMenu;
end;

procedure TFormMain.ButtonNewChatCompactClick(Sender: TObject);
begin
  SelectChat(CreateChat);
end;

procedure TFormMain.FOnChatEditClick(Sender: TObject);
var
  Button: TButton absolute Sender;
  ListItem: TListBoxItem;
begin
  if TFMXObjectHelper.FindNearestParentOfClass<TListBoxItem>(Button, ListItem) then
  begin
    TDialogService.InputQuery('Chat name', ['Name'], [ListItem.Text],
      procedure(const AResult: TModalResult; const AValues: array of string)
      begin
        if AResult = mrOk then
          ListItem.Text := AValues[0];
      end);
  end;
end;

procedure TFormMain.FOnChatItemClick(Sender: TObject);
var
  Item: TListBoxItem absolute Sender;
begin
  SelectChat(Item.TagString);
  if Mode = wmCompact then
    CloseMenu;
end;

procedure TFormMain.FOnChatItemTap(Sender: TObject; const Point: TPointF);
begin
  FOnChatItemClick(Sender);
end;

procedure TFormMain.SelectChat(const ChatId: string);
begin
  for var Control in LayoutChatsBox.Controls do
    if Control is TFrameChat then
    begin
      var Frame := Control as TFrameChat;
      Frame.Visible := Frame.ChatId = ChatId;
    end;
  for var i := 0 to Pred(ListBoxChatList.Count) do
    if ListBoxChatList.ListItems[i].TagString = ChatId then
    begin
      ListBoxChatList.ListItems[i].IsSelected := True;
      LabelChatName.Text := ListBoxChatList.ListItems[i].Text;
      Exit;
    end;
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FChatIdCount := 0;
  FOpenAI := TOpenAIComponent.Create(Self);
  FOpenAI.Token := API_TOKEN;
  ListBoxChatList.AniCalculations.Animation := True;
  Clear;
  FMode := wmFull;
  UpdateMode;
end;

procedure TFormMain.Clear;
begin
  FChatIdCount := 0;
  HideClearConfirm;
  ListBoxChatList.Clear;
  while LayoutChatsBox.ControlsCount > 0 do
    LayoutChatsBox.Controls[0].Free;
  SelectChat(CreateChat);
end;

procedure TFormMain.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Single);
begin
  FormResize(Sender);
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  LayoutMenuContainer.Width := Min(320, ClientWidth - 45);
  if ClientWidth < 768 then
    Mode := wmCompact
  else
    Mode := wmFull;
end;

procedure TFormMain.FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  Padding.Bottom := 0;
end;

procedure TFormMain.FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  Padding.Bottom := Bounds.Height;
end;

procedure TFormMain.UpdateMode;
begin
  for var Control in LayoutChatsBox.Controls do
    if Control is TFrameChat then
    begin
      var Frame := Control as TFrameChat;
      Frame.Mode := FMode;
    end;
  case FMode of
    wmCompact:
      begin
        RectangleMenu.Align := TAlignLayout.Client;
        RectangleMenu.Parent := LayoutMenuContainer;
        LayoutHead.Visible := True;
        ButtonCloseMenu.Visible := True;
      end;
    wmFull:
      begin
        RectangleMenu.Align := TAlignLayout.Left;
        RectangleMenu.Width := 260;
        RectangleMenu.Parent := Self;
        LayoutHead.Visible := False;
        ButtonCloseMenu.Visible := False;
        LayoutMenuContent.Visible := False;
        RectangleMenu.SendToBack;
      end;
  end;
end;

procedure TFormMain.SetMode(const Value: TWindowMode);
begin
  if FMode = Value then
    Exit;
  FMode := Value;
  UpdateMode;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

end.


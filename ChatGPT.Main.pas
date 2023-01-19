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
    procedure ButtonNewChatClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonCloseMenuClick(Sender: TObject);
    procedure ButtonNewChatCompactClick(Sender: TObject);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Single);
    procedure FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardHidden(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
  private
    FOpenAI: TOpenAI;
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
  public
    property OpenAI: TOpenAI read FOpenAI;
    constructor Create(AOwner: TComponent); override;
    property Mode: TWindowMode read FMode write SetMode;
  end;

const
  API_TOKEN = {$include MY_TOKEN.txt};

var
  FormMain: TFormMain;

const
  AniInterpolation = TInterpolationType.Quadratic;

implementation

uses
  FMX.Ani, System.Math;

{$R *.fmx}

{ TFormMain }

procedure TFormMain.ButtonCloseMenuClick(Sender: TObject);
begin
  CloseMenu;
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

  var Frame := TFrameChat.Create(LayoutChats);
  Frame.Align := TAlignLayout.Client;
  Frame.Parent := LayoutChats;
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
  for var Control in LayoutChats.Controls do
    if Control is TFrameChat then
    begin
      var Frame := Control as TFrameChat;
      Frame.Visible := Frame.ChatId = ChatId;
    end;
  for var i := 0 to Pred(ListBoxChatList.Count) do
  begin
    if ListBoxChatList.ListItems[i].TagString = ChatId then
    begin
      ListBoxChatList.ListItems[i].IsSelected := True;
      LabelChatName.Text := ListBoxChatList.ListItems[i].Text;
      Exit;
    end;
  end;
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FChatIdCount := 0;
  FOpenAI := TOpenAI.Create(Self);
  FOpenAI.Token := API_TOKEN;
  ListBoxChatList.AniCalculations.Animation := True;
  ListBoxChatList.Clear;
  FMode := wmFull;
  UpdateMode;
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
  for var Control in LayoutChats.Controls do
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
        RectangleMenu.Width := 244;
        RectangleMenu.Parent := Self;
        LayoutHead.Visible := False;
        ButtonCloseMenu.Visible := False;
        LayoutMenuContent.Visible := False;
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

end.


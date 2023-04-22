unit ChatGPT.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, OpenAI,
  FMX.Objects, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, ChatGPT.FrameChat,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Effects, FMX.Filter.Effects,
  FMX.Edit, ChatGPT.Classes, System.JSON;

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
    LayoutOverlay: TLayout;
    ButtonSettings: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
  private
    FOpenAI: TOpenAIComponent;
    FMode: TWindowMode;
    FToken: string;
    FTemperature: Single;
    FLang: string;
    FChatsFileName: string;
    FSettingsFileName: string;
    FSelectedChatId: string;    
    procedure SetMode(const Value: TWindowMode);
    procedure UpdateMode;
    procedure SelectChat(const ChatId: string);
    procedure DeleteChat(const ChatId: string);
    procedure FOnChatItemClick(Sender: TObject);
    {$HINTS OFF}
    procedure FOnChatItemTap(Sender: TObject; const Point: TPointF);
    {$HINTS ON}
    function CreateChat(JSON: TJSONObject = nil): string;
    procedure CloseMenu;
    procedure Clear;
    procedure ShowClearConfirm;
    procedure HideClearConfirm;
    procedure FOnChatEditClick(Sender: TObject);
    procedure FOnChatDeleteClick(Sender: TObject);
    procedure OpenSettings;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure SetToken(const Value: string);
    procedure SetLang(const Value: string);
    procedure SetTemperature(const Value: Single);
    function GetSettingsFileName: string;
    function GetChatsFileName: string;
  public
    procedure LoadChats;
    procedure SaveChats;
    property OpenAI: TOpenAIComponent read FOpenAI;
    constructor Create(AOwner: TComponent); override;
    property Mode: TWindowMode read FMode write SetMode;
    property Token: string read FToken write SetToken;
    property Temperature: Single read FTemperature write SetTemperature;
    property Lang: string read FLang write SetLang;
  end;

const
  URL_DISCORD = 'https://discord.com/invite/openai';
  URL_FAQ = 'https://help.openai.com/en/collections/3742473-chatgpt';

var
  FormMain: TFormMain;
  FAppFolder: string;

const
  AniInterpolation = TInterpolationType.Quadratic;

procedure OpenUrl(const URL: string);

implementation

uses
  {$IFDEF ANDROID}
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.NET,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ShellAPI, DarkModeApi.FMX,
  {$ENDIF}
  FMX.Ani, System.Math, System.Rtti, FMX.Utils, FMX.DialogService,
  System.IOUtils, ChatGPT.Settings, ChatGPT.Overlay;

{$R *.fmx}

procedure CreateAppFolder;
begin
  TDirectory.CreateDirectory(FAppFolder);
end;

procedure OpenUrl(const URL: string);
begin    
  {$IFDEF ANDROID}
  TAndroidHelper.Context.startActivity(TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW, StrToJURI(URL)));
  {$ENDIF}

  {$IFDEF MSWINDOWS}  
  ShellExecute(0, 'open', PChar(URL), nil, nil, 0);   
  {$ENDIF}
end;

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
  SelectChat(CreateChat);
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

procedure TFormMain.CloseMenu;
begin
  TAnimator.AnimateFloat(RectangleMenuBG, 'Opacity', 0, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloat(ButtonCloseMenu, 'Opacity', 0, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloat(LayoutMenuContainer, 'Opacity', 0, 0.2, TAnimationType.InOut, AniInterpolation);
  TAnimator.AnimateFloatWait(LayoutMenuContainer, 'Margins.Left', -(LayoutMenuContainer.Width - 45), 0.2, TAnimationType.InOut, AniInterpolation);
  LayoutMenuContent.Visible := False;
end;

function TFormMain.CreateChat(JSON: TJSONObject): string;
begin
  var Frame: TFrameChat;
  if Assigned(JSON) then
    Frame := TFrameChat.CreateFromJson(LayoutChatsBox, JSON)
  else
  begin
    Frame := TFrameChat.Create(LayoutChatsBox);
    Frame.Temperature := Temperature;
    Frame.LangSrc := Lang;
  end;

  Frame.Align := TAlignLayout.Client;
  Frame.Parent := LayoutChatsBox;
  Frame.API := OpenAI;
  Frame.Mode := Mode;
  Frame.Visible := False;  

  var ItemList := TListBoxItem.Create(ListBoxChatList);
  ItemList.HitTest := True;
  {$IFDEF ANDROID}
  ItemList.OnTap := FOnChatItemTap;
  {$ELSE}
  ItemList.OnClick := FOnChatItemClick;
  {$ENDIF}
  ItemList.Margins.Bottom := 8;
  ItemList.Text := Frame.Title;
  ItemList.TagString := Frame.ChatId;
  ItemList.ImageIndex := 1;
  ItemList.DisableDisappear := True;
  ItemList.StylesData['edit.OnClick'] := TValue.From<TNotifyEvent>(FOnChatEditClick);
  ItemList.StylesData['delete.OnClick'] := TValue.From<TNotifyEvent>(FOnChatDeleteClick);
  ListBoxChatList.AddObject(ItemList);
  ItemList.ApplyStyleLookup;
  Frame.MenuItem := ItemList;

  Result := Frame.ChatId;
end;

procedure TFormMain.DeleteChat(const ChatId: string);
begin
  for var Control in LayoutChatsBox.Controls do
    if Control is TFrameChat then
    begin
      var Frame := Control as TFrameChat;
      if Frame.ChatId = ChatId then
      begin
        Frame.Release;
        Break;
      end;
    end;
  for var i := 0 to Pred(ListBoxChatList.Count) do
    if ListBoxChatList.ListItems[i].TagString = ChatId then
    begin
      ListBoxChatList.ListItems[i].Release;
      Exit;
    end;
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

procedure TFormMain.ButtonSettingsClick(Sender: TObject);
begin
  OpenSettings;
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
        begin
          ListItem.Text := AValues[0];
          ListItem.StylesData['changed_title'] := True;
        end;
      end);
  end;
end;

procedure TFormMain.FOnChatDeleteClick(Sender: TObject);
var
  Button: TButton absolute Sender;
  ListItem: TListBoxItem;
begin
  if TFMXObjectHelper.FindNearestParentOfClass<TListBoxItem>(Button, ListItem) then
  begin
    DeleteChat(ListItem.TagString);
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

function TFormMain.GetSettingsFileName: string;
begin
  if FSettingsFileName.IsEmpty then
    FSettingsFileName := TPath.Combine(FAppFolder, 'settings.json');
  Result := FSettingsFileName;
end;

function TFormMain.GetChatsFileName: string;
begin
  if FChatsFileName.IsEmpty then
    FChatsFileName := TPath.Combine(FAppFolder, 'chats.json');
  Result := FChatsFileName;
end;

procedure TFormMain.LoadSettings;
begin
  try
    if not TFile.Exists(GetSettingsFileName) then
      Exit;
    var JsonText: string := '';
    try
      JsonText := TFile.ReadAllText(GetSettingsFileName, TEncoding.UTF8);
    except
      Exit;
    end;
    if JsonText.IsEmpty then
      Exit;
    var JSON := TJSONObject.ParseJSONValue(JsonText);
    if Assigned(JSON) then
    try
      Token := JSON.GetValue('api_key', '');
      Temperature := JSON.GetValue<Single>('temperature', 0.0);
      Lang := JSON.GetValue('translate_lang', '');
      FSelectedChatId := JSON.GetValue('selected_chat', '');
    finally
      JSON.Free;
    end;
  finally
    if Token.IsEmpty then
      OpenSettings;
  end;
end;

procedure TFormMain.SaveSettings;
begin
  var JSON := TJSONObject.Create;
  try
    JSON.AddPair('api_key', Token);
    JSON.AddPair('temperature', TJSONNumber.Create(Temperature));
    JSON.AddPair('translate_lang', Lang);   
    JSON.AddPair('selected_chat', FSelectedChatId);
    TFile.WriteAllText(GetSettingsFileName, JSON.ToJSON, TEncoding.UTF8);
  except
    //
  end;
  JSON.Free;
end;

procedure TFormMain.LoadChats;
begin
  try
    if not TFile.Exists(GetChatsFileName) then
      Exit;
    var JsonText: string := '';
    try
      JsonText := TFile.ReadAllText(GetChatsFileName, TEncoding.UTF8);
    except
      Exit;
    end;
    if JsonText.IsEmpty then
      Exit;
    var JSON := TJSONObject.ParseJSONValue(JsonText);
    if Assigned(JSON) then
    try
      var JSONChats: TJSONArray;
      if JSON.TryGetValue('items', JSONChats) then
        for var JChat in JSONChats do
          if JChat is TJSONObject then
            CreateChat(JChat as TJSONObject);
      SelectChat(FSelectedChatId);
    finally
      JSON.Free;
    end;
  finally
    if LayoutChatsBox.ControlsCount <= 0 then
      SelectChat(CreateChat);
  end;
end;

procedure TFormMain.SaveChats;
begin
  var JSON := TJSONObject.Create;
  try
    var JSONChats := TJSONArray.Create;
    JSON.AddPair('items', JSONChats);
    for var Control in LayoutChatsBox.Controls do
      if Control is TFrameChat then
        JSONChats.Add(TFrameChat(Control).SaveAsJson);
    TFile.WriteAllText(GetChatsFileName, JSON.ToJSON, TEncoding.UTF8);
  except
    //
  end;
  JSON.Free;
end;

procedure TFormMain.SelectChat(const ChatId: string);
begin
  FSelectedChatId := ChatId;
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
  FOpenAI := TOpenAIComponent.Create(Self);
  ListBoxChatList.AniCalculations.Animation := True;
  Clear;
  FMode := wmFull;
  UpdateMode;
  Temperature := 0;
  Lang := '';
  Token := '';
  LoadSettings;
  LoadChats;
end;

procedure TFormMain.Clear;
begin
  HideClearConfirm;
  ListBoxChatList.Clear;
  while LayoutChatsBox.ControlsCount > 0 do
    LayoutChatsBox.Controls[0].Free;
end;

procedure TFormMain.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Single);
begin
  FormResize(Sender);
end;

procedure TFormMain.OpenSettings;
begin
  LayoutOverlay.Visible := True;
  LayoutOverlay.BringToFront;
  TFrameSettings.Execute(LayoutOverlay,
    procedure(Frame: TFrameSettings)
    begin
      Frame.Mode := FMode;
      Frame.EditToken.Text := Token;
      Frame.EditLangSrc.Text := Lang;
      Frame.TrackBarTemp.Value := Temperature * 10;
    end,
    procedure(Frame: TFrameSettings; Success: Boolean)
    begin
      LayoutOverlay.Visible := False;
      if not Success then
        Exit;
      Token := Frame.EditToken.Text;
      Lang := Frame.EditLangSrc.Text;
      Temperature := Frame.TrackBarTemp.Value / 10;
      SaveSettings;
    end);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetWindowColorModeAsSystem;
  {$ENDIF}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SaveChats;
  SaveSettings;
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
  for var Control in LayoutOverlay.Controls do
    if Control is TFrameOveraly then
    begin
      var Frame := Control as TFrameOveraly;
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

procedure TFormMain.SetLang(const Value: string);
begin
  FLang := Value;
end;

procedure TFormMain.SetMode(const Value: TWindowMode);
begin
  if FMode = Value then
    Exit;
  FMode := Value;
  UpdateMode;
end;

procedure TFormMain.SetTemperature(const Value: Single);
begin
  FTemperature := Value;
end;

procedure TFormMain.SetToken(const Value: string);
begin
  FToken := Value;
  FOpenAI.Token := FToken;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  FAppFolder := TPath.Combine(TPath.GetHomePath, 'ChatGPT');
  CreateAppFolder;

end.


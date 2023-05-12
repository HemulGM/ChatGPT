unit ChatGPT.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, OpenAI,
  FMX.Objects, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, ChatGPT.FrameChat,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Effects, FMX.Filter.Effects,
  FMX.Edit, ChatGPT.Classes, System.JSON, FMX.ComboEdit, FMX.Menus;

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
    Line1: TLine;
    ButtonClear: TButton;
    ButtonFAQ: TButton;
    LayoutChatsBox: TLayout;
    Layout2: TLayout;
    ButtonClearConfirm: TButton;
    ButtonClearCancel: TButton;
    LayoutOverlay: TLayout;
    ButtonSettings: TButton;
    Line2: TLine;
    ButtonAbout: TButton;
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
    procedure ButtonFAQClick(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonAboutClick(Sender: TObject);
  private
    FOpenAI: TOpenAIComponent;
    FMode: TWindowMode;
    FToken: string;
    FTemperature: Single;
    FLang: string;
    FChatsFileName: string;
    FSettingsFileName: string;
    FSelectedChatId: string;
    FPresencePenalty: Single;
    FMaxTokens: Integer;
    FMaxTokensQuery: Integer;
    FFrequencyPenalty: Single;
    FOrganization: string;
    FBaseUrl: string;
    FModel: string;
    FTopP: Single;
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
    procedure SetFrequencyPenalty(const Value: Single);
    procedure SetMaxTokens(const Value: Integer);
    procedure SetMaxTokensQuery(const Value: Integer);
    procedure SetPresencePenalty(const Value: Single);
    procedure SetOrganization(const Value: string);
    procedure SetBaseUrl(const Value: string);
    procedure SetModel(const Value: string);
    function GetChatFrame(const ChatId: string): TFrameChat;
    procedure SetTopP(const Value: Single);
  protected
    procedure CreateHandle; override;
  public
    procedure LoadChats;
    procedure SaveChats;
    property OpenAI: TOpenAIComponent read FOpenAI;
    constructor Create(AOwner: TComponent); override;
    property Mode: TWindowMode read FMode write SetMode;
    property Token: string read FToken write SetToken;
    property Temperature: Single read FTemperature write SetTemperature;
    property Organization: string read FOrganization write SetOrganization;
    property BaseUrl: string read FBaseUrl write SetBaseUrl;
    property MaxTokens: Integer read FMaxTokens write SetMaxTokens;
    property MaxTokensQuery: Integer read FMaxTokensQuery write SetMaxTokensQuery;
    property PresencePenalty: Single read FPresencePenalty write SetPresencePenalty;
    property FrequencyPenalty: Single read FFrequencyPenalty write SetFrequencyPenalty;
    property TopP: Single read FTopP write SetTopP;
    property Model: string read FModel write SetModel;
    property Lang: string read FLang write SetLang;
  end;

const
  URL_DISCORD = 'https://discord.com/invite/openai';
  URL_FAQ = 'https://help.openai.com/en/collections/3742473-chatgpt';

const
  VersionName = '1.0.13';

var
  FormMain: TFormMain;
  FAppFolder, FImagesCacheFolder: string;

const
  AniInterpolation = TInterpolationType.Quadratic;

procedure OpenUrl(const URL: string);

implementation

uses
  {$IFDEF ANDROID}
  Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.NET,
  {$ENDIF}
  {$IFDEF IOS OR IOS64}
  MacApi.Helpers, iOSApi.Foundation, FMX.Helpers.iOS,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  ShellAPI, DarkModeApi.FMX,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}
  FMX.Ani, System.Math, System.Rtti, FMX.Utils, FMX.DialogService,
  System.Threading, System.Net.URLClient, System.IOUtils, ChatGPT.Settings,
  ChatGPT.Overlay, FMX.Styles, HGM.FMX.Ani, HGM.FMX.Image, OpenAI.API,
  ChatGPT.About;

{$R *.fmx}

procedure CreateAppFolder;
begin
  TDirectory.CreateDirectory(FAppFolder);
  TDirectory.CreateDirectory(FImagesCacheFolder);
end;

procedure OpenUrl(const URL: string);
begin
  {$IFDEF ANDROID}
  TAndroidHelper.Context.startActivity(TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW, StrToJURI(URL)));
  {$ENDIF}
  {$IFDEF IOS OR IOS64}
  SharedApplication.OpenURL(StrToNSUrl(URL));
  {$ENDIF}
  {$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(URL)));
  {$ENDIF POSIX}
  {$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(URL), nil, nil, 1);
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

procedure TFormMain.ButtonAboutClick(Sender: TObject);
begin
  LayoutOverlay.Visible := True;
  LayoutOverlay.BringToFront;
  TFrameAbout.Execute(LayoutOverlay,
    procedure(Frame: TFrameAbout)
    begin
      Frame.Mode := Mode;
    end,
    procedure(Frame: TFrameAbout; Success: Boolean)
    begin
      LayoutOverlay.Visible := False;
    end);
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
  TAnimator.AnimateFloatWithFinish(LayoutMenuContainer, 'Margins.Left', -(LayoutMenuContainer.Width - 45),
    procedure
    begin
      LayoutMenuContent.Visible := False;
    end, 0.2, TAnimationType.InOut, AniInterpolation);
end;

function TFormMain.CreateChat(JSON: TJSONObject): string;
begin
  var Frame: TFrameChat;
  var IsNew: Boolean := False;
  Frame := TFrameChat.Create(LayoutChatsBox);
  Frame.Parent := LayoutChatsBox;
  Frame.Align := TAlignLayout.Client;
  Frame.API := OpenAI;
  Frame.Mode := Mode;
  Frame.Visible := False;
  if Assigned(JSON) then
  begin
    Frame.LoadFromJson(JSON);
  end
  else
  begin
    Frame.Temperature := Temperature;
    Frame.PresencePenalty := PresencePenalty;
    Frame.FrequencyPenalty := FrequencyPenalty;
    Frame.Model := Model;
    Frame.MaxTokens := MaxTokens;
    Frame.MaxTokensQuery := MaxTokensQuery;
    Frame.LangSrc := Lang;
    IsNew := True;
  end;

  var ItemList := TListBoxItem.Create(ListBoxChatList);
  ItemList.HitTest := True;
  {$IFDEF ANDROID OR IOS OR IOS64}
  ItemList.OnTap := FOnChatItemTap;
  {$ELSE}
  ItemList.OnClick := FOnChatItemClick;
  {$ENDIF}
  ItemList.Margins.Bottom := 8;
  ItemList.TextSettings.WordWrap := False;
  ItemList.Text := Frame.Title.Replace(#13, ' ').Replace(#10, ' ').Replace('&', '');
  ItemList.TagString := Frame.ChatId;
  ItemList.ImageIndex := 1;
  ItemList.DisableDisappear := True;
  ItemList.StylesData['edit.OnClick'] := TValue.From<TNotifyEvent>(FOnChatEditClick);
  ItemList.StylesData['delete.OnClick'] := TValue.From<TNotifyEvent>(FOnChatDeleteClick);
  if IsNew then
    ListBoxChatList.InsertObject(0, ItemList)
  else
    ListBoxChatList.AddObject(ItemList);
  ItemList.ApplyStyleLookup;
  Frame.MenuItem := ItemList;

  Result := Frame.ChatId;
end;

procedure TFormMain.CreateHandle;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  SetWindowColorModeAsSystem;
  {$ENDIF}
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
  var DeletedIndex: Integer := -1;
  for var i := 0 to Pred(ListBoxChatList.Count) do
    if ListBoxChatList.ListItems[i].TagString = ChatId then
    begin
      DeletedIndex := i;
      ListBoxChatList.ListItems[i].Release;
      Break;
    end;
  if LayoutChatsBox.ControlsCount <= 0 then
    SelectChat(CreateChat)
  else if ListBoxChatList.Count > 0 then
  begin
    DeletedIndex := Max(0, Min(DeletedIndex - 1, ListBoxChatList.Count - 1));
    SelectChat(ListBoxChatList.ListItems[DeletedIndex].TagString);
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
    TDialogService.MessageDialog('Delete "' + ListItem.Text + '"?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrYes then
          DeleteChat(ListItem.TagString);
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
      FrequencyPenalty := JSON.GetValue<Single>('frequency_penalty', 0.0);
      PresencePenalty := JSON.GetValue<Single>('presence_penalty', 0.0);
      TopP := JSON.GetValue<Single>('top_p', 0.0);
      Model := JSON.GetValue('model', '');
      MaxTokens := JSON.GetValue<Integer>('max_tokens', 0);
      MaxTokensQuery := JSON.GetValue<Integer>('max_tokens_query', 0);
      Lang := JSON.GetValue('translate_lang', '');
      FSelectedChatId := JSON.GetValue('selected_chat', '');

      if JSON.GetValue('on_top', False) then
        FormStyle := TFormStyle.StayOnTop
      else
        FormStyle := TFormStyle.Normal;

      OpenAI.API.Client.ProxySettings := TProxySettings.Create(
        JSON.GetValue('proxy_host', ''),
        JSON.GetValue<integer>('proxy_port', 0),
        JSON.GetValue('proxy_username', ''),
        JSON.GetValue('proxy_password', ''));
      TBitmap.Client.ProxySettings := OpenAI.API.Client.ProxySettings;

      OpenAI.BaseURL := JSON.GetValue('base_url', OpenAI.BaseURL);

      var Headers: TNetHeaders;
      try
        var JHeaders: TJSONArray;
        if JSON.TryGetValue<TJSONArray>('custom_headers', JHeaders) then
          for var JHead in JHeaders do
            if JHead is TJSONObject then
            begin
              var HName: string;
              var HValue: string;
              if JHead.TryGetValue('name', HName) and JHead.TryGetValue('value', HValue) then
              begin
                SetLength(Headers, Length(Headers) + 1);
                Headers[High(Headers)] := TNetHeader.Create(HName, HValue);
              end;
            end;
      except
        //
      end;

      OpenAI.API.CustomHeaders := Headers;
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

    JSON.AddPair('frequency_penalty', TJSONNumber.Create(FrequencyPenalty));
    JSON.AddPair('presence_penalty', TJSONNumber.Create(PresencePenalty));
    JSON.AddPair('top_p', TJSONNumber.Create(TopP));
    JSON.AddPair('max_tokens', TJSONNumber.Create(MaxTokens));
    JSON.AddPair('max_tokens_query', TJSONNumber.Create(MaxTokensQuery));
    JSON.AddPair('model', Model);

    JSON.AddPair('on_top', FormStyle = TFormStyle.StayOnTop);

    JSON.AddPair('proxy_host', OpenAI.API.Client.ProxySettings.Host);
    JSON.AddPair('proxy_port', OpenAI.API.Client.ProxySettings.Port);
    JSON.AddPair('proxy_username', OpenAI.API.Client.ProxySettings.UserName);
    JSON.AddPair('proxy_password', OpenAI.API.Client.ProxySettings.Password);

    JSON.AddPair('base_url', OpenAI.BaseURL);

    if Length(OpenAI.API.CustomHeaders) > 0 then
    begin
      var JHeaders := TJSONArray.Create;
      JSON.AddPair('custom_headers', JHeaders);
      for var Header in OpenAI.API.CustomHeaders do
      begin
        var JHead := TJSONObject.Create;
        JHead.AddPair('name', Header.Name);
        JHead.AddPair('value', Header.Value);
        JHeaders.Add(JHead);
      end;
    end;

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
      SelectChat(CreateChat)
    else if (ListBoxChatList.Count > 0) and (ListBoxChatList.Selected = nil) then
      SelectChat(ListBoxChatList.ListItems[0].TagString);
  end;
end;

procedure TFormMain.SaveChats;
begin
  var JSON := TJSONObject.Create;
  try
    var JSONChats := TJSONArray.Create;
    JSON.AddPair('items', JSONChats);
    for var i := 0 to ListBoxChatList.Count - 1 do
    begin
      var Frame := GetChatFrame(ListBoxChatList.ListItems[i].TagString);
      if Assigned(Frame) then
        JSONChats.Add(Frame.SaveAsJson);
    end;
    TFile.WriteAllText(GetChatsFileName, JSON.ToJSON, TEncoding.UTF8);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
  JSON.Free;
end;

function TFormMain.GetChatFrame(const ChatId: string): TFrameChat;
begin
  for var Control in LayoutChatsBox.Controls do
    if Control is TFrameChat then
    begin
      var Frame := Control as TFrameChat;
      if Frame.ChatId = ChatId then
        Exit(Frame);
    end;
  Result := nil;
end;

procedure TFormMain.SelectChat(const ChatId: string);
begin
  FSelectedChatId := ChatId;
  for var Control in LayoutChatsBox.Controls do
    if Control is TFrameChat then
    begin
      var Frame := Control as TFrameChat;
      Frame.Visible := Frame.ChatId = ChatId;
      {$IFNDEF ANDROID OR IOS OR IOS64}
      Frame.MemoQuery.SetFocus;
      {$ENDIF}
      Frame.MemoQuery.PrepareForPaint;
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
  Visible := True;
  {$IFDEF NEW_MEMO}
  var Style := StyleBook.Style;
  if Assigned(Style) then
  begin
    var ObjStyle := Style.FindStyleResource('memostyle_clear');
    if Assigned(ObjStyle) then
    begin
      var Sel := ObjStyle.FindStyleResource('selection');
      if Assigned(Sel) and (Sel is TBrushObject) then
        (Sel as TBrushObject).Brush.Color := $FF1F2027;
    end;
    ObjStyle := Style.FindStyleResource('memostyle_code');
    if Assigned(ObjStyle) then
    begin
      var Sel := ObjStyle.FindStyleResource('selection');
      if Assigned(Sel) and (Sel is TBrushObject) then
        (Sel as TBrushObject).Brush.Color := $FF4E4E53;
    end;
    TStyleManager.UpdateScenes;
  end;
  {$ENDIF}
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

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveChats;
  SaveSettings;
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
      Frame.EditOrg.Text := Organization;
      Frame.EditLangSrc.Text := Lang;
      Frame.TrackBarTemp.Value := Temperature * 10;
      Frame.TrackBarPP.Value := PresencePenalty * 10;
      Frame.TrackBarFP.Value := FrequencyPenalty * 10;
      Frame.EditMaxTokens.Text := MaxTokens.ToString;
      Frame.EditQueryMaxToken.Text := MaxTokensQuery.ToString;
      Frame.ComboEditModel.Text := Model;
      Frame.TrackBarTopP.Value := TopP * 10;
      Frame.SwitchOnTop.IsChecked := FormStyle = TFormStyle.StayOnTop;
      Frame.EditProxyServer.Text := OpenAI.API.Client.ProxySettings.Host;
      Frame.EditProxyPort.Text := OpenAI.API.Client.ProxySettings.Port.ToString;
      Frame.EditProxyUsername.Text := OpenAI.API.Client.ProxySettings.UserName;
      Frame.EditProxyPassword.Text := OpenAI.API.Client.ProxySettings.Password;
      Frame.LabelVersion.Text := 'Version: ' + VersionName;
      Frame.EditBaseUrl.Text := OpenAI.BaseURL;
      for var Head in OpenAI.API.CustomHeaders do
        Frame.MemoCustomHeaders.Lines.AddPair(Head.Name, Head.Value);
    end,
    procedure(Frame: TFrameSettings; Success: Boolean)
    begin
      LayoutOverlay.Visible := False;
      if not Success then
        Exit;
      Token := Frame.EditToken.Text;
      Organization := Frame.EditOrg.Text;
      Lang := Frame.EditLangSrc.Text;
      Temperature := Frame.TrackBarTemp.Value / 10;
      PresencePenalty := Frame.TrackBarPP.Value / 10;
      FrequencyPenalty := Frame.TrackBarFP.Value / 10;
      TopP := Frame.TrackBarTopP.Value / 10;
      MaxTokens := StrToIntDef(Frame.EditMaxTokens.Text, 0);
      MaxTokensQuery := StrToIntDef(Frame.EditQueryMaxToken.Text, 0);
      Model := Frame.ComboEditModel.Text;
      if Frame.SwitchOnTop.IsChecked then
        FormStyle := TFormStyle.StayOnTop
      else
        FormStyle := TFormStyle.Normal;
      OpenAI.API.Client.ProxySettings := TProxySettings.Create(
        Frame.EditProxyServer.Text,
        StrToIntDef(Frame.EditProxyPort.Text, 0),
        Frame.EditProxyUsername.Text,
        Frame.EditProxyPassword.Text);
      TBitmap.Client.ProxySettings := OpenAI.API.Client.ProxySettings;
      OpenAI.BaseURL := Frame.EditBaseUrl.Text;

      var FHeaders: TNetHeaders;
      try
        for var Line in Frame.MemoCustomHeaders.Lines do
        begin
          var Pair := Line.Split([':']);
          if Length(Pair) = 2 then
          begin
            SetLength(FHeaders, Length(FHeaders) + 1);
            FHeaders[High(FHeaders)] := TNetHeader.Create(Pair[0], Pair[1]);
          end;
        end;
      except
        //
      end;
      OpenAI.API.CustomHeaders := FHeaders;

      if OpenAI.BaseURL.IsEmpty then
        OpenAI.BaseURL := TOpenAIAPI.URL_BASE;
      {$IFDEF MSWINDOWS}
      SetWindowColorModeAsSystem;
      {$ENDIF}
      SaveSettings;
    end);
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
  LayoutOverlay.Margins.Bottom := 0;
end;

procedure TFormMain.FormVirtualKeyboardShown(Sender: TObject; KeyboardVisible: Boolean; const Bounds: TRect);
begin
  LayoutOverlay.Margins.Bottom := Bounds.Height;
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

procedure TFormMain.SetBaseUrl(const Value: string);
begin
  FBaseUrl := Value;
  OpenAI.BaseURL := FBaseUrl;
end;

procedure TFormMain.SetFrequencyPenalty(const Value: Single);
begin
  FFrequencyPenalty := Value;
end;

procedure TFormMain.SetLang(const Value: string);
begin
  FLang := Value;
end;

procedure TFormMain.SetMaxTokens(const Value: Integer);
begin
  FMaxTokens := Value;
end;

procedure TFormMain.SetMaxTokensQuery(const Value: Integer);
begin
  FMaxTokensQuery := Value;
end;

procedure TFormMain.SetMode(const Value: TWindowMode);
begin
  if FMode = Value then
    Exit;
  FMode := Value;
  UpdateMode;
end;

procedure TFormMain.SetModel(const Value: string);
begin
  FModel := Value;
end;

procedure TFormMain.SetOrganization(const Value: string);
begin
  FOrganization := Value;
  OpenAI.Organization := FOrganization;
end;

procedure TFormMain.SetPresencePenalty(const Value: Single);
begin
  FPresencePenalty := Value;
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

procedure TFormMain.SetTopP(const Value: Single);
begin
  FTopP := Value;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  FAppFolder := TPath.Combine(TPath.GetHomePath, 'ChatGPT');
  FImagesCacheFolder := TPath.Combine(FAppFolder, 'images');
  CreateAppFolder;
  TBitmap.CachePath := FImagesCacheFolder;

end.


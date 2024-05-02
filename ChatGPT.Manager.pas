unit ChatGPT.Manager;

interface

uses
  System.SysUtils, System.Classes, System.Actions, FMX.Controls, FMX.ActnList,
  FMX.StdActns, FMX.Graphics, FMX.MediaLibrary.Actions, System.ImageList,
  System.Generics.Collections, FMX.ImgList, FMX.SVGIconImageList,
  OpenAI.Chat.Functions;

type
  TModelData = record
    Name: string;
    Context: Integer;
    Tokens: Integer;
    DataDate: string;
    Legacy: Boolean;
    class function Create(const Name: string; Context, Tokens: Integer; const DataDate: string; Legacy: Boolean = False): TModelData; static;
  end;

  TManager = class(TDataModule)
    SVGIconImageList: TSVGIconImageList;
    ActionListMain: TActionList;
    ShowShareSheetAction: TShowShareSheetAction;
    procedure ShowShareSheetActionBeforeExecute(Sender: TObject);
  private
    FShareBitmap: TBitmap;
    FCanShare: Boolean;
    FAppFolder: string;
    FImagesCacheFolder: string;
    FAudioCacheFolder: string;
    FSendByEnter: Boolean;
    FOverlayContainer: TControl;
    FGPTFuncList: TList<IChatFunction>;
    FActualModels: TArray<TModelData>;
    function GetCanShare: Boolean;
    procedure CreateAppFolder;
    procedure SetSendByEnter(const Value: Boolean);
    procedure SetOverlayContainer(const Value: TControl);
    procedure SetGPTFuncList(const Value: TList<IChatFunction>);
    procedure SetActualModels(const Value: TArray<TModelData>);
    procedure FillActualModels;
  public
    procedure ShareBitmap(Bitmap: TBitmap);
    property CanShare: Boolean read FCanShare;
    property AudioCacheFolder: string read FAudioCacheFolder;
    property ImagesCacheFolder: string read FImagesCacheFolder;
    property AppFolder: string read FAppFolder;
    property SendByEnter: Boolean read FSendByEnter write SetSendByEnter;
    property OverlayContainer: TControl read FOverlayContainer write SetOverlayContainer;
    property GPTFuncList: TList<IChatFunction> read FGPTFuncList write SetGPTFuncList;
    property ActualModels: TArray<TModelData> read FActualModels write SetActualModels;
    constructor Create(AOwner: TComponent); override;
  end;

const
  ActualModels: TArray<string> = [
    'gpt-3.5-turbo-0613',
    'gpt-3.5-turbo-0301',
    'gpt-3.5-turbo-1106',
    'gpt-3.5-turbo',
    'gpt-3.5-turbo-16k',
    'gpt-4-0613',
    'gpt-4-0314',
    'gpt-4',
    'gpt-4-32k-0613',
    'gpt-4-32k-0314',
    'gpt-4-32k'];
  Legacy = True;

var
  Manager: TManager;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses
  FMX.platform, FMX.Dialogs, FMX.MediaLibrary, System.IOUtils, HGM.FMX.Image;

{$R *.dfm}

constructor TManager.Create(AOwner: TComponent);
begin
  inherited;
  FCanShare := GetCanShare;
  FAppFolder := TPath.Combine(TPath.GetHomePath, 'ChatGPT');
  FImagesCacheFolder := TPath.Combine(FAppFolder, 'images');
  FAudioCacheFolder := TPath.Combine(FAppFolder, 'audios');
  TBitmap.CachePath := FImagesCacheFolder;
  CreateAppFolder;
  FillActualModels;
end;

procedure TManager.FillActualModels;
begin
  FActualModels := [
    TModelData.Create('gpt-3.5-turbo-0613', 4096, 4096, 'Sep 2021', Legacy),
    TModelData.Create('gpt-3.5-turbo-0301', 4096, 4096, 'Sep 2021', Legacy),
    TModelData.Create('gpt-3.5-turbo-1106', 16385, 4096, 'Sep 2021'),
    TModelData.Create('gpt-3.5-turbo', 4096, 4096, 'Sep 2021'),
    TModelData.Create('gpt-3.5-turbo-16k', 16385, 4096, 'Sep 2021'),
    TModelData.Create('gpt-4-0613', 8192, 4096, 'Sep 2021'),
    TModelData.Create('gpt-4-0314', 8192, 4096, 'Sep 2021', Legacy),
    TModelData.Create('gpt-4', 8192, 4096, 'Sep 2021'),
    TModelData.Create('gpt-4-32k-0613', 32768, 4096, 'Sep 2021'),
    TModelData.Create('gpt-4-32k-0314', 32768, 4096, 'Sep 2021', Legacy),
    TModelData.Create('gpt-4-32k', 32768, 4096, 'Sep 2021'),
    TModelData.Create('gpt-4-1106-preview', 128000, 4096, 'Apr 2023'),
    TModelData.Create('gpt-4-vision-preview', 128000, 4096, 'Apr 2023')
    ];
end;

procedure TManager.CreateAppFolder;
begin
  try
    TDirectory.CreateDirectory(FAppFolder);
    TDirectory.CreateDirectory(FImagesCacheFolder);
    TDirectory.CreateDirectory(FAudioCacheFolder);
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TManager.SetActualModels(const Value: TArray<TModelData>);
begin
  FActualModels := Value;
end;

procedure TManager.SetGPTFuncList(const Value: TList<IChatFunction>);
begin
  FGPTFuncList := Value;
end;

procedure TManager.SetOverlayContainer(const Value: TControl);
begin
  FOverlayContainer := Value;
end;

procedure TManager.SetSendByEnter(const Value: Boolean);
begin
  FSendByEnter := Value;
end;

procedure TManager.ShareBitmap(Bitmap: TBitmap);
begin
  FShareBitmap := Bitmap;
  ShowShareSheetAction.Execute;
end;

procedure TManager.ShowShareSheetActionBeforeExecute(Sender: TObject);
begin
  ShowShareSheetAction.Bitmap.Assign(FShareBitmap);
end;

function TManager.GetCanShare: Boolean;
begin
  var FSharingService: IFMXShareSheetActionsService;
  Result := TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetActionsService, FSharingService);
end;

{ TModelData }

class function TModelData.Create(const Name: string; Context, Tokens: Integer; const DataDate: string; Legacy: Boolean): TModelData;
begin
  Result.Name := Name;
  Result.Context := Context;
  Result.Tokens := Tokens;
  Result.DataDate := DataDate;
  Result.Legacy := Legacy;
end;

end.


unit ChatGPT.FrameImage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, FMX.Filter.Effects, FMX.Menus;

type
  TFrameImage = class(TFrame)
    RectangleImage: TRectangle;
    AniIndicator: TAniIndicator;
    FillRGBEffect1: TFillRGBEffect;
    PopupMenuCopy: TPopupMenu;
    MenuItemCopy: TMenuItem;
    procedure RectangleImageClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
  private
    procedure SetImage(const Value: string);
  public
    property Image: string write SetImage;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  ChatGPT.FrameImagePreview, FMX.Platform, ChatGPT.FrameUIMessage, HGM.FMX.Image;

{$R *.fmx}

{ TFrameImage }

constructor TFrameImage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
end;

procedure TFrameImage.MenuItemCopyClick(Sender: TObject);
begin
  var ClipBoard: IFMXClipboardService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipBoard) then
  begin
    ClipBoard.SetClipboard(RectangleImage.Fill.Bitmap.Bitmap);
    ShowUIMessage('Image coppied');
  end
  else
    ShowUIMessage('ClipBoard error');
end;

procedure TFrameImage.RectangleImageClick(Sender: TObject);
begin
  RectangleImage.Visible := False;
  TFramePreview.ShowPreview(RectangleImage.Fill.Bitmap.Bitmap, RectangleImage.AbsoluteRect,
    procedure
    begin
      RectangleImage.Visible := True;
    end);
end;

procedure TFrameImage.SetImage(const Value: string);
begin
  RectangleImage.Fill.Bitmap.Bitmap.LoadFromUrlAsync(RectangleImage, Value, False,
    procedure(Bitmap: TBitmap)
    begin
      AniIndicator.Visible := False;
      RectangleImage.Fill.Kind := TBrushKind.Bitmap;
      RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    end);
end;

end.


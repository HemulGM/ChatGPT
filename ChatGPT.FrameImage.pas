unit ChatGPT.FrameImage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, FMX.Filter.Effects, FMX.Menus,
  FMX.Controls.Presentation;

type
  TFrameImage = class(TFrame)
    RectangleImage: TRectangle;
    AniIndicator: TAniIndicator;
    FillRGBEffect1: TFillRGBEffect;
    PopupMenuCopy: TPopupMenu;
    MenuItemCopy: TMenuItem;
    LabelError: TLabel;
    procedure RectangleImageClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure RectangleImagePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    FImage: string;
    FIsLoaded: Boolean;
    procedure SetImage(const Value: string);
  public
    property Image: string read FImage write SetImage;
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
  FIsLoaded := False;
  Name := '';
  RectangleImage.HitTest := False;
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
  TFramePreview.ShowPreview(RectangleImage.Fill.Bitmap.Bitmap, RectangleImage,
    procedure
    begin
      RectangleImage.Visible := True;
    end);
end;

procedure TFrameImage.RectangleImagePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  if not FIsLoaded then
  begin
    FIsLoaded := True;
    RectangleImage.Fill.Bitmap.Bitmap.LoadFromUrlAsync(RectangleImage, FImage, True,
      procedure(Success: Boolean)
      begin
        AniIndicator.Visible := False;
        if Success then
        begin
          RectangleImage.Fill.Kind := TBrushKind.Bitmap;
          RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
          RectangleImage.HitTest := True;
        end
        else
        begin
          LabelError.Visible := True;
        end;
      end);
  end;
end;

procedure TFrameImage.SetImage(const Value: string);
begin
  FImage := Value;
end;

end.


unit DALLE.FrameImage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  TFrameImage = class(TFrame)
    RectangleImage: TRectangle;
    procedure RectangleImageClick(Sender: TObject);
  private
    procedure SetImage(const Value: string);
  public
    property Image: string write SetImage;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  System.NetEncoding, DALLE.FrameImagePreview, HGM.FMX.Image;

{$R *.fmx}

{ TFrameImage }

constructor TFrameImage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
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
  RectangleImage.Fill.Bitmap.Bitmap.LoadFromUrlAsync(Value, False,
    procedure(Bitmap: TBitmap)
    begin
      RectangleImage.Fill.Kind := TBrushKind.Bitmap;
      RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    end);
end;

end.


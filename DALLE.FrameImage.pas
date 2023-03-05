unit DALLE.FrameImage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Effects, FMX.Filter.Effects;

type
  TFrameImage = class(TFrame)
    RectangleImage: TRectangle;
    AniIndicator: TAniIndicator;
    FillRGBEffect1: TFillRGBEffect;
    procedure RectangleImageClick(Sender: TObject);
  private
    procedure SetImage(const Value: string);
  public
    property Image: string write SetImage;
    constructor Create(AOwner: TComponent); override;
  end;

  TBitmapHelper = class helper for TBitmap
  private
    class function Get(const URL: string): TMemoryStream; static;
  public
    function LoadFromUrl(const Url: string; UseCache: Boolean = True): Boolean;
    function LoadFromUrlAsync(const Url: string; UseCache: Boolean = True; AfterLoaded: TProc<TBitmap> = nil): Boolean;
  end;

implementation

uses
  System.NetEncoding, DALLE.FrameImagePreview, System.Threading,
  System.Net.HttpClient;

{$R *.fmx}

function TBitmapHelper.LoadFromUrlAsync(const Url: string; UseCache: Boolean; AfterLoaded: TProc<TBitmap>): Boolean;
begin
  Result := False;
  TTask.Run(
    procedure
    begin
      try
        Self.LoadFromUrl(Url, UseCache);
      except
        Self.Assign(nil);
      end;
      if Assigned(AfterLoaded) then
      begin
        TThread.ForceQueue(nil,
          procedure
          begin
            AfterLoaded(Self);
          end);
      end;
    end);
end;

class function TBitmapHelper.Get(const URL: string): TMemoryStream;
var
  HTTP: THTTPClient;
begin
  Result := TMemoryStream.Create;
  if URL.IsEmpty then
    Exit;
  HTTP := THTTPClient.Create;
  HTTP.HandleRedirects := True;
  try
    try
      if (HTTP.Get(URL, Result).StatusCode = 200) and (Result.Size > 0) then
        Result.Position := 0;
    finally
      HTTP.Free;
    end;
  except
    //
  end;
end;

function TBitmapHelper.LoadFromUrl(const Url: string; UseCache: Boolean): Boolean;
var
  Mem: TMemoryStream;
  FLoaded: Boolean;
begin
  Result := False;
  Mem := Get(Url);
  try
    try
      if Mem.Size > 0 then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            try
              Self.LoadFromStream(Mem);
              FLoaded := True;
            except
              FLoaded := False;
            end;
          end);
        Result := FLoaded;
      end;
    finally
      Mem.Free;
    end;
  except
  end;
end;

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
      AniIndicator.Visible := False;
      RectangleImage.Fill.Kind := TBrushKind.Bitmap;
      RectangleImage.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    end);
end;

end.


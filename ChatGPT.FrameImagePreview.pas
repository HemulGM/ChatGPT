unit ChatGPT.FrameImagePreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.Ani;

type
  TFramePreview = class(TFrame)
    RectangleBG: TRectangle;
    Image: TImage;
    LayoutControlsContent: TLayout;
    ButtonDownload: TButton;
    Rectangle1: TRectangle;
    SaveDialogJPG: TSaveDialog;
    LayoutControls: TLayout;
    procedure FrameClick(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure FrameResized(Sender: TObject);
  private
    FInitImageBounds: TControl;
    FOnClose: TProc;
    procedure SetImageBounds(Rect: TControl);
  public
    class procedure ShowPreview(Bitmap: TBitmap; InitBounds: TControl; OnClose: TProc);
  end;

implementation

uses
  HGM.FMX.Ani;

{$R *.fmx}

{ TFramePreview }

procedure TFramePreview.ButtonDownloadClick(Sender: TObject);
begin
  if SaveDialogJPG.Execute then
    Image.Bitmap.SaveToFile(SaveDialogJPG.FileName);
end;

procedure TFramePreview.FrameClick(Sender: TObject);
begin
  var ToRect := FInitImageBounds.AbsoluteRect;
  TAnimator.AnimateFloat(RectangleBG, 'Opacity', 0);
  TAnimator.AnimateFloat(Image, 'Position.X', ToRect.Left);
  TAnimator.AnimateFloat(Image, 'Position.Y', ToRect.Top);
  TAnimator.AnimateFloat(Image, 'Width', ToRect.Width);
  TAnimator.AnimateFloat(LayoutControlsContent, 'Margins.Bottom', -LayoutControlsContent.Height);
  TAnimator.AnimateFloatWithFinish(Image, 'Height', ToRect.Height,
    procedure
    begin
      if Assigned(FOnClose) then
        FOnClose;
      Release;
    end);
end;

procedure TFramePreview.FrameResized(Sender: TObject);
begin
  Image.BoundsRect := LayoutControls.BoundsRect;
end;

procedure TFramePreview.SetImageBounds(Rect: TControl);
begin
  FInitImageBounds := Rect;
  Image.BoundsRect := FInitImageBounds.AbsoluteRect;
  TAnimator.AnimateFloat(Image, 'Position.X', LayoutControls.BoundsRect.Left);
  TAnimator.AnimateFloat(Image, 'Position.Y', LayoutControls.BoundsRect.Top);
  TAnimator.AnimateFloat(Image, 'Width', LayoutControls.BoundsRect.Width);
  TAnimator.AnimateFloat(Image, 'Height', LayoutControls.BoundsRect.Height);
end;

class procedure TFramePreview.ShowPreview(Bitmap: TBitmap; InitBounds: TControl; OnClose: TProc);
begin
  var Frame := TFramePreview.Create(InitBounds);
  Frame.Parent := Application.MainForm;
  Frame.Align := TAlignLayout.Contents;
  Frame.Image.Bitmap := Bitmap;
  Frame.BringToFront;
  Frame.SetImageBounds(InitBounds);
  Frame.RectangleBG.Opacity := 0.001;
  Frame.FOnClose := OnClose;
  Frame.LayoutControlsContent.Margins.Bottom := -Frame.LayoutControlsContent.Height;
  TAnimator.AnimateFloat(Frame.RectangleBG, 'Opacity', 1);
  TAnimator.AnimateFloat(Frame.LayoutControlsContent, 'Margins.Bottom', 0);
end;

end.


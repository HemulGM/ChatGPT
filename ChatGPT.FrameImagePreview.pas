unit ChatGPT.FrameImagePreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, FMX.Ani, StrUtils;

type
  TFramePreview = class(TFrame)
    RectangleBG: TRectangle;
    Image: TImage;
    LayoutControlsContent: TLayout;
    Button1: TButton;
    Rectangle1: TRectangle;
    SaveDialogJPG: TSaveDialog;
    LayoutControls: TLayout;
    procedure FrameClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FrameResized(Sender: TObject);
  private
    FInitImageBounds: TRectF;
    FOnClose: TProc;
    procedure SetImageBounds(Rect: TRectF);
  public
    class procedure ShowPreview(Bitmap: TBitmap; InitBounds: TRectF; OnClose: TProc);
  end;

implementation

{$R *.fmx}

{ TFramePreview }

procedure TFramePreview.Button1Click(Sender: TObject);
begin
  if SaveDialogJPG.Execute then
  begin
    if RightStr(SaveDialogJPG.FileName, 4) = '.jpg' then
      Image.Bitmap.SaveToFile(SaveDialogJPG.FileName)
    else
      Image.Bitmap.SaveToFile(SaveDialogJPG.FileName + '.jpg');
  end;
end;

procedure TFramePreview.FrameClick(Sender: TObject);
begin
  TAnimator.AnimateFloat(RectangleBG, 'Opacity', 0);
  TAnimator.AnimateFloat(Image, 'Position.X', FInitImageBounds.Left);
  TAnimator.AnimateFloat(Image, 'Position.Y', FInitImageBounds.Top);
  TAnimator.AnimateFloat(Image, 'Width', FInitImageBounds.Width);
  TAnimator.AnimateFloat(LayoutControlsContent, 'Margins.Bottom', -LayoutControlsContent.Height);
  TAnimator.AnimateFloatWait(Image, 'Height', FInitImageBounds.Height);
  if Assigned(FOnClose) then
    FOnClose;
  TThread.ForceQueue(nil, Free);
end;

procedure TFramePreview.FrameResized(Sender: TObject);
begin
  Image.BoundsRect := LayoutControls.BoundsRect;
end;

procedure TFramePreview.SetImageBounds(Rect: TRectF);
begin
  FInitImageBounds := Rect;
  Image.BoundsRect := Rect;
  TAnimator.AnimateFloat(Image, 'Position.X', LayoutControls.BoundsRect.Left);
  TAnimator.AnimateFloat(Image, 'Position.Y', LayoutControls.BoundsRect.Top);
  TAnimator.AnimateFloat(Image, 'Width', LayoutControls.BoundsRect.Width);
  TAnimator.AnimateFloat(Image, 'Height', LayoutControls.BoundsRect.Height);
end;

class procedure TFramePreview.ShowPreview(Bitmap: TBitmap; InitBounds: TRectF; OnClose: TProc);
begin
  var Frame := TFramePreview.Create(Application.MainForm);
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


unit ChatGPT.FramePlainText;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.Memo.Style, ChatGPT.Classes;

type
  TFrameText = class(TFrame)
    MemoText: TMemo;
    procedure FrameResize(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    function GetContentHeight: Single;
    procedure Fill(Data: TPart);
  end;

implementation

uses
  System.Math;

{$R *.fmx}

{ TFrameText }

constructor TFrameText.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  {$IFDEF ANDROID}
  Memo.HitTest := False;
  {$ENDIF}
  MemoText.TextSettings.VertAlign := TTextAlign.Center;
end;

procedure TFrameText.Fill(Data: TPart);
begin
  MemoText.Text := Data.Content;
  FrameResize(nil);
end;

procedure TFrameText.FrameResize(Sender: TObject);
begin
  Height := GetContentHeight;
end;

function TFrameText.GetContentHeight: Single;
begin
  (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
  (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  var ContentH := MemoText.ContentBounds.Height;
  if (ContentH + 5) < 30 then
    MemoText.Margins.Top := 25 - ContentH
  else
    MemoText.Margins.Top := 0;
  Result := Max(ContentH + 5, 30) +
    MemoText.Margins.Top +
    MemoText.Margins.Bottom;
end;

end.


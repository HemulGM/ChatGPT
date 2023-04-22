unit ChatGPT.FrameCode;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.Memo.Style, ChatGPT.Classes;

type
  TFrameCode = class(TFrame)
    RectangleHead: TRectangle;
    RectangleClient: TRectangle;
    MemoCode: TMemo;
    LabelLanguage: TLabel;
    LayoutCopyCode: TLayout;
    Path1: TPath;
    Label1: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure LayoutCopyCodeClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    function GetContentHeight: Single;
    procedure Fill(Data: TPart);
  end;

implementation

uses
  System.Math, FMX.Clipboard, FMX.Platform, ChatGPT.FrameUIMessage;

{$R *.fmx}

{ TFrameCode }

constructor TFrameCode.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
end;

procedure TFrameCode.Fill(Data: TPart);
begin
  MemoCode.Text := Data.Content;
  if Data.Language.IsEmpty then
    LabelLanguage.Text := ''
  else
    LabelLanguage.Text := Data.Language;
  FrameResize(nil);
end;

procedure TFrameCode.FrameResize(Sender: TObject);
begin
  Height := GetContentHeight;
end;

function TFrameCode.GetContentHeight: Single;
begin
  (MemoCode.Presentation as TStyledMemo).InvalidateContentSize;
  (MemoCode.Presentation as TStyledMemo).PrepareForPaint;
  Result := Max(MemoCode.ContentBounds.Height + 20, 30) +
    MemoCode.Margins.Top +
    MemoCode.Margins.Bottom +
    RectangleHead.Height;
end;

procedure TFrameCode.LayoutCopyCodeClick(Sender: TObject);
begin
  var ClipBoard: IFMXClipboardService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipBoard) then
  begin
    ClipBoard.SetClipboard(MemoCode.Text);
    ShowUIMessage('Coppied');
  end
  else
    ShowUIMessage('Clipboard error');
end;

end.


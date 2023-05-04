unit ChatGPT.FrameCode;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, FMX.Memo.Style, ChatGPT.Classes, FMX.TextLayout,
  ChatGPT.Code;

type
  TMemo = class(FMX.Memo.TMemo)
  protected
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
  end;

  TFrameCode = class(TFrame)
    RectangleHead: TRectangle;
    RectangleClient: TRectangle;
    MemoCode: TMemo;
    LabelLanguage: TLabel;
    LayoutCopyCode: TLayout;
    PathCopy: TPath;
    LabelCopy: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure LayoutCopyCodeClick(Sender: TObject);
    procedure MemoCodeMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure LabelCopyResize(Sender: TObject);
    procedure RectangleClientDblClick(Sender: TObject);
    procedure MemoCodeExit(Sender: TObject);
  private
    FCodeSyntax: TCodeSyntax;
    FOnWheel: TMouseWheelEvent;
    procedure FOnStyleLookup(Sender: TObject);
    procedure SetOnWheel(const Value: TMouseWheelEvent);
    {$IFDEF NEW_MEMO}
    procedure UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
    function IsJson(const Value: string): Boolean;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetContentHeight: Single;
    procedure Fill(Data: TPart);
    property OnWheel: TMouseWheelEvent read FOnWheel write SetOnWheel;
  end;

implementation

uses
  System.Math, FMX.Clipboard, System.JSON, FMX.Platform, ChatGPT.FrameUIMessage;

{$R *.fmx}

{ TFrameCode }

constructor TFrameCode.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  FCodeSyntax := nil;
  {$IFDEF ANDROID OR IOS OR IOS64}
  MemoCode.HitTest := False;
  {$ENDIF}
  MemoCode.OnApplyStyleLookup := FOnStyleLookup;
  {$IFDEF NEW_MEMO}
  (MemoCode.Presentation as TStyledMemo).OnUpdateLayoutParams := UpdateLayout;
  {$IFDEF ANDROID OR IOS OR IOS64}
  (MemoCode.Presentation as TStyledMemo).NeedSelectorPoints := True;
  {$ENDIF}
  {$ENDIF}
end;

destructor TFrameCode.Destroy;
begin
  FCodeSyntax.Free;
  inherited;
end;

function TFrameCode.IsJson(const Value: string): Boolean;
begin
  Result := False;
  try
    var JSON := TJSONObject.ParseJSONValue(Value);
    if Assigned(JSON) then
    begin
      JSON.Free;
      Result := True;
    end;
  except
    Result := False;
  end;
end;

procedure TFrameCode.Fill(Data: TPart);
begin
  {$IFDEF NEW_MEMO}
  if Assigned(FCodeSyntax) then
  begin
    FCodeSyntax.Free;
    FCodeSyntax := nil;
  end;
  if Data.Language.IsEmpty then
    if IsJson(Data.Content) then
      Data.Language := 'json';
  if not Data.Language.IsEmpty then
    FCodeSyntax := TCodeSyntax.FindSyntax(Data.Language, MemoCode.Font, MemoCode.FontColor);
  {$ENDIF}
  MemoCode.Text := Data.Content;
  if Data.Language.IsEmpty then
    LabelLanguage.Text := ''
  else
    LabelLanguage.Text := Data.Language;
  FrameResize(nil);
end;

procedure TFrameCode.FOnStyleLookup(Sender: TObject);
begin
  FrameResize(nil);
end;

procedure TFrameCode.FrameResize(Sender: TObject);
begin
  Height := GetContentHeight;
end;

function TFrameCode.GetContentHeight: Single;
begin
  {$IFDEF NEW_MEMO}
  //(MemoCode.Presentation as TFixedStyledMemo).InvalidateContentSize;
  //(MemoCode.Presentation as TFixedStyledMemo).PrepareForPaint;
  {$ELSE}
  (MemoCode.Presentation as TStyledMemo).InvalidateContentSize;
  (MemoCode.Presentation as TStyledMemo).PrepareForPaint;
  {$ENDIF}
  Result := Max(MemoCode.ContentBounds.Height + 20, 30) +
    MemoCode.Margins.Top +
    MemoCode.Margins.Bottom +
    RectangleHead.Height;
end;

procedure TFrameCode.LabelCopyResize(Sender: TObject);
begin
  LayoutCopyCode.Width := LabelCopy.Width + PathCopy.Width + 8;
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

procedure TFrameCode.MemoCodeExit(Sender: TObject);
begin
  {$IFDEF ANDROID OR IOS OR IOS64}
  MemoCode.HitTest := False;
  {$ENDIF}
end;

procedure TFrameCode.MemoCodeMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if ssShift in Shift then
  begin
    //MemoCode.DisableMouseWheel := False;
    //MemoCode.Model
    //Exit;
  end;

  if (MemoCode.SelLength > 0) and (Root.Captured = IControl((MemoCode.Presentation as TStyledMemo))) then
  begin
    Handled := True;
    if Assigned(FOnWheel) then
      FOnWheel(Sender, Shift, WheelDelta, Handled);
  end;
end;

procedure TFrameCode.RectangleClientDblClick(Sender: TObject);
begin
  {$IFDEF ANDROID OR IOS OR IOS64}
  MemoCode.HitTest := True;
  {$ENDIF}
end;

procedure TFrameCode.SetOnWheel(const Value: TMouseWheelEvent);
begin
  FOnWheel := Value;
end;

{$IFDEF NEW_MEMO}
procedure TFrameCode.UpdateLayout(Sender: TObject; Layout: TTextLayout; const Index: Integer);
begin
  if not Assigned(Layout) then
    Exit;
  Layout.ClearAttributes;
  Layout.Padding.Top := 1;
  Layout.Padding.Bottom := 1;
  if Assigned(FCodeSyntax) then
    for var Attr in FCodeSyntax.GetAttributesForLine(MemoCode.Lines[Index]) do
      Layout.AddAttribute(Attr.Range, Attr.Attribute);
end;
{$ENDIF}

{ TMemo }

procedure TMemo.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
end;

end.


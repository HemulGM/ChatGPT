unit FMX.RichEdit.Style;

interface

uses
  FMX.Text.TextEditor, FMX.Text.LinesLayout, FMX.TextLayout, ChatGPT.Code, FMX.Memo.Style.New,
  FMX.Controls.Presentation, FMX.Text, FMX.ScrollBox.Style, FMX.Controls,
  FMX.Graphics, System.UITypes;

type
  TRichEditLinesLayout = class(TLinesLayout)
  private
    FCodeSyntax: TCodeSyntax;
  protected
    procedure UpdateLayoutParams(const ALineIndex: Integer; const ALayout: TTextLayout); override;
  public
    constructor Create(const ALineSource: ITextLinesSource; const AScrollableContent: IScrollableContent);
    destructor Destroy; override;

    procedure ReplaceLine(const AIndex: Integer; const ALine: string); override;
  public
    procedure SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
  end;

  TRichEditTextEditor = class(TTextEditor)
  protected
    function CreateLinesLayout: TLinesLayout; override;
  end;

  TRichEditStyled = class(TStyledMemo)
  protected
    function CreateEditor: TTextEditor; override;
  public
    procedure UpdateVisibleLayoutParams;
    procedure SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
  end;

implementation

uses
  System.SysUtils, FMX.Presentation.Style, FMX.Presentation.Factory;

{ TRichEditTextEditor }

function TRichEditTextEditor.CreateLinesLayout: TLinesLayout;
begin
  Result := TRichEditLinesLayout.Create(Lines, ScrollableContent);
end;

{ TRichEditLinesLayout }

constructor TRichEditLinesLayout.Create(const ALineSource: ITextLinesSource;
  const AScrollableContent: IScrollableContent);
begin
  inherited;
end;

destructor TRichEditLinesLayout.Destroy;
begin
  FreeAndNil(FCodeSyntax);
  inherited;
end;

procedure TRichEditLinesLayout.ReplaceLine(const AIndex: Integer; const ALine: string);
begin
  inherited;
  // We have to reapply style attributes after line modification
  Items[AIndex].InvalidateLayout;
end;

procedure TRichEditLinesLayout.SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
begin
  if Assigned(FCodeSyntax) then
  begin
    FCodeSyntax.Free;
    FCodeSyntax := nil;
  end;
  FCodeSyntax := TCodeSyntax.FindSyntax(Lang, DefFont, DefColor);
  if not Assigned(FCodeSyntax) then
    FCodeSyntax := TCodeSyntax.FindSyntax('md', DefFont, DefColor);
end;

procedure TRichEditLinesLayout.UpdateLayoutParams(const ALineIndex: Integer; const ALayout: TTextLayout);
begin
  if not Assigned(FCodeSyntax) then
    Exit;

  ALayout.BeginUpdate;
  try
    inherited;
    ALayout.ClearAttributes;
    for var Attr in FCodeSyntax.GetAttributesForLine(LinesSource[ALineIndex], ALineIndex) do
    begin
      Attr.Attribute.Font.Family := TextSettings.Font.Family;
      ALayout.AddAttribute(Attr.Range, Attr.Attribute);
    end;
  finally
    ALayout.EndUpdate;
  end;
end;

{ TRichEditStyled }

function TRichEditStyled.CreateEditor: TTextEditor;
begin
  Result := TRichEditTextEditor.Create(Self, Memo.Content, Model, Self);
end;

procedure TRichEditStyled.SetCodeSyntaxName(const Lang: string; const DefFont: TFont; DefColor: TAlphaColor);
begin
  TRichEditLinesLayout(Editor.LinesLayout).SetCodeSyntaxName(Lang, DefFont, DefColor);
end;

procedure TRichEditStyled.UpdateVisibleLayoutParams;
begin
  for var I := 0 to Editor.LinesLayout.Count - 1 do
  begin
    var Line := Editor.LinesLayout.Items[I];
    if Line.Layout <> nil then
      TRichEditLinesLayout(Editor.LinesLayout).UpdateLayoutParams(I, Line.Layout);
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register('RichEditStyled', TStyledPresentationProxy<TRichEditStyled>);
finalization
  TPresentationProxyFactory.Current.Unregister('RichEditStyled', TStyledPresentationProxy<TRichEditStyled>);
end.

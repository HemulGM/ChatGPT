unit ChatGPT.Code;

interface

uses
  System.SysUtils, FMX.Graphics, System.UITypes, System.Generics.Collections,
  FMX.TextLayout;

type
  TKeyWord = class
    Word: TArray<string>;
    Font: TFont;
    Color: TAlphaColor;
    constructor Create;
    destructor Destroy; override;
  end;

  TKeyWords = class(TObjectList<TKeyWord>)
    function FindWord(const Value: string; out Key: TKeyWord): Boolean;
  end;

  TTextAttributedRangeData = record
  public
    Range: TTextRange;
    Attribute: TTextAttribute;
    constructor Create(const ARange: TTextRange; const AAttribute: TTextAttribute);
  end;

  TLineTextAttributedRange = class(TList<TTextAttributedRangeData>)
  private
    FText: string;
  public
    property Text: string read FText write FText;
  end;

  TCodeSyntaxClass = class of TCodeSyntax;

  TRegisteredSyntax = record
    SyntaxClass: TCodeSyntaxClass;
    Languages: TArray<string>;
  end;

  TCachedAttributes = TDictionary<Integer, TArray<TTextAttributedRangeData>>;

  TCodeSyntax = class abstract
  private
    class var
      FRegitered: TList<TRegisteredSyntax>;
  protected
    FCached: TCachedAttributes;
    FDefaultFont: TFont;
    FDefaultColor: TAlphaColor;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); virtual;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; virtual; abstract;
    procedure DropCache; virtual;
    class function FindSyntax(const Language: string; DefaultFont: TFont; DefaultColor: TAlphaColor): TCodeSyntax;
    class procedure RegisterSyntax(Languages: TArray<string>; CodeSyntaxClass: TCodeSyntaxClass);
  end;

implementation

{ TKeyWord }

constructor TKeyWord.Create;
begin
  inherited;
  Font := TFont.Create;
end;

destructor TKeyWord.Destroy;
begin
  Font.Free;
  inherited;
end;

{ TKeyWords }

function TKeyWords.FindWord(const Value: string; out Key: TKeyWord): Boolean;
begin
  var LowValue := Value.ToLower;
  for var KeyWord in Self do
    for var Word in KeyWord.Word do
    begin
      if Word = LowValue then
      begin
        Key := KeyWord;
        Exit(True);
      end;
    end;
  Result := False;
end;

{ TTextAttributedRangeData }

constructor TTextAttributedRangeData.Create(const ARange: TTextRange; const AAttribute: TTextAttribute);
begin
  Self.Range := ARange;
  Self.Attribute := AAttribute;
end;

{ TCodeSyntax }

constructor TCodeSyntax.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited Create;
  FCached := TCachedAttributes.Create;
  FDefaultFont := DefaultFont;
  FDefaultColor := DefaultColor;
end;

destructor TCodeSyntax.Destroy;
begin
  FCached.Free;
  inherited;
end;

procedure TCodeSyntax.DropCache;
begin
  FCached.Clear;
end;

class function TCodeSyntax.FindSyntax(const Language: string; DefaultFont: TFont; DefaultColor: TAlphaColor): TCodeSyntax;
begin
  if not Assigned(FRegitered) then
    Exit(nil);
  for var Item in FRegitered do
    for var Lang in Item.Languages do
      if Lang = Language.ToLower then
        Exit(Item.SyntaxClass.Create(DefaultFont, DefaultColor));
  Result := nil;
end;

class procedure TCodeSyntax.RegisterSyntax(Languages: TArray<string>; CodeSyntaxClass: TCodeSyntaxClass);
begin
  if not Assigned(FRegitered) then
    FRegitered := TList<TRegisteredSyntax>.Create;
  var Reg: TRegisteredSyntax;
  Reg.SyntaxClass := CodeSyntaxClass;
  Reg.Languages := Languages;
  FRegitered.Add(Reg);
end;

initialization

finalization
  TCodeSyntax.FRegitered.Free;

end.


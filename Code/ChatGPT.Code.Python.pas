unit ChatGPT.Code.Python;

interface

uses
  System.SysUtils, ChatGPT.Code, System.Generics.Collections, FMX.TextLayout,
  FMX.Graphics, System.UITypes;

type
  TCodeSyntaxPython = class(TCodeSyntax)
  private
    FKeyWords: TKeyWords;
    FStringKey, FNumKey, FCommentKey, FCallKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string): TArray<TTextAttributedRangeData>; override;
  end;

implementation

{ TCodeSyntaxPython }

constructor TCodeSyntaxPython.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  var KeyWord: TKeyWord;
  FKeyWords := TKeyWords.Create;

  KeyWord := TKeyWord.Create;

  KeyWord.Word := ['import', 'from', 'def', 'return', 'for', 'if', 'elif', 'else',
    'not', 'await', 'async', 'yield', 'as', 'with', 'pass', 'none', 'break', 'try',
    'except', 'raise', 'class', 'finally', 'is', 'and', 'continue', 'lambda', 'nonlocal',
    'while', 'assert', 'del', 'global', 'or'];
  KeyWord.Color := $FFF47067;  //red
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['in', '==', '=', '**', 'false', 'true'];
  KeyWord.Color := $FF1E8BF0;   //blue
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FF87D0FF;
  FStringKey.Font.Assign(FDefaultFont);

  FNumKey := TKeyWord.Create;
  FNumKey.Color := $FF46A9FF;
  FNumKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := $FF46A9FF;
  FCommentKey.Font.Assign(FDefaultFont);

  FCallKey := TKeyWord.Create;
  FCallKey.Color := $FFDCBDFB;
  FCallKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxPython.Destroy;
begin
  FStringKey.Free;
  FCallKey.Free;
  FCommentKey.Free;
  FNumKey.Free;
  FKeyWords.Free;
  inherited;
end;

function TCodeSyntaxPython.GetAttributesForLine(const Line: string): TArray<TTextAttributedRangeData>;
const
  Seps =[' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '&'];
begin
  var Buf: string := '';
  var IsString: Boolean := False;
  var IsComment: Boolean := False;
  for var C := 0 to Line.Length do
  begin
    if Line.IsEmpty then
      Continue;
    if IsString then
    begin
      if Line.Chars[C] = '''' then
      begin
        IsString := False;
        if not Buf.IsEmpty then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
            TTextRange.Create(C - Buf.Length, Buf.Length + 1),
            TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
            )];
          Buf := '';
        end;
        Continue;
      end;
      Buf := Buf + Line.Chars[C];
      Continue;
    end;
    if Line.Chars[C] = '''' then
    begin
      IsString := True;
      Buf := Buf + Line.Chars[C];
      Continue;
    end;
    if Line.Chars[C] = '#' then
    begin
      Result := Result + [
        TTextAttributedRangeData.Create(
        TTextRange.Create(C, Line.Length - C),
        TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
        )];
      Exit;
    end;
    if IsComment then
    begin
      if Line.Chars[C] = '"' then
      begin
        IsComment := False;
        if not Buf.IsEmpty then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
            TTextRange.Create(C - Buf.Length, Buf.Length + 1),
            TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
            )];
          Buf := '';
        end;
        Continue;
      end;
      Buf := Buf + Line.Chars[C];
      Continue;
    end;
    if Line.Chars[C] = '"' then
    begin
      IsComment := True;
      Buf := Buf + Line.Chars[C];
      Continue;
    end;

    if (C = Line.Length) or CharInSet(Line.Chars[C], Seps) then
    begin
      if not Buf.IsEmpty then
      begin
        var KeyWord: TKeyWord;
        var FL: Extended;
        if Line.Chars[C] = '(' then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
            TTextRange.Create(C - Buf.Length, Buf.Length),
            TTextAttribute.Create(FCallKey.Font, FCallKey.Color)
            )];
        end;
        if (TryStrToFloat(Buf.Replace('.', ','), FL) or Buf.StartsWith('$')) then
        begin
          Result := Result + [TTextAttributedRangeData.Create(
            TTextRange.Create(C - Buf.Length, Buf.Length),
            TTextAttribute.Create(FNumKey.Font, FNumKey.Color)
            )];
        end
        else if Buf.StartsWith('#') then
        begin
          Result := Result + [TTextAttributedRangeData.Create(
            TTextRange.Create(C - Buf.Length, Buf.Length),
            TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
            )];
        end
        else if FKeyWords.FindWord(Buf, KeyWord) then
        begin
          Result := Result + [TTextAttributedRangeData.Create(
            TTextRange.Create(C - Buf.Length, Buf.Length),
            TTextAttribute.Create(KeyWord.Font, KeyWord.Color)
            )];
        end;

        Buf := '';
      end;
    end
    else
      Buf := Buf + Line.Chars[C];
  end;
end;

initialization
  TCodeSyntax.RegisterSyntax(['python'], TCodeSyntaxPython);

end.


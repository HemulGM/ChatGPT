unit ChatGPT.Code.pascal;

interface

uses
  System.SysUtils, ChatGPT.Code, System.Generics.Collections, FMX.TextLayout,
  FMX.Graphics, System.UITypes;

type
  TCodeSyntaxPascal = class(TCodeSyntax)
  private
    FKeyWords: TKeyWords;
    FStringKey, FNumKey, FCommentKey, FDirectiveKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

{ TCodeSyntaxPascal }

constructor TCodeSyntaxPascal.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  var KeyWord: TKeyWord;
  FKeyWords := TKeyWords.Create;

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['var', 'string', 'as', 'inherited', 'destructor',
    'procedure', 'constructor', 'function', 'uses', 'implementation', 'private',
    'initialization', 'finalization', 'or', 'and', 'xor', 'div', 'mod', 'in',
    'public', 'override', 'overload', 'type', 'unit', 'interface', 'nil', 'not',
    'shr', 'shl', 'platform', 'deprecated', 'packed', 'index', 'cdecl', 'stdcall',
    'register', 'fastcall', 'file', 'goto', 'program', 'downto', 'label',
    'raise', 'array', 'of', 'is', 'const', 'read', 'write', 'operator', 'static',
    'inline', 'abstract', 'protected', 'strict', 'virtual', 'to', 'property', 'message', 'set'];
  KeyWord.Color := $FFFFE0BC;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['begin', 'end', 'asm', 'end.', 'class', 'record', 'try', 'finally', 'except'];
  KeyWord.Color := $FFC22700;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  //KeyWord.Font.Size := 25;
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['if', 'then', 'case', 'do', 'else', 'for', 'with', 'repeat', 'until', 'while'];
  KeyWord.Color := $FFFF9900;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FF7FAAFF;
  FStringKey.Font.Assign(FDefaultFont);

  FNumKey := TKeyWord.Create;
  FNumKey.Color := $FFFF7F85;
  FNumKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := $FF88E775;
  FCommentKey.Font.Assign(FDefaultFont);

  FDirectiveKey := TKeyWord.Create;
  FDirectiveKey.Color := $FF3CB1FF;
  FDirectiveKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxPascal.Destroy;
begin
  FStringKey.Free;
  FDirectiveKey.Free;
  FCommentKey.Free;
  FNumKey.Free;
  FKeyWords.Free;
  inherited;
end;

function TCodeSyntaxPascal.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
const
  Seps =[' ', ';', ')', '(', '[', ']', ':', '<', '>', ',', '+', '-', '=', '*', '/', '&'];
begin
  if FCached.TryGetValue(Index, Result) then
    Exit;
  try
    var Buf: string := '';
    var IsString: Boolean := False;
    var IsComment: Boolean := False;
    for var C := 0 to Line.Length do
    begin
      if Line.IsEmpty then
        Continue;
      if IsComment then
      begin
        if Line.Chars[C] = '}' then
        begin
          IsComment := False;
          if not Buf.IsEmpty then
          begin
            if Buf.StartsWith('{$') then
              Result := Result + [
                TTextAttributedRangeData.Create(
                TTextRange.Create(C - Buf.Length, Buf.Length + 1),
                TTextAttribute.Create(FDirectiveKey.Font, FDirectiveKey.Color)
                )]
            else
              Result := Result + [
                TTextAttributedRangeData.Create(
                TTextRange.Create(C - Buf.Length, Buf.Length + 1),
                TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
                )];
            Buf := '';
          end;
          Continue;
        end;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;
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
      if C <> Line.Length then
      begin
        if (Line.Chars[C] = '/') and (Line.Chars[C + 1] = '/') then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
            TTextRange.Create(C, Line.Length - C),
            TTextAttribute.Create(FCommentKey.Font, FCommentKey.Color)
            )];
          Exit;
        end;
        if Line.Chars[C] = '{' then
        begin
          IsComment := True;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;
        if Line.Chars[C] = '''' then
        begin
          IsString := True;
          Buf := Buf + Line.Chars[C];
          Continue;
        end;
      end;

      if (C = Line.Length) or CharInSet(Line.Chars[C], Seps) then
      begin
        if not Buf.IsEmpty then
        begin
          var KeyWord: TKeyWord;
          var Num: Extended;
          if (TryStrToFloat(Buf.Replace('.', ','), Num) or Buf.StartsWith('$')) then
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
  finally
    FCached.AddOrSetValue(Index, Result);
  end;
end;

initialization
  TCodeSyntax.RegisterSyntax(['pascal', 'delphi', 'fpc', 'freepascal', 'free-pascal'], TCodeSyntaxPascal);

end.


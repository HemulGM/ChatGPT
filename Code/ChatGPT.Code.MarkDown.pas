unit ChatGPT.Code.MarkDown;

interface

uses
  System.SysUtils, ChatGPT.Code, System.Generics.Collections, FMX.TextLayout,
  FMX.Graphics, System.UITypes;

type
  TCodeSyntaxMD = class(TCodeSyntax)
  private
    FMonoKey, FNumKey, FCommentKey, FDirectiveKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function ParseLine(const Line: string; out Output: string): TArray<TTextAttributedRangeData>;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

{ TCodeSyntaxMD }

constructor TCodeSyntaxMD.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  FMonoKey := TKeyWord.Create;
  FMonoKey.Color := TAlphaColorRec.White;
  FMonoKey.Font.Assign(FDefaultFont);
  FMonoKey.Font.Family := 'Consolas';

  FNumKey := TKeyWord.Create;
  FNumKey.Color := DefaultColor;
  FNumKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := DefaultColor;
  FCommentKey.Font.Assign(FDefaultFont);

  FDirectiveKey := TKeyWord.Create;
  FDirectiveKey.Color := DefaultColor;
  FDirectiveKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxMD.Destroy;
begin
  FMonoKey.Free;
  FDirectiveKey.Free;
  FCommentKey.Free;
  FNumKey.Free;
  inherited;
end;

function TCodeSyntaxMD.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
const
  Seps =[' ', '.', ',', '(', '[', ']', ':', '<', '>', '+', '-', '=', '*', '/', '&'];
begin
  if FCached.TryGetValue(Index, Result) then
    Exit;
  try
    var Buf: string := '';
    var IsMono := False;
    for var C := 0 to Line.Length do
    begin
      if Line.IsEmpty then
        Continue;
      if IsMono then
      begin
        if Line.Chars[C] = '`' then
        begin
          Buf := Buf + Line.Chars[C];
          IsMono := False;
          if not Buf.IsEmpty then
          begin
            Result := Result + [
              TTextAttributedRangeData.Create(
              TTextRange.Create(C - Buf.Length, Buf.Length + 1),
              TTextAttribute.Create(FMonoKey.Font, FMonoKey.Color)
              )];
            Buf := '';
          end;
          Continue;
        end;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;
      if Line.Chars[C] = '`' then
      begin
        Buf := '';
        IsMono := True;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;

    {
    if (C = Line.Length) or CharInSet(Line.Chars[C], Seps) then
    begin
      if not Buf.IsEmpty then
      begin
        if Buf.StartsWith('#') then
        begin
          Result := Result + [TTextAttributedRangeData.Create(
            TTextRange.Create(C - Buf.Length, Buf.Length),
            TTextAttribute.Create(FStringKey.Font, FStringKey.Color)
            )];
        end;

        Buf := '';
      end;
    end
    else  }
      Buf := Buf + Line.Chars[C];
    end;
  finally
    FCached.AddOrSetValue(Index, Result);
  end;
end;

function TCodeSyntaxMD.ParseLine(const Line: string; out Output: string): TArray<TTextAttributedRangeData>;
begin
  var Buf: string := '';
  var IsMono := -1;
  for var C := 0 to Line.Length do
  begin
    if Line.IsEmpty then
      Continue;
    //-------------
    if Line.Chars[C] = '`' then
    begin
      if IsMono <> -1 then
      begin
        IsMono := -1;
        if not Buf.IsEmpty then
        begin
          Result := Result + [
            TTextAttributedRangeData.Create(
            TTextRange.Create(IsMono + 1, Buf.Length),
            TTextAttribute.Create(FMonoKey.Font, FMonoKey.Color)
            )];
          Buf := '';
        end;
        Continue;
      end
      else
      begin
        Output := Output + Buf;
        Buf := '';
        IsMono := C;
        Buf := Buf + Line.Chars[C];
        Continue;
      end;
    end;

    {
    if Line.Chars[C] = '#' then
    begin
      Buf := Buf + Line.Chars[C];
      Inc(HashCnt);
    end
    else
    begin
      if HashCnt > 0 then
      begin

      end;
      HashCnt := 0;
    end;  }


    Buf := Buf + Line.Chars[C];
  end;
  Output := Output + Buf;
end;

initialization
  TCodeSyntax.RegisterSyntax(['md'], TCodeSyntaxMD);

end.


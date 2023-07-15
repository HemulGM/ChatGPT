unit ChatGPT.Code.SQL;

interface

uses
  System.SysUtils, ChatGPT.Code, System.Generics.Collections, FMX.TextLayout,
  FMX.Graphics, System.UITypes;

type
  TCodeSyntaxSQL = class(TCodeSyntax)
  private
    FKeyWords: TKeyWords;
    FStringKey, FNumKey, FCommentKey, FDirectiveKey: TKeyWord;
  public
    constructor Create(DefaultFont: TFont; DefaultColor: TAlphaColor); override;
    destructor Destroy; override;
    function GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>; override;
  end;

implementation

{ TCodeSyntaxSQL }

constructor TCodeSyntaxSQL.Create(DefaultFont: TFont; DefaultColor: TAlphaColor);
begin
  inherited;

  var KeyWord: TKeyWord;
  FKeyWords := TKeyWords.Create;

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['add', 'constraint', 'alter',
    'all', 'any', 'as', 'asc', 'backup', 'check', 'by',
    'column', 'create', 'replace',
    'database', 'default', 'delete', 'desc', 'distinct', 'drop', 'exec',
    'exists', 'foreign', 'from', 'group', 'having',
    'index', 'inner', 'insert', 'key',
    'limit', 'order', 'outer',
    'primary', 'procedure', 'right', 'join', 'rownum', 'select',
    'into', 'set', 'table', 'top', 'truncate', 'union',
    'unique', 'update', 'values', 'view', 'where', 'on', 'full', 'left'];
  KeyWord.Color := $FF638FCF;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['ascii', 'char_length', 'character_length', 'concat', 'concat_ws',
    'field', 'find_in_set', 'format', 'instr', 'lcase', 'length', 'locate',
    'lower', 'lpad', 'ltrim', 'mid', 'position', 'repeat', 'reverse',
    'rpad', 'rtrim', 'space', 'strcmp', 'substr', 'substring', 'substring_index', 'trim',
    'ucase', 'upper', 'adddate', 'addtime', 'curdate', 'current_date', 'current_time',
    'current_timestamp', 'curtime', 'date', 'datediff', 'date_add', 'date_format',
    'date_sub', 'day', 'dayname', 'dayofmonth', 'dayofweek', 'dayofyear', 'extract',
    'from_days', 'hour', 'last_day', 'localtime', 'localtimestamp', 'makedate', 'maketime',
    'microsecond', 'minute', 'month', 'monthname', 'now', 'period_add', 'period_diff',
    'quarter', 'second', 'sec_to_time', 'str_to_date', 'subdate', 'subtime', 'sysdate',
    'time', 'time_format', 'time_to_sec', 'timediff', 'timestamp', 'to_days', 'week',
    'weekday', 'weekofyear', 'year', 'yearweek', 'bin', 'binary', 'case', 'cast',
    'coalesce', 'connection_id', 'conv', 'convert', 'current_user',
    'if', 'ifnull', 'isnull', 'last_insert_id', 'nullif', 'session_user', 'system_user', 'user', 'version',
    'any_value', 'array_agg', 'array_concat_agg', 'avg', 'bit_and', 'bit_or', 'bit_xor', 'count',
    'countif', 'logical_and', 'logical_or', 'max', 'min', 'string_agg', 'sum'];
  KeyWord.Color := $FFC22700;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  //KeyWord.Font.Size := 25;
  FKeyWords.Add(KeyWord);

  KeyWord := TKeyWord.Create;
  KeyWord.Word := ['string', 'bytes', 'between', 'in', 'is', 'null', 'true', 'false', 'not', 'and', 'or', 'like'];
  KeyWord.Color := $FF5E5E5E;
  KeyWord.Font.Assign(FDefaultFont);
  KeyWord.Font.Style := [TFontStyle.fsBold];
  FKeyWords.Add(KeyWord);

  FStringKey := TKeyWord.Create;
  FStringKey.Color := $FF468141;
  FStringKey.Font.Assign(FDefaultFont);

  FNumKey := TKeyWord.Create;
  FNumKey.Color := $FFFF7F85;
  FNumKey.Font.Assign(FDefaultFont);

  FCommentKey := TKeyWord.Create;
  FCommentKey.Color := $FF468141;
  FCommentKey.Font.Assign(FDefaultFont);

  FDirectiveKey := TKeyWord.Create;
  FDirectiveKey.Color := $FF3CB1FF;
  FDirectiveKey.Font.Assign(FDefaultFont);
end;

destructor TCodeSyntaxSQL.Destroy;
begin
  FStringKey.Free;
  FDirectiveKey.Free;
  FCommentKey.Free;
  FNumKey.Free;
  FKeyWords.Free;
  inherited;
end;

function TCodeSyntaxSQL.GetAttributesForLine(const Line: string; const Index: Integer): TArray<TTextAttributedRangeData>;
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
  TCodeSyntax.RegisterSyntax(['sql'], TCodeSyntaxSQL);

end.


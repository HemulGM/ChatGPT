unit ChatGPT.FrameMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, System.Generics.Collections, FMX.BehaviorManager,
  ChatGPT.FrameImage;

type
  TPartType = (ptText, ptCode);

  TPart = record
    PartType: TPartType;
    Content: string;
  end;

  TFrameMessage = class(TFrame)
    RectangleBG: TRectangle;
    MemoText: TMemo;
    LayoutInfo: TLayout;
    RectangleUser: TRectangle;
    Path1: TPath;
    RectangleBot: TRectangle;
    Path2: TPath;
    LayoutContent: TLayout;
    LayoutContentText: TLayout;
    LayoutAudio: TLayout;
    Rectangle1: TRectangle;
    Path3: TPath;
    FlowLayoutImages: TFlowLayout;
    procedure MemoTextChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FIsUser: Boolean;
    FText: string;
    FIsError: Boolean;
    FIsAudio: Boolean;
    FImages: TArray<string>;
    procedure SetIsUser(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetIsError(const Value: Boolean);
    procedure ParseText(const Value: string);
    procedure SetIsAudio(const Value: Boolean);
    procedure BuildContent(Parts: TList<TPart>);
    procedure SetImages(const Value: TArray<string>);
  public
    procedure UpdateContentSize;
    property Text: string read FText write SetText;
    property Images: TArray<string> read FImages write SetImages;
    property IsUser: Boolean read FIsUser write SetIsUser;
    property IsAudio: Boolean read FIsAudio write SetIsAudio;
    property IsError: Boolean read FIsError write SetIsError;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  System.Math, FMX.Memo.Style;

{$R *.fmx}

procedure TFrameMessage.UpdateContentSize;
begin
  // Memo
  var H := Padding.Top + Padding.Bottom;
  for var Control in LayoutContentText.Controls do
    if Control is TMemo then
    begin
      ((Control as TMemo).Presentation as TStyledMemo).InvalidateContentSize;
      ((Control as TMemo).Presentation as TStyledMemo).PrepareForPaint;
      (Control as TMemo).Height := Max((Control as TMemo).ContentBounds.Height + (Control as TMemo).TagFloat * 2, 30);
      H := H + Max((Control as TMemo).Height, 30);
      H := H + Control.Margins.Top + Control.Margins.Bottom;
    end;

  //Flow
  if FlowLayoutImages.Visible then
  begin
    var ItemW := Min(256, Max(Trunc(FlowLayoutImages.Width / FlowLayoutImages.ControlsCount), 48));
    if ItemW = 48 then
      ItemW := Trunc(FlowLayoutImages.Width / Trunc(FlowLayoutImages.Width / 48));
    H := 0;
    for var Control in FlowLayoutImages.Controls do
    begin
      Control.Size.Size := TSizeF.Create(ItemW, ItemW);
      H := Max(Control.Position.Y + Control.Height, H);
    end;
    if FlowLayoutImages.Height <> H then
      FlowLayoutImages.Height := H;
  end;

  //Frame
  H := Padding.Top + Padding.Bottom;
  for var Control in LayoutContentText.Controls do
    if Control.Visible then
      H := H + Control.Height + Control.Margins.Top + Control.Margins.Bottom;


  if Height <> H then
    Height := H;
end;

constructor TFrameMessage.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  {$IFDEF ANDROID}
  MemoText.HitTest := False;
  {$ENDIF}
  IsAudio := False;
  MemoText.Visible := False;
  FlowLayoutImages.Visible := False;
end;

procedure TFrameMessage.FrameResize(Sender: TObject);
begin
  LayoutContent.Width := Min(Width - (Padding.Left + Padding.Right), 650);
  UpdateContentSize;
end;

procedure TFrameMessage.MemoTextChange(Sender: TObject);
begin
  UpdateContentSize;
end;

procedure TFrameMessage.SetImages(const Value: TArray<string>);
begin
  FImages := Value;
  while FlowLayoutImages.ControlsCount > 0 do
    FlowLayoutImages.Controls[0].Free;
  for var Item in FImages do
  begin
    var Frame := TFrameImage.Create(FlowLayoutImages);
    Frame.Parent := FlowLayoutImages;
    Frame.Image := Item;
  end;
  FlowLayoutImages.Visible := FlowLayoutImages.ControlsCount > 0;
  UpdateContentSize;
end;

procedure TFrameMessage.SetIsAudio(const Value: Boolean);
begin
  FIsAudio := Value;
  LayoutAudio.Visible := FIsAudio;
end;

procedure TFrameMessage.SetIsError(const Value: Boolean);
begin
  FIsError := Value;
  MemoText.FontColor := $FFEF4444;
end;

procedure TFrameMessage.SetIsUser(const Value: Boolean);
begin
  FIsUser := Value;
  RectangleUser.Visible := FIsUser;
  RectangleBot.Visible := not FIsUser;

  if FIsUser then
  begin
    RectangleBG.Fill.Color := $00FFFFFF;
    MemoText.FontColor := $FFECECF1;
  end
  else
  begin
    RectangleBG.Fill.Color := $14FFFFFF;
    MemoText.FontColor := $FFD1D5E3;
  end;
end;

procedure TFrameMessage.ParseText(const Value: string);

  function CreatePart(AType: TPartType; AContent: string): TPart;
  begin
    Result.PartType := AType;
    Result.Content := AContent.Trim([#13, #10, ' ']);
  end;

var
  Parts: TList<TPart>;
  CodePairs: Integer;
  IsCode: Boolean;
  Buf: string;
begin
  if Value.Contains('```') then
  begin
    Parts := TList<TPart>.Create;
    try
      CodePairs := 0;
      Buf := '';
      IsCode := False;
      for var C in Value do
      begin
        if C = '`' then
        begin
          Inc(CodePairs);
          if CodePairs = 3 then
          begin
            if IsCode then
            begin
              if not Buf.IsEmpty then
                Parts.Add(CreatePart(ptCode, Buf));
              IsCode := False;
            end
            else
            begin
              if not Buf.IsEmpty then
                Parts.Add(CreatePart(ptText, Buf));
              IsCode := True;
            end;
            Buf := '';
            CodePairs := 0;
          end;
        end
        else
        begin
          CodePairs := 0;
          Buf := Buf + C;
        end;
      end;
      if IsCode then
      begin
        if not Buf.IsEmpty then
          Parts.Add(CreatePart(ptCode, Buf));
      end
      else
      begin
        if not Buf.IsEmpty then
          Parts.Add(CreatePart(ptText, Buf));
      end;

      BuildContent(Parts);
    finally
      Parts.Free;
    end;
  end
  else
  begin
    MemoText.Text := Value;
    MemoText.Visible := True;
    (MemoText.Presentation as TStyledMemo).InvalidateContentSize;
    (MemoText.Presentation as TStyledMemo).PrepareForPaint;
  end;
  UpdateContentSize;
end;

procedure TFrameMessage.BuildContent(Parts: TList<TPart>);
begin
  var IsFirstText: Boolean := True;
  for var Part in Parts do
  begin
    begin
      var Memo: TMemo;
      if IsFirstText then
      begin
        Memo := MemoText;
        MemoText.Visible := True;
        IsFirstText := False;
      end
      else
      begin
        Memo := TMemo.Create(LayoutContentText);
        with Memo do
        begin
          Parent := LayoutContentText;
          Caret.Color := $00FFFFFF;
          DisableMouseWheel := True;
          ReadOnly := True;
          StyledSettings := [TStyledSetting.Style];
          CanParentFocus := True;
          Cursor := crDefault;
          DisableFocusEffect := True;
          EnableDragHighlight := False;
          OnChange := MemoTextChange;
          {$IFDEF ANDROID}
          Memo.HitTest := False;
          {$ENDIF}
          OnChangeTracking := MemoTextChange;
          if Part.PartType = ptCode then
          begin
            StyleLookup := 'memostyle_code';
            Margins.Rect := TRectF.Create(0, 5, 0, 5);
            TagFloat := 5;
            TextSettings.WordWrap := False;
            TextSettings.Font.Family := 'Consolas';
            TextSettings.FontColor := $FFC6C6C6;
            ShowScrollBars := True;
            AutoHide := TBehaviorBoolean.True;
          end
          else
          begin
            StyleLookup := 'memostyle_clear';
            TextSettings.Font.Size := 16;
            TextSettings.FontColor := $FFECECF1;
            ShowScrollBars := False;
            TextSettings.WordWrap := True;
            TagFloat := 2;
          end;
          ApplyStyleLookup;
        end;
      end;
      Memo.Text := Part.Content;
      (Memo.Presentation as TStyledMemo).InvalidateContentSize;
      (Memo.Presentation as TStyledMemo).PrepareForPaint;
      Memo.Align := TAlignLayout.None;
      Memo.Position.Y := 10000;
      Memo.Align := TAlignLayout.Top;
    end;
  end;
end;

procedure TFrameMessage.SetText(const Value: string);
begin
  if not Value.IsEmpty then
    FText := Value
  else
    FText := 'empty';
  FText := FText.Trim([' ', #13, #10]);
  ParseText(FText);
end;

end.


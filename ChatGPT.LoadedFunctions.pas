unit ChatGPT.LoadedFunctions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Overlay, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts,
  FMX.ListBox;

type
  TFrameLoadedFunctions = class(TFrameOveraly)
    LayoutClient: TLayout;
    RectangleFrame: TRectangle;
    Layout2: TLayout;
    ButtonOk: TButton;
    ListBoxItems: TListBox;
    LabelEmpty: TLabel;
    Label1: TLabel;
    Label20: TLabel;
    Rectangle1: TRectangle;
    procedure FrameResize(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
  private
    FLayoutClientWidth, FLayoutClientHeight: Single;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure Execute(AParent: TControl);
  end;

var
  FrameLoadedFunctions: TFrameLoadedFunctions;

implementation

uses
  System.Math, ChatGPT.Main;

{$R *.fmx}

procedure TFrameLoadedFunctions.ButtonOkClick(Sender: TObject);
begin
  Release;
end;

constructor TFrameLoadedFunctions.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  FLayoutClientWidth := LayoutClient.Width;
  FLayoutClientHeight := LayoutClient.Height;
  for var Item in FormMain.GPTFuncList do
  begin
    var ListItem := TListBoxItem.Create(ListBoxItems);
    ListItem.Text := Item.GetDescription;
    ListItem.ItemData.Detail := Item.GetName;
    ListBoxItems.AddObject(ListItem);
  end;
  LabelEmpty.Visible := ListBoxItems.Count <= 0;
end;

class procedure TFrameLoadedFunctions.Execute(AParent: TControl);
begin
  var Frame := TFrameLoadedFunctions.Create(AParent);
  Frame.Parent := AParent;
  Frame.Align := TAlignLayout.Contents;
  Frame.BringToFront;
end;

procedure TFrameLoadedFunctions.FrameResize(Sender: TObject);
begin
  LayoutClient.Width := Min(FLayoutClientWidth, Width);
  LayoutClient.Height := Min(FLayoutClientHeight, Height);
end;

end.


unit ChatGPT.ImportExport;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  ChatGPT.Overlay, FMX.Objects, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, FMX.Layouts, ChatGPT.Classes, FMX.Edit;

type
  TFrameImportExport = class(TFrameOveraly)
    LayoutClient: TLayout;
    RectangleFrame: TRectangle;
    LayoutActions: TLayout;
    ButtonOk: TButton;
    LabelCaption: TLabel;
    ButtonCancel: TButton;
    RadioButtonImport: TRadioButton;
    RadioButtonExport: TRadioButton;
    Layout1: TLayout;
    VertScrollBox: TVertScrollBox;
    LayoutExport: TLayout;
    Label17: TLabel;
    EditExport: TEdit;
    ClearEditButtonExport: TClearEditButton;
    Path2: TPath;
    Label9: TLabel;
    EditButtonExportFile: TEditButton;
    Path1: TPath;
    LayoutImport: TLayout;
    Label1: TLabel;
    EditImport: TEdit;
    ClearEditButtonImport: TClearEditButton;
    Path3: TPath;
    EditButtonImportFile: TEditButton;
    Path4: TPath;
    Label2: TLabel;
    SaveDialogExport: TSaveDialog;
    OpenDialogImport: TOpenDialog;
    Label3: TLabel;
    procedure FrameResize(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure RadioButtonExportChange(Sender: TObject);
    procedure EditButtonExportFileClick(Sender: TObject);
    procedure EditButtonImportFileClick(Sender: TObject);
    procedure RectangleBGClick(Sender: TObject);
  private
    FLayoutClientWidth, FLayoutClientHeight: Single;
    FProcCallback: TProc<TFrameImportExport, Boolean>;
  protected
    procedure SetMode(const Value: TWindowMode); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Cancel; override;
    class procedure Execute(AParent: TControl; ProcSet: TProc<TFrameImportExport>; ProcExecuted: TProc<TFrameImportExport, Boolean>);
  end;

var
  FrameImportExport: TFrameImportExport;

implementation

uses
  System.Math;

{$R *.fmx}

{ TFrameImportExport }

procedure TFrameImportExport.ButtonCancelClick(Sender: TObject);
begin
  Cancel;
end;

procedure TFrameImportExport.ButtonOkClick(Sender: TObject);
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, True);
  Release;
end;

procedure TFrameImportExport.Cancel;
begin
  if Assigned(FProcCallback) then
    FProcCallback(Self, False);
  Release;
end;

constructor TFrameImportExport.Create(AOwner: TComponent);
begin
  inherited;
  Name := '';
  FLayoutClientWidth := LayoutClient.Width;
  FLayoutClientHeight := LayoutClient.Height;
  RadioButtonExportChange(nil);
end;

procedure TFrameImportExport.EditButtonExportFileClick(Sender: TObject);
begin
  if SaveDialogExport.Execute then
  begin
    EditExport.Text := SaveDialogExport.FileName;
    RadioButtonExport.IsChecked := True;
  end;
end;

procedure TFrameImportExport.EditButtonImportFileClick(Sender: TObject);
begin
  if OpenDialogImport.Execute then
  begin
    EditImport.Text := OpenDialogImport.FileName;
    RadioButtonImport.IsChecked := True;
  end;
end;

class procedure TFrameImportExport.Execute(AParent: TControl; ProcSet: TProc<TFrameImportExport>; ProcExecuted: TProc<TFrameImportExport, Boolean>);
begin
  var Frame := TFrameImportExport.Create(AParent);
  Frame.Parent := AParent;
  Frame.FProcCallback := ProcExecuted;
  Frame.Align := TAlignLayout.Contents;
  Frame.BringToFront;
  if Assigned(ProcSet) then
    ProcSet(Frame);
  Frame.ButtonCancel.SetFocus;
end;

procedure TFrameImportExport.FrameResize(Sender: TObject);
begin
  LayoutClient.Width := Min(FLayoutClientWidth, Width);
  LayoutClient.Height := Min(FLayoutClientHeight, Height);
end;

procedure TFrameImportExport.RadioButtonExportChange(Sender: TObject);
begin
  LayoutExport.Enabled := RadioButtonExport.IsChecked;
  LayoutImport.Enabled := RadioButtonImport.IsChecked;
end;

procedure TFrameImportExport.RectangleBGClick(Sender: TObject);
begin
  Cancel;
end;

procedure TFrameImportExport.SetMode(const Value: TWindowMode);
begin
  inherited;
  if Mode = TWindowMode.Compact then
  begin
    LayoutClient.Align := TAlignLayout.Client;
    RectangleFrame.Corners := [];
  end
  else
  begin
    LayoutClient.Align := TAlignLayout.Center;
    RectangleFrame.Corners := AllCorners;
  end;
  FrameResize(nil);
end;

end.


unit ChatGPT.Android;

{$IFDEF ANDROID}

interface

uses
  System.SysUtils, System.Types, System.IOUtils,
  System.Messaging, FMX.Dialogs;

type
  TDialog = class
  private
    FProc: TProc<string>;
    FRequestCode: Integer;
    procedure ResultCallback(const Sender: TObject; const M: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Show(const MimeType: string; Proc: TProc<string>);
  end;

procedure OpenFileDialog(const MimeType: string; Proc: TProc<string>);

implementation

uses
  System.Permissions,Androidapi.Helpers, Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Webkit, Androidapi.JNI.Net, Androidapi.JNI.App,
  Androidapi.JNI.Support, FMX.Platform.Android;

constructor TDialog.Create;
begin
  FRequestCode := Random(9999999);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, ResultCallback);
end;

destructor TDialog.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, ResultCallback);
end;

function GetRealPathFromURI(Uri: Jnet_Uri): string;
var
  Cursor: JCursor;
  ColumnIndex: Integer;
begin
  Cursor := TAndroidHelper.Context.getContentResolver.query(Uri, nil, nil, nil, nil);
  ColumnIndex := Cursor.getColumnIndexOrThrow(StringToJString('_data'));
  Cursor.moveToFirst;
  Result := JStringToString(Cursor.getString(ColumnIndex));
  Cursor.close;
end;

procedure TDialog.ResultCallback(const Sender: TObject; const M: TMessage);
begin
  try
    if TMessageResultNotification(M).RequestCode = FRequestCode then
      if TMessageResultNotification(M).ResultCode = TJActivity.JavaClass.RESULT_OK then
      try
        FProc(GetRealPathFromURI(TMessageResultNotification(M).Value.getData));
      except
        on E: Exception do
          ShowMessage(E.Message);
      end;
  finally
    Free;
  end;
end;

procedure TDialog.Show(const MimeType: string; Proc: TProc<string>);
begin
  FProc := Proc;
  var Intent: JIntent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_PICK);
  Intent.setType(StringToJString(MimeType));
  //Intent.putExtra(TJIntent.JavaClass.EXTRA_ALLOW_MULTIPLE, False);
  MainActivity.startActivityForResult(Intent, FRequestCode);
end;

procedure OpenFileDialog(const MimeType: string; Proc: TProc<string>);
begin
  PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.READ_EXTERNAL_STORAGE)],
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
        TDialog.Create.Show(MimeType, Proc);
    end);
end;

{$ELSE}

interface

implementation

{$ENDIF}

end.


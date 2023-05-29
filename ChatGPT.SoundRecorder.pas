unit ChatGPT.SoundRecorder;

interface

uses
  System.Classes, System.SysUtils, FMX.Media;

type
  TAudioRecord = class(TComponent)
  private
    FFileName: string;
    FMicrophone: TAudioCaptureDevice;
    FOnStartRecord: TNotifyEvent;
    procedure MicrophonePermissionRequest(Sender: TObject; const&Message: string; const AccessGranted: Boolean);
    procedure SetOnStartRecord(const Value: TNotifyEvent);
    function GetIsAvailableDevice: Boolean;
  public
    function IsMicrophoneRecording: Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure StartRecord(const FileName: string);
    procedure StopRecord;
    property Microphone: TAudioCaptureDevice read FMicrophone write FMicrophone;
    property OnStartRecord: TNotifyEvent read FOnStartRecord write SetOnStartRecord;
    property IsAvailableDevice: Boolean read GetIsAvailableDevice;
  end;

implementation

{ TAudioRecord }

constructor TAudioRecord.Create(AOwner: TComponent);
begin
  inherited;
  FMicrophone := TCaptureDeviceManager.Current.DefaultAudioCaptureDevice;
  if Assigned(FMicrophone) then
    FMicrophone.OnPermissionRequest := MicrophonePermissionRequest;
end;

function TAudioRecord.GetIsAvailableDevice: Boolean;
begin
  Result := Assigned(FMicrophone);
end;

function TAudioRecord.IsMicrophoneRecording: Boolean;
begin
  if not Assigned(FMicrophone) then
    Exit(False);
  Result := FMicrophone.State = TCaptureDeviceState.Capturing;
end;

procedure TAudioRecord.MicrophonePermissionRequest(Sender: TObject; const Message: string; const AccessGranted: Boolean);
begin
  if not Assigned(FMicrophone) then
    Exit;
  if AccessGranted then
  begin
    FMicrophone.StartCapture;
    if Assigned(FOnStartRecord) then
      FOnStartRecord(Self);
  end
  else
    raise Exception.Create('Cannot record audio without the relevant permission being granted: '#13#10 + Message);
end;

procedure TAudioRecord.SetOnStartRecord(const Value: TNotifyEvent);
begin
  FOnStartRecord := Value;
end;

procedure TAudioRecord.StartRecord(const FileName: string);
begin
  if not Assigned(FMicrophone) then
    raise Exception.Create('No suitable device');
  FFileName := FileName;
  FMicrophone.FileName := FFileName;
  FMicrophone.RequestPermission;
end;

procedure TAudioRecord.StopRecord;
begin
  if not Assigned(FMicrophone) then
    raise Exception.Create('No suitable device');
  if IsMicrophoneRecording then
    FMicrophone.StopCapture;
end;

end.


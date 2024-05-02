{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2012-2023 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Media.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Win.ComObj, System.Variants, System.Classes, System.SysUtils, System.Math, System.Types, System.SyncObjs,
  Winapi.Windows, Winapi.DirectShow9, Winapi.Direct3D9, Winapi.ActiveX, FMX.Context.DX9, FMX.Types, FMX.Types3D,
  FMX.Forms, FMX.Media, FMX.Platform.Win, FMX.Graphics, FMX.Consts;

type

{ TWindowsCaptureDeviceManager }

  TWindowsCaptureDeviceManager = class(TCaptureDeviceManager)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{ TWindowsCaptureDevice }

  TWindowsAudioCaptureDevice = class(TAudioCaptureDevice)
  private
    FMoniker: IMoniker;
    FBaseFilter: IBaseFilter;
    FGraphBuilder: IGraphBuilder;
    FCaptureGraphBuilder: ICaptureGraphBuilder2;
    FMediaControl: IMediaControl;
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    function GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string; override;
    function GetDeviceState: TCaptureDeviceState; override;
    function GetFilterString: string; override;
  public
    constructor Create(AManager: TCaptureDeviceManager; const AMoniker: IMoniker; const ADefault: Boolean); reintroduce;
    destructor Destroy; override;
  end;

{ TVMRRenderer }

  TVMRRenderer = class(TInterfacedPersistent, IVMRSurfaceAllocator9, IVMRImagePresenter9)
  private
    FVideoTexture: IDirect3DTexture9; // must be RenderTarget to allow GPU copy
    FSysMemSurface: IDirect3DSurface9;
    FSurface: IDirect3DSurface9;
    FSurfaceAllocatorNotify: IVMRSurfaceAllocatorNotify9;
    FBaseFilter: IBaseFilter;
    FVMRFilterConfig: IVMRFilterConfig9;
    FSampleBufferReady: TSampleBufferReadyEvent;
    FRunning: Boolean;
    FIsSkipFrames: Boolean;
    FRequestedBufferSize: TPoint;
    FBufferLoadTime: TMediaTime;
    FBufferLoaded: Boolean;
    FIsFinishedProcessingEvent: TEvent;
    { IVMRSurfaceAllocator9 }
    function InitializeDevice(dwUserID: DWORD; lpAllocInfo: PVMR9AllocationInfo;
      var lpNumBuffers: DWORD): HResult; stdcall;
    function TerminateDevice(dwID: DWORD): HResult; stdcall;
    function GetSurface(dwUserID: DWORD; SurfaceIndex: DWORD; SurfaceFlags: DWORD;
      out lplpSurface: IDirect3DSurface9): HResult; stdcall;
    function AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult; stdcall;
    { IVMRImagePresenter9 }
    function StartPresenting(dwUserID: DWORD): HResult; stdcall;
    function StopPresenting(dwUserID: DWORD): HResult; stdcall;
    function PresentImage(dwUserID: DWORD; lpPresInfo: PVMR9PresentationInfo): HResult; stdcall;
    { }
    function AddToGraph(const FGraphBuilder: IGraphBuilder): Boolean;
    function LoadBufferToSystemMemory: Boolean;
    procedure SampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean);
    { sync }
    procedure Sync;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetSkipFrames;
  end;

{ TWindowsCaptureDevice }

  TWindowsVideoCaptureDevice = class(TVideoCaptureDevice)
  private const
    FrameDurationScale = 10000000;
  private
    FMoniker: IMoniker;
    FBaseFilter: IBaseFilter;
    FGraphBuilder: IGraphBuilder;
    FCaptureGraphBuilder: ICaptureGraphBuilder2;
    FMediaControl: IMediaControl;
    FVMRRenderer: TVMRRenderer;
    procedure VMRRendererSampleReady(Sender: TObject; const ATime: TMediaTime);
  protected
    procedure DoStartCapture; override;
    procedure DoStopCapture; override;
    procedure DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean); override;
    function GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string; override;
    function GetDeviceState: TCaptureDeviceState; override;
    /// <summary>Inherited from TVideoCaptureDevice.</summary>
    function GetCaptureSetting: TVideoCaptureSetting; override;
    /// <summary>Inherited from TVideoCaptureDevice.</summary>
    function DoSetCaptureSetting(const ASetting: TVideoCaptureSetting): Boolean; override;
    /// <summary>Inherited from TVideoCaptureDevice.</summary>
    function DoGetAvailableCaptureSettings: TArray<TVideoCaptureSetting>; override;
    /// <summary>Inherited from TVideoCaptureDevice.</summary>
    procedure DoSetQuality(const Value: TVideoCaptureQuality); override;
  public
    constructor Create(AManager: TCaptureDeviceManager; const AMoniker: IMoniker; const ADefault: Boolean); reintroduce;
    destructor Destroy; override;
  end;

{ TWindowsMedia }

  TWindowsMedia = class(TMedia)
  private
    FGraphBuilder: IGraphBuilder;
    FMediaControl: IMediaControl;
    FMediaSeeking: IMediaSeeking;
    FBasicAudio: IBasicAudio;
    FVMR: IBaseFilter;
    FVMRFilterConfig9: IVMRFilterConfig9;
    FVMRWindowlessControl9: IVMRWindowlessControl9;
    FVMRFilterConfig7: IVMRFilterConfig;
    FVMRWindowlessControl7: IVMRWindowlessControl;
    FWnd: HWND;
    FWaitCompletionThread: TThread;
    FMediaState: TMediaState;
    { Initialization }
    procedure InitWnd;
    procedure DestroyWnd;
    function InitMediaControl: Boolean;
    function InitGraphTree: Boolean;
    function InitVideoMixingRenderer9: Boolean;
    function LocateVideoMixingRenderer9: Boolean;
    function InitVideoMixingRenderer7: Boolean;
    function LocateVideoMixingRenderer7: Boolean;
    { Playback thread listener }
    procedure StartWaitCompletionThread;
    procedure StopAndFreeWaitCompletionThread;
    procedure WaitCompletionThreadTerminated(Sender: TObject);
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    function GetDuration: TMediaTime; override;
    function GetCurrent: TMediaTime; override;
    procedure SetCurrent(const Value: TMediaTime); override;
    function GetVideoSize: TPointF; override;
    function GetMediaState: TMediaState; override;
    function GetVolume: Single; override;
    procedure SetVolume(const Value: Single); override;
    procedure UpdateMediaFromControl; override;
    procedure DoPlay; override;
    procedure DoStop; override;
  public
    constructor Create(const AFileName: string); override;
    destructor Destroy; override;
  end;

  /// <summary>This thread is waiting for completion of playback of a media file.</summary>
  TWaitMediaCompletionThread = class(TThread)
  private
    FMediaEvent: IMediaEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const AMediaEvent: IMediaEvent);
  end;

  TWindowsMediaCodec = class(TCustomMediaCodec)
  public
    function CreateFromFile(const AFileName: string): TMedia; override;
  end;

implementation

uses
  System.RTLConsts, System.SysConst, System.Generics.Collections, Winapi.UrlMon;

const
  MinAllowedVolume = -10000;

{ TWindowsCaptureDeviceManager }

constructor TWindowsCaptureDeviceManager.Create;
var
  Moniker: IMoniker;
  DevEnum: ICreateDevEnum;
  Enum: IEnumMoniker;
  Default: Boolean;
begin
  inherited;
  CoCreateInstance(CLSID_SystemDeviceEnum, nil, CLSCTX_INPROC_SERVER,
    IID_ICreateDevEnum, DevEnum);
  if DevEnum <> nil then
  begin
    // audio devices
    Default := True;
    DevEnum.CreateClassEnumerator(CLSID_AudioInputDeviceCategory, Enum, 0);
    if Enum <> nil then
      while (Enum.Next(1, Moniker, nil) = S_OK) do
      begin
        TWindowsAudioCaptureDevice.Create(Self, Moniker, Default);
        Default := False;
      end;
    Enum := nil;
    // video devices
    Default := True;
    DevEnum.CreateClassEnumerator(CLSID_VideoInputDeviceCategory, Enum, 0);
    if Enum <> nil then
      while (Enum.Next(1, Moniker, nil) = S_OK) do
      begin
        TWindowsVideoCaptureDevice.Create(Self, Moniker, Default);
        Default := False;
      end;
  end;
end;

destructor TWindowsCaptureDeviceManager.Destroy;
begin
  inherited;
end;

{ TWindowsAudioCaptureDevice }

constructor TWindowsAudioCaptureDevice.Create(AManager: TCaptureDeviceManager;
  const AMoniker: IMoniker; const ADefault: Boolean);
begin
  inherited Create(AManager, ADefault);

  if AMoniker = nil then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AMoniker']);

  FMoniker := AMoniker;
end;

destructor TWindowsAudioCaptureDevice.Destroy;
begin
  inherited;
end;

procedure InitBaseFilter(const Self: TWindowsAudioCaptureDevice); forward; overload;

const                   //{3C78B8E2-6C4D-11d1-ADE2-0000F8754B99}
  CLSID_WavDest: TGUID = '{8A667154-F9CB-11D2-AD8A-0060B0575ABC}';

procedure InitAudioFilter(const Self: TWindowsAudioCaptureDevice);
var
  HR: HResult;
begin
  if Self.FBaseFilter <> nil then
    Exit;

  HR := Self.FMoniker.BindToObject(nil, nil, IID_IBaseFilter, Self.FBaseFilter);
  //HR := coCreateInstance(CLSID_WavDest, nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, Self.FBaseFilter);
  if Failed(HR) then
    raise ECaptureDeviceException.CreateRes(@SCannotCreateDirectShowCaptureFilter);
end;

procedure TWindowsAudioCaptureDevice.DoStartCapture;
var
  HR: HResult;
  ppf: IBaseFilter;
  sink: IFileSinkfilter;
  FailedToRun: Boolean;
begin
  InitAudioFilter(Self);

  FailedToRun := True;
  // Create GraphBuilder
  HR := CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC_SERVER,
    IID_ICaptureGraphBuilder2, FCaptureGraphBuilder);
  if Succeeded(HR) then
  begin
    HR := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, FGraphBuilder);
    if Succeeded(HR) then
    begin
      FCaptureGraphBuilder.SetFiltergraph(FGraphBuilder);
      // Get IMediaControl
      FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
      HR := FGraphBuilder.AddFilter(FBaseFilter, 'Capture Filter');
      if Succeeded(HR) then
      begin
        HR := FCaptureGraphBuilder.SetOutputFileName(MEDIASUBTYPE_WAVE, PChar(FFileName), ppf, sink);
        if Succeeded(HR) then
        begin
          HR := FCaptureGraphBuilder.RenderStream(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Audio, FBaseFilter, nil, ppf);
          if Succeeded(HR) then
          begin
            if FMediaControl <> nil then
            begin
              HR := FMediaControl.Run;
              FailedToRun := Failed(HR);
              if not FailedToRun then
                Exit; // Success
            end;
          end;
        end;
      end;
    end;
  end;
  FCaptureGraphBuilder := nil;
  FGraphBuilder := nil;
  FMediaControl := nil;

  if FailedToRun then
    raise ECaptureDeviceException.CreateRes(@SCannotRunDirectShowFilterGraph);
end;

procedure TWindowsAudioCaptureDevice.DoStopCapture;
begin
  if FMediaControl <> nil then
  begin
    FMediaControl.StopWhenReady;
    FCaptureGraphBuilder := nil;
    FGraphBuilder := nil;
    FMediaControl := nil;
  end;
end;

function TWindowsAudioCaptureDevice.GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string;
var
  PropBag: IPropertyBag;
  V: OleVariant;
begin
  FMoniker.BindToStorage(nil, nil, IPropertyBag, PropBag);
  if PropBag <> nil then
  begin
    case Prop of
      TProperty.Description:
        begin
          PropBag.Read('Description', V, nil);
          Result := V;
        end;
      TProperty.Name:
        begin
          PropBag.Read('FriendlyName', V, nil);
          Result := V;
        end;
      TProperty.UniqueID:
        begin
          PropBag.Read('WaveInID', V, nil);
          Result := V;
        end;
    else
      Result := '';
    end;
  end
  else
    Result := '';
end;

function TWindowsAudioCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FMediaControl <> nil then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

function TWindowsAudioCaptureDevice.GetFilterString: string;
begin
  Result := SVWAVFiles + '(*.wav)|*.wav';
end;

procedure InitBaseFilter(const Self: TWindowsAudioCaptureDevice);
var
  HR: HResult;
begin
  if Self.FBaseFilter <> nil then
    Exit;

  HR := Self.FMoniker.BindToObject(nil, nil, IID_IBaseFilter, Self.FBaseFilter);

  if Failed(HR) then
    raise ECaptureDeviceException.CreateRes(@SCannotCreateDirectShowCaptureFilter);
end;

{ TVMRRenderer }

constructor TVMRRenderer.Create;
begin
  inherited Create;
  FIsFinishedProcessingEvent := TEvent.Create;
  FIsFinishedProcessingEvent.SetEvent;
  FIsSkipFrames := False;
end;

destructor TVMRRenderer.Destroy;
begin
  FreeAndNil(FIsFinishedProcessingEvent);
  FVideoTexture := nil;
  FSysMemSurface := nil;
  FSurfaceAllocatorNotify := nil;
  FBaseFilter := nil;
  FVMRFilterConfig := nil;
  inherited;
end;

function TVMRRenderer.AddToGraph(const FGraphBuilder: IGraphBuilder): Boolean;
var
  HR: HResult;
begin
  // Create VMR
  Result := True;
  HR := CoCreateInstance(CLSID_VideoMixingRenderer9, nil, CLSCTX_INPROC, IID_IBaseFilter, FBaseFilter);
  if Succeeded(HR) then
  begin
    FBaseFilter.QueryInterface(IVMRFilterConfig9, FVMRFilterConfig);
    HR := FVMRFilterConfig.SetRenderingMode(VMR9Mode_Renderless);
    if Succeeded(HR) then
    begin
      FBaseFilter.QueryInterface(IVMRSurfaceAllocatorNotify9, FSurfaceAllocatorNotify);
      HR := FSurfaceAllocatorNotify.AdviseSurfaceAllocator(Cardinal(Self), Self);
      if Succeeded(HR) then
      begin
        IVMRSurfaceAllocator9(Self).AdviseNotify(FSurfaceAllocatorNotify);
        HR := FGraphBuilder.AddFilter(FBaseFilter, 'Video Mixing Renderer9');
        if Succeeded(HR) then
          Exit; // Success
      end;
    end;
  end;
  Result := False;
  FVMRFilterConfig := nil;
  FBaseFilter := nil;
end;

{ IVMRSurfaceAllocator9 }

function TVMRRenderer.GetSurface(dwUserID, SurfaceIndex, SurfaceFlags: DWORD; out lplpSurface: IDirect3DSurface9): HResult;
begin
  if FSurface <> nil then
  begin
    lplpSurface := FSurface;
    Result := S_OK;
  end
  else
    Result := E_FAIL;
end;

function TVMRRenderer.InitializeDevice(dwUserID: DWORD; lpAllocInfo: PVMR9AllocationInfo; var lpNumBuffers: DWORD): HResult;
begin
  Result := E_NOTIMPL;
  if TCustomDX9Context.SharedDevice <> nil then
    Result := FSurfaceAllocatorNotify.AllocateSurfaceHelper(lpAllocInfo, lpNumBuffers, FSurface);
end;

function TVMRRenderer.TerminateDevice(dwID: DWORD): HResult;
begin
  FVideoTexture := nil;
  FSysMemSurface := nil;
  FSurface := nil;
  Result := S_OK;
end;

function TVMRRenderer.AdviseNotify(lpIVMRSurfAllocNotify: IVMRSurfaceAllocatorNotify9): HResult;
begin
  Result := E_NOTIMPL;
  if TCustomDX9Context.SharedDevice <> nil then
    Result := lpIVMRSurfAllocNotify.SetD3DDevice(TCustomDX9Context.SharedDevice, TCustomDX9Context.Direct3D9Obj.GetAdapterMonitor(D3DADAPTER_DEFAULT));
end;

{ IVMRImagePresenter9 }

function TVMRRenderer.StartPresenting(dwUserID: DWORD): HResult;
begin
  Result := S_OK;
  FRunning := True;
  FRequestedBufferSize := TPoint.Zero;
  FBufferLoaded := False;
end;

function TVMRRenderer.StopPresenting(dwUserID: DWORD): HResult;
begin
  FRunning := False;
  Result := S_OK;
end;

function TVMRRenderer.PresentImage(dwUserID: DWORD; lpPresInfo: PVMR9PresentationInfo): HResult;
begin
  TMonitor.Enter(Self);
  try
    if FIsSkipFrames then
      Exit(S_OK);
  finally
    TMonitor.Exit(Self);
  end;

  if FRunning and not FBufferLoaded then
  begin
    FIsFinishedProcessingEvent.ResetEvent;
    try
      if LoadBufferToSystemMemory then
      begin
        FBufferLoadTime := lpPresInfo.rtStart;
        FBufferLoaded := True;
        TThread.Synchronize(nil, Sync);
      end;
    finally
      FIsFinishedProcessingEvent.SetEvent;
    end;
  end;

  Result := S_OK;
end;

function TVMRRenderer.LoadBufferToSystemMemory: Boolean;
var
  Device: IDirect3DDevice9;
  Desc: TD3DSurfaceDesc;
  CopyRect: TRect;
  VideoSurface: IDirect3DSurface9;
begin
  Result := False;

  if (FSurface <> nil) and Succeeded(FSurface.GetDesc(Desc)) then
  begin
    Device := TCustomDX9Context.SharedDevice;

    if not FRequestedBufferSize.IsZero then
      CopyRect := TRect.Create(0, 0, FRequestedBufferSize.X, FRequestedBufferSize.Y)
    else
      CopyRect := TRect.Create(0, 0, Desc.Width, Desc.Height);

    if (FVideoTexture = nil) or (Integer(Desc.Width) <> CopyRect.Width) or
      (Integer(Desc.Height) <> CopyRect.Height) then
    begin
      FSysMemSurface := nil;
      FVideoTexture := nil;

      if Failed(Device.CreateTexture(CopyRect.Width, CopyRect.Height, 1, D3DUSAGE_RENDERTARGET, D3DFMT_A8R8G8B8,
        D3DPOOL_DEFAULT, FVideoTexture, nil)) then
        Exit;

      if Failed(Device.CreateOffscreenPlainSurface(CopyRect.Width, CopyRect.Height, D3DFMT_A8R8G8B8, D3DPOOL_SYSTEMMEM,
        FSysMemSurface, nil)) then
      begin
        FVideoTexture := nil;
        Exit;
      end;
    end;

    if Succeeded(FVideoTexture.GetSurfaceLevel(0, VideoSurface)) then
    begin
      if Failed(Device.StretchRect(FSurface, nil, VideoSurface, @CopyRect, D3DTEXF_LINEAR)) then
        Exit;

      Result := Succeeded(Device.GetRenderTargetData(VideoSurface, FSysMemSurface));
    end;
  end;
end;

procedure TVMRRenderer.SampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean);

  procedure MoveResetAlpha(Source, Dest: Pointer; Count: Integer);
  begin
    while Count > 0 do
    begin
      PLongWord(Dest)^ := PLongWord(Source)^ or $FF000000;
      Inc(NativeInt(Source), 4);
      Inc(NativeInt(Dest), 4);
      Dec(Count);
    end;
  end;

var
  Desc: TD3DSurfaceDesc;
  DescValid: Boolean;
  Lock: TD3DLockedRect;
  Map: TBitmapData;
  I: Integer;
begin
  if not ASetSize and ABitmap.IsEmpty then
    raise EBitmapIncorrectSize.Create(SBitmapIncorrectSize);

  if FSurface <> nil then
  begin
    if FVideoTexture <> nil then
      DescValid := Succeeded(FVideoTexture.GetLevelDesc(0, Desc))
    else
      DescValid := Succeeded(FSurface.GetDesc(Desc));

    if not DescValid then
      raise EBitmapIncorrectSize.Create(SRetrieveSurfaceDescription);

    if ASetSize then
    begin
      ABitmap.SetSize(Desc.Width, Desc.Height);
      FRequestedBufferSize := TPoint.Zero;
    end
    else if (Integer(Desc.Width) <> ABitmap.Width) or (Integer(Desc.Height) <> ABitmap.Height) then
    begin
      FRequestedBufferSize := TPoint.Create(ABitmap.Width, ABitmap.Height);

      // Buffer that was loaded previously has incorrect size, therefore it needs to be reloaded again here.
      if not LoadBufferToSystemMemory then
        raise ERetrieveSurfaceContents.Create(SRetrieveSurfaceContents);
    end;

    if Failed(FSysMemSurface.LockRect(Lock, nil, D3DLOCK_READONLY)) then
      raise ERetrieveSurfaceContents.Create(SRetrieveSurfaceContents);
    try
      if not ABitmap.Map(TMapAccess.Write, Map) then
        raise EAcquireBitmapAccess.Create(SAcquireBitmapAccess);
      try
        for I := 0 to ABitmap.Height - 1 do
          MoveResetAlpha(Pointer(NativeInt(Lock.pBits) + Lock.Pitch * I), Map.GetScanline(I), Desc.Width);
      finally
        ABitmap.Unmap(Map);
      end;
    finally
      FSysMemSurface.UnlockRect;
    end;
  end;
end;

procedure TVMRRenderer.SetSkipFrames;
begin
  TMonitor.Enter(Self);
  try
    FIsSkipFrames := True;
  finally
    TMonitor.Exit(Self);
  end;

  // When we process camera frame we synchronize rendered frame with Main UI Thread and for this purpose
  // we use TThread.Synchronize. When user stops camera, it uses synchronouse method IMediaControl.Stop,
  // which blocks UI thread and as result main thread cannot make synchronization and we get dead lock. 
  // For avoiding it we need to invoke CheckSynchronize while current frame is processed.
  while FRunning and (FIsFinishedProcessingEvent.WaitFor(20) = TWaitResult.wrTimeout) do
    CheckSynchronize;
end;

procedure TVMRRenderer.Sync;
begin
  if FBufferLoaded then
  begin
    if Assigned(FSampleBufferReady) then
      FSampleBufferReady(Self, FBufferLoadTime);

    FBufferLoaded := False;
  end;
end;

{ TWindowsVideoCaptureDevice }

function PinMatchesCategory(APin: IPin; ACategory: TGuid): Boolean;
var
  LSet: IKsPropertySet;
  PinCategory: TGuid;
  Value: DWORD;
  LResult: HRESULT;
begin
  Result := False;

  if Supports(APin, IKsPropertySet, LSet) then
  begin
    LResult := LSet.Get(AMPROPSETID_Pin, AMPROPERTY_PIN_CATEGORY, nil, 0, PinCategory, SizeOf(TGuid), Value);
    if Succeeded(LResult) and (Value = SizeOf(TGuid)) then
      Result := (PinCategory = ACategory);
  end;
end;

function GetStreamConfig(const APin: IPin): IAMStreamConfig;
begin
  Supports(APin, IAMStreamConfig, Result);
end;

function GetCapturePin(const AFilter: IBaseFilter): IPin;
var
  PinsEnum: IEnumPins;
  Pin: IPin;
  Config: IAMStreamConfig;
begin
  Result := nil;
  AFilter.EnumPins(PinsEnum);
  if PinsEnum <> nil then
    while PinsEnum.Next(1, Pin, nil) = S_OK do
      if PinMatchesCategory(Pin, PIN_CATEGORY_CAPTURE) then
      begin
        if Supports(Pin, IAMStreamConfig, Config) then
          Exit(Pin);
      end;
end;

constructor TWindowsVideoCaptureDevice.Create(AManager: TCaptureDeviceManager;
  const AMoniker: IMoniker; const ADefault: Boolean);
begin
  inherited Create(AManager, ADefault);

  if AMoniker = nil then
    raise EArgumentNilException.CreateResFmt(@SParamIsNil, ['AMoniker']);

  FMoniker := AMoniker;
end;

destructor TWindowsVideoCaptureDevice.Destroy;
begin
  inherited;
end;

procedure InitBaseFilter(const Self: TWindowsVideoCaptureDevice); forward; overload;

procedure TWindowsVideoCaptureDevice.DoStartCapture;
var
  HR: HResult;
  FailedToRun: Boolean;
begin
  InitBaseFilter(Self);

  FailedToRun := False;
  // Create GraphBuilder
  HR := CoCreateInstance(CLSID_CaptureGraphBuilder2, nil, CLSCTX_INPROC_SERVER,
    IID_ICaptureGraphBuilder2, FCaptureGraphBuilder);
  if Succeeded(HR) then
  begin
    HR := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IFilterGraph2, FGraphBuilder);
    if Succeeded(HR) then
    begin
      FCaptureGraphBuilder.SetFiltergraph(FGraphBuilder);
      // Get IMediaControl
      FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl);
      // Set Capture
      HR := FGraphBuilder.AddFilter(FBaseFilter, 'Capture');
      if Succeeded(HR) then
      begin
        // Create VMR
        FVMRRenderer := TVMRRenderer.Create;
        FVMRRenderer.FSampleBufferReady := VMRRendererSampleReady;
        FVMRRenderer.AddToGraph(FGraphBuilder);
        // Start
        HR := FCaptureGraphBuilder.RenderStream(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FBaseFilter, nil, FVMRRenderer.FBaseFilter);
        if Succeeded(HR) then
        begin
          if FMediaControl <> nil then
          begin
            HR := FMediaControl.Run;
            FailedToRun := Failed(HR);
            if not FailedToRun then
              Exit; // Success
          end;
        end;
      end;
    end;
  end;
  FMediaControl := nil;
  FCaptureGraphBuilder := nil;
  FGraphBuilder := nil;
  FreeAndNil(FVMRRenderer);

  if FailedToRun then
    raise ECaptureDeviceException.CreateRes(@SCannotRunDirectShowFilterGraph);
end;

procedure TWindowsVideoCaptureDevice.DoStopCapture;
begin
  if (FMediaControl <> nil) then
  begin
    FVMRRenderer.SetSkipFrames;
    FMediaControl.StopWhenReady;
    FMediaControl := nil;
    FCaptureGraphBuilder := nil;
    FGraphBuilder := nil;
    FreeAndNil(FVMRRenderer);
  end;
end;

function TWindowsVideoCaptureDevice.DoGetAvailableCaptureSettings: TArray<TVideoCaptureSetting>;
var
  MediaEnum: IEnumMediaTypes;
  Pin: IPin;
  MediaType: PAMMediaType;
  OldMediaType: PAMMediaType;
  MediaHeader: PVideoInfoHeader;
  List: TList<TVideoCaptureSetting>;
  Setting: TVideoCaptureSetting;
  Config: IAMStreamConfig;
begin
  InitBaseFilter(Self);

  SetLength(Result, 0);
  List := TList<TVideoCaptureSetting>.Create;
  try
    Pin := GetCapturePin(FBaseFilter);
    if Pin <> nil then
    begin
      Config := GetStreamConfig(Pin);
      Config.GetFormat(OldMediaType);
      try
        Config.SetFormat(nil);
        Pin.EnumMediaTypes(MediaEnum);
        if MediaEnum <> nil then
          while MediaEnum.Next(1, MediaType, nil) = S_OK do
            if (MediaType.formattype = FORMAT_VideoInfo) and (MediaType.cbFormat >= SizeOf(VIDEOINFOHEADER)) and
              (MediaType.pbFormat <> nil) then
            begin
              MediaHeader := MediaType.pbFormat;
              Setting.Width := MediaHeader.bmiHeader.biWidth;
              Setting.Height := MediaHeader.bmiHeader.biHeight;
              Setting.FrameRate := FrameDurationScale / MediaHeader.AvgTimePerFrame;
              if not List.Contains(Setting) then
                List.Add(Setting);
            end;
      finally
        Config.SetFormat(OldMediaType);
      end;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TWindowsVideoCaptureDevice.DoSetCaptureSetting(const ASetting: TVideoCaptureSetting): Boolean;
var
  MediaEnum: IEnumMediaTypes;
  Pin: IPin;
  MediaType, SelectedMediaType: PAMMediaType;
  MediaHeader: PVideoInfoHeader;
  Found: Boolean;
  Config: IAMStreamConfig;
  DesiredAvgDuration: Double;
begin
  InitBaseFilter(Self);

  Found := False;
  Result := False;
  DesiredAvgDuration := FrameDurationScale / ASetting.FrameRate;
  Pin := GetCapturePin(FBaseFilter);

  if Pin <> nil then
  begin
    // We store the current configuration in order to reset it if there was any problem
    Config := GetStreamConfig(Pin);
    Config.GetFormat(SelectedMediaType);
    try
      Config.SetFormat(nil);
      Pin.EnumMediaTypes(MediaEnum);
      if MediaEnum <> nil then
        while (Found = False) and (MediaEnum.Next(1, MediaType, nil) = S_OK) do
        begin
          if (MediaType.formattype = FORMAT_VideoInfo) and (MediaType.cbFormat >= SizeOf(VIDEOINFOHEADER)) and
            (MediaType.pbFormat <> nil) then
          begin
            MediaHeader := MediaType.pbFormat;

            if (MediaHeader.bmiHeader.biWidth = ASetting.Width) and (MediaHeader.bmiHeader.biHeight = ASetting.Height) and
              (MediaHeader.AvgTimePerFrame = DesiredAvgDuration) then
            begin
              // As described in Direct Show, this is an estimation
              Found := True;
              SelectedMediaType := MediaType;
              Break;
            end;
          end;
        end;
    finally
      Result := Found and Succeeded(Config.SetFormat(SelectedMediaType));
    end;
  end;
end;

procedure TWindowsVideoCaptureDevice.DoSetQuality(const Value: TVideoCaptureQuality);
var
  SettingsList: TArray<TVideoCaptureSetting>;
  Priority: TVideoCaptureSettingPriority;
begin
  if Value <> TVideoCaptureQuality.CaptureSettings then
  begin
    Priority := CaptureSettingPriority;
    try
      CaptureSettingPriority := TVideoCaptureSettingPriority.Resolution;
      SettingsList := AvailableCaptureSettings;
    finally
      CaptureSettingPriority := Priority;
    end;

    if Length(SettingsList) > 0 then
    begin
      case Value of
        TVideoCaptureQuality.PhotoQuality: CaptureSetting := SettingsList[0];
        TVideoCaptureQuality.HighQuality: CaptureSetting := SettingsList[0];
        TVideoCaptureQuality.MediumQuality: CaptureSetting := SettingsList[Length(SettingsList) div 2];
        TVideoCaptureQuality.LowQuality: CaptureSetting := SettingsList[High(SettingsList)];
      end;
    end;
  end;
  inherited;
end;

function TWindowsVideoCaptureDevice.GetCaptureSetting: TVideoCaptureSetting;
var
  PinsEnum: IEnumPins;
  Pin: IPin;
  MediaType: PAMMediaType;
  MediaHeader: PVideoInfoHeader;
  Config: IAMStreamConfig;
begin
  InitBaseFilter(Self);

  Result := TVideoCaptureSetting.Create;
  FBaseFilter.EnumPins(PinsEnum);

  if PinsEnum <> nil then
    while (PinsEnum.Next(1, Pin, nil) = S_OK) and Supports(Pin, IAMStreamConfig, Config) do
    begin
      Config.GetFormat(MediaType);
      MediaHeader := MediaType.pbFormat;
      Exit(TVideoCaptureSetting.Create(MediaHeader.bmiHeader.biWidth, MediaHeader.bmiHeader.biHeight,
        FrameDurationScale / MediaHeader.AvgTimePerFrame));
    end;
end;

procedure TWindowsVideoCaptureDevice.DoSampleBufferToBitmap(const ABitmap: TBitmap; const ASetSize: Boolean);
begin
  FVMRRenderer.SampleBufferToBitmap(ABitmap, ASetSize);
end;

function TWindowsVideoCaptureDevice.GetDeviceProperty(const Prop: TCaptureDevice.TProperty): string;
var
  PropBag: IPropertyBag;
  HR: HRESULT;
  V: OleVariant;
begin
  HR := FMoniker.BindToStorage(nil, nil, IPropertyBag, PropBag);
  if Succeeded(HR) then
  begin
    case Prop of
      TProperty.Description:
        begin
          PropBag.Read('Description', V, nil);
          Result := V;
        end;
      TProperty.Name:
        begin
          PropBag.Read('FriendlyName', V, nil);
          Result := V;
        end;
      TProperty.UniqueID:
        begin
          PropBag.Read('DevicePath', V, nil);
          Result := V;
        end;
    else
      Result := string.Empty;
    end;
  end
  else
    Result := string.Empty;
end;

function TWindowsVideoCaptureDevice.GetDeviceState: TCaptureDeviceState;
begin
  if FMediaControl <> nil then
    Result := TCaptureDeviceState.Capturing
  else
    Result := TCaptureDeviceState.Stopped;
end;

procedure TWindowsVideoCaptureDevice.VMRRendererSampleReady(Sender: TObject; const ATime: TMediaTime);
begin
  SampleBufferReady(ATime);
end;

procedure InitBaseFilter(const Self: TWindowsVideoCaptureDevice);
var
  HR: HResult;
begin
  if Self.FBaseFilter <> nil then
    Exit;

  HR := Self.FMoniker.BindToObject(nil, nil, IID_IBaseFilter, Self.FBaseFilter);

  if Failed(HR) then
    raise ECaptureDeviceException.CreateRes(@SCannotCreateDirectShowCaptureFilter);
end;

{ TWindowsMedia }

constructor TWindowsMedia.Create(const AFileName: string);
begin
  if (not FileExists(AFileName)) and (IsValidURL(nil, PChar(AFileName), 0) <> S_OK) then
    raise EFileNotFoundException.Create(SFileNotFound);

  inherited;
  FMediaState := TMediaState.Unavailable;
  InitWnd;
  if InitMediaControl and InitVideoMixingRenderer9 then
  begin
    if not LocateVideoMixingRenderer9 then
    begin
      FVMR := nil;
      FVMRFilterConfig9 := nil;
      if InitVideoMixingRenderer7 then
        LocateVideoMixingRenderer7
      else
        raise EMediaException.Create(SMediaGlobalError);
    end;
    if InitGraphTree then
      Exit;
  end;
  DestroyWnd;
  FGraphBuilder := nil;
  FMediaControl := nil;
  raise EMediaException.CreateFmt(SMediaFileNotSupported, [ExtractFileName(AFileName)]);
end;

destructor TWindowsMedia.Destroy;
begin
  StopAndFreeWaitCompletionThread;
  DestroyWnd;
  if FMediaControl <> nil then
  begin
    FMediaControl.StopWhenReady;
    FMediaControl := nil;
  end;
  FGraphBuilder := nil;
  inherited Destroy;
end;

procedure TWindowsMedia.DestroyWnd;
begin
  DestroyWindow(FWnd);
end;

procedure TWindowsMedia.DoPlay;
begin
  FMediaState := TMediaState.Playing;
  FMediaControl.Run;

  StartWaitCompletionThread;
end;

procedure TWindowsMedia.DoStop;
begin
  StopAndFreeWaitCompletionThread;

  FMediaControl.StopWhenReady;
  FMediaState := TMediaState.Stopped;
end;

procedure TWindowsMedia.UpdateMediaFromControl;
var
  P: TPointF;
  R: TRect;
  Bounds: TRectF;
  Form: TCommonCustomForm;
  BoundsInt: TRect;
begin
  if FWnd = 0 then
    Exit;

  if (Control <> nil) and not (csDesigning in Control.ComponentState) and (Control.ParentedVisible) and
     (Control.Root <> nil) and (Control.Root.GetObject is TCommonCustomForm) then
  begin
    Form := TCommonCustomForm(Control.Root.GetObject);
    var Scale := Form.Handle.Scale;
    SetParent(FWnd, WindowHandleToPlatform(Form.Handle).Wnd);

    P := GetVideoSize;
    Bounds := TRectF.Create(0, 0, P.X, P.Y);
    Bounds.Fit(TRectF.Create(0, 0, Control.AbsoluteWidth, Control.AbsoluteHeight));
    Bounds.Offset(Control.AbsoluteRect.TopLeft);
    BoundsInt := TRectF.Create(Bounds.Left * Scale, Bounds.Top * Scale,
                               Bounds.Right * Scale, Bounds.Bottom * Scale).Round;

    SetWindowPos(FWnd, 0, BoundsInt.Left, BoundsInt.Top, BoundsInt.Width, BoundsInt.Height, 0);

    R := TRect.Create(0, 0, BoundsInt.Width, BoundsInt.Height);
    if FVMRWindowlessControl9 <> nil then
      FVMRWindowlessControl9.SetVideoPosition(nil, @R);
    if FVMRWindowlessControl7 <> nil then
      FVMRWindowlessControl7.SetVideoPosition(nil, @R);
    ShowWindow(FWnd, SW_SHOW)
  end
  else
  begin
    SetParent(FWnd, ApplicationHWND);
    ShowWindow(FWnd, SW_HIDE)
  end;
end;

procedure TWindowsMedia.WaitCompletionThreadTerminated(Sender: TObject);
begin
  FMediaState := TMediaState.Stopped;
  // IMediaControl doesn't stop, even if the playback is already finished.
  // This causes the video to continue playing if the user changes the Current position.
  FMediaControl.Pause;
end;

function TWindowsMedia.GetDuration: TMediaTime;
var
  LDuration: Int64;
begin
  if FMediaSeeking = nil then
    Result := 0
  else
  begin
    FMediaSeeking.GetDuration(LDuration);
    Result := LDuration;
  end;
end;

function TWindowsMedia.GetCurrent: TMediaTime;
var
  CurrentPosition: Int64;
begin
  if FMediaSeeking = nil then
    Result := 0
  else
  begin
    FMediaSeeking.GetCurrentPosition(CurrentPosition);
    Result := CurrentPosition;
  end;
end;

procedure TWindowsMedia.SetCurrent(const Value: TMediaTime);
var
  CurrentPosition, S: Int64;
begin
  if FMediaSeeking <> nil then
  begin
    CurrentPosition := Value;
    FMediaSeeking.SetPositions(CurrentPosition, AM_SEEKING_AbsolutePositioning, S, AM_SEEKING_SeekToKeyFrame);
  end;
end;

function TWindowsMedia.GetVolume: Single;
var
  LVolume: Integer;
begin
  if FBasicAudio <> nil then
  begin
    FBasicAudio.get_Volume(LVolume);
    if LVolume > MinAllowedVolume then
      Result := EnsureRange(Power(10, -LVolume / MinAllowedVolume), 0, 1)
    else
      Result := 0;
  end
  else
    Result := 1.0;
end;

function TWindowsMedia.InitGraphTree: Boolean;
begin
  Result := False;
  if Succeeded(FGraphBuilder.RenderFile(PChar(FileName), nil)) then
  begin
    FMediaControl.StopWhenReady;
    // Get IMediaSeeking
    FGraphBuilder.QueryInterface(IID_IMediaSeeking, FMediaSeeking);
    // Get IBaseAudio
    FGraphBuilder.QueryInterface(IID_IBasicAudio, FBasicAudio);
    Result := True; // Success
  end;
end;

function TWindowsMedia.InitMediaControl: Boolean;
begin
  Result := Succeeded(CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, FGraphBuilder)) and
    Succeeded(FGraphBuilder.QueryInterface(IID_IMediaControl, FMediaControl));
end;

function TWindowsMedia.InitVideoMixingRenderer7: Boolean;
const
  VideoMixingRenderer7 = 'Video Mixing Renderer7';
begin
  Result := Succeeded(CoCreateInstance(CLSID_VideoMixingRenderer, nil, CLSCTX_INPROC, IID_IBaseFilter, FVMR)) and
    Succeeded(FVMR.QueryInterface(IVMRFilterConfig, FVMRFilterConfig7)) and
    Succeeded(FGraphBuilder.AddFilter(FVMR, VideoMixingRenderer7)) and
    Succeeded(FVMRFilterConfig7.SetRenderingMode(VMRMode_Windowless));
end;

function TWindowsMedia.InitVideoMixingRenderer9: Boolean;
const
  VideoMixingRenderer9 = 'Video Mixing Renderer9';
begin
  Result := Succeeded(CoCreateInstance(CLSID_VideoMixingRenderer9, nil, CLSCTX_INPROC, IID_IBaseFilter, FVMR)) and
    Succeeded(FVMR.QueryInterface(IVMRFilterConfig9, FVMRFilterConfig9)) and
    Succeeded(FGraphBuilder.AddFilter(FVMR, VideoMixingRenderer9)) and
    Succeeded(FVMRFilterConfig9.SetRenderingMode(VMR9Mode_Windowless));
end;

procedure TWindowsMedia.InitWnd;
var
  WindowClass: TWndClass;
begin
  if not GetClassInfo(hInstance, PChar('VMRWindow'), WindowClass) then
  begin
    FillChar(WindowClass, SizeOf(WindowClass), 0);
    WindowClass.Style := CS_HREDRAW or CS_VREDRAW;
    WindowClass.lpfnWndProc := @DefWindowProc;
    WindowClass.cbClsExtra := 0;
    WindowClass.cbWndExtra := 0;
    WindowClass.hInstance := hInstance;
    WindowClass.hCursor := LoadCursorW(0, PChar(IDC_ARROW));
    WindowClass.hbrBackground := GetStockObject(NULL_BRUSH);
    WindowClass.lpszMenuName := nil;
    WindowClass.lpszClassName := PChar('VMRWindow');
    if Winapi.Windows.RegisterClass(WindowClass) = 0 then
      RaiseLastOSError;
  end;
  FWnd := CreateWindowEx(0, WindowClass.lpszClassName, nil, WS_CHILDWINDOW,
    0, 0, 0, 0, GetDesktopWindow, 0, hInstance, nil);
  ShowWindow(FWnd, SW_HIDE);
end;

function TWindowsMedia.LocateVideoMixingRenderer7: Boolean;
var
  R: TRect;
begin
  Result := False;
  if Succeeded(FVMR.QueryInterface(IVMRWindowlessControl, FVMRWindowlessControl7)) then
  begin
    FVMRWindowlessControl7.SetVideoClippingWindow(FWnd);
    GetClientRect(FWnd, R);
    FVMRWindowlessControl7.SetVideoPosition(nil, @R);
    Result := True;
  end
end;

function TWindowsMedia.LocateVideoMixingRenderer9: Boolean;
var
  R: TRect;
begin
  Result := False;
  if Succeeded(FVMR.QueryInterface(IVMRWindowlessControl9, FVMRWindowlessControl9)) then
  begin
    FVMRWindowlessControl9.SetVideoClippingWindow(FWnd);
    GetClientRect(FWnd, R);
    FVMRWindowlessControl9.SetVideoPosition(nil, @R);
    Result := True;
  end
end;

function TWindowsMedia.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if Supports(FGraphBuilder, IID, Obj) then
    Exit(S_OK);
  if Supports(FMediaControl, IID, Obj) then
    Exit(S_OK);
  if Supports(FMediaSeeking, IID, Obj) then
    Exit(S_OK);
  if Supports(FBasicAudio, IID, Obj) then
    Exit(S_OK);
  if Supports(FVMR, IID, Obj) then
    Exit(S_OK);
  if Supports(FVMRFilterConfig9, IID, Obj) then
    Exit(S_OK);
  if Supports(FVMRWindowlessControl9, IID, Obj) then
    Exit(S_OK);
  if Supports(FVMRFilterConfig7, IID, Obj) then
    Exit(S_OK);
  if Supports(FVMRWindowlessControl7, IID, Obj) then
    Exit(S_OK);
  Result := E_NOTIMPL;
end;

procedure TWindowsMedia.SetVolume(const Value: Single);
var
  LVolume : Single;
begin
  if FBasicAudio = nil then
    Exit;

  if Value > 0 then
  begin
    LVolume := EnsureRange(-MinAllowedVolume * Log10(Value), MinAllowedVolume, 0);
    FBasicAudio.put_Volume(Round(LVolume));
  end
  else
    FBasicAudio.put_Volume(MinAllowedVolume);
end;

procedure TWindowsMedia.StartWaitCompletionThread;
var
  MediaEvent: IMediaEvent;
begin
  if (FWaitCompletionThread = nil) or FWaitCompletionThread.Finished then
  begin
    StopAndFreeWaitCompletionThread;
    if Supports(FGraphBuilder, IMediaEvent, MediaEvent) then
    begin
      FWaitCompletionThread := TWaitMediaCompletionThread.Create(MediaEvent);
      FWaitCompletionThread.OnTerminate := WaitCompletionThreadTerminated;
    end;
  end;
end;

procedure TWindowsMedia.StopAndFreeWaitCompletionThread;
begin
  if FWaitCompletionThread <> nil then
  begin
    FWaitCompletionThread.Terminate;
    FWaitCompletionThread.WaitFor;
    FreeAndNil(FWaitCompletionThread);
  end;
end;

function TWindowsMedia.GetMediaState: TMediaState;
begin
  Result := FMediaState;
end;

function TWindowsMedia.GetVideoSize: TPointF;
var
  W, H, AW, AH: Integer;
begin
  if FVMRWindowlessControl9 <> nil then
  begin
    FVMRWindowlessControl9.GetNativeVideoSize(W, H, AW, AH);
    Result := TPointF.Create(W, H);
  end
  else if FVMRWindowlessControl7 <> nil then
  begin
    FVMRWindowlessControl7.GetNativeVideoSize(W, H, AW, AH);
    Result := TPointF.Create(W, H);
  end
  else
    Result := TPointF.Zero;
end;

{ TWindowsMediaCodec }

function TWindowsMediaCodec.CreateFromFile(const AFileName: string): TMedia;
begin
  Result := TWindowsMedia.Create(AFileName);
end;

{ TWaitMediaCompletionThread }

constructor TWaitMediaCompletionThread.Create(const AMediaEvent: IMediaEvent);
begin
  inherited Create(False);
  FMediaEvent := AMediaEvent;
end;

procedure TWaitMediaCompletionThread.Execute;
const
  CheckingStateInterval = 50;
var
  EventCode: Integer;
begin
  EventCode := 0;
  try
    while (EventCode = 0) and not Terminated do
      FMediaEvent.WaitForCompletion(CheckingStateInterval, EventCode);
  finally
    FMediaEvent := nil;
  end;
end;

initialization
  TMediaCodecManager.RegisterMediaCodecClass('.avi', SVAviFiles, TMediaType.Video, TWindowsMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.wmv', SVWMVFiles, TMediaType.Video, TWindowsMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp4', SVMP4Files, TMediaType.Video, TWindowsMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.m4v', SVMP4Files, TMediaType.Video, TWindowsMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mov', SVMOVFiles, TMediaType.Video, TWindowsMediaCodec);

  TMediaCodecManager.RegisterMediaCodecClass('.wma', SVWMAFiles, TMediaType.Audio, TWindowsMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.mp3', SVMP3Files, TMediaType.Audio, TWindowsMediaCodec);
  TMediaCodecManager.RegisterMediaCodecClass('.wav', SVWAVFiles, TMediaType.Audio, TWindowsMediaCodec);

  TMediaCodecManager.RegisterMediaCodecClass(SAllFilesExt, SDefault, TMediaType.Video, TWindowsMediaCodec);

finalization
end.

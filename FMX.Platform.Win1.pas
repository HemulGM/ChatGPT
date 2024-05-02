{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2023 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Win;

(*$HPPEMIT '#if defined(WIN32) && defined(CreateWindow)'*)
(*$HPPEMIT '  #define __SAVE_CREATEWINDOW CreateWindow'*)
(*$HPPEMIT '  #undef  CreateWindow'*)
(*$HPPEMIT '#endif'*)

(*$HPPEMIT END '#if defined(__SAVE_CREATEWINDOW)'*)
(*$HPPEMIT END '  #define CreateWindow __SAVE_CREATEWINDOW'*)
(*$HPPEMIT END '  #undef  __SAVE_CREATEWINDOW'*)
(*$HPPEMIT END '#endif'*)

{$HPPEMIT NOUSINGNAMESPACE}
{$R-}

interface

{$SCOPEDENUMS ON}

uses
  System.Messaging, Winapi.CommCtrl, Winapi.Windows, Winapi.ActiveX, System.Types, Winapi.Messages, System.Classes,
  System.UITypes, System.UIConsts, System.Generics.Collections, FMX.Forms, FMX.Platform, FMX.Types, FMX.Graphics,
  FMX.ZOrder.Win;

type
  TWinDropTarget = class;

  PRgnRects = ^TRgnRects;
  TRgnRects = array [0..MaxInt div SizeOf(TRect) - 1] of TRect;

  TUpdateRects = array of TRectF;

  TWinWindowHandle = class(TWindowHandle)
  private class var
    FForcedScale: Single;
  private
    FForm: TCommonCustomForm;
    FWnd: HWND;
    FZOrderManager: TWinZOrderManager;
    FBufferBitmap: THandle;
    FBitmapInfo: TBitmapInfo;
    FBufferBits: Pointer;
    FBufferHandle: THandle;
    FBufferSize: TSize;
    FDisableDeactivate: Boolean;
    FWinDropTarget: TWinDropTarget;
    FCurrentScale: Single;
    FClientSize: TSizeF;
    FWndClientSize: TSize;
    FBounds: TRectF;
    FWndBounds: TRect;
    FNearestIntegerMultiple: Integer;
    procedure UpdateLayer;
    function GetZOrderManager: TWinZOrderManager;
    procedure SetBounds(const Value: TRectF);
    procedure SetWndBounds(const Value: TRect);
    procedure SetClientSize(const Value: TSizeF);
    procedure CalculateClientSizeFromWindow;
    procedure ApplyWndBoundsToWnd;
    procedure ApplyWndClientSizeToWnd;
    procedure CalcNearestIntegerMultiple;
    function GetNearestIntegerMultiple: Integer;
    function GetWndBorderSize: TSize;
  protected
    function GetBounds: TRectF; virtual;
    function GetClientSize: TSizeF; virtual;
    function GetWndBounds: TRect; virtual;
    function GetWndClientSize: TSize; virtual;
    function GetTransparency: Boolean; virtual;
    function GetScale: Single; override;
    { WinAPI Messages }
    procedure WMDpiChanged(var AMessage: TWMDpi); message WM_DPICHANGED;
    procedure WMWindowPosChanging(var AMessage: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMWindowPosChanged(var AMessage: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMSize(var AMessage: TWMSize); message WM_SIZE;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  public
    constructor Create(const AForm: TCommonCustomForm; const AWnd: HWND);
    destructor Destroy; override;
    class procedure SetForcedScale(const ANewScale: Single);
    procedure SetForcedScaleForForm(const ANewScale: Single);
    { Buffer }
    procedure CreateBuffer(const Width, Height: Integer {px});
    procedure ResizeBuffer(const Width, Height: Integer {px});
    procedure FreeBuffer;
    { Rounding to match scale }
    /// <summary>Rounds physical size of client window to match scale and logical client window size.</summary>
    function RoundWndClientSizeToMatchScale(const AWndClientSize: TSize): TSize;
    /// <summary>Rounds physical size of window to match scale and logical window bounds.</summary>
    function RoundWndSizeToMatchScale(const AWndSize: TSize): TSize;
    { PX to DP conversions }
    /// <summary>Converts physical rect to logical.</summary>
    function WndToForm(const Rect: TRect): TRectF; overload;
    /// <summary>Converts physical rect to logical.</summary>
    function WndToForm(const Rect: TRectF): TRectF; overload;
    /// <summary>Converts logical rect to physical.</summary>
    function FormToWnd(const Rect: TRectF): TRectF;
    property NearestIntegerMultiple: Integer read GetNearestIntegerMultiple;
  public
    /// <summary>Native WinAPI window handle.</summary>
    property Wnd: HWND read FWnd;
    /// <summary>Returns related FMX form.</summary>
    property Form: TCommonCustomForm read FForm;
    /// <summary>The Z-Order manager responsible for using native controls on this form.</summary>
    property ZOrderManager: TWinZOrderManager read GetZOrderManager;
    /// <summary>Is form transparent or not?</summary>
    property Transparency: Boolean read GetTransparency;
    { Buffer }
    property BufferBits: Pointer read FBufferBits;
    property BufferHandle: THandle read FBufferHandle;
    /// <summary>Allocated buffer size for native window in px.</summary>
    property BufferSize: TSize read FBufferSize;
    { Frame/Bounds }
    /// <summary>Logical form's client size (dp).</summary>
    property ClientSize: TSizeF read GetClientSize write SetClientSize;
    /// <summary>Logical form bounds (dp).</summary>
    property Bounds: TRectF read GetBounds write SetBounds;
    /// <summary>Physical size of client part (px).</summary>
    property WndClientSize: TSize read GetWndClientSize;
    /// <summary>Physical form bounds (px).</summary>
    property WndBounds: TRect read GetWndBounds write SetWndBounds;
    /// <summary>Total physical thickness of window frame (px).</summary>
    property WndBorderSize: TSize read GetWndBorderSize;
  end;

  TWinDropTarget = class(TComponent, IDropTarget)
  private
    Form: TCommonCustomForm;
    function GetDataObject: TDragObject;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
  end;

function PxToDp(const AValue: TPoint): TPointF;
function DpToPx(const AValue: TPointF): TPoint;

function FindWindow(Handle: HWND): TCommonCustomForm;
function WindowHandleToPlatform(const AHandle: TWindowHandle): TWinWindowHandle;

function FmxHandleToHWND(const FmxHandle: TWindowHandle): HWND;
function FormToHWND(Form: TCommonCustomForm): HWND;
function ApplicationHWND: HWND;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

const
  IDC_NODROP    = PChar(32760);
  IDC_DRAG      = PChar(32759);
  IDC_MULTIDRAG = PChar(32756);
  IDC_SQLWAIT   = PChar(32755);

type
  TApplicationHWNDProc = function: HWND;

procedure RegisterApplicationHWNDProc(const Proc: TApplicationHWNDProc);

procedure ShutDown; cdecl;

implementation

{$SCOPEDENUMS OFF}

uses
  System.IOUtils, Winapi.CommDlg, Winapi.ShlObj, Winapi.MMSystem, Winapi.ShellAPI,
  Winapi.MultiMon, Winapi.Imm, Winapi.UxTheme, Winapi.ShellScaling, System.Variants, System.SysUtils, System.Math,
  System.Math.Vectors, System.StrUtils, System.DateUtils, System.RTLConsts, System.SyncObjs, System.Rtti, System.Devices,
  FMX.Consts, FMX.Menus, FMX.Helpers.Win, FMX.Printer, FMX.Printer.Win, FMX.Dialogs.Win, FMX.Canvas.GDIP, FMX.Canvas.D2D,
  FMX.Context.DX9, FMX.Context.DX11, FMX.Canvas.GPU, FMX.Forms.Border.Win, FMX.Controls.Win, FMX.Gestures.Win,
  FMX.TextLayout, FMX.Text, FMX.Types3D, FMX.VirtualKeyboard, FMX.Controls, FMX.BehaviorManager, FMX.Styles,
  FMX.MultiTouch.Win, FMX.ImgList, FMX.WebBrowser, FMX.Surfaces, FMX.Utils, FMX.KeyMapping, FMX.AcceleratorKey,
  FMX.AcceleratorKey.Win, FMX.Platform.Screen.Win, FMX.Platform.SaveState.Win, FMX.Platform.Device.Win,
  FMX.Platform.Metrics.Win, FMX.VirtualKeyboard.Win, FMX.Platform.Timer.Win, FMX.Platform.Logger.Win,
  FMX.Platform.Menu.Win;

type
  MySTGMEDIUM = record // for compatibility
    Tymed: DWORD;
    case Integer Of
      0:
        (HBITMAP: HBITMAP; UnkForRelease: Pointer { IUnknown } );
      1:
        (HMETAFILEPICT: THandle);
      2:
        (HENHMETAFILE: THandle);
      3:
        (HGLOBAL: HGLOBAL);
      4:
        (lpszFileName: POleStr);
      5:
        (stm: Pointer { IStream } );
      6:
        (stg: Pointer { IStorage } );
  end;

  { TDropSource }

  TDataObjectInfo = record
    FormatEtc: TFormatEtc;
    StgMedium: TStgMedium;
    OwnedByDataObject: Boolean;
  end;

  TDataObjectInfoArray = array of TDataObjectInfo;

  TDropSource = class(TComponent, IDataObject, IDropSource)
  private
    Data: TDragObject;
    Formats: TDataObjectInfoArray;
    { IDropSource }
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HRESULT; stdcall;
    function GiveFeedback(dwEffect: Longint): HRESULT; stdcall;
    { IDataObject }
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HRESULT; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HRESULT; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: BOOL): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out EnumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
    function dAdvise(const FormatEtc: TFormatEtc; advf: Longint; const advsink: IAdviseSink;
      out dwConnection: Longint): HRESULT; stdcall;
    function dUnadvise(dwConnection: Longint): HRESULT; stdcall;
    function EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;
    { For IDropSourceHelper }
    function FindFormatEtc(TestFormatEtc: TFormatEtc): Integer;
    function EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
    function HGlobalClone(HGLOBAL: THandle): THandle;
    function RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
    function StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
      CopyInMedium: Boolean): HRESULT;
    function CanonicalIUnknown(const TestUnknown: IUnknown): IUnknown;
  end;

type
  { TPlatformWin }

  TFullScreenSavedState = record
    BorderStyle: TFmxFormBorderStyle;
    WindowState: TWindowState;
    Position: TPointF;
    Size: TSizeF;
    IsFullscreen: Boolean;
  end;

  TFormInfo = class
    WasLeftMouseButtonPressed: Boolean;
  end;

  TImmManager = class;

  TPlatformWin = class(TInterfacedObject, IFMXApplicationService, IFMXWindowService, IFMXDragDropService,
    IFMXCursorService, IFMXMouseService, IFMXTextService, IFMXContextService, IFMXCanvasService, IFMXWindowBorderService,
    IFMXFullScreenWindowService, IFMXGestureRecognizersService, IFMXWindowsTouchService, IFMXKeyMappingService,
    IFMXWindowConstraintsService)
  private
    FTitle: string;
    FDefaultTitle: string;
    FApplicationHWNDProc: TApplicationHWNDProc;
    FApplicationHWND: HWND;
    FIsOutApplicationHWND: Boolean;
    FFullScreenSupport: TDictionary<TCommonCustomForm, TFullScreenSavedState>;
    FDiableUpdateState: Boolean;
    FThreadSyncHandle: HWND;
    FInPaintUpdateRects: TDictionary<TWindowHandle, TUpdateRects>;
    FTerminating: Boolean;
    FRunning: Boolean;
    FCursor: TCursor;
    FCaptionChangedId: Integer;
    FMultiTouchManager: TMultiTouchManagerWin;
    FEnabledInteractiveGestures: TInteractiveGestures;
    FDragAndDropActive: Boolean;
    FKeyMapping: TKeyMapping;
    FAcceleratorKeyRegistry: IFMXAcceleratorKeyRegistryService;
    FIsPostQuitMessage: Boolean;
    FTimerService: TWinTimerService;
    FFormsInfo: TObjectDictionary<TCommonCustomForm, TFormInfo>;
    { IMM }
    FImmManagers: TObjectDictionary<TCommonCustomForm, TImmManager>;
    procedure ThreadSync(var Msg: TMessage);
    procedure WakeMainThread(Sender: TObject);
    function CreateAppHandle: HWND;
    procedure MinimizeApp;
    procedure RestoreApp;
    procedure UpdateAppTitle;
    procedure CaptionChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    function GetApplicationHWND: HWND;
    procedure SetApplicationHWNDProc(const Value: TApplicationHWNDProc);
    procedure UpdateApplicationHwnd;

    { IFMXKeyMappingService }
    /// <summary>Registers a platform key as the given virtual key.</summary>
    function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
    /// <summary>Unegisters a platform key as the given virtual key.</summary>
    function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
    /// <summary>Obtains the virtual key from a given platform key.</summary>
    function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
    /// <summary>Obtains the platform key from a given virtual key.</summary>
    function VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
    function GetImmManager(const Index: TCommonCustomForm): TImmManager;
    function GetFormInfo(const Index: TCommonCustomForm): TFormInfo;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function Terminating: Boolean;
    function Running: Boolean;
    procedure Terminate;
    function GetVersionString: string;
    property ApplicationHWND: HWND read GetApplicationHWND;
    property ApplicationHWNDProc: TApplicationHWNDProc read FApplicationHWNDProc write SetApplicationHWNDProc;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
    procedure ShowWindow(const AForm: TCommonCustomForm);
    procedure HideWindow(const AForm: TCommonCustomForm);
    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
    procedure Activate(const AForm: TCommonCustomForm);
    function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
    function CanShowModal: Boolean;
    procedure InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
    procedure InvalidateImmediately(const AForm: TCommonCustomForm);
    procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
    function GetClientSize(const AForm: TCommonCustomForm): TPointF;
    procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
    procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
    procedure SetCapture(const AForm: TCommonCustomForm);
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    { IFMXWindowConstraintsService }
    procedure SetConstraints(const AForm: TCommonCustomForm; const AMinWidth, AMinHeight, AMaxWidth, AMaxHeight: Single);
    { IFMXFullScreenWindowService }
    // for desingtime and testing only
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
    { IFMXWindowBorderService }
    function CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
    { IFMXDragDropService }
    procedure BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
    { IFMXCursorService }
    procedure SetCursor(const ACursor: TCursor);
    function GetCursor: TCursor;
    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXContextService }
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    { IFMXWindowsTouchService }
    procedure HookTouchHandler(const AForm: TCommonCustomForm);
    procedure UnhookTouchHandler(const AForm: TCommonCustomForm);

    property ImmManager[const Index: TCommonCustomForm]: TImmManager read GetImmManager;
    property TimerService: TWinTimerService read FTimerService;
    /// <summary>Returns additional platform dependent data about fmx form.</summary>
    property FormInfo[const Index: TCommonCustomForm]: TFormInfo read GetFormInfo;
  end;

  TOpenForm = class(TCommonCustomForm);
  TTextServiceWin = class;

  TImmManager = class
  private
    [Weak] FForm: TCommonCustomForm;
    function GetFormHandle: HWND;
    procedure UpdateCompositionAttributes(const AContext: HIMC; const ATextService: TTextServiceWin);
    procedure UpdateCompositionCursorPos(const AContext: HIMC; const ATextService: TTextServiceWin);
    procedure ProcessImeParameters(const AContext: HIMC; const AParameters: LPARAM; const ATextService: TTextServiceWin);
  protected
    procedure WMStartComposition(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMEndComposition(var Message: TMessage); message WM_IME_ENDCOMPOSITION;
    procedure WMSetContext(var Message: TMessage); message WM_IME_SETCONTEXT;
    procedure WMNotify(var Message: TMessage); message WM_IME_NOTIFY;

    function GetComposition(const AContext: HIMC): string;
    function GetResultComposition(const AContext: HIMC): string;
  public
    constructor Create(const AForm: TCommonCustomForm);

    function UpdateIMEWindowPosition: LRESULT;

    property Form: TCommonCustomForm read FForm;
    property FormHandle: HWND read GetFormHandle;
  end;

{ Text Service }

  TTextServiceWin = class(TTextService)
  private const
    LCID_Korean_Default = (SUBLANG_KOREAN shl 10) + LANG_KOREAN;
  private
    FMarkedText: string;
    FIsInputting: Boolean;
    FMarkedTextCursorPosition: Integer;
    procedure SetMarkedTextCursorPosition(const Value: Integer);
    procedure RecreateImmContext(const AFormHandle: TWindowHandle);
    procedure Reset;
  protected
    procedure MarkedTextPositionChanged; override;
    procedure CaretPositionChanged; override;
    function GetMarketTextAttributes: TArray<TMarkedTextAttribute>; override;
  public
    CompAttrBuf: array of Byte;

    procedure InternalSetMarkedText(const AMarkedText: string); override;
    function InternalGetMarkedText: string; override;
    procedure InternalStartIMEInput;
    procedure InternalEndIMEInput;

    procedure RefreshImePosition; override;

    function TargetClausePosition: TPoint; override;

    procedure EnterControl(const AFormHandle: TWindowHandle); override;
    procedure ExitControl(const AFormHandle: TWindowHandle); override;

    function HasMarkedText: Boolean; override;

    { Deprecated }
    procedure DrawSingleLine(const ACanvas: TCanvas; const ARect: TRectF; const AFirstVisibleChar: Integer;
      const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;
    procedure DrawSingleLine(const ACanvas: TCanvas; const S: string; const ARect: TRectF; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;

    /// <summary>Returns caret position in <c>MarkedText</c> bounds.</summary>
    property MarkedTextCursorPosition: Integer read FMarkedTextCursorPosition write SetMarkedTextCursorPosition;
  end;

var
  VirtualKeyboardWin: TWinVirtualKeyboard;
  MultiDisplayWin: TWinMultiDisplay;
  MenuServiceWin: TWinMenuService;

const
  CF_FMOBJECT = CF_PRIVATEFIRST + 1;

var
  WindowAtom: TAtom;
  WindowAtomString: string;
  PlatformWin: TPlatformWin;
  CapturedGestureControl: TComponent;
  /// <summary>This registered window message is responsible for getting an instance of the component by handle.</summary>
  FM_GET_OBJECT_INSTANCE: DWORD;
  ControlAtom: TAtom;
  ControlAtomString: string;

function PxToDp(const AValue: TPoint): TPointF;
begin
  if MultiDisplayWin = nil then
    Result := AValue
  else
    Result := MultiDisplayWin.PxToDp(AValue);
end;

function DpToPx(const AValue: TPointF): TPoint;
begin
  if MultiDisplayWin = nil then
    Result := AValue.Truncate
  else
    Result := MultiDisplayWin.DpToPx(AValue);
end;

function FindWindow(Handle: HWND): TCommonCustomForm;
begin
  if Handle = 0 then
    Result := nil
  else if GlobalFindAtomW(PChar(WindowAtomString)) = WindowAtom then
    Result := Pointer(GetProp(Handle, MakeIntAtom(WindowAtom)))
  else
    Result := nil;
end;

function ObjectFromHWnd(Handle: HWnd): TWinControl;
var
  OwningProcess: DWORD;
  ProcessId: DWORD;
begin
  ProcessId := GetCurrentProcessId;
  if (GetWindowThreadProcessId(Handle, OwningProcess) <> 0) and (OwningProcess = ProcessId) then
    Result := Pointer(SendMessage(Handle, FM_GET_OBJECT_INSTANCE, ProcessId, 0))
  else
    Result := nil;
end;

{ Find a TControl given a window handle }
{ The global atom table is trashed when the user logs off.  The extra test
  below protects UI interactive services after the user logs off.
  Added additional tests to enure that Handle is at least within the same
  process since otherwise a bogus result can occur due to problems with
  GlobalFindAtom in Winapi.Windows.  }
function FindNativeControl(const AHandle: HWND): TControl;
var
  OwningProcess: DWORD;
begin
  Result := nil;
  if (AHandle <> 0) and (GetWindowThreadProcessID(AHandle, OwningProcess) <> 0) and (OwningProcess = GetCurrentProcessId) then
  begin
    if GlobalFindAtom(PChar(ControlAtomString)) = ControlAtom then
      Result := Pointer(GetProp(AHandle, MakeIntAtom(ControlAtom)))
    else
      Result := ObjectFromHWnd(AHandle);
  end;
end;

function IsFMXControl(const AHandle: HWND): Boolean;
begin
  Result := FindNativeControl(AHandle) <> nil;
end;

function FmxHandleToHWND(const FmxHandle: TWindowHandle): HWND;
begin
  if FmxHandle <> nil then
    Result := WindowHandleToPlatform(FmxHandle).Wnd
  else
    Result := 0;
end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TWinWindowHandle;
begin
  Result := TWinWindowHandle(AHandle);
end;

function FormToHWND(Form: TCommonCustomForm): HWND;
begin
  if (Form <> nil) and (Form.Handle is TWinWindowHandle) then
    Result := TWinWindowHandle(Form.Handle).Wnd
  else
    Result := 0;
end;

function ApplicationHWND: HWND;
begin
  if PlatformWin <> nil then
    Result := PlatformWin.GetApplicationHWND
  else
    Result := 0;
end;

procedure DisableProcessWindowsGhosting;
var
  DisableProcessWindowsGhostingProc: procedure;
begin
  DisableProcessWindowsGhostingProc := GetProcAddress(GetModuleHandle('user32.dll'), 'DisableProcessWindowsGhosting');
  if Assigned(DisableProcessWindowsGhostingProc) then
    DisableProcessWindowsGhostingProc;
end;

procedure RegisterCorePlatformServices;
var
  MetricsService: TWinMetricsServices;
begin
  PlatformWin := TPlatformWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, PlatformWin.TimerService);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowConstraintsService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDragDropService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXCursorService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowBorderService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, TWinLoggerService.Create);
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowsTouchService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXKeyMappingService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceService, TWinDeviceServices.Create);
  TPlatformServices.Current.AddPlatformService(IFMXSaveStateService, TWinSaveStateService.Create);

  MetricsService := TWinMetricsServices.Create;
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, MetricsService);

  VirtualKeyboardWin := TWinVirtualKeyboard.Create;
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, VirtualKeyboardWin);

  MultiDisplayWin := TWinMultiDisplay.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, MultiDisplayWin);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, MultiDisplayWin);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, MultiDisplayWin);

  MenuServiceWin := TWinMenuService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMenuService, MenuServiceWin);

  // If application becomes inactive while a modal dialog is opened it may hang, so we have to DisableProcessWindowsGhosting
  DisableProcessWindowsGhosting;
end;

procedure UnregisterCorePlatformServices;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXMenuService);
    TPlatformServices.Current.RemovePlatformService(IFMXDeviceService);
    TPlatformServices.Current.RemovePlatformService(IFMXDeviceMetricsService);
    TPlatformServices.Current.RemovePlatformService(IFMXApplicationService);
    TPlatformServices.Current.RemovePlatformService(IFMXSystemFontService);
    TPlatformServices.Current.RemovePlatformService(IFMXTimerService);
    TPlatformServices.Current.RemovePlatformService(IFMXWindowConstraintsService);
    TPlatformServices.Current.RemovePlatformService(IFMXWindowService);
    TPlatformServices.Current.RemovePlatformService(IFMXDragDropService);
    TPlatformServices.Current.RemovePlatformService(IFMXCursorService);
    TPlatformServices.Current.RemovePlatformService(IFMXMouseService);
    TPlatformServices.Current.RemovePlatformService(IFMXScreenService);
    TPlatformServices.Current.RemovePlatformService(IFMXLocaleService);
    TPlatformServices.Current.RemovePlatformService(IFMXTextService);
    TPlatformServices.Current.RemovePlatformService(IFMXContextService);
    TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
    TPlatformServices.Current.RemovePlatformService(IFMXWindowBorderService);
    TPlatformServices.Current.RemovePlatformService(IFMXSystemInformationService);
    TPlatformServices.Current.RemovePlatformService(IFMXFullScreenWindowService);
    TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardService);
    TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
    TPlatformServices.Current.RemovePlatformService(IFMXLoggingService);
    TPlatformServices.Current.RemovePlatformService(IFMXListingService);
    TPlatformServices.Current.RemovePlatformService(IFMXSaveStateService);
    TPlatformServices.Current.RemovePlatformService(IFMXGestureRecognizersService);
    TPlatformServices.Current.RemovePlatformService(IFMXWindowsTouchService);
    TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
    TPlatformServices.Current.RemovePlatformService(IFMXKeyMappingService);
  end;
end;

procedure RaiseIfNil(const AObject: TObject; const AArgumentName: string);
begin
  if AObject = nil then
    raise EArgumentException.CreateFmt(SParamIsNil, [AArgumentName]);
end;

{ TPlatformWin }

constructor TPlatformWin.Create;
begin
  inherited;
  FIsOutApplicationHWND := False;
  WindowAtomString := Format('FIREMONKEY%.8X', [GetCurrentProcessID]);
  WindowAtom := GlobalAddAtomW(PChar(WindowAtomString));
  ControlAtomString := Format('ControlOfs%.8X%.8X', [HInstance, GetCurrentThreadID]);
  ControlAtom := GlobalAddAtom(PChar(ControlAtomString));
  FM_GET_OBJECT_INSTANCE := RegisterWindowMessage(PChar('FM_GET_OBJECT_INSTANCE'));  // Do not localize

  FFullScreenSupport := TDictionary<TCommonCustomForm, TFullScreenSavedState>.Create;
  FInPaintUpdateRects := TDictionary<TWindowHandle, TUpdateRects>.Create;
  FThreadSyncHandle := AllocateHWnd(ThreadSync);
  FKeyMapping := TKeyMapping.Create;
  FAcceleratorKeyRegistry := TWinAcceleratorKeyRegistry.Create;
  System.Classes.WakeMainThread := WakeMainThread;
  Application := TApplication.Create(nil);
  FCaptionChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TMainCaptionChangedMessage,
    CaptionChangedHandler);
  FRunning := False;

  FImmManagers := TObjectDictionary<TCommonCustomForm, TImmManager>.Create([doOwnsValues]);
  FFormsInfo := TObjectDictionary<TCommonCustomForm, TFormInfo>.Create([doOwnsValues]);
  FTimerService := TWinTimerService.Create;
end;

destructor TPlatformWin.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMainCaptionChangedMessage, FCaptionChangedId);
  FreeAndNil(FFormsInfo);
  FreeAndNil(FImmManagers);
  FreeAndNil(Application);
  FreeAndNil(FInPaintUpdateRects);
  FreeAndNil(FFullScreenSupport);
  FAcceleratorKeyRegistry := nil;
  FTimerService := nil;
  FreeAndNil(FKeyMapping);
  GlobalDeleteAtom(ControlAtom);
  GlobalDeleteAtom(WindowAtom);

  if FThreadSyncHandle <> 0 then
    DeallocateHWnd(FThreadSyncHandle);
  if PlatformWin = Self then
    PlatformWin := nil;
  inherited;
end;

{ App }

{$WARN SYMBOL_PLATFORM OFF}
procedure TPlatformWin.Run;
var
  MainForm: TCommonCustomForm;
begin
  FRunning := True;
  Application.RealCreateForms;
  UpdateAppTitle;
  if Application.MainForm <> nil then
  begin
    MainForm := Application.MainForm;
    case CmdShow of
      SW_SHOWMINNOACTIVE:
        MainForm.WindowState := TWindowState.wsMinimized;
      SW_SHOWMAXIMIZED:
        MainForm.WindowState := TWindowState.wsMaximized;
    end;
  end;

  repeat
    try
      Application.HandleMessage;
    except
      Application.HandleException(Self);
    end;
  until Application.Terminated;
end;
{$WARN SYMBOL_PLATFORM DEFAULT}

function TPlatformWin.Terminating: Boolean;
begin
  Result := FTerminating;
end;

function TPlatformWin.Running: Boolean;
begin
  Result := FRunning;
end;

procedure TPlatformWin.Terminate;
begin
  FRunning := False;
  FTerminating := True;
  FIsPostQuitMessage := True;
  TMessageManager.DefaultManager.SendMessage(nil, TApplicationTerminatingMessage.Create);

  FMultiTouchManager.Free; // release multitouch early so that it does not hold reference to services

  // We don't need destroy application handle, if we receive it from out through ApplicationHWNDProc.
  if IsLibrary and not FIsOutApplicationHWND and (FApplicationHWND <> 0) then
  begin
    WinApi.Windows.DestroyWindow(FApplicationHWND);
    FApplicationHWND := 0;
  end;
end;

function TPlatformWin.HandleMessage: Boolean;
const
  CN_BASE = $BC00;

  function IsMainForm(const AHandle: HWND): Boolean;
  begin
    Result := (Application.MainForm <> nil) and (AHandle = FormToHWND(Application.MainForm));
  end;

  function DefineTargetControlHandle(const AWnd: HWND): HWND;
  begin
    if IsMainForm(AWnd) then
      Result := FormToHWND(Application.MainForm)
    else
    begin
      Result := AWnd;
      // Find the nearest Native FMX component.  Non-FMX Control windows wont know what
      // to do with CN_BASE offset messages anyway. TOleControl.WndProc needs this for TranslateAccelerator
      while not IsFMXControl(Result) and (Result <> 0) do
        Result := GetParent(Result);
      if Result = 0 then
        Result := AWnd;
    end;
  end;

  function IsKeyMsg(var Msg: TMsg): Boolean;
  var
    Wnd: HWND;
    WndProcessID, ProcessID: Cardinal;
  begin
    if not InRange(Msg.Message, WM_KEYFIRST, WM_KEYLAST) then
      Exit(False);

    Result := False;
    Wnd := GetCapture;
    if Wnd = 0 then
    begin
      Wnd := DefineTargetControlHandle(Msg.HWnd);
      Result := SendMessage(Wnd, CN_BASE + Msg.Message, Msg.WParam, Msg.LParam) <> 0;
    end
    else
    begin
      GetWindowThreadProcessId(Wnd, WndProcessId);
      GetWindowThreadProcessId(ApplicationHWND, ProcessId);
      if (WndProcessID = ProcessID) then
        Result := SendMessage(Wnd, CN_BASE + Msg.Message, Msg.WParam, Msg.LParam) <> 0;
    end;
  end;

var
  Msg: TMsg;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    Result := True;
    if Msg.Message = WM_QUIT then
      Application.Terminated := True
    else if not IsKeyMsg(Msg) then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
      if FIsPostQuitMessage then
        PostQuitMessage(0);
    end;
  end;
end;

procedure TPlatformWin.WaitMessage;
begin
  Winapi.Windows.WaitMessage;
end;

function TPlatformWin.GetDefaultTitle: string;
begin
  Result := FDefaultTitle;
end;

function TPlatformWin.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TPlatformWin.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    if (Value = SAppDefault) and (FDefaultTitle <> '') then
      FTitle := FDefaultTitle
    else
      FTitle := Value;
    UpdateAppTitle;
  end;
end;

function TPlatformWin.GetVersionString: string;
const
  UndefinedVersionInfo = Cardinal(-1);
var
  VersionInfo: Cardinal;
begin
  VersionInfo := GetFileVersion(ParamStr(0));
  if VersionInfo <> UndefinedVersionInfo then
    Result := Format('%d.%d', [HiWord(VersionInfo), LoWord(VersionInfo)])
  else
    Result := string.Empty;
end;

procedure TPlatformWin.UpdateApplicationHwnd;
var
  OldApplicationHandler: HWND;
begin
  OldApplicationHandler := FApplicationHWND;

  // The first, we try to extract application handle from IDE.
  if Assigned(FApplicationHWNDProc) then
  begin
    FApplicationHWND := FApplicationHWNDProc;
    FIsOutApplicationHWND := FApplicationHWND <> 0;
  end;

  // If IDE or user doesn't set outter application handle, we create our own, if it wasn't created before.
  if FApplicationHWND = 0 then
  begin
    FApplicationHWND := CreateAppHandle;
    FIsOutApplicationHWND := False;
  end;

  if OldApplicationHandler <> FApplicationHWND then
    UpdateAppTitle;
end;

procedure TPlatformWin.UpdateAppTitle;
begin
  if FApplicationHWND <> 0 then
  begin
    { Don't update the title when working in the IDE }
    if (Application <> nil) and (Application.MainForm <> nil) and ((FTitle = SAppDefault) or
      (FTitle = GetDefaultTitle)) then
      SetWindowText(FApplicationHWND, PChar(Application.MainForm.Caption))
    else if FTitle.IsEmpty then
      SetWindowText(FApplicationHWND, PChar(GetDefaultTitle))
    else
      SetWindowText(FApplicationHWND, PChar(FTitle));
  end;
end;

procedure TPlatformWin.CaptionChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  UpdateAppTitle;
end;

procedure TPlatformWin.WakeMainThread(Sender: TObject);
begin
  PostMessage(FThreadSyncHandle, WM_NULL, 0, 0);
end;

{ Text Service }

function TTextServiceWin.GetMarketTextAttributes: TArray<TMarkedTextAttribute>;
var
  I: Integer;
begin
  if FMarkedText.IsEmpty or (Length(CompAttrBuf) = 0) then
  begin
    Result := [];
    Exit;
  end;

  SetLength(Result, FMarkedText.Length);
  for I := 0 to FMarkedText.Length - 1 do
    case CompAttrBuf[I] of
      ATTR_INPUT:
        Result[I] := TMarkedTextAttribute.Input;
      ATTR_TARGET_CONVERTED:
        Result[I] := TMarkedTextAttribute.TargetConverted;
      ATTR_CONVERTED:
        Result[I] := TMarkedTextAttribute.Converted;
      ATTR_TARGET_NOTCONVERTED:
        Result[I] := TMarkedTextAttribute.TargetNotConverted;
      ATTR_INPUT_ERROR:
        Result[I] := TMarkedTextAttribute.InputError;
    end;
end;

procedure TTextServiceWin.SetMarkedTextCursorPosition(const Value: Integer);
begin
  FMarkedTextCursorPosition := Value;
end;

procedure TTextServiceWin.InternalSetMarkedText(const AMarkedText: string);
var
  TextInput: ITextInput;
begin
  if FIsInputting and Supports(Owner, ITextInput, TextInput) then
  begin
    FMarkedText := AMarkedText;
    FMarkedTextCursorPosition := EnsureRange(FMarkedTextCursorPosition, 0, FMarkedText.Length);
    TextInput.IMEStateUpdated;
  end;
end;

procedure TTextServiceWin.InternalStartIMEInput;
var
  TextInput: ITextInput;
begin
  if not FIsInputting and Supports(Owner, ITextInput, TextInput) then
  begin
    FIsInputting := True;
    Reset;
    TextInput.StartIMEInput;
  end;
end;

procedure TTextServiceWin.MarkedTextPositionChanged;
begin
  inherited;
  if FIsInputting then
    RefreshImePosition;
end;

procedure TTextServiceWin.RecreateImmContext(const AFormHandle: TWindowHandle);
var
  IMC: HIMC;
  OldIMC: HIMC;
  WindowHandle: HWND;
begin
  // Our styled controls doesn't have window handle, so form shares IMM context between all text styled controls.
  // It leads to issues in IMM with keeping in memory previous IMM composition string, when user switches focus
  // between out styled text input controls. Even if we rest composition string by ImmSetCompositionString,
  // OS sends WM_IME_COMPOSITION notification message with the previous composition string and this behavior
  // is different, when user uses different IMM provides. So the recreating context  guarantees blank IME context
  // without previous states.

  WindowHandle := WindowHandleToPlatform(AFormHandle).Wnd;
  IMC := ImmCreateContext;
  OldIMC := ImmAssociateContext(WindowHandle, IMC);
  ImmDestroyContext(OldIMC);
  ImmReleaseContext(WindowHandle, IMC);
end;

procedure TTextServiceWin.RefreshImePosition;

  function FindForm: TCommonCustomForm;
  var
    TmpObject: TFmxObject;
  begin
    TmpObject := Owner.GetObject;
    while (TmpObject <> nil) and not (TmpObject is TCommonCustomForm) do
      TmpObject := TmpObject.Parent;

    if TmpObject is TCommonCustomForm then
      Result := TCommonCustomForm(TmpObject)
    else
      Result := nil;
  end;

var
  Form: TCommonCustomForm;
begin
  inherited;
  Form := FindForm;
  if Form <> nil then
    PlatformWin.ImmManager[Form].UpdateIMEWindowPosition;
end;

procedure TTextServiceWin.Reset;
begin
  CompAttrBuf := nil;
  FMarkedText := string.Empty;
  FMarkedTextCursorPosition := 0;
end;

function TTextServiceWin.InternalGetMarkedText: string;
begin
  Result := FMarkedText;
end;

procedure TTextServiceWin.CaretPositionChanged;
begin
  inherited;
  if FIsInputting then
    RefreshImePosition;
end;

function TTextServiceWin.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
  Result.X := Result.X + MarkedTextCursorPosition;
end;

procedure TTextServiceWin.InternalEndIMEInput;
var
  TextInput: ITextInput;
begin
  if FIsInputting and Supports(Owner, ITextInput, TextInput) then
  begin
    FIsInputting := False;
    FMarkedTextCursorPosition := 0;
    TextInput.EndIMEInput;
    FMarkedText := string.Empty;
  end;
end;

procedure TTextServiceWin.EnterControl(const AFormHandle: TWindowHandle);
var
  WindowHandle: HWND;
begin
  RaiseIfNil(AFormHandle, 'AFormHandle');

  WindowHandle := WindowHandleToPlatform(AFormHandle).Wnd;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWBService) then
    SetFocus(WindowHandle);

  if ImeMode <> TImeMode.imDisable then
    RecreateImmContext(AFormHandle);
  TImeModeHelper.SetIme(WindowHandle, ImeMode);
end;

procedure TTextServiceWin.ExitControl(const AFormHandle: TWindowHandle);
begin
  RaiseIfNil(AFormHandle, 'AFormHandle');

  if ImeMode <> TImeMode.imDisable then
    RecreateImmContext(AFormHandle);

  Reset;
  TImeModeHelper.ResetIme(WindowHandleToPlatform(AFormHandle).Wnd, ImeMode);
end;

procedure TTextServiceWin.DrawSingleLine(const ACanvas: TCanvas; const ARect: TRectF; const AFirstVisibleChar: Integer;
  const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  S: string;
begin
  S := CombinedText;
  S := S.Substring(AFirstVisibleChar - 1, S.Length - AFirstVisibleChar + 1);
  DrawSingleLine(ACanvas, S, ARect, AFont, AOpacity, AFlags, ATextAlign, AVTextAlign, AWordWrap);
end;

procedure TTextServiceWin.DrawSingleLine(const ACanvas: TCanvas; const S: string; const ARect: TRectF; const Font: TFont;
  const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);

  procedure UnderlineRegion(const ARegions: TRegion);
  var
    I: Integer;
    Region: TRectF;
    HalfThickness: Single;
    StartPoint, EndPoint: TPointF;
  begin
    HalfThickness := ACanvas.Stroke.Thickness / 2;
    for I := Low(ARegions) to High(ARegions) do
    begin
      Region := ACanvas.AlignToPixel(ARegions[I]);

      StartPoint := TPointF.Create(Region.Left, Region.Bottom);
      StartPoint.Offset(-HalfThickness, -HalfThickness);
      EndPoint := Region.BottomRight;
      EndPoint.Offset(-HalfThickness, -HalfThickness);
      ACanvas.DrawLine(StartPoint, EndPoint, AOpacity);
    end;
  end;

  procedure UnderlineMarkedText(const ALayout: TTextLayout; const AAttributes: TArray<TMarkedTextAttribute>);
  var
    SavedState: TCanvasSaveState;
    Region: TRegion;
    TextRange: TTextRange;
    PositionInLine: Integer;
    I: Integer;
  begin
    SavedState := ACanvas.SaveState;
    try
      ACanvas.Stroke.Assign(ACanvas.Fill);
      ACanvas.Stroke.Thickness := 2;
      ACanvas.Stroke.Dash := TStrokeDash.Solid;
      for I := 0 to FMarkedText.Length - 1 do
      begin
        case AAttributes[I] of
          TMarkedTextAttribute.Input:
            begin
              ACanvas.Stroke.Thickness := 1;
              ACanvas.Stroke.Dash := TStrokeDash.Dash
            end;
          TMarkedTextAttribute.TargetConverted:
            begin
              ACanvas.Stroke.Thickness := 2;
              ACanvas.Stroke.Dash := TStrokeDash.Solid;
            end;
          TMarkedTextAttribute.Converted:
            begin
              ACanvas.Stroke.Thickness := 1;
              ACanvas.Stroke.Dash := TStrokeDash.Solid;
            end;
          TMarkedTextAttribute.TargetNotConverted:
            begin
              ACanvas.Stroke.Thickness := 4;
              ACanvas.Stroke.Dash := TStrokeDash.Solid;
            end;
          TMarkedTextAttribute.InputError:
            begin
              ACanvas.Stroke.Thickness := 1;
              ACanvas.Stroke.Dash := TStrokeDash.Dot
            end;
        end;

        // CaretPosition -> MarkedTextPosition
        if CaretPosition.X > S.Length - FMarkedText.Length then
          PositionInLine := S.Length - FMarkedText.Length
        else
          PositionInLine := CaretPosition.X;
        TextRange := TTextRange.Create(PositionInLine + I, 1);

        Region := ALayout.RegionForRange(TextRange);
        UnderlineRegion(Region);
      end;
    finally
      ACanvas.RestoreState(SavedState);
    end;
  end;

var
  Layout: TTextLayout;
  Attributes: TArray<TMarkedTextAttribute>;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(ACanvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    try
      Layout.TopLeft := ARect.TopLeft;
      Layout.MaxSize := TPointF.Create(ARect.Width, ARect.Height);
      Layout.WordWrap := AWordWrap;
      Layout.HorizontalAlign := ATextAlign;
      Layout.VerticalAlign := AVTextAlign;
      Layout.Font := Font;
      Layout.Color := ACanvas.Fill.Color;
      Layout.Opacity := AOpacity;
      Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
      Layout.Text := S;
    finally
      Layout.EndUpdate;
    end;
    Layout.RenderLayout(ACanvas);

    Attributes := GetMarketTextAttributes;
    if not FMarkedText.IsEmpty and (Length(Attributes) > 0) then
      UnderlineMarkedText(Layout, Attributes);
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextServiceWin.HasMarkedText: Boolean;
begin
  Result := not FMarkedText.IsEmpty;
end;

function TPlatformWin.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceWin;
end;

{ Window }

procedure TWinWindowHandle.ApplyWndBoundsToWnd;
var
  CurrentWndRect: TRect;
begin
  GetWindowRect(Wnd, CurrentWndRect);
  if CurrentWndRect <> FWndBounds then
    SetWindowPos(Wnd, 0, FWndBounds.Left, FWndBounds.Top, FWndBounds.Width, FWndBounds.Height, SWP_NOACTIVATE or SWP_NOZORDER);
end;

procedure TWinWindowHandle.CalcNearestIntegerMultiple;
const
  MaxMul = 9;
var
  I: Integer;
begin
  if not SameValue(Frac(Scale), 0, TEpsilon.Scale) then
  begin
    FNearestIntegerMultiple := Round(Scale);
    for I := 2 to MaxMul do
      if SameValue(Frac(Scale * I), 0, TEpsilon.Scale) then
      begin
        FNearestIntegerMultiple := Trunc(Scale * I);
        Break;
      end;
  end
  else
    FNearestIntegerMultiple := Round(Scale);
end;

class procedure TWinWindowHandle.SetForcedScale(const ANewScale: Single);
begin
  FForcedScale := Max(1, ANewScale);
end;

procedure TWinWindowHandle.SetForcedScaleForForm(const ANewScale: Single);
begin
  SetForcedScale(ANewScale);
  CalcNearestIntegerMultiple;
  FWndClientSize := TSizeF.Create(FClientSize.Width * Scale, FClientSize.Height * Scale).Ceiling;
  FWndClientSize := RoundWndClientSizeToMatchScale(FWndClientSize);
  FClientSize := TSizeF.Create(FWndClientSize.Width / Scale, FWndClientSize.Height / Scale);
  ApplyWndClientSizeToWnd;

  if Form <> nil then
    Form.RecreateResources;
end;

constructor TWinWindowHandle.Create(const AForm: TCommonCustomForm; const AWnd: HWND);

  function WndBoundsToBounds(const AWndBounds: TRect): TRectF;
  begin
    Result.TopLeft := MultiDisplayWin.PXToDP(AWndBounds.TopLeft);
    Result.Width := FWndBounds.Width / Scale;
    Result.Height := FWndBounds.Height / Scale;
  end;

begin
  inherited Create;
  FForm := AForm;
  FWnd := AWnd;
  GetWindowRect(Wnd, FWndBounds);
  FBounds := WndBoundsToBounds(FWndBounds);
  CalculateClientSizeFromWindow;
end;

destructor TWinWindowHandle.Destroy;
begin
  FreeBuffer;
  FreeAndNil(FZOrderManager);
  inherited;
end;

procedure TWinWindowHandle.WMDpiChanged(var AMessage: TWMDpi);
var
  NewScale: Single;
begin
  NewScale := AMessage.YDpi / StandardDpi;
  if not SameValue(FCurrentScale, NewScale, TEpsilon.Scale) then
  begin
    FCurrentScale := NewScale;
    CalcNearestIntegerMultiple;
    WndBounds := AMessage.ScaledRect^;
    if Form <> nil then
    begin
      // DPI affects the canvas size and native menu metrics.
      Form.RecreateResources;
      Form.RecreateOsMenu;
    end;
    AMessage.Result := 0;
  end;
end;

procedure TWinWindowHandle.CreateBuffer(const Width, Height: Integer);
begin
  FreeBuffer;
  FBufferSize := TSize.Create(Width, Height);
  FBitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
  FBitmapInfo.bmiHeader.biPlanes := 1;
  FBitmapInfo.bmiHeader.biBitCount := 32;
  FBitmapInfo.bmiHeader.biCompression := BI_RGB;
  FBitmapInfo.bmiHeader.biWidth := FBufferSize.Width;
  if FBitmapInfo.bmiHeader.biWidth <= 0 then
    FBitmapInfo.bmiHeader.biWidth := 1;
  FBitmapInfo.bmiHeader.biHeight := -FBufferSize.Height;
  if FBitmapInfo.bmiHeader.biHeight >= 0 then
    FBitmapInfo.bmiHeader.biHeight := -1;
  try
    FBufferBitmap := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBufferBits), 0, 0);
    if FBufferBits = nil then
      RaiseLastOSError;
    try
      FBufferHandle := CreateCompatibleDC(0);
      if FBufferHandle = 0 then
        RaiseLastOSError;
      try
        if SelectObject(FBufferHandle, FBufferBitmap) = 0 then
          RaiseLastOSError;
      except
        DeleteDC(FBufferHandle);
        FBufferHandle := 0;
        raise;
      end;
    except
      DeleteObject(FBufferBitmap);
      FBufferBits := nil;
      raise;
    end;
  except
    FBufferBitmap := 0;
    raise;
  end;
end;

procedure TWinWindowHandle.ResizeBuffer(const Width, Height: Integer);
begin
  if FBufferHandle = 0 then
    CreateBuffer(Width, Height)
  else if (FBitmapInfo.bmiHeader.biWidth <> Width) or (Abs(FBitmapInfo.bmiHeader.biHeight) <> Height) then
  begin
    FreeBuffer;
    CreateBuffer(Width, Height)
  end;
end;

function TWinWindowHandle.RoundWndClientSizeToMatchScale(const AWndClientSize: TSize): TSize;

  function GetScale(const AValue: Integer): Single;
  begin
    if SameValue(Frac(AValue/ Scale), 0, TEpsilon.Scale) then
      Result := Scale
    else
      Result := NearestIntegerMultiple;
  end;

var
  LClientSize: TSizeF;
  FinalScale: TPointF;
begin
  FinalScale.X := GetScale(AWndClientSize.Width);
  FinalScale.Y := GetScale(AWndClientSize.Height);

  LClientSize := TSizeF.Create(Round(AWndClientSize.Width / FinalScale.X), Round(AWndClientSize.Height / FinalScale.Y)); // dp
  Result.Width := Round(LClientSize.Width * FinalScale.X); // px
  Result.Height := Round(LClientSize.Height * FinalScale.Y); // px
end;

function TWinWindowHandle.RoundWndSizeToMatchScale(const AWndSize: TSize): TSize;
var
  WndClientSize: TSize;
begin
  WndClientSize := AWndSize - WndBorderSize; // px
  WndClientSize := RoundWndClientSizeToMatchScale(WndClientSize); // px
  Result := WndClientSize + WndBorderSize; // px
end;

function TWinWindowHandle.GetClientSize: TSizeF;
var
  LRect: TRect;
begin
  // We could return the FClientSize here. However, for the rendering process, it is important for us to know exactly
  // the actual size. If the form is on HiDPI screen, we use rounding client size for avoiding rendering artefacts.
  Winapi.Windows.GetClientRect(Wnd, LRect);
  Result := TSizeF.Create(LRect.Width / Scale, LRect.Height / Scale);
end;

function TWinWindowHandle.GetNearestIntegerMultiple: Integer;
begin
  if FNearestIntegerMultiple = 0 then
    CalcNearestIntegerMultiple;
  Result := FNearestIntegerMultiple;
end;

procedure TWinWindowHandle.ApplyWndClientSizeToWnd;
var
  LWindowRect: TRect;
  NewWindowSize: TSize;
begin
  GetWindowRect(Wnd, LWindowRect); // px
  NewWindowSize := FWndClientSize + WndBorderSize; // px

  if LWindowRect.Size <> NewWindowSize then
  begin
    LWindowRect.Size := NewWindowSize;
    WndBounds := LWindowRect;
  end;
end;

procedure TWinWindowHandle.SetClientSize(const Value: TSizeF);
begin
  if FClientSize <> Value then
  begin
    FClientSize := Value;
    FWndClientSize := TSizeF.Create(Value.Width * Scale, Value.Height * Scale).Ceiling;
    FWndClientSize := RoundWndClientSizeToMatchScale(FWndClientSize);

    ApplyWndClientSizeToWnd;
    if Form <> nil then
      TOpenForm(FForm).ResizeHandle;
  end;
end;

function TWinWindowHandle.GetWndBorderSize: TSize;
var
  LClientRect, LWindowRect: TRect;
begin
  GetClientRect(Wnd, LClientRect); // px
  ClientToScreen(Wnd, LClientRect.TopLeft);
  ClientToScreen(Wnd, LClientRect.BottomRight);
  GetWindowRect(Wnd, LWindowRect); // px
  Result := TSize.Create(LWindowRect.Width - LClientRect.Width, LWindowRect.Height - LClientRect.Height); // px;
end;

function TWinWindowHandle.GetBounds: TRectF;
begin
  Result := FBounds;
end;

procedure TWinWindowHandle.SetBounds(const Value: TRectF);

  function CalculateNewWndBounds: TRect;
  begin
    Result.TopLeft := MultiDisplayWin.DpToPx(Value.TopLeft);
    if FBounds.Width = Value.Width then
      Result.Width := FWndBounds.Width
    else
      Result.Width := Trunc(Value.Width * Scale);
    if FBounds.Height = Value.Height then
      Result.Height := FWndBounds.Height
    else
      Result.Height := Trunc(Value.Height * Scale);
  end;

begin
  if FBounds <> Value then
  begin
    FWndBounds := CalculateNewWndBounds;
    FBounds := Value;
    ApplyWndBoundsToWnd;
    CalculateClientSizeFromWindow;
  end;
end;

function TWinWindowHandle.GetWndBounds: TRect;
begin
  Result := FWndBounds;
end;

procedure TWinWindowHandle.SetWndBounds(const Value: TRect);

  function CalculateNewBounds: TRectF;
  begin
    Result.TopLeft := MultiDisplayWin.PxToDp(Value.TopLeft);
    if FWndBounds.Width = Value.Width then
      Result.Width := FBounds.Width
    else
      Result.Width := Value.Width / Scale;
    if FWndBounds.Height = Value.Height then
      Result.Height := FBounds.Height
    else
      Result.Height := Value.Height / Scale;
  end;

begin
  if FWndBounds <> Value then
  begin
    FBounds := CalculateNewBounds;
    FWndBounds := Value;
    ApplyWndBoundsToWnd;
    CalculateClientSizeFromWindow;
  end;
end;

function TWinWindowHandle.GetWndClientSize: TSize;
begin
  Result := FWndClientSize;
end;

procedure TWinWindowHandle.FreeBuffer;
begin
  if FBufferHandle <> 0 then
  begin
    DeleteDC(FBufferHandle);
    FBufferHandle := 0;
  end;
  if FBufferBitmap <> 0 then
  begin
    DeleteObject(FBufferBitmap);
    FBufferBitmap := 0;
  end;
end;

function TWinWindowHandle.GetScale: Single;
begin
  if not SameValue(FForcedScale, 0, TEpsilon.Scale) then
    Result := FForcedScale
  else if not SameValue(FCurrentScale, 0, TEpsilon.Scale) then
    Result := FCurrentScale
  else
    Result := GetWndScale(FWnd);

  FCurrentScale := Result;
end;

function TWinWindowHandle.GetTransparency: Boolean;
begin
  Result := (Form <> nil) and Form.Transparency;
end;

function TWinWindowHandle.GetZOrderManager: TWinZOrderManager;
begin
  if FZOrderManager = nil then
    FZOrderManager := TWinZOrderManager.Create(Self);
  Result := FZOrderManager;
end;

procedure TWinWindowHandle.CalculateClientSizeFromWindow;
var
  NewWndClientSize: TRect;
  WasResized: Boolean;
begin
  GetClientRect(Wnd, NewWndClientSize); // px
  WasResized := FWndClientSize <> NewWndClientSize.Size;
  FWndClientSize := NewWndClientSize.Size;
  FClientSize := TSizeF.Create(FWndClientSize.Width / Scale, FWndClientSize.Height / Scale);

  if WasResized and FForm.IsHandleAllocated then
    TOpenForm(FForm).ResizeHandle;
end;

procedure TWinWindowHandle.UpdateLayer;
var
  Blend: TBlendFunction;
  Origin, Size, BitmapOrigin: TPoint;
  ContextObject: IContextObject;
begin
  if FBufferHandle <> 0 then
  begin
    { Copy from Context }
    if Supports(Form, IContextObject, ContextObject) and (ContextObject.Context <> nil) then
      ContextObject.Context.CopyToBits(FBufferBits, Form.Width * 4, Rect(0, 0, Form.Width, Form.Height));
    { Update }
    Origin := WindowHandleToPlatform(Form.Handle).WndBounds.TopLeft;
    Size := TPoint(FBufferSize);
    Blend.BlendOp := AC_SRC_OVER;
    Blend.AlphaFormat := $01; // AC_SRC_ALPHA;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := $FF;
    BitmapOrigin := Point(0, 0);
    UpdateLayeredWindow(Wnd, 0, @Origin, @Size, FBufferHandle, @BitmapOrigin, $00000000, @Blend, ULW_ALPHA);
  end;
end;

function TWinWindowHandle.FormToWnd(const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.Left * Scale, Rect.Top * Scale, Rect.Right * Scale, Rect.Bottom * Scale);
end;

function TWinWindowHandle.WndToForm(const Rect: TRect): TRectF;
begin
  Result := WndToForm(TRectF.Create(Rect));
end;

procedure TWinWindowHandle.WMWindowPosChanging(var AMessage: TWMWindowPosChanging);

  function IsResized(const AWindowPos: PWindowPos): Boolean;
  begin
    Result := (AWindowPos^.Flags and SWP_NOSIZE) <> SWP_NOSIZE;
  end;

  function IsMoved(const AWindowPos: PWindowPos): Boolean;
  begin
    Result := (AWindowPos^.Flags and SWP_NOMOVE) <> SWP_NOMOVE;
  end;

var
  WindowPos: PWindowPos;
  AdjustedWndSize: TSize;
begin
  // We need to round the window size based on the current screen scaling factor for avoiding issue
  // with rounding in rendering process.
  WindowPos := AMessage.WindowPos;
  if IsResized(WindowPos) then
  begin
    AdjustedWndSize := TSize.Create(WindowPos^.cx, WindowPos^.cy); // px
    AdjustedWndSize := RoundWndSizeToMatchScale(AdjustedWndSize); // px
    if IsMoved(WindowPos) then
    begin
      if WindowPos^.x <> WndBounds.Left then
        WindowPos^.x := WindowPos^.x + WindowPos^.cx - AdjustedWndSize.Width;
      if WindowPos^.y <> WndBounds.Top then
        WindowPos^.y := WindowPos^.y + WindowPos^.cy - AdjustedWndSize.Height;
    end;
    WindowPos^.cx := AdjustedWndSize.Width; // px
    WindowPos^.cy := AdjustedWndSize.Height; // px
  end;
end;

procedure TWinWindowHandle.WMWindowPosChanged(var AMessage: TWMWindowPosChanged);
var
  WindowPos: PWindowPos;
begin
  WindowPos := AMessage.WindowPos;
  if (WindowPos^.Flags and SWP_NOSIZE = 0) or (WindowPos^.Flags and SWP_NOMOVE = 0) then
  begin
    WndBounds := TRect.Create(TPoint.Create(WindowPos^.x, WindowPos^.y), WindowPos^.cx, WindowPos^.cy);
    if Form <> nil then
      Form.SetBoundsF(Bounds);
  end;
end;

procedure TWinWindowHandle.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
var
  LMinMaxInfo: PMinMaxInfo;
  MinSize: TSize;
  MaxSize: TSize;
begin
  if csReading in Form.ComponentState then
    Exit;

  LMinMaxInfo := Message.MinMaxInfo;
  // Calculating constraints based on rounding ClientSize.
  MinSize := TSize.Create(Trunc(Form.Constraints.MinWidth * Scale), Trunc(Form.Constraints.MinHeight * Scale));
  MinSize := RoundWndSizeToMatchScale(MinSize);
  MaxSize := TSize.Create(Trunc(Form.Constraints.MaxWidth * Scale), Trunc(Form.Constraints.MaxHeight * Scale));
  MaxSize := RoundWndSizeToMatchScale(MaxSize);

  if Form.Constraints.MinWidth > 0 then
    LMinMaxInfo^.ptMinTrackSize.x := MinSize.Width;

  if Form.Constraints.MinHeight > 0 then
    LMinMaxInfo^.ptMinTrackSize.y := MinSize.Height;

  if Form.Constraints.MaxWidth > 0 then
    LMinMaxInfo^.ptMaxTrackSize.x := MaxSize.Width;

  if Form.Constraints.MaxHeight > 0 then
    LMinMaxInfo^.ptMaxTrackSize.y := MaxSize.Height;
end;

procedure TWinWindowHandle.WMSize(var AMessage: TWMSize);
begin
  CalculateClientSizeFromWindow;
end;

function TWinWindowHandle.WndToForm(const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.Left / Scale, Rect.Top / Scale, Rect.Right / Scale, Rect.Bottom / Scale);
end;

function WMPaint(HWND: HWND; uMsg: UINT; wParam: wParam; LPARAM: LPARAM): LRESULT; stdcall;
var
  I, rgnStatus: Integer;
  Region: HRgn;
  RegionSize: DWORD;
  RegionData: PRgnData;
  R: TRect;
  LForm: TCommonCustomForm;
  UpdateRects, InPaintUpdateRects: TUpdateRects;
  PS: TPaintStruct;
  Wnd: Winapi.Windows.HWND;
  PaintControl: IPaintControl;
begin
  LForm := FindWindow(HWND);
  if LForm <> nil then
  begin
    Wnd := FormToHWND(LForm);
    GetUpdateRect(Wnd, R, False);
    Region := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    if Region <> 0 then
      try
        rgnStatus := GetUpdateRgn(Wnd, Region, False);
        if rgnStatus in [SIMPLEREGION, COMPLEXREGION] then
        begin
          RegionSize := GetRegionData(Region, $FFFF, nil);
          if RegionSize > 0 then
          begin
            GetMem(RegionData, RegionSize);
            try
              if RegionSize = GetRegionData(Region, RegionSize, RegionData) then
              begin
                SetLength(UpdateRects, RegionData.rdh.nCount);
                for I := 0 to RegionData.rdh.nCount - 1 do
                  UpdateRects[I] := WindowHandleToPlatform(LForm.Handle).WndToForm(PRgnRects(@RegionData.buffer[0])[I]);
              end;
            finally
              FreeMem(RegionData, RegionSize);
            end;
            if Supports(LForm, IPaintControl, PaintControl) then
            begin
              PaintControl.ContextHandle := BeginPaint(Wnd, PS);
              try
                if PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, InPaintUpdateRects) and
                  (Length(InPaintUpdateRects) > 0) then
                begin
                  // add update rects from FInPaintUpdateRects
                  for I := 0 to High(InPaintUpdateRects) do
                  begin
                    SetLength(UpdateRects, Length(UpdateRects) + 1);
                    UpdateRects[High(UpdateRects)] := WindowHandleToPlatform(LForm.Handle).WndToForm(InPaintUpdateRects[I]);
                  end;
                end;
                PaintControl.PaintRects(UpdateRects);
                if PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, InPaintUpdateRects) and
                  (Length(InPaintUpdateRects) > 0) then
                begin
                  // paint second time - when Repaint called in painting
                  PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, UpdateRects);
                  SetLength(InPaintUpdateRects, 0);
                  PlatformWin.FInPaintUpdateRects.AddOrSetValue(LForm.Handle, InPaintUpdateRects);
                  PaintControl.PaintRects(UpdateRects);
                end;
                PaintControl.ContextHandle := 0;
              finally
                EndPaint(Wnd, PS);
              end;
            end;
          end;
        end;
      finally
        DeleteObject(Region);
      end;
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM);
  end
  else
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM);
end;

// When the WM_GESTURENOTIFY message is received, use SetGestureConfig to specify the gestures to receive.
// This message should always be bubbled up using the DefWindowProc function.
procedure WMGestureNotify(const AForm: TCommonCustomForm; uMsg: UINT; AGestureNotify: LPARAM);
const
  // Gestures
  CPan: array [Boolean] of Cardinal = (0, GC_PAN);
  CZoom: array [Boolean] of Cardinal = (0, GC_ZOOM);
  CRotate: array [Boolean] of Cardinal = (0, GC_ROTATE);
  CPressAndTap: array [Boolean] of Cardinal = (0, GC_PRESSANDTAP);
  CTwoFingerTap: array [Boolean] of Cardinal = (0, GC_TWOFINGERTAP);
var
  ScreenPointPx: TPoint;
  ScreenPointDp: TPointF;
  LControl: TComponent;
  LConfigs: array of TGestureConfig;
  LGestures: TInteractiveGestures;
  LObj: IControl;
  LGObj: IGestureControl;
  LGestureNotify: ^GESTURENOTIFYSTRUCT;
begin
  LGestureNotify := Pointer(AGestureNotify);

  // Get the location of the gesture.
  ScreenPointPx := SmallPointToPoint(LGestureNotify.ptsLocation); // px
  ScreenPointDp := MultiDisplayWin.PxToDp(ScreenPointPx); // dp

  // Find the object that the gesture belongs to.
  LObj := AForm.ObjectAtPoint(ScreenPointDp);
  if LObj = nil then
    LControl := AForm
  else
    LControl := LObj.GetObject;

  if Supports(LControl, IGestureControl, LGObj) then
    LGestures := LGObj.GetListOfInteractiveGestures;

  SetLength(LConfigs, 5);
  ZeroMemory(@LConfigs[0], SizeOf(GestureConfig) * 5);

  // Pan gesture & options
  LConfigs[0].dwID := GID_PAN;
  LConfigs[0].dwWant := CPan[TInteractiveGesture.Pan in LGestures] or GC_PAN_WITH_SINGLE_FINGER_VERTICALLY or
    GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY or GC_PAN_WITH_INERTIA;
  LConfigs[0].dwBlock := CPan[not (TInteractiveGesture.Pan in LGestures)] or GC_PAN_WITH_GUTTER;

  // Zoom gesture
  LConfigs[1].dwID := GID_ZOOM;
  LConfigs[1].dwWant := CZoom[TInteractiveGesture.Zoom in LGestures];
  LConfigs[1].dwBlock := CZoom[not (TInteractiveGesture.Zoom in LGestures)];

  // Rotate gesture
  LConfigs[2].dwID := GID_ROTATE;
  LConfigs[2].dwWant := CRotate[TInteractiveGesture.Rotate in LGestures];
  LConfigs[2].dwBlock := CRotate[not (TInteractiveGesture.Rotate in LGestures)];

  // TwoFingerTap gesture
  LConfigs[3].dwID := GID_TWOFINGERTAP;
  LConfigs[3].dwWant := CTwoFingerTap[TInteractiveGesture.TwoFingerTap in LGestures];
  LConfigs[3].dwBlock := CTwoFingerTap[not (TInteractiveGesture.TwoFingerTap in LGestures)];

  // PressAnTap gesture
  LConfigs[4].dwID := GID_PRESSANDTAP;
  LConfigs[4].dwWant := CPressAndTap[TInteractiveGesture.PressAndTap in LGestures];
  LConfigs[4].dwBlock := CPressAndTap[not (TInteractiveGesture.PressAndTap in LGestures)];

  SetGestureConfig(FormToHWND(AForm), 0, Length(LConfigs), @LConfigs[0], SizeOf(TGestureConfig));
end;

function WMGesture(const AForm: TCommonCustomForm; uMsg: UINT; AParam: wParam; AGestureInfo: LPARAM): LRESULT;

  function PxToDp(const APX: Integer): Single; overload;
  begin
    Result := APX / AForm.Handle.Scale;
  end;

  function PxToDp(const APhysicalPoint: TPointF): TPointF; overload;
  begin
    Result := APhysicalPoint / AForm.Handle.Scale;
  end;

var
  ScreenPointPx: TPoint;
  ScreenPointDp: TPointF;
  LControl: TComponent;
  LGestureInfo: GestureInfo;
  EventInfo: TGestureEventInfo;
  Obj: IControl;
  LGObj: IGestureControl;
begin
  Result := 0;
  ZeroMemory(@LGestureInfo, SizeOf(LGestureInfo));
  LGestureInfo.cbSize := SizeOf(LGestureInfo);
  if GetGestureInfo(AGestureInfo, LGestureInfo) then
    try
      ZeroMemory(@EventInfo, SizeOf(EventInfo));
      EventInfo.GestureID := LGestureInfo.dwID + igiFirst;

      // Get the control
      ScreenPointPx := TPoint.Create(LGestureInfo.ptsLocation.X, LGestureInfo.ptsLocation.Y); // px
      ScreenPointDp := MultiDisplayWin.PxToDp(ScreenPointPx); // dp
      Obj := AForm.ObjectAtPoint(ScreenPointDp);
      if Obj = nil then
        LControl := AForm
      else
        LControl := Obj.GetObject;

      if EventInfo.GestureID = igiBegin then
        CapturedGestureControl := LControl;

      // Don't pass GID_BEGIN and GID_END to the control
      if (EventInfo.GestureID <> igiBegin) and (EventInfo.GestureID <> igiEnd) then
      begin
        // Set EventInfo fields from LGestureInfo
        EventInfo.Location := AForm.ScreenToClient(ScreenPointDp);
        EventInfo.Flags := [];
        if LGestureInfo.dwFlags and GF_BEGIN = GF_BEGIN then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfBegin);
        if LGestureInfo.dwFlags and GF_INERTIA = GF_INERTIA then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfInertia);
        if LGestureInfo.dwFlags and GF_END = GF_END then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfEnd);
        case EventInfo.GestureID of
          igiPan:
            begin
              EventInfo.Distance := Trunc(PxToDp(Cardinal(LGestureInfo.ullArguments)));
              EventInfo.InertiaVector := PxToDp(
                TPointF(SmallPointToPoint(InertiaVectorFromArgument(LGestureInfo.ullArguments))));
            end;
          igiZoom, igiTwoFingerTap:
            EventInfo.Distance := Cardinal(LGestureInfo.ullArguments);
          igiPressAndTap:
            begin
              // ullArguments is distance/offset. Add to Location to make TapLocation
              ScreenPointDp := PxToDp(TPointF(SmallPointToPoint(TSmallPoint(Cardinal(LGestureInfo.ullArguments)))));
              // EventInfo.TapLocation := AForm.ScreenToClient(TPointF(ScreenPointDp.X + LGestureInfo.ptsLocation.X, ScreenPointDp.Y + LGestureInfo.ptsLocation.Y));
            end;
          igiRotate:
            EventInfo.Angle := RotateAngleFromArgument(LGestureInfo.ullArguments);
        end;
        // send message to the control
        if Supports(CapturedGestureControl, IGestureControl, LGObj) then
          LGObj.CMGesture(EventInfo);
      end
      else
        Result := DefWindowProc(FormToHWND(AForm), uMsg, AParam, AGestureInfo);

      if EventInfo.GestureID = igiEnd then
        CapturedGestureControl := nil;
    finally
      CloseGestureInfoHandle(AGestureInfo);
    end;
end;

procedure InitializeMultiTouch(const AForm: TCommonCustomForm);
begin
  if PlatformWin.FMultiTouchManager = nil then
    PlatformWin.FMultiTouchManager := TMultiTouchManagerWin.Create(AForm)
  else if PlatformWin.FMultiTouchManager.Parent <> AForm then
    PlatformWin.FMultiTouchManager.Parent := AForm;
end;

function WMTouch(const AForm: TCommonCustomForm; uMsg: UINT; TouchInputCount: wParam; TouchInputInfo: LPARAM): LRESULT;
var
  TouchCount: Integer;
  Inputs: array of TTouchInput;
  Input: TTouchInput;
  I: Integer;
  Touches: TTouches;
  Action: TTouchAction;
  ScreenPixel: TPoint;
  ScreenPoint: TPointF;
  Control: IControl;
  MidPoint: TPointF;
begin
  Result := 0;
  TouchCount := LoWord(Cardinal(TouchInputCount));
  SetLength(Inputs, TouchCount);
  Action := TTouchAction.None;

  if GetTouchInputInfo(TouchInputInfo, TouchCount, @Inputs[0], SizeOf(TTouchInput)) then
    try
      SetLength(Touches, TouchCount);
      for I := 0 to TouchCount - 1 do
      begin
        Input := Inputs[I]; // Screen px

        if (Input.dwFlags and TOUCHEVENTF_DOWN) <> 0 then
          Action := TTouchAction.Down
        else if (Input.dwFlags and TOUCHEVENTF_UP) <> 0 then
          Action := TTouchAction.Up
        else if (Input.dwFlags and TOUCHEVENTF_MOVE) <> 0 then
          Action := TTouchAction.Move;

        // TOUCHINFO point coordinates is in 1/100 of a pixel
        ScreenPixel := TPointF.Create(Input.X / 100, Input.Y / 100).Round; // px
        ScreenPoint := PxToDp(ScreenPixel); // dp

        Touches[I].Location := AForm.ScreenToClient(ScreenPoint);
      end;

      if Length(Touches) = 1 then
        Control := AForm.ObjectAtPoint(ScreenPoint)
      else if Length(Touches) = 2 then
      begin
        MidPoint := Touches[0].Location.MidPoint(Touches[1].Location); // dp
        Control := AForm.ObjectAtPoint(AForm.ClientToScreen(MidPoint));
      end
      else
        Control := nil;

      InitializeMultiTouch(AForm);
      PlatformWin.FMultiTouchManager.SetEnabledGestures(PlatformWin.FEnabledInteractiveGestures);
      PlatformWin.FMultiTouchManager.HandleTouches(Touches, Action, Control);
      Result := DefWindowProc(FormToHWND(AForm), uMsg, TouchInputCount, TouchInputInfo);
    finally
      CloseTouchInputHandle(TouchInputInfo);
    end;
end;

procedure HandleMouseGestures(const AForm: TCommonCustomForm; uMsg: UINT; const AScreenPoint: TPointF);
var
  GestureObj: IGestureControl;
  Control: TComponent;
  Obj: IControl;
  Action: TTouchAction;
  AbsolutePoint: TPointF;
begin
  if not TWinTouchGestureEngine.Supported(AForm) then
    Exit;

  if not ((uMsg <> WM_LBUTTONDOWN) and (PlatformWin.FMultiTouchManager = nil)) then
  begin
    Obj := AForm.ObjectAtPoint(AScreenPoint);
    if Obj = nil then
      Control := AForm
    else
      Control := Obj.GetObject;
    if Supports(Control, IGestureControl, GestureObj) then
    begin
      Control := GestureObj.GetFirstControlWithGestureEngine;
      if Control <> nil then
      begin
        case uMsg of
          WM_MOUSEMOVE:
            Action := TTouchAction.Move;
          WM_LBUTTONDOWN:
            Action := TTouchAction.Down;
          WM_LBUTTONUP:
            Action := TTouchAction.Up;
        else
          Action := TTouchAction.None;
        end;

        InitializeMultiTouch(AForm);
        AbsolutePoint := AForm.ScreenToClient(AScreenPoint);
        PlatformWin.FMultiTouchManager.HandleMouseGestures(AbsolutePoint, Action, Obj);
      end;
    end;
  end;
end;

var
  LastKeyIsDeadKey: Boolean = False;
  LastMousePos: TPoint;

const
  ImpossibleMousePosition: TPoint = (X: Low(FixedInt); Y: Low(FixedInt));

function WndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  UpdateRects: array of TRectF;
  LForm: TCommonCustomForm;
  Wnd: Winapi.Windows.HWND;
  WindowBorder: TWindowBorderWin;

  procedure ProcessUpdateMessages;
  var
    Msg: TMsg;
  begin
    SetLength(UpdateRects, 1);
    UpdateRects[0] := TRectF.Create(TSmallPoint(Cardinal(wParam)).X, TSmallPoint(Cardinal(wParam)).Y,
      TSmallPoint(Cardinal(lParam)).X, TSmallPoint(Cardinal(lParam)).Y);
    while PeekMessage(Msg, hwnd, WM_ADDUPDATERECT, WM_ADDUPDATERECT, PM_REMOVE) do
    begin
      if Msg.Message = WM_QUIT then
      begin
        { Repost WM_QUIT messages }
        PostQuitMessage(Msg.wParam);
        Break;
      end;
      SetLength(UpdateRects, Length(UpdateRects) + 1);
      UpdateRects[High(UpdateRects)] := RectF(TSmallPoint(Cardinal(Msg.wParam)).X, TSmallPoint(Cardinal(Msg.wParam)).Y,
        TSmallPoint(Cardinal(Msg.lParam)).X, TSmallPoint(Cardinal(Msg.lParam)).Y);
    end;
  end;

  procedure PrepareClosePopups;
  begin
    if (Screen <> nil) and (LForm <> nil) and not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate then
    begin
      if LForm.FormStyle = TFormStyle.Popup then
        Screen.PrepareClosePopups(LForm)
      else
        Screen.PrepareClosePopups(nil);
    end;
  end;

  procedure ClosePopupList;
  begin
    if (Screen <> nil) and (LForm <> nil) and not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate then
      Screen.ClosePopupForms;
  end;

  procedure InitialActionsOfPopups;
  begin
    case uMsg of
      WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_NCRBUTTONDBLCLK, WM_NCMBUTTONDBLCLK:
        begin
          PrepareClosePopups;
          ClosePopupList;
        end;
      WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK:
        PrepareClosePopups;
    end;
  end;

  procedure FinalActionsOfPopups;
  begin
    case uMsg of
      WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP, WM_DESTROY:
        begin
          LForm := FindWindow(hwnd);
          ClosePopupList;
        end;
    end;
  end;

  function DispatchMouseWheelToPopups: Boolean;
  var
    I: Integer;
    Handled: Boolean;
  begin
    Handled := False;
    if Screen <> nil then
      for I := Screen.PopupFormCount - 1 downto 0 do
        if Screen.PopupForms[I].Visible then
        begin
          Screen.PopupForms[I].MouseWheel(KeysToShiftState(wParam), TSmallPoint(Cardinal(wParam)).Y, Handled);
          if Handled then
            Break;
        end;
    Result := Handled;
  end;

  procedure CurrentChar(Msg: tagMsg; var Key: Word; var Ch: WideChar; var Shift: TShiftState);
  begin
    Key := wParam;
    Ch := WideChar(Msg.wParam);
    Shift := KeyDataToShiftState(lParam);
    if (Ch >= ' ') then
    begin
      if ((Shift * [ssAlt, ssCtrl]) = [ssAlt, ssCtrl]) then
      begin
        // AltGr + Char (in German keyboard)
        Shift := Shift - [ssAlt, ssCtrl];
      end;
      if (([ssAlt, ssCtrl, ssCommand] * Shift) = []) then
        Key := 0;
    end;
    if ((([ssAlt, ssCtrl, ssCommand] * Shift) <> []) or (Ch < ' ')) and (Key > 0) then
      Ch := #0;
  end;

  function FormPxToDp(const AForm: TCommonCustomForm; const APoint: TPoint): TPointF;
  var
    LScale: Single;
  begin
    LScale := AForm.Handle.Scale;
    Result := (TPointF(APoint) / LScale).Round;
  end;

const
  FlagZOrder: UINT = SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE;
var
  P: TPoint;
  H: Boolean;
  Key: Word;
  Ch: WideChar;
  tme: TTRACKMOUSEEVENT;
  Message: TMessage;
  Shift: TShiftState;
  Placement: TWindowPlacement;
  Msg: tagMsg;
  PaintControl: IPaintControl;
  Obj: IControl;
  MenuDisplayed: Boolean;
  OldWindowState: TWindowState;
  CharMsg, DeadCharMsg: UInt32;
  WindowPoint: TPoint;
  FormPoint: TPointF;
  ScreenPoint: TPointF;
begin
  Result := 0;
  LForm := FindWindow(hwnd);

  Message.Msg := uMsg;
  Message.WParam := wParam;
  Message.LParam := lParam;
  Message.Result := 0;
  // Check to see if this is a design message
  if (LForm <> nil) and (LForm.Designer <> nil) and LForm.Designer.IsDesignMsg(LForm, Message) then
    Exit;

  if LForm <> nil then
  begin
    Wnd := FormToHWND(LForm);
    try
      InitialActionsOfPopups;
      try
        case uMsg of
          WM_NCHITTEST,
          WM_NCACTIVATE,
          WM_NCADDUPDATERECT,
          WM_NCMOUSEMOVE,
          WM_NCLBUTTONDOWN,
          WM_NCLBUTTONUP,
          WM_NCCALCSIZE,
          WM_NCPAINT,
          WM_NCMOUSELEAVE:
            Result := WMNCMessages(LForm, uMsg, wParam, lParam);
          $B000 + 74: // CM_DESTROYHANDLE
            begin
              if (LForm.ClassName = 'TFormContainerForm') and (wParam = 1) then
              begin
                // IDE parent recreate
                SetParent(Wnd, GetDesktopWindow);
                SetWindowPos(Wnd, 0, $A000, $A000, 10, 10, SWP_NOSIZE or SWP_NOZORDER);
              end;
            end;
          WM_DESTROY:
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          WM_ACTIVATE:
            begin
              if not ((TFmxFormState.Recreating in LForm.FormState) or (LForm.FormStyle = TFormStyle.Popup) or
                WindowHandleToPlatform(LForm.Handle).FDisableDeactivate) then
              begin
                if LoWord(wParam) <> 0 then
                begin
                  if HiWord(wParam) = 0 then
                    LForm.Activate;
                  // If the window is minimized, then do nothing.
                end
                else
                begin
                  PrepareClosePopups;
                  LForm.Deactivate;
                  ClosePopupList;
                end;
              end;
              Result := 0;
            end;
          WM_MOUSEACTIVATE:
            begin
              if not (TFmxFormState.Recreating in LForm.FormState) then
              begin
                if LForm.FormStyle = TFormStyle.Popup then
                  Result := MA_NOACTIVATE
                else
                  // Default result if nothing happens
                  Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
              end;
            end;
          WM_SETTEXT:
            begin
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
              // We have own logic for rendering caption for styled border, So we have to redraw border.
              if LForm.Border.IsSupported then
              begin
               if not PeekMessage(Msg, Wnd, WM_NCADDUPDATERECT, WM_NCADDUPDATERECT, PM_NOREMOVE) then
                 SendMessage(Wnd, WM_NCADDUPDATERECT, 0, 0);
              end;
            end;
          WM_ERASEBKGND:
            begin
              Result := 1;
            end;
          WM_PAINT:
            begin
              Result := WMPaint(hwnd, uMsg, wParam, lParam);
            end;
          WM_DPICHANGED:
            begin
              MultiDisplayWin.UpdateDisplayInformation;
              LForm.Handle.Dispatch(Message);
              Result := 0;
            end;
          WM_DISPLAYCHANGE:
            begin
              MultiDisplayWin.UpdateDisplayInformation;
            end;
          WM_ADDUPDATERECT:
            begin
              ProcessUpdateMessages;
              if Supports(LForm, IPaintControl, PaintControl) then
                PaintControl.PaintRects(UpdateRects);
              WindowHandleToPlatform(LForm.Handle).UpdateLayer;
            end;
          WM_SIZE:
            begin
              LForm.Handle.Dispatch(Message);
              Result := 0;
            end;
          WM_WINDOWPOSCHANGING:
            begin
              if [csLoading, csDesigning] * LForm.ComponentState = [csLoading] then
              begin
                if (LForm.Position in [TFormPosition.Default, TFormPosition.DefaultPosOnly]) and
                  (LForm.WindowState <> TWindowState.wsMaximized) then
                begin
                  PWindowPos(lParam)^.Flags := PWindowPos(lParam)^.Flags or SWP_NOMOVE;
                end;
                if (LForm.Position in [TFormPosition.Default, TFormPosition.DefaultSizeOnly]) and
                  (LForm.BorderStyle in [TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin]) then
                begin
                  PWindowPos(lParam)^.Flags := PWindowPos(lParam)^.Flags or SWP_NOSIZE;
                end;
              end;
              if not ((PWindowPos(lParam)^.Flags and FlagZOrder) = FlagZOrder) then
              begin
                if (Screen <> nil) and (LForm <> nil) and (not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate) then
                  if (LForm.FormStyle = TFormStyle.Popup) then
                    ClosePopupList;
              end;
              LForm.Handle.Dispatch(Message);
              if (TFmxFormState.InDesigner in LForm.FormState) and (TFmxFormState.WasNotShown in LForm.FormState) then
                TOpenForm(LForm).ShowInDesigner;
            end;
          WM_WINDOWPOSCHANGED:
            begin
              Placement.Length := SizeOf(TWindowPlacement);
              GetWindowPlacement(hwnd, Placement);
              if (Application.MainForm <> nil) and (LForm = Application.MainForm) and (Placement.showCmd = SW_SHOWMINIMIZED) then
              begin
                PlatformWin.MinimizeApp;
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
              end
              else
              begin
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
                LForm.Handle.Dispatch(Message);
                { update state }
                PlatformWin.FDiableUpdateState := True;
                try
                  OldWindowState := LForm.WindowState;
                  Placement.Length := SizeOf(TWindowPlacement);
                  GetWindowPlacement(hwnd, Placement);
                  case Placement.showCmd of
                    SW_SHOWMINIMIZED:
                      LForm.WindowState := TWindowState.wsMinimized;
                    SW_SHOWMAXIMIZED:
                      LForm.WindowState := TWindowState.wsMaximized;
                  else
                    if csDesigning in LForm.ComponentState then
                    begin
                      { for using Metro-style interface in designer we set Maximized but we can change window size }
                      if LForm.WindowState <> TWindowState.wsMaximized then
                        LForm.WindowState := TWindowState.wsNormal;
                    end
                    else
                    begin
                      if not (TFmxFormState.WasNotShown in LForm.FormState) then
                        LForm.WindowState := TWindowState.wsNormal;
                    end;
                  end;
                  if OldWindowState <> LForm.WindowState then
                  begin
                    PostMessage(hwnd, WM_CLOSEMENU, 0, 0);
                    PrepareClosePopups;
                    ClosePopupList;
                  end;
                finally
                  PlatformWin.FDiableUpdateState := False;
                end;
                WMNCMessages(LForm, uMsg, wParam, lParam);
              end;
            end;
          WM_GETMINMAXINFO:
            LForm.Handle.Dispatch(Message);
          WM_CLOSE:
            LForm.Close;
          WM_LBUTTONDOWN:
            begin
              PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := True;
              LastMousePos := ImpossibleMousePosition;
              WindowPoint := TWMLButtonDown(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              ScreenPoint := LForm.ClientToScreen(FormPoint); // dp
              LForm.MouseDown(TMouseButton.mbLeft, MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
              HandleMouseGestures(LForm, uMsg, ScreenPoint);
            end;
          WM_LBUTTONDBLCLK:
            begin
              PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := True;
              LastMousePos := ImpossibleMousePosition;
              WindowPoint := TWMLButtonDblClk(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // Form point in dp
              LForm.MouseDown(TMouseButton.mbLeft, MouseToShiftState(wParam) + [ssDouble], FormPoint.X, FormPoint.Y);
            end;
          WM_CANCELMODE:
          begin
            // If the user shows dialog from mouse down event, WinApi rejects MouseUp event from queue. In this case,
            // Form.Captured control are not reset and we cannot complete click event correctly. As result button stay
            // pressed.
            // When modal dialog is appeared, WinApi sends WM_CANCELMODE message for rejecting other related events.
            // We save this flag for future emulation MouseUp event, if it's required.

            // It's not Ok for FMX, because FMX depends on normal sequence of mouse events Down -> Up, so
            // Form waits final MouseUp event.
            // If we receive WM_CANCELMODE message while MouseDown is processed, we should emulate MouseUp event.
            if PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed then
            begin
              GetCursorPos(P);
              SendMessage(hwnd, WM_LBUTTONUP, 0, PointToLParam(P));
            end;
            PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := False;
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          end;
          WM_LBUTTONUP:
            begin
              PlatformWin.FormInfo[LForm].WasLeftMouseButtonPressed := False;
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              WindowPoint := TWMLButtonUp(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              ScreenPoint := LForm.ClientToScreen(FormPoint); // dp
              if LForm.Border.IsSupported and WindowBorder.NCClick then
                Result := WMNCMessages(LForm, uMsg, wParam, lParam)
              else
              begin
                LastMousePos := ImpossibleMousePosition;
                LForm.MouseUp(TMouseButton.mbLeft, MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
              end;
              HandleMouseGestures(LForm, uMsg, ScreenPoint);
            end;
           WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
            begin
              LastMousePos := ImpossibleMousePosition;
              WindowPoint := TWMMouse(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              ScreenPoint := LForm.ClientToScreen(FormPoint); // dp
              Obj := LForm.ObjectAtPoint(ScreenPoint);
              if (Obj <> nil) and (Obj.GetObject <> nil) and not (csDesigning in Obj.GetObject.ComponentState) then
              begin
                Obj.SetFocus;
                MenuDisplayed := Obj.ShowContextMenu(ScreenPoint);
              end
              else
                MenuDisplayed := False;
              if not MenuDisplayed then
              begin
                if uMsg = WM_RBUTTONDBLCLK then
                  LForm.MouseDown(TMouseButton.mbRight, MouseToShiftState(wParam) + [ssDouble], FormPoint.X, FormPoint.Y);
                LForm.MouseDown(TMouseButton.mbRight, MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
              end;
            end;
          WM_RBUTTONUP:
            begin
              LastMousePos := ImpossibleMousePosition;
              WindowPoint := TWMRButtonUp(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              LForm.MouseUp(TMouseButton.mbRight, MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
            end;
          WM_MBUTTONDOWN:
            begin
              LastMousePos := ImpossibleMousePosition;
              WindowPoint := TWMMButtonDown(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              LForm.MouseDown(TMouseButton.mbMiddle, MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
            end;
          WM_MBUTTONUP:
            begin
              LastMousePos := ImpossibleMousePosition;
              WindowPoint := TWMMButtonUp(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              LForm.MouseUp(TMouseButton.mbMiddle, MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              LastMousePos := ImpossibleMousePosition;
              WindowPoint := TWMMButtonDblClk(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              LForm.MouseDown(TMouseButton.mbMiddle, MouseToShiftState(wParam) + [ssDouble], FormPoint.X, FormPoint.Y);
            end;
          WM_MENUSELECT,
          WM_INITMENUPOPUP:
            MenuServiceWin.Dispatch(Message);
          WM_MOUSEMOVE:
            begin
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);

              if LForm.Border.IsSupported then
              begin
                if WindowBorder.NCClick then
                  Result := WMNCMessages(LForm, uMsg, wParam, lParam)
                else
                begin
                  WindowBorder.MouseLeave;
                  GetCursorPos(P);
                  if P <> LastMousePos then
                  begin
                    LastMousePos := P;
                    WindowPoint := TWMMouseMove(Message).Pos; // Form point in px
                    FormPoint := FormPxToDp(LForm, WindowPoint); // dp
                    LForm.MouseMove(MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
                  end;
                end;
              end
              else
              begin
                GetCursorPos(P);
                if P <> LastMousePos then
                begin
                  LastMousePos := P;
                  WindowPoint := TWMMouseMove(Message).Pos; // Form point in px
                  FormPoint := FormPxToDp(LForm, WindowPoint); // dp
                  LForm.MouseMove(MouseToShiftState(wParam), FormPoint.X, FormPoint.Y);
                end;
              end;
              tme.cbSize := SizeOf(tme);
              tme.hwndTrack := hwnd;
              tme.dwFlags := TME_LEAVE;
              tme.dwHoverTime := HOVER_DEFAULT;
              TrackMouseEvent(tme);

              WindowPoint := TWMMouseMove(Message).Pos; // Form point in px
              FormPoint := FormPxToDp(LForm, WindowPoint); // dp
              ScreenPoint := LForm.ClientToScreen(FormPoint); // dp
              HandleMouseGestures(LForm, uMsg, ScreenPoint);
            end;
          WM_MOUSELEAVE:
            begin
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              if LForm.Border.IsSupported and WindowBorder.NCClick then
                Result := WMNCMessages(LForm, uMsg, wParam, lParam)
              else
                LForm.MouseLeave;
            end;
          WM_MOUSEWHEEL:
            begin
              H := DispatchMouseWheelToPopups;
              if not H then
                LForm.MouseWheel(KeysToShiftState(wParam), TSmallPoint(Cardinal(wParam)).Y, H);
              Result := Integer(H = True);
            end;
          WM_GETDLGCODE:
            begin
              Result := DLGC_WANTTAB or dlgc_WantArrows or DLGC_WANTCHARS;
            end;
          WM_CHAR:
            begin
              Ch := WideChar(wParam);
              Key := 0;
              LForm.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
              LForm.KeyUp(Key, Ch, KeyDataToShiftState(lParam));
              Result := 0;
            end;
          WM_KEYDOWN,
          WM_SYSKEYDOWN:
            begin
              // Check if this key translates to a WM_CHAR/WM_SYSCHAR message
              // and if it does, pass KeyDown with character code
              // and clear the original WM_CHAR from the queue
              Msg.hwnd := hwnd;
              Msg.Message := uMsg;
              Msg.wParam := wParam;
              Msg.lParam := lParam;

              Result := 0;

              if uMsg = WM_SYSKEYDOWN then
              begin
                CharMsg := WM_SYSCHAR;
                DeadCharMsg := WM_SYSDEADCHAR;
              end
              else
              begin
                CharMsg := WM_CHAR;
                DeadCharMsg := WM_DEADCHAR;
              end;

              LastKeyIsDeadKey := False;
              if PeekMessage(Msg, hwnd, DeadCharMsg, DeadCharMsg, PM_NOREMOVE + PM_NOYIELD) then
                LastKeyIsDeadKey := True
              else if TranslateMessage(Msg) then
              begin
                if PeekMessage(Msg, hwnd, CharMsg, CharMsg, PM_REMOVE) then
                begin
                  CurrentChar(Msg, Key, Ch, Shift);
                  // clear duplicate WM_CHAR
                  if CharMsg = WM_CHAR then
                    PeekMessage(Msg, hwnd, CharMsg, CharMsg, PM_REMOVE);
                  LForm.KeyDown(Key, Ch, Shift);
                end
                else
                begin
                  Key := wParam;
                  Ch := #0;
                  LForm.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
                end;
              end;

              // always let the system handle system shortcuts
              if (uMsg = WM_SYSKEYDOWN) and (Key <> 0) then
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_DEADCHAR:
            begin
              PeekMessage(Msg, hwnd, WM_DEADCHAR, WM_DEADCHAR, PM_REMOVE);
              PeekMessage(Msg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE);
            end;
          WM_KEYUP:
            begin
              Ch := #0;
              Key := wParam;
              Shift := KeyDataToShiftState(lParam);
              if LastKeyIsDeadKey then
              begin
                Result := 0;
              end
              else
              begin
                Msg.hwnd := hwnd;
                Msg.Message := WM_KEYDOWN;
                Msg.wParam := wParam;
                Msg.lParam := Msg.lParam and $7FFFFFFF;
                if TranslateMessage(Msg) then
                  if PeekMessage(Msg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE) then
                  begin
                    CurrentChar(Msg, Key, Ch, Shift);
                  end;
                LForm.KeyUp(Key, Ch, Shift);
                Result := 0;
              end
            end;
          WM_SYSKEYUP:
            begin
              if (wParam = VK_MENU) or (wParam = VK_F10) then
              begin
                LForm.EnterMenuLoop;
                Result := 0;
              end
              else
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_RELEASEFORM:
            begin
              LForm.Free;
            end;
          { IME }
          WM_INPUTLANGCHANGE:
            begin
              // Result := SetIMECompositionWndPosition(LForm, uMsg, wParam, lParam);
              // OnInputLangChange();
            end;
          WM_IME_SETCONTEXT,
          WM_IME_STARTCOMPOSITION,
          WM_IME_COMPOSITION,
          WM_IME_NOTIFY,
          WM_IME_ENDCOMPOSITION:
            begin
              PlatformWin.ImmManager[LForm].Dispatch(Message);
              Result := Message.Result;
            end;
          WM_COMMAND:
            begin
              MenuServiceWin.Dispatch(Message);
              Result := Message.Result;
            end;
          WM_GESTURENOTIFY:
            begin
              WMGestureNotify(LForm, uMsg, lParam);
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_GESTURE:
            begin
              Result := WMGesture(LForm, uMsg, wParam, lParam);
            end;
          WM_TOUCH:
            begin
              Result := WMTouch(LForm, uMsg, wParam, lParam);
            end;
          WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
            Result := SendMessage(lParam, CN_BASE + uMsg, wParam, lParam);
        else
          Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          // Default result if nothing happens
        end;
      finally
        FinalActionsOfPopups;
      end;
    except
      on E: Exception do
        Application.HandleException(E);
    end;
  end
  else { if LForm = nil }
  begin
    if (PlatformWin <> nil) and (hwnd = PlatformWin.FApplicationHWND) then
    begin
      case uMsg of
        WM_QUERYENDSESSION:
          Result := 1;
        WM_ENDSESSION:
          begin
            if Application.MainForm <> nil then
            try
              if Screen <> nil then
                Screen.ActiveForm := nil;
              Application.MainForm.Close;
            except
              on E: Exception do
                Application.HandleException(E);
            end;

            var EndSessionMsg := TWMEndSession(Message);
            EndSessionMsg.Result := 0;
            if EndSessionMsg.EndSession then
            begin
              TMessageManager.DefaultManager.SendMessage(nil, TSaveStateMessage.Create);
              Application.Terminate;
              TThread.ForceQueue(nil,
                procedure
                begin
                  Halt;
                end);
            end;
          end;
        WM_SETTINGCHANGE:
          MultiDisplayWin.UpdateDisplayInformation;
        WM_SYSCOMMAND:
          begin
            case TWMSysCommand(Message).CmdType and $FFF0 of
              SC_MINIMIZE:
              begin
                PlatformWin.MinimizeApp;
                Result := 0;
              end;
              SC_RESTORE:
              begin
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
                PlatformWin.RestoreApp;
              end
            else
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          end;
        WM_CLOSE:
          begin
            if Application.MainForm <> nil then
            begin
              try
                if Screen <> nil then
                  Screen.ActiveForm := nil;
                Application.MainForm.Close;
              except
                on E: Exception do
                  Application.HandleException(E);
              end;
              Exit;
            end;
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          end;
        WM_DESTROY:
          Application.Terminate;
        WM_ACTIVATEAPP:
          begin
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            if BOOL(wParam) then
              PlatformWin.RestoreApp;
          end;
      else
        Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
      end;
    end
    else
      Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
  // Default result if nothing happens
end;

var
  FMAppClass: TWndClass = (
    style: 0;
    lpfnWndProc: @WndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TFMAppClass');

function TPlatformWin.CreateAppHandle: HWND;
var
  TempClass: TWndClass;
  P: PChar;
  ClassRegistered: Boolean;
  ModuleName: array [0 .. 255] of Char;
begin
  GetModuleFileName(MainInstance, ModuleName, Length(ModuleName));
  P := AnsiStrRScan(ModuleName, '\');
  if P <> nil then
    StrCopy(ModuleName, P + 1);
  P := AnsiStrScan(ModuleName, '.');
  if P <> nil then
    P^ := #0;
  CharLower(CharNext(ModuleName));
  FDefaultTitle := ModuleName;
  if Application <> nil then
    FTitle := Application.Title
  else
    FTitle := FDefaultTitle;
  FMAppClass.hInstance := hInstance;
  ClassRegistered := GetClassInfo(hInstance, FMAppClass.lpszClassName, TempClass);
  FMAppClass.hIcon := LoadIconW(MainInstance, PChar('MAINICON'));
  if not ClassRegistered or (TempClass.lpfnWndProc <> @WndProc) then
  begin
    if ClassRegistered then
      Winapi.Windows.UnregisterClass(FMAppClass.lpszClassName, hInstance);
    Winapi.Windows.RegisterClass(FMAppClass);
  end;
  Result := CreateWindowEx(WS_EX_WINDOWEDGE or WS_EX_APPWINDOW, FMAppClass.lpszClassName, PChar(FTitle),
                           WS_POPUP or WS_GROUP, 0, 0, 0, 0, GetDesktopWindow, 0, hInstance, nil);
  Winapi.Windows.ShowWindow(Result, SW_SHOWNORMAL);
end;

function TPlatformWin.GetApplicationHWND: HWND;
begin
  UpdateApplicationHwnd;
  Result := FApplicationHWND;
end;

function TPlatformWin.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
var
  DesignerForm: IDesignerForm;
  IsDesignerForm: Boolean;
  WindowClass: TWndClass;
  LDropTarget: TWinDropTarget;
  Style, ExStyle: DWORD;
  Wnd: HWND;
  ParentWnd: HWND;
  WndClassName: string;

  procedure DefineWindowStyle(const AForm: TCommonCustomForm; var AStyle: DWORD; var AExStyle: DWORD);
  begin
    case AForm.FormStyle of
      TFormStyle.Popup:
        begin
          AStyle := AStyle or WS_POPUP;
          AExStyle := AExStyle or WS_EX_NOACTIVATE;
        end;
      TFormStyle.StayOnTop:
        AExStyle := AExStyle or WS_EX_TOPMOST;
    end;
    if AForm.Transparency then
    begin
      AStyle := AStyle or WS_POPUP;
      AExStyle := AExStyle or WS_EX_LAYERED;
    end
    else
    begin
      case AForm.BorderStyle of
        TFmxFormBorderStyle.None:
          AStyle := AStyle or WS_POPUP or WS_SYSMENU;
        TFmxFormBorderStyle.Single, TFmxFormBorderStyle.ToolWindow:
          AStyle := AStyle or WS_CAPTION or WS_BORDER;
        TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin:
          AStyle := AStyle or WS_CAPTION or WS_THICKFRAME;
      end;
      if AForm.BorderStyle in [TFmxFormBorderStyle.ToolWindow, TFmxFormBorderStyle.SizeToolWin] then
        AExStyle := AExStyle or WS_EX_TOOLWINDOW;
      if AForm.BorderStyle <> TFmxFormBorderStyle.None then
      begin
        if TBorderIcon.biMinimize in AForm.BorderIcons then
          AStyle := AStyle or WS_MINIMIZEBOX;
        if TBorderIcon.biMaximize in AForm.BorderIcons then
          AStyle := AStyle or WS_MAXIMIZEBOX;
        if TBorderIcon.biSystemMenu in AForm.BorderIcons then
          AStyle := AStyle or WS_SYSMENU;
      end;
    end;
  end;

  function DefineParentWnd(const AForm: TCommonCustomForm): HWND;
  begin
    Result := 0;
    // Trying to use the parent form
    if AForm.ParentForm <> nil then
    begin
      if AForm.ParentForm.Handle = nil then
        raise EArgumentException.CreateFMT(SNotInstance, ['ParentForm.Handle']) at ReturnAddress;
      Result := FormToHWND(AForm.ParentForm);
    end;
    if Result <> 0 then
      Exit;

    // For Dialogs and Popups we use a handle of the active form
    if ((TFmxFormState.Modal in AForm.FormState) or (AForm.FormStyle = TFormStyle.Popup)) then
      if (Screen <> nil) and (Screen.ActiveForm <> nil) then
        Result := FormToHWND(Screen.ActiveForm);
    if Result <> 0 then
      Exit;

    // If none parent then we use handle of Application
    Result := ApplicationHWND;
  end;

begin
  RaiseIfNil(AForm, 'AForm');

  LDropTarget := nil;
  Style := WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
  ExStyle := 0;
  WndClassName := 'FM' + AForm.ClassName;
  IsDesignerForm := TFmxFormState.InDesigner in AForm.FormState;
  if not GetClassInfo(hInstance, PChar(WndClassName), WindowClass) then
  begin
    FillChar(WindowClass, SizeOf(WindowClass), 0);
    WindowClass.style := CS_DBLCLKS or CS_HREDRAW or CS_VREDRAW;
    WindowClass.lpfnWndProc := @WndProc;
    WindowClass.cbClsExtra := 0;
    WindowClass.cbWndExtra := 0;
    WindowClass.hInstance := hInstance;
    WindowClass.hIcon := LoadIconW(MainInstance, PChar('MAINICON'));
    if csDesigning in AForm.ComponentState then
      WindowClass.hCursor := LoadCursorW(0, PChar(IDC_ARROW))
    else
      WindowClass.hCursor := 0;
    WindowClass.hbrBackground := GetStockObject(NULL_BRUSH);
    WindowClass.lpszMenuName := nil;
    WindowClass.lpszClassName := PChar(WndClassName);
    if Winapi.Windows.RegisterClass(WindowClass) = 0 then
      RaiseLastOSError;
  end;
  if (csDesigning in AForm.ComponentState) or IsDesignerForm then
  begin
    Style := Style or WS_CHILD;
    // Parent handle going to set in IDE.
    // Now set temporary value
    ParentWnd := GetDesktopWindow;
  end
  else
  begin
    DefineWindowStyle(AForm, Style, ExStyle);
    ParentWnd := DefineParentWnd(AForm);
  end;
  Wnd := CheckWinapiHandle(CreateWindowEx(ExStyle, WindowClass.lpszClassName, PChar(AForm.Caption), Style,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), ParentWnd, 0,
    hInstance, nil));
  try
    SetProp(Wnd, MakeIntAtom(WindowAtom), THandle(AForm));
    try
      if not ((csDesigning in AForm.ComponentState) or Supports(AForm, IDesignerForm, DesignerForm)) then
      begin
        LDropTarget := TWinDropTarget.Create(nil);
        LDropTarget.Form := AForm;
      end;
      try
        if LDropTarget <> nil then
          RegisterDragDrop(Wnd, LDropTarget);
        Result := TWinWindowHandle.Create(AForm, Wnd);
        TWinWindowHandle(Result).FWinDropTarget := LDropTarget;
      except
        if LDropTarget <> nil then
          RevokeDragDrop(Wnd);
        raise;
      end;
    except
      FreeAndNil(LDropTarget);
      raise;
    end;
  except
    Winapi.Windows.DestroyWindow(Wnd);
    raise;
  end;
  TWinWindowHandle(Result).FWinDropTarget := LDropTarget;

  FImmManagers.Add(AForm, TImmManager.Create(AForm));
  FFormsInfo.Add(AForm, TFormInfo.Create);
end;

function TPlatformWin.CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
begin
{$WARN SYMBOL_DEPRECATED OFF}
  Result := FMX.Forms.Border.Win.CreateWindowBorder(AForm);
{$WARN SYMBOL_DEPRECATED DEFAULT}
end;

procedure TPlatformWin.DestroyWindow(const AForm: TCommonCustomForm);

  procedure DestroyOwnedFormHandles(const AHandle: HWND);
  var
    Form: TCommonCustomForm;
    ChildHandle: HWND;
  begin
    for Form in FFormsInfo.Keys do
    begin
      ChildHandle := FormToHWND(Form);
      if (ChildHandle <> 0) and (GetWindow(ChildHandle, GW_OWNER) = AHandle) then
        TOpenForm(Form).DestroyHandle;
    end;
  end;

var
  Wnd: HWND;
  DesignerForm: IDesignerForm;
begin
  RaiseIfNil(AForm, 'AForm');

  HideWindow(AForm);
  Wnd := FormToHWND(AForm);

  // The Destroyable form can own another fmx forms. So we have to destroy all related fmx form handles.
  DestroyOwnedFormHandles(Wnd);

  if not ((csDesigning in AForm.ComponentState) or Supports(AForm, IDesignerForm, DesignerForm)) then
    RevokeDragDrop(Wnd);
  WindowHandleToPlatform(AForm.Handle).FWinDropTarget.Free;
  RemoveProp(Wnd, MakeIntAtom(WindowAtom));
  Winapi.Windows.DestroyWindow(Wnd);

  FImmManagers.Remove(AForm);
  FFormsInfo.Remove(AForm);
end;

procedure TPlatformWin.ReleaseWindow(const AForm: TCommonCustomForm);
begin
end;

procedure TPlatformWin.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  InvalidateWindowRect(AForm, AForm.ClientRect);
end;

procedure TPlatformWin.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
var
  WR: TRect;
  Wnd: HWND;
  PaintControl: IPaintControl;
  UpdateRects: TUpdateRects;
  I: Integer;
begin
  RaiseIfNil(AForm, 'AForm');

  if IntersectRect(R, TRectF.Create(0, 0, AForm.ClientWidth, AForm.ClientHeight)) then
  begin
    Wnd := FormToHWND(AForm);
    if AForm.Transparency and not (csDesigning in AForm.ComponentState) then
    begin
      PostMessage(Wnd, WM_ADDUPDATERECT, Integer(SmallPoint(Round(R.Left), Round(R.Top))),
        Integer(SmallPoint(Round(R.Right), Round(R.Bottom))));
    end
    else
    begin
      R := WindowHandleToPlatform(AForm.Handle).FormToWnd(R);
      if Supports(AForm, IPaintControl, PaintControl) and (PaintControl.ContextHandle <> 0) then
      begin
        // In Paint
        if not FInPaintUpdateRects.TryGetValue(AForm.Handle, UpdateRects) then
        begin
          SetLength(UpdateRects, 1);
          UpdateRects[0] := R;
          FInPaintUpdateRects.Add(AForm.Handle, UpdateRects);
          Exit;
        end
        else
          for I := 0 to High(UpdateRects) do
            if (UpdateRects[I] = R) or (UpdateRects[I].Contains(R.TopLeft) and UpdateRects[I].Contains(R.BottomRight))
            then
              Exit;
        SetLength(UpdateRects, Length(UpdateRects) + 1);
        UpdateRects[High(UpdateRects)] := R;
        FInPaintUpdateRects.AddOrSetValue(AForm.Handle, UpdateRects);
      end
      else
      begin
        WR := TRect.Create(Trunc(R.Left), Trunc(R.Top), Ceil(R.Right), Ceil(R.Bottom));
        Winapi.Windows.InvalidateRect(Wnd, @WR, False);
      end;
    end;
  end;
end;

procedure TPlatformWin.MinimizeApp;
var
  AnimationEnable: Boolean;

  function GetAnimation: Boolean;
  var
    Info: TAnimationInfo;
  begin
    Info.cbSize := SizeOf(TAnimationInfo);
    if SystemParametersInfo(SPI_GETANIMATION, Info.cbSize, @Info, 0) then
      Result := Info.iMinAnimate <> 0
    else
      Result := False;
  end;

  procedure SetAnimation(Value: Boolean);
  var
    Info: TAnimationInfo;
  begin
    Info.cbSize := SizeOf(TAnimationInfo);
    Info.iMinAnimate := Integer(BOOL(Value));
    SystemParametersInfo(SPI_SETANIMATION, Info.cbSize, @Info, 0);
  end;

  procedure MinimiseAllForms;
  var
    I: Integer;
    WindowHandle: HWND;
  begin
    for I := 0 to Screen.FormCount - 1 do
    begin
      WindowHandle := FormToHWND(Screen.Forms[I]);
      if IsWindowVisible(WindowHandle) then
        DefWindowProc(WindowHandle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
    end;
  end;

var
  MainForm: TCommonCustomForm;
  WndPos: TPoint;
begin
  AnimationEnable := GetAnimation;
  try
    SetAnimation(True);
    if Application.MainForm <> nil then
    begin
      MainForm := Application.MainForm;
      WndPos := MultiDisplayWin.DpToPx(TPointF.Create(MainForm.Left, MainForm.Top));
      SetWindowPos(ApplicationHWND, FormToHWND(Application.MainForm), WndPos.X, WndPos.Y, Trunc(MainForm.Width), 0, SWP_SHOWWINDOW);
      MinimiseAllForms;
    end;
    DefWindowProc(ApplicationHWND, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  finally
    SetAnimation(AnimationEnable);
  end;
end;

function TPlatformWin.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    Result := WindowHandleToPlatform(AForm.Handle).Bounds
  else
    Result := TRectF.Create(0, 0, AForm.Width, AForm.Height);
end;

function TPlatformWin.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    Result := AForm.Handle.Scale
  else
    Result := MultiDisplayWin.GetScreenScale;
end;

procedure TPlatformWin.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  RaiseIfNil(AForm, 'AForm');

  { for using Metro-style interface in designer we set Maximized but we can change window size }
  if AForm.IsHandleAllocated and (AForm.WindowState in [TWindowState.wsNormal, TWindowState.wsMaximized]) then
    WindowHandleToPlatform(AForm.Handle).Bounds := ARect;
end;

procedure TPlatformWin.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  RaiseIfNil(AForm, 'AForm');

  SetWindowText(FormToHWND(AForm), ACaption);
end;

procedure TPlatformWin.RegisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.RegisterCanvasClasses;
  FMX.Canvas.D2D.RegisterCanvasClasses;
  FMX.Canvas.GDIP.RegisterCanvasClasses;
end;

procedure TPlatformWin.UnhookTouchHandler(const AForm: TCommonCustomForm);
begin
  if TOSVersion.Check(6, 1) then
    UnregisterTouchWindow(FormToHWND(AForm));
end;

procedure TPlatformWin.UnregisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.UnregisterCanvasClasses;
  FMX.Canvas.D2D.UnregisterCanvasClasses;
  FMX.Canvas.GDIP.UnregisterCanvasClasses;
end;

procedure TPlatformWin.RegisterContextClasses;
begin
  FMX.Context.DX9.RegisterContextClasses;
  FMX.Context.DX11.RegisterContextClasses;
end;

procedure TPlatformWin.UnregisterContextClasses;
begin
  FMX.Context.DX9.UnregisterContextClasses;
  FMX.Context.DX11.UnregisterContextClasses;
end;

procedure TPlatformWin.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  Winapi.Windows.ReleaseCapture;
end;

procedure TPlatformWin.SetApplicationHWNDProc(const Value: TApplicationHWNDProc);
begin
  if @FApplicationHWNDProc <> @Value then
  begin
    FApplicationHWNDProc := Value;
    UpdateApplicationHwnd;
  end;
end;

procedure TPlatformWin.SetCapture(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  Winapi.Windows.SetCapture(FormToHWND(AForm));
end;

function TPlatformWin.GetClientSize(const AForm: TCommonCustomForm): TPointF;
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    Result := TSizeF.Create(WindowHandleToPlatform(AForm.Handle).ClientSize)
  else
    Result := TSizeF.Create(AForm.Width, AForm.Height);
end;

procedure TPlatformWin.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    WindowHandleToPlatform(AForm.Handle).ClientSize := TSizeF.Create(ASize);
end;

procedure TPlatformWin.SetConstraints(const AForm: TCommonCustomForm; const AMinWidth, AMinHeight, AMaxWidth, AMaxHeight: Single);
begin
  RaiseIfNil(AForm, 'AForm');
  // The constraints are set in TWinWindowHandle.WMGetMinMaxInfo.
end;

procedure TPlatformWin.HideWindow(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  SetWindowPos(FormToHWND(AForm), 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
    SWP_NOACTIVATE);
end;

const
  ShowCommands: array [TWindowState] of Integer = (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);
  ShowPopupCommands: array [TWindowState] of Integer = (SW_SHOWNOACTIVATE, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);

procedure TPlatformWin.ShowWindow(const AForm: TCommonCustomForm);
type
  TDeactivationState = record
    Handle: TWinWindowHandle;
    SavedDisableDeactivate: Boolean;
  end;

  function GetActiveWindowHandle: TWinWindowHandle;
  var
    ActiveForm: TCommonCustomForm;
  begin
    if Screen = nil then
      Result := nil
    else
    begin
      ActiveForm := Screen.ActiveForm;
      if ActiveForm = nil then
        Result := nil
      else
        Result := WindowHandleToPlatform(ActiveForm.Handle);
    end;
  end;

  function DisableDeactivating(const AFormHandle: TWinWindowHandle): TDeactivationState;
  begin
    if AFormHandle = nil then
    begin
      Result.Handle := nil;
      Result.SavedDisableDeactivate := False;
    end
    else
    begin
      Result.Handle := AFormHandle;
      Result.SavedDisableDeactivate := AFormHandle.FDisableDeactivate;
      AFormHandle.FDisableDeactivate := True;
    end;
  end;

  procedure RestoreDeactivating(const AState: TDeactivationState);
  begin
    if AState.Handle <> nil then
      AState.Handle.FDisableDeactivate := AState.SavedDisableDeactivate;
  end;

const
  uFlags = SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE;
var
  Wnd, WndParent: HWND;
  FormSize: TSize;
  DeactivationState: TDeactivationState;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  if AForm.FormStyle = TFormStyle.Popup then
  begin
    // When new popup form is showing, it can lead to closing other popup, for avoiding it we disable autoclosing
    // popup on this period.
    DeactivationState := DisableDeactivating(GetActiveWindowHandle);
    try
      Winapi.Windows.ShowWindow(Wnd, ShowPopupCommands[AForm.WindowState]);
    finally
      RestoreDeactivating(DeactivationState);
    end;
  end
  else
    Winapi.Windows.ShowWindow(Wnd, ShowCommands[AForm.WindowState]);

  if AForm.Transparency and not (csDesigning in AForm.ComponentState) then
  begin
    FormSize := TSize.Create(AForm.Width, AForm.Height); // dp
    PostMessage(Wnd, WM_ADDUPDATERECT, Integer(SmallPoint(0, 0)), Integer(SmallPoint(FormSize.Width, FormSize.Height)));
  end;

  if AForm.FormStyle in [TFormStyle.StayOnTop, TFormStyle.Popup] then
  begin
    WndParent := GetParent(Wnd);
    if (WndParent = GetDesktopWindow) or (WndParent = 0) then
      SetWindowPos(Wnd, HWND_TOPMOST, 0, 0, 0, 0, uFlags)
    else
      SetWindowPos(Wnd, HWND_TOP, 0, 0, 0, 0, uFlags);
  end;
end;

procedure TPlatformWin.BringToFront(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  SetWindowPos(Wnd, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TPlatformWin.SendToBack(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  SetWindowPos(Wnd, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TPlatformWin.Activate(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  RaiseIfNil(AForm, 'AForm');

  Wnd := FormToHWND(AForm);
  if not IsWindowVisible(Wnd) or IsIconic(Wnd) then
  begin
    if AForm.FormStyle = TFormStyle.Popup then
      Winapi.Windows.ShowWindow(Wnd, SW_RESTORE or SW_SHOWNOACTIVATE)
    else
      Winapi.Windows.ShowWindow(Wnd, SW_RESTORE);
  end;
  if AForm.FormStyle <> TFormStyle.Popup then
    Winapi.Windows.SetActiveWindow(Wnd);
end;

procedure TPlatformWin.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
var
  Wnd: HWND;

  procedure DoSetState(const AState: TWindowState);
  begin
    if AForm.FormStyle = TFormStyle.Popup then
      Winapi.Windows.ShowWindow(Wnd, ShowCommands[AState] or SW_SHOWNOACTIVATE)
    else
    begin
      if (Application.MainForm = AForm) and (AState = TWindowState.wsMinimized) then
        Winapi.Windows.ShowWindow(ApplicationHWND, ShowCommands[AState])
      else
        Winapi.Windows.ShowWindow(Wnd, ShowCommands[AState])
    end;
  end;

begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.Visible and not FDiableUpdateState then
  begin
    Wnd := FormToHWND(AForm);
    if AForm.FullScreen then
      try
        FDiableUpdateState := True;
        AForm.WindowState := TWindowState.wsMaximized;
        if not Winapi.Windows.IsZoomed(Wnd) then
          DoSetState(TWindowState.wsMaximized);
      finally
        FDiableUpdateState := False;
      end
    else
      DoSetState(AState);
  end;
end;

function TPlatformWin.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
var
  WindowList: Pointer;
  CloseRes: TCloseAction;
  Wnd: HWND;
begin
  RaiseIfNil(AForm, 'AForm');

  Result := mrNone;
  if GetCapture <> 0 then
    SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  Winapi.Windows.ReleaseCapture;
  AForm.HandleNeeded;
  WindowList := DisableTaskWindows(FormToHWND(AForm));
  try
    CloseRes := TCloseAction.caNone;
    AForm.Show;
    AForm.ModalResult := mrNone;
    Wnd := FormToHWND(AForm);
    SetActiveWindow(Wnd);
    SetFocus(Wnd);
    Screen.ActiveForm := AForm;
    repeat
      if not Application.HandleMessage then
        WaitMessage;
      if Application.Terminated then
        Break
      else if AForm.ModalResult <> mrNone then
      begin
        CloseRes := AForm.CloseModal;
        Result := AForm.ModalResult;
      end;
    until Result <> mrNone;
    if not Application.Terminated and (CloseRes <> TCloseAction.caFree) then
      AForm.Hide;
  finally
    EnableTaskWindows(WindowList);
  end;

  Result := AForm.ModalResult;
end;

function TPlatformWin.CanShowModal: Boolean;
begin
  Result := True;
end;

function TPlatformWin.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF {dp}): TPointF;
var
  P: TPoint;
  Scale: Single;
begin
  RaiseIfNil(AForm, 'AForm');

  Scale := WindowHandleToPlatform(AForm.Handle).Scale;
  P := (Point * Scale).Round;
  Winapi.Windows.ClientToScreen(FormToHWND(AForm), P);

  Result := MultiDisplayWin.PxToDp(P);
end;

function TPlatformWin.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF {dp}): TPointF;
var
  P: TPoint;
  Scale: Single;
begin
  RaiseIfNil(AForm, 'AForm');

  P := MultiDisplayWin.DpToPx(Point);
  Winapi.Windows.ScreenToClient(FormToHWND(AForm), P);
  Scale := WindowHandleToPlatform(AForm.Handle).Scale;
  Result := TPointF.Create(P) / Scale;
end;

{ Menus }

procedure TPlatformWin.ThreadSync(var Msg: TMessage);
begin
  if Msg.Msg = WM_NULL then
  begin
    CheckSynchronize;
    Msg.Result := 0;
  end
  else
    Msg.Result := DefWindowProc(FThreadSyncHandle, Msg.Msg, Msg.wParam, Msg.LPARAM);
end;

procedure TPlatformWin.RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Exclude(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformWin.RestoreApp;

  function TryFindForm(const AFormHandle: HWND; out AForm: TCommonCustomForm): Boolean;
  var
    I: Integer;
    Form: TCommonCustomForm;
  begin
    for I := 0 to Screen.FormCount - 1 do
    begin
      Form := Screen.Forms[I];
      if FormToHWND(Form) = AFormHandle then
      begin
        AForm := Form;
        Exit(True);
      end;
    end;
    Result := False;
  end;

var
  LWND: HWND;
  Form: TCommonCustomForm;
begin
  if Screen = nil then
    Exit;

  if Screen.ActiveForm = nil then
  begin
    LWND := GetActiveWindow;
    if (LWND <> 0) and (LWND <> ApplicationHWND) then
    begin
      if TryFindForm(LWND, Form) then
        Form.Activate
      else
        SetActiveWindow(LWND);
      Exit;
    end;

    if Application.MainForm <> nil then
      Application.MainForm.Activate;
  end
  else
    Screen.ActiveForm.Activate;
end;

procedure TPlatformWin.HookTouchHandler(const AForm: TCommonCustomForm);
begin
  if TOSVersion.Check(6, 1) and TWinTouchGestureEngine.Supported(AForm) then
    RegisterTouchWindow(FormToHWND(AForm), TWF_WANTPALM);
end;

{ Drag and Drop }

const
  SID_IDropTargetHelper = '{4657278B-411B-11d2-839A-00C04FD918D0}';
  CLSID_DragDropHelper: TGUID = (D1: $4657278A; D2: $411B; D3: $11D2; D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));

type
  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; const dataObj: IDataObject; var pt: TPoint;
      dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function DragOver(var pt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; var pt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function Show(Show: BOOL): HRESULT; stdcall;
  end;

var
  FDropTargetHelper: IDropTargetHelper;
  FDataObj: IDataObject;

function TWinDropTarget.GetDataObject: TDragObject;
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  str: string;
  Drop: HDrop;
  I, numFiles, buflen: Integer;
  buffer: MarshaledString;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FDataObj = nil then
    Exit;
  // get file name first
  FormatEtc.cfFormat := CF_HDROP;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.Tymed := TYMED_HGLOBAL;
  // get FireMonkey
  if PlatformWin.FDragAndDropActive then
  begin
    FormatEtc.cfFormat := CF_FMOBJECT;
    if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
    begin
      Result := TDropSource(StgMedium.HGLOBAL).Data;
      Exit;
    end;
  end;
  // files
  str := '';
  FormatEtc.cfFormat := CF_HDROP;
  if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      Drop := HDrop(GlobalLock(StgMedium.HGLOBAL));
      { Replace Text }
      numFiles := DragQueryFile(Drop, $FFFFFFFF, nil, 0);
      SetLength(Result.Files, numFiles);
      for I := 0 to numFiles - 1 do
      begin
        // Setting length of buffer plus 1 for string end char (#0)
        buflen := DragQueryFile(Drop, I, nil, 0) + 1;
        buffer := StrAlloc(buflen);
        DragQueryFile(Drop, I, buffer, buflen);
        Result.Files[I] := buffer;
        StrDispose(buffer);
        if I = 0 then
          Result.Data := Result.Files[0];
      end;
    finally
      GlobalUnlock(StgMedium.HGLOBAL);
      { Free the memory }
      ReleaseStgMedium(StgMedium);
    end;
    Exit;
  end;
  // get text
  FormatEtc.cfFormat := CF_UNICODETEXT;
  if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      { Lock the global memory handle to get a pointer to the data }
      str := PChar(GlobalLock(StgMedium.HGLOBAL));
      Result.Data := str;
    finally
      { Finished with the pointer }
      GlobalUnlock(StgMedium.HGLOBAL);
      { Free the memory }
      ReleaseStgMedium(StgMedium);
    end;
    Exit;
  end;
end;

function TWinDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
var
  Res: HRESULT;
begin
  try
    FDataObj := dataObj;
    Result := S_OK;
    dwEffect := DROPEFFECT_NONE;
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDropTargetHelper,
      FDropTargetHelper))) and (FDropTargetHelper <> nil) then
    begin
      Res := FDropTargetHelper.DragEnter(FormToHWND(Form), dataObj, pt, dwEffect);
      if (Failed(Res)) then
        FDropTargetHelper := nil;
    end;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TWinDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
var
  P: TPointF;
  Operation: TDragOperation;
begin
  Result := E_UNEXPECTED;
  try
    dwEffect := DROPEFFECT_NONE;
    P := MultiDisplayWin.PxToDp(pt); // dp
    Operation := TDragOperation.None;
    Form.DragOver(GetDataObject, P, Operation);
    case Operation of
      TDragOperation.None:
        dwEffect := DROPEFFECT_NONE;
      TDragOperation.Move:
        dwEffect := DROPEFFECT_MOVE;
      TDragOperation.Copy:
        dwEffect := DROPEFFECT_COPY;
      TDragOperation.Link:
        dwEffect := DROPEFFECT_LINK;
    end;

    // do NOT translate the screen coordinates to form coordinates because
    // it seems that the drop target helper needs screen coordinates
    if FDropTargetHelper <> nil then
      FDropTargetHelper.DragOver(pt, dwEffect);
    Result := S_OK;
  except
    dwEffect := DROPEFFECT_NONE;
  end;
end;

function TWinDropTarget.DragLeave: HRESULT;
begin
  Form.DragLeave;
  if (FDropTargetHelper <> nil) then
    FDropTargetHelper.DragLeave;
  FDropTargetHelper := nil;
  FDataObj := nil;
  Result := S_OK;
end;

function TWinDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
var
  P: TPointF;
begin
  Result := S_OK;
  try
    if dataObj = nil then
      Exit;
    P := MultiDisplayWin.PxToDp(pt); // dp
    Form.DragDrop(GetDataObject, P);
    // do NOT translate the screen coordinates to form coordinates because
    // it seems that the drop target helper needs screen coordinates
    if FDropTargetHelper <> nil then
      FDropTargetHelper.Drop(dataObj, pt, dwEffect)
  finally
    FDataObj := nil;
    FDropTargetHelper := nil;
  end;
end;

{ IDropSource }

function TDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Integer): HRESULT;
var
  ContinueDrop: Boolean;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
  begin
    ContinueDrop := True;
    if ContinueDrop then
      Result := DRAGDROP_S_DROP
    else
      Result := DRAGDROP_S_CANCEL;
  end
  else
    Result := S_OK;
end;

function TDropSource.GiveFeedback(dwEffect: Integer): HRESULT;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

{ IDataObject }

function TDropSource.dAdvise(const FormatEtc: TFormatEtc; advf: Longint; const advsink: IAdviseSink;
  out dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.dUnadvise(dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumFormatEtc(dwDirection: Longint; out EnumFormatEtc: IEnumFormatEtc): HRESULT;
begin
  if (dwDirection = DATADIR_GET) then
    Result := OleRegEnumFormatEtc(IEnumFormatEtc, dwDirection, EnumFormatEtc)
  else
    Result := E_NOTIMPL;
end;

function TDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

function TDropSource.EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
begin
  Result := (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and (FormatEtc1.ptd = FormatEtc2.ptd) and
    (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and (FormatEtc1.lindex = FormatEtc2.lindex) and
    (FormatEtc1.Tymed = FormatEtc2.Tymed)
end;

function TDropSource.FindFormatEtc(TestFormatEtc: TFormatEtc): Integer;
var
  I: Integer;
  Found: Boolean;
begin
  I := 0;
  Found := False;
  Result := -1;
  while (I < Length(Formats)) and not Found do
  begin
    Found := EqualFormatEtc(Formats[I].FormatEtc, TestFormatEtc);
    if Found then
      Result := I;
    Inc(I);
  end
end;

function TDropSource.HGlobalClone(HGLOBAL: THandle): THandle;
// Returns a global memory block that is a copy of the passed memory block.
var
  Size: LongWord;
  Data, NewData: PByte;
begin
  Size := GlobalSize(HGLOBAL);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(HGLOBAL);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data, NewData, Size);
    finally
      GlobalUnlock(Result);
    end
  finally
    GlobalUnlock(HGLOBAL)
  end
end;

function TDropSource.RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
var
  I: Integer;
begin
  Result := E_INVALIDARG;
  I := FindFormatEtc(Format);
  if (I > -1) and Formats[I].OwnedByDataObject then
    Result := StgMediumIncRef(Formats[I].StgMedium, StgMedium, False)
end;

function TDropSource.StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
  CopyInMedium: Boolean): HRESULT;
begin
  Result := S_OK;
  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  case InStgMedium.Tymed of
    TYMED_HGLOBAL:
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.HGLOBAL := HGlobalClone(InStgMedium.HGLOBAL);
          if OutStgMedium.HGLOBAL = 0 then
            Result := E_OUTOFMEMORY
        end
        else
          // Don't generate a copy just use ourselves and the copy previoiusly saved
          MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_FILE:
      begin
        if CopyInMedium then
        begin
          OutStgMedium.lpszFileName := CoTaskMemAlloc(lstrLenW(InStgMedium.lpszFileName));
          // !!          StrCopyW(PChar(OutStgMedium.lpszFileName), PChar(InStgMedium.lpszFileName))
        end
        else
          MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_ISTREAM:

      IUnknown(MySTGMEDIUM(OutStgMedium).stm)._AddRef;
    TYMED_ISTORAGE:
      IUnknown(MySTGMEDIUM(OutStgMedium).stg)._AddRef;
    TYMED_GDI:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED; // Don't know how to copy GDI objects right now
    TYMED_MFPICT:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED;
    // Don't know how to copy MetaFile objects right now
    TYMED_ENHMF:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED;
    // Don't know how to copy enhanced metafiles objects right now
  else
    Result := DV_E_TYMED
  end;

  // I still have to do this. The Compiler will call _Release on the above Self as IDataObject
  // casts which is not what is necessary.  The DataObject is released correctly.
  if (MySTGMEDIUM(OutStgMedium).UnkForRelease <> nil) and (Result = S_OK) then
    IUnknown(MySTGMEDIUM(OutStgMedium).UnkForRelease)._AddRef
end;

function TDropSource.GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HRESULT;
var
  Global: THandle;
  P: Pointer;
  TextData: string;
  B: TBitmap;
  BitmapHandle: HBITMAP;
begin
  FillChar(Medium, SizeOf(Medium), 0);
  Result := DV_E_FORMATETC;
  if QueryGetData(FormatEtcIn) <> S_OK then
    Exit;

  case FormatEtcIn.cfFormat of
    CF_UNICODETEXT:
      begin
        TextData := Data.Data.ToString;
        Global := GlobalAlloc(0, (Length(TextData) + 1) * 2);
        P := GlobalLock(Global);
        try
          Move(PChar(TextData)^, P^, GlobalSize(Global));
        finally
          GlobalUnlock(Global);
        end;
        Medium.Tymed := TYMED_HGLOBAL;
        Medium.HGLOBAL := Global;
        Result := S_OK;
      end;
    CF_BITMAP:
      begin
        BitmapHandle := 0;
        if Data.Data.IsType<TBitmap> then
          BitmapHandle := BitmapToWinBitmap(TBitmap(Data.Data.AsObject), TAlphaColorRec.White)
        else if Data.Data.IsType<TBitmapSurface> then
        begin
          B := TBitmap.Create;
          try
            B.Assign(TBitmapSurface(Data.Data.AsObject));
            BitmapHandle := BitmapToWinBitmap(B, TAlphaColorRec.White);
          finally
            B.Free;
          end;
        end;
        if BitmapHandle <> 0 then
        begin
          Medium.Tymed := TYMED_GDI;
          Medium.HBITMAP := BitmapHandle;
          Result := S_OK;
        end;
      end;
    CF_FMOBJECT:
      if PlatformWin.FDragAndDropActive then
      begin
        Medium.Tymed := TYMED_HGLOBAL;
        Medium.HGLOBAL := THandle(Self);
        Result := S_OK;
      end;
  end;
  if (Result <> S_OK) and (Formats <> nil) then
  begin
    Result := QueryGetData(FormatEtcIn);
    if (Result = S_OK) and (RetrieveOwnedStgMedium(FormatEtcIn, Medium) = E_INVALIDARG) then
      Result := E_UNEXPECTED
  end
end;

function TDropSource.GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TDropSource.QueryGetData(const FormatEtc: TFormatEtc): HRESULT;
var
  I: Integer;
begin
  Result := DV_E_FORMATETC;
  case FormatEtc.cfFormat of
    CF_UNICODETEXT:
      if not Data.Data.IsObject then
        Result := S_OK;
    CF_BITMAP:
      if Data.Data.IsObject and (Data.Data.IsType<TBitmap> or Data.Data.IsType<TBitmapSurface>) then
        Result := S_OK;
    CF_FMOBJECT:
      if PlatformWin.FDragAndDropActive then
        Result := S_OK;
  end;
  if Result <> S_OK then
  begin
    if Formats <> nil then
    begin
      I := 0;
      Result := DV_E_FORMATETC;
      while (I < Length(Formats)) and (Result = DV_E_FORMATETC) do
      begin
        if Formats[I].FormatEtc.cfFormat = FormatEtc.cfFormat then
        begin
          if (Formats[I].FormatEtc.dwAspect = FormatEtc.dwAspect) then
          begin
            if (Formats[I].FormatEtc.Tymed and FormatEtc.Tymed <> 0) then
              Result := S_OK
            else
              Result := DV_E_TYMED;
          end
          else
            Result := DV_E_DVASPECT;
        end
        else
          Result := DV_E_FORMATETC;
        Inc(I)
      end
    end
    else
      Result := E_UNEXPECTED;
  end;
end;

function TDropSource.CanonicalIUnknown(const TestUnknown: IUnknown): IUnknown;
// Uses COM object identity: An explicit call to the IUnknown::QueryInterface
// method, requesting the IUnknown interface, will always return the same
// pointer.
begin
  if TestUnknown <> nil then
  begin
    if Supports(TestUnknown, IUnknown, Result) then
      IUnknown(Result)._Release
      // Don't actually need it just need the pointer value
    else
      Result := TestUnknown
  end
  else
    Result := TestUnknown
end;

function TDropSource.SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: BOOL): HRESULT;
var
  Index: Integer;
begin
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc);
  if Index > -1 then
  begin
    // Yes we already have that format type stored.  Just use the TClipboardFormat
    // in the List after releasing the data
    ReleaseStgMedium(Formats[Index].StgMedium);
    FillChar(Formats[Index].StgMedium, SizeOf(Formats[Index].StgMedium), #0);
  end
  else
  begin
    // It is a new format so create a new TDataObjectInfo record and store it in
    // the Format array
    SetLength(Formats, Length(Formats) + 1);
    Formats[Length(Formats) - 1].FormatEtc := FormatEtc;
    Index := Length(Formats) - 1;
  end;
  // The data is owned by the TClipboardFormat object
  Formats[Index].OwnedByDataObject := True;

  if fRelease then
  begin
    // We are simply being given the data and we take control of it.
    Formats[Index].StgMedium := Medium;
    Result := S_OK
  end
  else
    // We need to reference count or copy the data and keep our own references
    // to it.
    Result := StgMediumIncRef(Medium, Formats[Index].StgMedium, True);

  // Can get a circular reference if the client calls GetData then calls
  // SetData with the same StgMedium.  Because the unkForRelease and for
  // the IDataObject can be marshalled it is necessary to get pointers that
  // can be correctly compared.
  // See the IDragSourceHelper article by Raymond Chen at MSDN.
  if MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease <> nil then
  begin
    if CanonicalIUnknown(Self) = CanonicalIUnknown(IUnknown(MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease)) then
    begin
      IUnknown(MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease)._Release;
      MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease := nil
    end;
  end;
end;

{ Platform DragDrop }

procedure TPlatformWin.AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Include(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformWin.BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
var
  DropSource: TDropSource;
  DropEffect: Longint;
  DragSourceHelper: IDragSourceHelper;
  SHDRAGIMAGE: TSHDRAGIMAGE;
  DragMousePos, Offset: TPointF;
  Control: IControl;
begin
  DropSource := TDropSource.Create(nil);
  try
    DropSource.Data := Data;

    DragMousePos := GetMousePos;
    // CoCreateInstance takes too long to execute so mouse position becomes different when called later
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDragSourceHelper, DragSourceHelper)
      ))
      and (DragSourceHelper <> nil) then
    begin
      if Data.Source is TControl then
        Offset := TControl(Data.Source).AbsoluteToLocal(AForm.ScreenToClient(DragMousePos))
      else
        Offset := PointF((ABitmap.Width div 2), ABitmap.Height div 2);

      FillChar(SHDRAGIMAGE, SizeOf(SHDRAGIMAGE), 0);
      SHDRAGIMAGE.sizeDragImage.cx := ABitmap.Width;
      SHDRAGIMAGE.sizeDragImage.cy := ABitmap.Height;
      SHDRAGIMAGE.ptOffset.X := Round(Offset.X);
      SHDRAGIMAGE.ptOffset.Y := Round(Offset.Y);
      SHDRAGIMAGE.hbmpDragImage := BitmapToWinBitmap(ABitmap, True);
      if not Succeeded(DragSourceHelper.InitializeFromBitmap(@SHDRAGIMAGE, DropSource)) then
        DeleteObject(SHDRAGIMAGE.hbmpDragImage);
    end;

    if not IsThemeActive then
    begin
      SetCursor(crDrag);
      FDragAndDropActive := True;
      try
        DoDragDrop(DropSource, DropSource, DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, DropEffect);
      finally
        FDragAndDropActive := False;
        SetCursor(crDefault);
      end;
    end
    else
    try
      FDragAndDropActive := True;
      DoDragDrop(DropSource, DropSource, DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, DropEffect);
    finally
      FDragAndDropActive := False;
    end;

    if (DropEffect = 0) and Supports(Data.Source, IControl, Control) then
      Control.DragEnd;

    if DragSourceHelper <> nil then
      DragSourceHelper := nil;
  finally
    DropSource.Free;
  end;
end;

{ Mouse }

procedure TPlatformWin.SetCursor(const ACursor: TCursor);
const
  CustomCursorMap: array [crSizeAll .. crNone] of PChar = (
    nil, nil, nil, nil, nil, IDC_SQLWAIT, IDC_MULTIDRAG, nil, nil, IDC_NODROP, IDC_DRAG, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil);

  CursorMap: array [crSizeAll .. crNone] of PChar = (
    IDC_SIZEALL, IDC_HAND, IDC_HELP, IDC_APPSTARTING, IDC_NO, nil, nil, IDC_SIZENS, IDC_SIZEWE, nil, nil, IDC_WAIT,
    IDC_UPARROW, IDC_SIZEWE, IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZEALL, IDC_IBEAM, IDC_CROSS, IDC_ARROW, nil);

  function IsDefaultOrInvalidCursor(const ACursor: TCursor): Boolean;
  begin
    Result := (ACursor = crDefault) or not InRange(ACursor, crSizeAll, crNone);
  end;

var
  NewCursor: HCURSOR;
begin
  if not FDragAndDropActive then
  begin
    // We don't set cursor by default, when we create window. So we should use crArrow cursor by default.
    if IsDefaultOrInvalidCursor(ACursor) and not (csDesigning in Application.ComponentState) then
      FCursor := crArrow
    else
      FCursor := ACursor;

    if InRange(FCursor, crSizeAll, crNone) then
    begin
      if CustomCursorMap[FCursor] <> nil then
        NewCursor := LoadCursorW(HInstance, CustomCursorMap[FCursor])
      else
        NewCursor := LoadCursorW(0, CursorMap[FCursor]);
      Winapi.Windows.SetCursor(NewCursor);
    end;
  end;
end;

function TPlatformWin.GetCursor: TCursor;
begin
  Result := FCursor;
end;

function TPlatformWin.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
var
  SavedState: TFullScreenSavedState;
begin
  if FFullScreenSupport.TryGetValue(AForm, SavedState) then
    Result := SavedState.IsFullScreen
  else
    Result := False;
end;

function TPlatformWin.GetImmManager(const Index: TCommonCustomForm): TImmManager;
begin
  Result := FImmManagers[Index];
end;

procedure TPlatformWin.SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
var
  SavedState: TFullScreenSavedState;
  LClean: TFullScreenSavedState;
begin
  RaiseIfNil(AForm, 'AForm');

  if AValue and not (TFmxFormState.Showing in AForm.FormState) then
    AForm.Visible := True;

  if not FFullScreenSupport.TryGetValue(AForm, SavedState) then
  begin
    FillChar(SavedState, SizeOf(SavedState), 0);
    FFullScreenSupport.Add(AForm, SavedState);
  end;

  if AValue and (AForm.Visible or (TFmxFormState.Showing in AForm.FormState)) then
  begin
    SavedState.IsFullscreen := AValue;
    SavedState.WindowState := AForm.WindowState;
    SavedState.BorderStyle := AForm.BorderStyle;
    if AForm.WindowState = TWindowState.wsNormal then
    begin
      SavedState.Size := TSizeF.Create(AForm.Width, AForm.Height);
      SavedState.Position := TPointF.Create(AForm.Left, AForm.Top);
    end;
    FFullScreenSupport.Items[AForm] := SavedState;
    if AForm.WindowState = TWindowState.wsMinimized then
      AForm.WindowState := TWindowState.wsMaximized;
    AForm.BorderStyle := TFmxFormBorderStyle.None;
    AForm.WindowState := TWindowState.wsMaximized;
  end
  else if SavedState.IsFullscreen <> AValue then
  begin
    LClean := SavedState;
    LClean.BorderStyle := TFmxFormBorderStyle.None;
    LClean.WindowState := TWindowState.wsMaximized;
    LClean.IsFullscreen := AValue;

    FFullScreenSupport.Items[AForm] := LClean;
    if (SavedState.Size.Width > 0) and (SavedState.Size.Height > 0) then
    begin
      AForm.BorderStyle := SavedState.BorderStyle;
      AForm.SetBoundsF(SavedState.Position.X, SavedState.Position.Y, SavedState.Size.Width, SavedState.Size.Height);
      AForm.WindowState := SavedState.WindowState;
    end;
  end;
end;

procedure TPlatformWin.SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
end;

function TPlatformWin.GetMousePos: TPointF;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := MultiDisplayWin.PxToDp(P);
end;

function TPlatformWin.GetFormInfo(const Index: TCommonCustomForm): TFormInfo;
begin
  Result := FFormsInfo[Index];
end;

function TPlatformWin.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  RaiseIfNil(AHandle, 'AHandle');

  if AHandle is TWinWindowHandle then
    Result := TWinWindowHandle(AHandle).Form
  else
    Result := nil;
end;

function TPlatformWin.RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
begin
  Result := FKeyMapping.RegisterKeyMapping(PlatformKey, VirtualKey, KeyKind);
end;

function TPlatformWin.UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
  Result := FKeyMapping.UnregisterKeyMapping(PlatformKey);
end;

function TPlatformWin.PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
begin
  Result := FKeyMapping.PlatformKeyToVirtualKey(PlatformKey, KeyKind);
end;

function TPlatformWin.VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
begin
  Result := FKeyMapping.VirtualKeyToPlatformKey(VirtualKey);
end;

procedure RegisterApplicationHWNDProc(const Proc: TApplicationHWNDProc);
begin
  if PlatformWin <> nil then
    PlatformWin.ApplicationHWNDProc := Proc
  else
    raise EArgumentNilException.Create(SArgumentNil);
end;

{ TImmManager }

constructor TImmManager.Create(const AForm: TCommonCustomForm);
begin
  FForm := AForm;
end;

function TImmManager.GetComposition(const AContext: HIMC): string;
var
  BufferLength: Integer;
begin
  BufferLength := ImmGetCompositionString(AContext, GCS_COMPSTR, nil, 0);
  SetLength(Result, BufferLength div SizeOf(Char));
  ImmGetCompositionString(AContext, GCS_COMPSTR, PChar(Result), BufferLength);
end;

function TImmManager.GetFormHandle: HWND;
begin
  Result := FormToHWND(Form);
end;

function TImmManager.GetResultComposition(const AContext: HIMC): string;
var
  BufferLength: Integer;
begin
  BufferLength := ImmGetCompositionString(AContext, GCS_RESULTSTR, nil, 0);
  SetLength(Result, BufferLength div SizeOf(Char));
  ImmGetCompositionString(AContext, GCS_RESULTSTR, PChar(Result), BufferLength);
end;

procedure TImmManager.ProcessImeParameters(const AContext: HIMC; const AParameters: LPARAM; const ATextService: TTextServiceWin);
var
  CompositionString: string;
begin
  CompositionString := GetComposition(AContext);
  ATextService.InternalSetMarkedText(CompositionString);

  if AParameters and GCS_COMPATTR <> 0 then
    UpdateCompositionAttributes(AContext, ATextService);

  if AParameters and GCS_CURSORPOS <> 0 then
    UpdateCompositionCursorPos(AContext, ATextService)
  else if GetKeyboardLayout(0) and $FFFF = TTextServiceWin.LCID_Korean_Default then
    // Native Korean IME Input system doesn't use cursor position. Instead of it it hightlights the last character in
    // marked text. Since FMX doesn't support displaying character selection, we just move caret in the end of marked
    // text and use underline attribute for highlighting last character.
    ATextService.MarkedTextCursorPosition := Max(1, ATextService.MarkedText.Length);
end;

procedure TImmManager.UpdateCompositionAttributes(const AContext: HIMC; const ATextService: TTextServiceWin);
var
  BufferLength: Integer;
begin
  BufferLength := ImmGetCompositionString(AContext, GCS_COMPATTR, nil, 0);
  if BufferLength > 0 then
  begin
    SetLength(ATextService.CompAttrBuf, BufferLength);
    ImmGetCompositionString(AContext, GCS_COMPATTR, @(ATextService.CompAttrBuf[0]), BufferLength);
  end;
end;

procedure TImmManager.UpdateCompositionCursorPos(const AContext: HIMC; const ATextService: TTextServiceWin);
var
 CursorPos: Integer;
begin
  CursorPos := ImmGetCompositionString(AContext, GCS_CURSORPOS, nil, 0);
  if CursorPos >= 0 then
    ATextService.MarkedTextCursorPosition := CursorPos;
end;

function TImmManager.UpdateIMEWindowPosition: LRESULT;

  function DpToPx(const APoint: TPointF): TPoint; overload;
  var
    FormScale: Single;
  begin
    FormScale := Form.Handle.Scale;
    Result := TPointF.Create(APoint.X * FormScale, APoint.Y * FormScale).Round;
  end;

  function DpToPx(const ARect: TRectF): TRect; overload;
  begin
    Result.TopLeft := DpToPx(ARect.TopLeft);
    Result.BottomRight := DpToPx(ARect.BottomRight);
  end;

  function DefineControlRect(const AFocusedControl: IControl; const ATextInput: ITextInput): TRectF;
  var
    SelectionRect: TRectF;
  begin
    SelectionRect := ATextInput.GetSelectionRect;
    Result.TopLeft := Form.ScreenToClient(AFocusedControl.LocalToScreen(SelectionRect.TopLeft));
    Result.BottomRight := Form.ScreenToClient(AFocusedControl.LocalToScreen(SelectionRect.BottomRight));
  end;

var
  IMC: HIMC;
  Candidate: TCandidateForm;
  TextInput: ITextInput;
begin
  Result := 0;

  if not Supports(Form.Focused, ITextInput, TextInput) then
    Exit;

  IMC := ImmGetContext(FormHandle);
  if IMC = 0 then
    Exit;

  try
    Candidate.dwIndex := 0;
    Candidate.dwStyle := CFS_POINT;
    Candidate.ptCurrentPos := DpToPx(TextInput.GetTargetClausePointF);
    Result := LRESULT(ImmSetCandidateWindow(IMC, @Candidate));

    Candidate.dwStyle := CFS_EXCLUDE;
    Candidate.rcArea := DpToPx(DefineControlRect(Form.Focused, TextInput));
    ImmSetCandidateWindow(IMC, @Candidate);
  finally
    ImmReleaseContext(FormHandle, IMC);
  end;
end;

procedure TImmManager.WMComposition(var Message: TMessage);
var
  IMC: HIMC;
  S: string;
  TextInput: ITextInput;
  I: Integer;
  Key: Word;
  Ch: Char;
  IsProcessed: Boolean;
begin
  UpdateIMEWindowPosition;

  IsProcessed := False;

  // User selected result string
  if Message.LParam and GCS_RESULTSTR <> 0 then
  begin
    IsProcessed := True;
    IMC := ImmGetContext(FormHandle);
    if IMC <> 0 then
    begin
      try
        if Supports(Form.Focused, ITextInput, TextInput) then
        begin
          TTextServiceWin(TextInput.GetTextService).Reset;
          TextInput.IMEStateUpdated;
        end;
        S := GetResultComposition(IMC);
      finally
        ImmReleaseContext(FormHandle, IMC);
      end;

      for I := 0 to S.Length - 1 do
      begin
        Key := 0;
        Ch := S.Chars[I];
        Form.KeyDown(Key, Ch, []);
        Form.KeyUp(Key, Ch, []);
      end;
    end;
  end;

  // User is inputting current composition string
  if Message.LParam and GCS_COMPSTR <> 0 then
  begin
    IsProcessed := True;
    IMC := ImmGetContext(FormHandle);
    if IMC <> 0 then
      try
        if Supports(Form.Focused, ITextInput, TextInput) then
        begin
          ProcessImeParameters(IMC, Message.LParam, TTextServiceWin(TextInput.GetTextService));
          TextInput.IMEStateUpdated;
        end;
      finally
        ImmReleaseContext(FormHandle, IMC);
      end;
  end;

  if IsProcessed then
    Message.Result := 0
  else
    Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TImmManager.WMEndComposition(var Message: TMessage);
var
  TextInput: ITextInput;
begin
  UpdateIMEWindowPosition;

  if Supports(Form.Focused, ITextInput, TextInput) then
    TTextServiceWin(TextInput.GetTextService).InternalEndIMEInput;
end;

procedure TImmManager.WMNotify(var Message: TMessage);
begin
  if Message.WParam = IMN_OPENCANDIDATE then
    Message.Result := UpdateIMEWindowPosition
  else
    Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TImmManager.WMSetContext(var Message: TMessage);
begin
  Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam and ISC_SHOWUIALLCANDIDATEWINDOW);
end;

procedure TImmManager.WMStartComposition(var Message: TMessage);
var
  TextInput: ITextInput;
begin
  UpdateIMEWindowPosition;

  if Supports(Form.Focused, ITextInput, TextInput) then
    TTextServiceWin(TextInput.GetTextService).InternalStartIMEInput;

  if System.SysUtils.Win32MajorVersion >= 6 then
    Message.Result := DefWindowProc(FormHandle, Message.Msg, Message.WParam, Message.LParam)
  else
    Message.Result := 0;
end;

procedure ShutDown;
begin
  if IsLibrary then
  begin
    FMX.Canvas.D2D.UnregisterCanvasClasses;
    FMX.Context.DX11.UnregisterContextClasses;
    FMX.Forms.FinalizeForms;
  end;
end;

initialization
  OleInitialize(nil);
  CapturedGestureControl := nil;
  LastMousePos := ImpossibleMousePosition;
end.

unit HGM.FMX.Image;

interface

uses
  System.Classes, System.Types, System.SysUtils, FMX.Forms, FMX.Graphics,
  FMX.Objects, System.Threading, System.Generics.Collections,
  System.Net.HttpClient;

type
  TCallbackObject = record
    Owner: TComponent;
    Bitmap: TBitmap;
    Url: string;
    Task: ITask;
    OnDone: TProc<Boolean>;
  end;

  TObjectOwner = class(TComponent)
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  TBitmapHelper = class helper for TBitmap
  private
    class var
      Pool: TThreadPool;
      FCallbackList: TThreadList<TCallbackObject>;
      FObjectOwner: TComponent;
      FClient: THTTPClient;
    class procedure AddCallback(Callback: TCallbackObject);
    class procedure Ready(const Url: string; Stream: TStream);
    class function Get(const URL: string): TMemoryStream; static;
    class function GetClient: THTTPClient; static;
  public
    class procedure RemoveCallback(const AOwner: TComponent);
    procedure LoadFromUrl(const Url: string; UseCache: Boolean = True);
    procedure LoadFromUrlAsync(AOwner: TComponent; const Url: string; Cache: Boolean = True; OnDone: TProc<Boolean> = nil); overload;
    procedure LoadFromResource(ResName: string); overload;
    procedure LoadFromResource(Instanse: NativeUInt; ResName: string); overload;
    procedure SaveToStream(Stream: TStream; const Ext: string); overload;
    procedure SaveToFile(const AFileName: string; const Ext: string); overload;
    class function CreateFromUrl(const Url: string; UseCache: Boolean = True): TBitmap;
    class function CreateFromResource(ResName: string; Url: string = ''): TBitmap;
    class property Client: THTTPClient read GetClient;
  end;

implementation

uses
  FMX.Surfaces, FMX.Types, FMX.Consts;

{ TBitmapHelper }

class procedure TBitmapHelper.AddCallback(Callback: TCallbackObject);
begin
  Callback.Owner.FreeNotification(FObjectOwner);
  FCallbackList.Add(Callback);
end;

class function TBitmapHelper.CreateFromResource(ResName, Url: string): TBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromResource(ResName);
end;

class function TBitmapHelper.CreateFromUrl(const Url: string; UseCache: Boolean): TBitmap;
begin
  Result := TBitmap.Create;
  Result.LoadFromUrl(Url, False);
end;

procedure TBitmapHelper.LoadFromResource(ResName: string);
begin
  LoadFromResource(HInstance, ResName);
end;

procedure TBitmapHelper.LoadFromResource(Instanse: NativeUInt; ResName: string);
var
  Mem: TResourceStream;
begin
  Mem := TResourceStream.Create(Instanse, ResName, RT_RCDATA);
  try
    Self.LoadFromStream(Mem);
  finally
    Mem.Free;
  end;
end;

procedure TBitmapHelper.LoadFromUrl(const Url: string; UseCache: Boolean);
var
  Mem: TMemoryStream;
begin
  Mem := Get(Url);
  try
    Self.LoadFromStream(Mem);
  finally
    Mem.Free;
  end;
end;

class function TBitmapHelper.Get(const URL: string): TMemoryStream;
begin
  if URL.IsEmpty then
    raise Exception.Create('Empty URL');
  Result := TMemoryStream.Create;
  try
    if (GetClient.Get(URL, Result).StatusCode = 200) and (Result.Size > 0) then
      Result.Position := 0;
  except
    Result.Free;
    Result := nil;
  end;
end;

class function TBitmapHelper.GetClient: THTTPClient;
begin
  if not Assigned(FClient) then
  begin
    FClient := THTTPClient.Create;
    FClient.HandleRedirects := True;
  end;
  Result := FClient;
end;

procedure TBitmapHelper.LoadFromUrlAsync(AOwner: TComponent; const Url: string; Cache: Boolean; OnDone: TProc<Boolean>);
begin
  if AOwner = nil then
    raise Exception.Create('You must specify an owner (responsible) who will ensure that the Bitmap is not destroyed before the owner');
  var Callback: TCallbackObject;
  Callback.Owner := AOwner;
  Callback.Bitmap := Self;
  Callback.Url := Url;
  Callback.OnDone := OnDone;
  Callback.Task := TTask.Run(
    procedure
    begin
      try
        var Mem := Get(Url);
        //if Cache then
        //  AddCache(Url, Mem);
        TThread.ForceQueue(nil,
          procedure
          begin
            Ready(Url, Mem);
          end);
      except
        TThread.ForceQueue(nil,
          procedure
          begin
            Ready(Url, nil);
          end);
      end;
    end, Pool);
  AddCallback(Callback);
end;

class procedure TBitmapHelper.Ready(const Url: string; Stream: TStream);
begin
  var List := FCallbackList.LockList;
  try
    for var i := List.Count - 1 downto 0 do
      if List[i].Url = Url then
      begin
        try
          var Success: Boolean := False;
          try
            if Assigned(Stream) then
            begin
              Stream.Position := 0;
              List[i].Bitmap.LoadFromStream(Stream);
              Success := True;
            end
            else
              List[i].Bitmap.Assign(nil);
          finally
            if Assigned(List[i].OnDone) then
              List[i].OnDone(Success);
          end;
        except
            //
        end;
        List.Delete(i);
      end;
  finally
    FCallbackList.UnlockList;
  end;
end;

class procedure TBitmapHelper.RemoveCallback(const AOwner: TComponent);
begin
  var List := FCallbackList.LockList;
  try
    for var i := List.Count - 1 downto 0 do
      if List[i].Owner = AOwner then
      begin
        List[i].Task.Cancel;
        List.Delete(i);
      end;
  finally
    FCallbackList.UnlockList;
  end;
end;

procedure TBitmapHelper.SaveToFile(const AFileName, Ext: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream, Ext);
  finally
    Stream.Free;
  end;
end;

procedure TBitmapHelper.SaveToStream(Stream: TStream; const Ext: string);
var
  Surf: TBitmapSurface;
begin
  TMonitor.Enter(Self);
  try
    Surf := TBitmapSurface.Create;
    try
      Surf.Assign(Self);
      if not TBitmapCodecManager.SaveToStream(Stream, Surf, Ext) then
        raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
    finally
      Surf.Free;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

{ TObjectOwner }

procedure TObjectOwner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation <> TOperation.opRemove then
    Exit;
  var List := TBitmap.FCallbackList.LockList;
  try
    for var i := List.Count - 1 downto 0 do
      if List[i].Owner = AComponent then
      begin
        List[i].Task.Cancel;
        List.Delete(i);
      end;
  finally
    TBitmap.FCallbackList.UnlockList;
  end;
end;

initialization
  TBitmap.Pool := TThreadPool.Create;
  TBitmap.FCallbackList := TThreadList<TCallbackObject>.Create;
  TBitmap.FObjectOwner := TObjectOwner.Create(nil);
  TBitmap.FClient := nil;

finalization
  TBitmap.Pool.Free;
  TBitmap.FCallbackList.Free;
  TBitmap.FObjectOwner.Free;
  TBitmap.FClient.Free;

end.


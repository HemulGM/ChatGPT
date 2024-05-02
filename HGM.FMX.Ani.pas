unit HGM.FMX.Ani;

interface

uses
  FMX.Ani, FMX.Types, System.Classes, System.SysUtils, System.Types;

type
  TAniFreeNotification = class(TInterfacedObject, IFreeNotification)
  private
    FProc: TProc;
  public
    procedure FreeNotification(AObject: TObject);
    constructor Create(Proc: TProc);
  end;

  TRectAnimation = class(FMX.Ani.TRectAnimation)
  protected
    procedure FirstFrame; override;
  end;

  { TPositionAnimation }

  TPositionAnimation = class(TCustomPropertyAnimation)
  private
    FStartRect: TPosition;
    FCurrent: TPosition;
    FStopRect: TPosition;
    FStartFromCurrent: Boolean;
    procedure SetStartRect(const Value: TPosition);
    procedure SetStopRect(const Value: TPosition);
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationType default TAnimationType.in;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: TPosition read FStartRect write SetStartRect;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TPosition read FStopRect write SetStopRect;
    property Trigger;
    property TriggerInverse;
  end;

  TAnimatorHelper = class helper for TAnimator
    class procedure DetachPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
    class procedure AnimateFloat(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateRect(const Target: TFmxObject; const APropertyName: string; const NewValue: TRectF; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimatePosition(const Target: TFmxObject; const APropertyName: string; const NewValue: TPointF; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateFloatWithFinish(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Finish: TProc; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
  end;

implementation

uses
  System.Rtti, FMX.Utils, System.TypInfo;

{ TAnimatorHelper }

class procedure TAnimatorHelper.AnimateFloat(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TFloatAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.OnProcess := Update;
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.AnimateFloatWithFinish(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Finish: TProc; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TFloatAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.AddFreeNotify(TAniFreeNotification.Create(Finish));
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.AnimatePosition(const Target: TFmxObject; const APropertyName: string; const NewValue: TPointF; Update: TNotifyEvent; Duration: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TPositionAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TPositionAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue.Point := NewValue;
  Animation.OnProcess := Update;
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.AnimateRect(const Target: TFmxObject; const APropertyName: string; const NewValue: TRectF; Update: TNotifyEvent; Duration: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TRectAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TRectAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue.Rect := NewValue;
  Animation.OnProcess := Update;
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.DetachPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
var
  I: Integer;
begin
  I := Target.ChildrenCount - 1;
  while I >= 0 do
  begin
    if (Target.Children[I] is TCustomPropertyAnimation) and
       (CompareText(TCustomPropertyAnimation(Target.Children[I]).PropertyName, APropertyName) = 0) then
      TFloatAnimation(Target.Children[I]).StopAtCurrent;
    if I > Target.ChildrenCount then
      I := Target.ChildrenCount;
    Dec(I);
  end;
end;

{ TAniFreeNotification }

constructor TAniFreeNotification.Create(Proc: TProc);
begin
  inherited Create;
  FProc := Proc;
end;

procedure TAniFreeNotification.FreeNotification(AObject: TObject);
begin
  if Assigned(FProc) then
    FProc;
  Free;
end;

{ TRectAnimation }

procedure TRectAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkClass) then
        StartValue.Rect := TBounds(P.GetValue(FInstance).AsObject).Rect;
    end;
  end;
end;

{ TPositionAnimation }

procedure TPositionAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TPositionAnimation;
begin
  if Dest is TPositionAnimation then
  begin
    DestAnimation := TPositionAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TPositionAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartRect := TPosition.Create(TPointF.Zero);
  FStopRect := TPosition.Create(TPointF.Zero);
  FCurrent := TPosition.Create(TPointF.Zero);
end;

destructor TPositionAnimation.Destroy;
begin
  FreeAndNil(FCurrent);
  FreeAndNil(FStartRect);
  FreeAndNil(FStopRect);
  inherited;
end;

procedure TPositionAnimation.FirstFrame;
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkClass) then
        StartValue.Point := TPosition(P.GetValue(FInstance).AsObject).Point;
    end;
  end;
end;

procedure TPositionAnimation.ProcessAnimation;
begin
  if FInstance <> nil then
  begin
    { calc value }
    FCurrent.X := InterpolateSingle(FStartRect.X, FStopRect.X,
      NormalizedTime);
    FCurrent.Y := InterpolateSingle(FStartRect.Y, FStopRect.Y,
      NormalizedTime);

    if (FRttiProperty <> nil) and FRttiProperty.PropertyType.IsInstance then
      TBounds(FRttiProperty.GetValue(FInstance).AsObject).Assign(FCurrent);
  end;
end;

procedure TPositionAnimation.SetStartRect(const Value: TPosition);
begin
  FStartRect.Assign(Value);
end;

procedure TPositionAnimation.SetStopRect(const Value: TPosition);
begin
  FStopRect.Assign(Value);
end;

end.


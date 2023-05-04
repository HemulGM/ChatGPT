unit HGM.FMX.Ani;

interface

uses
  FMX.Ani, FMX.Types, System.Classes, System.SysUtils;

type
  TAniFreeNotification = class(TInterfacedObject, IFreeNotification)
  private
    FProc: TProc;
  public
    procedure FreeNotification(AObject: TObject);
    constructor Create(Proc: TProc);
  end;

  TAnimatorHelper = class helper for TAnimator
    class procedure DetachPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
    class procedure AnimateFloat(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateFloatWithFinish(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Finish: TProc; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
  end;

implementation

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

class procedure TAnimatorHelper.DetachPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
var
  I: Integer;
begin
  I := Target.ChildrenCount - 1;
  while I >= 0 do
  begin
    if (Target.Children[I] is TCustomPropertyAnimation) and
      (CompareText(TCustomPropertyAnimation(Target.Children[I]).PropertyName, APropertyName) = 0) then
    begin
      var Anim := TFloatAnimation(Target.Children[I]);
      Anim.Parent := nil;
      Anim.Stop;
    end;
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

end.


unit ChatGPT.Functions.external;

interface

uses
  System.Classes, OpenAI.Chat.Functions, ChatGPT.Functions.external.Intf;

type
  TChatFunctionExternal = class(TChatFunction, IChatFunction)
  private
    FFuncRef: IChatFunctionExternal;
  protected
    function GetDescription: string; override;
    function GetName: string; override;
    function GetParameters: string; override;
  public
    constructor Create(const Func: IChatFunctionExternal); reintroduce;
    destructor Destroy; override;
    function Execute(const Args: string): string; override;
  end;

implementation

{ TChatFunctionExternal }

constructor TChatFunctionExternal.Create(const Func: IChatFunctionExternal);
begin
  inherited Create;
  FFuncRef := Func;
end;

destructor TChatFunctionExternal.Destroy;
begin
  FFuncRef := nil;
  inherited;
end;

function TChatFunctionExternal.Execute(const Args: string): string;
begin
  Result := FFuncRef.Execute(Args);
end;

function TChatFunctionExternal.GetDescription: string;
begin
  Result := FFuncRef.GetDescription;
end;

function TChatFunctionExternal.GetName: string;
begin
  Result := FFuncRef.GetName;
end;

function TChatFunctionExternal.GetParameters: string;
begin
  Result := FFuncRef.GetParameters;
end;

end.


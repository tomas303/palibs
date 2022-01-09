unit tal_uwinfunc;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, trl_iExecutor, Forms;

type

  { TProcessMessagesFunc }

  TProcessMessagesFunc = class(TInterfacedObject, IFluxFunc)
  private
    fID: integer;
    fProcessMessages: IFluxNotifier;
  protected
    procedure Execute(const AAction: IFluxAction);
    function GetID: integer;
  public
    constructor Create(AID: integer; AProcessMessages: IFluxNotifier);
  end;

  { TCloseQueryFunc }

  TCloseQueryFunc = class(TInterfacedObject, IFluxFunc)
  private
    fID: integer;
  protected
    procedure Execute(const AAction: IFluxAction);
    function GetID: integer;
  public
    constructor Create(AID: integer);
  end;

implementation

{ TCloseQueryFunc }

procedure TCloseQueryFunc.Execute(const AAction: IFluxAction);
begin
  raise EExecutorStop.Create('');
end;

function TCloseQueryFunc.GetID: integer;
begin
  Result := fID;
end;

constructor TCloseQueryFunc.Create(AID: integer);
begin
  inherited Create;
  fID := AID;
end;

{ TProcessMessagesFunc }

procedure TProcessMessagesFunc.Execute(const AAction: IFluxAction);
begin
  Application.ProcessMessages;
  fProcessMessages.Notify;
end;

function TProcessMessagesFunc.GetID: integer;
begin
  Result := fID;
end;

constructor TProcessMessagesFunc.Create(AID: integer; AProcessMessages: IFluxNotifier);
begin
  inherited Create;
  fID := AID;
  fProcessMessages := AProcessMessages;
end;

end.

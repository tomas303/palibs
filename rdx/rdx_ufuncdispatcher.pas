unit rdx_ufuncdispatcher;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, fgl, sysutils, trl_iExecutor;

type

  TFuncs = specialize TFPGInterfacedObjectList<IFluxFunc>;
  TIDFuncs = specialize TFPGMapObject<Integer, TFuncs>;

  { TAsyncFuncRun }

  TAsyncFuncRun = class(TInterfacedObject, IExecute)
  private
    fAction: IFluxAction;
    fFunc: IFluxFunc;
  protected
    procedure Execute;
  public
    constructor Create(const AAction: IFluxAction; const AFunc: IFluxFunc);
  end;


  { TRdxFuncDispatcher }

  TRdxFuncDispatcher = class(TInterfacedObject, IFluxDispatcher)
  protected
    fIDFuncs: TIDFuncs;
  protected
    procedure FluxDispatch(const AAction: IFluxAction);
    procedure RegisterFunc(const AFunc: IFluxFunc);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fExecutor: IExecutor;
  published
    property Executor: IExecutor read fExecutor write fExecutor;
  end;

implementation

{ TAsyncFuncRun }

procedure TAsyncFuncRun.Execute;
begin
  fFunc.Execute(fAction);
end;

constructor TAsyncFuncRun.Create(const AAction: IFluxAction; const AFunc: IFluxFunc);
begin
  inherited Create;
  fAction := AAction;
  fFunc := AFunc;
end;

{ TRdxFuncDispatcher }

procedure TRdxFuncDispatcher.FluxDispatch(const AAction: IFluxAction);
var
  mFuncs: TFuncs;
  mFunc: IFluxFunc;
begin
  mFuncs := fIDFuncs[AAction.ID];
  for mFunc in mFuncs do
    Executor.Add(TAsyncFuncRun.Create(AAction, mFunc));
end;

procedure TRdxFuncDispatcher.RegisterFunc(const AFunc: IFluxFunc);
var
  mFuncs: TFuncs;
  mInd: integer;
begin
  mInd := fIDFuncs.IndexOf(AFunc.ID);
  if mInd = -1 then begin
    mFuncs := TFuncs.Create;
    mInd := fIDFuncs.Add(AFunc.ID, mFuncs);
  end;
  mFuncs := fIDFuncs.Data[mInd];
  if mFuncs.IndexOf(AFunc) = -1 then
    mFuncs.Add(AFunc);
end;

procedure TRdxFuncDispatcher.AfterConstruction;
begin
  inherited AfterConstruction;
  fIDFuncs := TIDFuncs.Create(True);
end;

procedure TRdxFuncDispatcher.BeforeDestruction;
begin
  FreeAndNil(fIDFuncs);
  inherited BeforeDestruction;
end;

end.


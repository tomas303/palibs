unit rdx_ufuncdispatcher;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, fgl, sysutils;

type

  { TRdxFuncDispatcher }

  TRdxFuncDispatcher = class(TInterfacedObject, IFluxDispatcher, IFluxFuncReg)
  protected type
    TFuncs = specialize TFPGInterfacedObjectList<IFluxFunc>;
    TIDFuncs = specialize TFPGMapObject<Integer, TFuncs>;
  protected
    fIDFuncs: TIDFuncs;
    procedure SetAddFunc(AValue: IFluxFunc);
  protected
    // IFluxDispatcher
    procedure Dispatch(const AAction: IFluxAction);
  protected
    // IFluxFuncReg
    procedure RegisterFunc(const AFunc: IFluxFunc);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property AddFunc: IFluxFunc write SetAddFunc;
  end;

implementation

{ TRdxFuncDispatcher }

procedure TRdxFuncDispatcher.SetAddFunc(AValue: IFluxFunc);
begin
  RegisterFunc(AValue);
end;

procedure TRdxFuncDispatcher.Dispatch(const AAction: IFluxAction);
var
  mFuncs: TFuncs;
  mFunc: IFluxFunc;
begin
  mFuncs := fIDFuncs[AAction.ID];
  for mFunc in mFuncs do begin
    mFunc.Execute(AAction);
  end;
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


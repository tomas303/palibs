unit rdx_ufuncdispatcher;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, fgl, sysutils;

type

  { TRdxFuncDispatcher }

  TRdxFuncDispatcher = class(TInterfacedObject, IFluxDispatcher)
  protected type
    TFuncs = specialize TFPGInterfacedObjectList<IFluxFunc>;
  protected
    fFuncs: TFuncs;
    procedure SetAddFunc(AValue: IFluxFunc);
  protected
    // IFluxFunc
    procedure Dispatch(const AAction: IFluxAction);
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
  fFuncs.Add(AValue);
end;

procedure TRdxFuncDispatcher.Dispatch(const AAction: IFluxAction);
var
  mFunc: IFluxFunc;
begin
  for mFunc in fFuncs do begin
    mFunc.Execute(AAction);
  end;
end;

procedure TRdxFuncDispatcher.AfterConstruction;
begin
  inherited AfterConstruction;
  fFuncs := TFuncs.Create;
end;

procedure TRdxFuncDispatcher.BeforeDestruction;
begin
  FreeAndNil(fFuncs);
  inherited BeforeDestruction;
end;

end.


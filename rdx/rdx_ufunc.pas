unit rdx_ufunc;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rdx_iredux, trl_iprops, trl_iinjector, trl_idifactory,
  flu_iflux, fgl;

type

  { TRdxFunc }

  TRdxFunc = class(TInterfacedObject, IRdxFunc)
  protected type
    TFuncs = specialize TFPGInterfacedObjectList<IRdxFunc>;
  protected
    fFuncs: TFuncs;
    function DoResize(const AMainForm: IProps; const AAction: IFluxAction): IProps;
    function DefaultMainForm: IProps;
    function FindProps(const AState: IRdxState; const APath: string): IProps;
    procedure SetAddFunc(AValue: IRdxFunc);
  protected
    // IRdxFunc
    function Redux(const AState: IRdxState; const AAction: IFluxAction): IRdxState;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fInjector: IInjector;
    fFactory: IDIFactory;
  published
    property Injector: IInjector read fInjector write fInjector;
    property Factory: IDIFactory read fFactory write fFactory;
    property AddFunc: IRdxFunc write SetAddFunc;
  end;

implementation

{ TRdxFunc }

procedure TRdxFunc.SetAddFunc(AValue: IRdxFunc);
begin
  fFuncs.Add(AValue);
end;

function TRdxFunc.DoResize(const AMainForm: IProps;
  const AAction: IFluxAction): IProps;
begin
  if AAction = nil then begin
    Result := IProps(Factory.Locate(IProps));
    Result.SetInt('Left', 5);
    Result.SetInt('Top', 5);
    Result.SetInt('Width', 600);
    Result.SetInt('Height', 600);
  end
  else
  if not AMainForm.Equals(AAction.Props) then
    Result := AAction.Props.Clone
  else
    Result := AMainForm;
end;

function TRdxFunc.DefaultMainForm: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
  Result
    .SetInt('Left', 500)
    .SetInt('Top', 30)
    .SetInt('Width', 500)
    .SetInt('Height', 300);
end;

function TRdxFunc.FindProps(const AState: IRdxState; const APath: string
  ): IProps;
var
  mProp: IProp;
begin
  mProp := (AState as IPropFinder).Find(APath);
  if mProp = nil then
    raise Exception.CreateFmt('No props for %s', [APath]);
  Result := mProp.AsInterface as IProps;
end;

function TRdxFunc.Redux(const AState: IRdxState; const AAction: IFluxAction
  ): IRdxState;
var
  mFunc: IRdxFunc;
  mState: IRdxState;
begin
  Result := AState;
  for mFunc in fFuncs do begin
    mState := mFunc.Redux(Result, AAction);
    if mState <> nil then
      Result := mState;
  end;
end;

procedure TRdxFunc.AfterConstruction;
begin
  inherited AfterConstruction;
  fFuncs := TFuncs.Create;
end;

procedure TRdxFunc.BeforeDestruction;
begin
  FreeAndNil(fFuncs);
  inherited BeforeDestruction;
end;

end.


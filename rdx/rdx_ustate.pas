unit rdx_ustate;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, flu_iflux, trl_iprops, trl_idifactory;

type

  { TRdxState }

  TRdxState = class(TInterfacedObject, IFluxState, IPropFinder)
  protected
    // IPropFinder
    function Find(const APath: string): IProp;
  protected
    // IFluxState
    function Props(const AID: string): IProps;
  protected
    fFactory: IDIFactory;
    fData: IProps;
  published
    property Factory: IDIFactory read fFactory write fFactory;
    property Data: IProps read fData write fData;
  end;

implementation

{ TRdxState }

function TRdxState.Find(const APath: string): IProp;
var
  mPath: TStringArray;
  i: integer;
  mFinder: IPropFinder;
begin
  Result := nil;
  mPath := APath.Split('.');
  mFinder := Data as IPropFinder;
  for i := 0 to High(mPath) do
  begin
    Result := mFinder.Find(mPath[i]);
    if Result = nil then
      raise Exception.CreateFmt('Property %s identified by key %s not found', [mPath[i], APath]);
    if i = High(mPath) then
      Break;
    if not Supports(Result.AsInterface, IPropFinder, mFinder) then
      raise Exception.CreateFmt('Property %s identified by key %s does not support find', [mPath[i], APath]);
    mFinder := Result.AsInterface as IPropFinder;
  end;
end;

function TRdxState.Props(const AID: string): IProps;
var
  mProp: IProp;
begin
  mProp := Data.PropByName[AID];
  if mProp = nil then
  begin
    Result := IProps(Factory.Locate(IProps));
    Data.SetIntf(AID, Result);
  end
  else
  begin
    Result := mProp.AsInterface as IProps;
  end;
end;

end.


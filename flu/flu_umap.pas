unit flu_umap;

{$mode objfpc}{$H+}

interface

uses
  sysutils, trl_iprops, flu_iflux, fgl;

type

  { TFluxStateToPropsMap }

  TFluxStateToPropsMap = class(TInterfacedObject, IPropsMap)
  protected type
    TPaths = specialize TFPGList<TPropPath>;
  protected
    fPaths: TPaths;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    // IPropsMap
    procedure Map(const AProps: IProps);
  protected
    fRdxState: IFluxState;
    procedure SetAddPath(const APath: string);
  published
    property State: IFluxState read fRdxState write fRdxState;
    property AddPath: string write SetAddPath;
  end;

implementation

{ TFluxStateToPropsMap }

procedure TFluxStateToPropsMap.AfterConstruction;
begin
  inherited AfterConstruction;
  fPaths := TPaths.Create;
end;

procedure TFluxStateToPropsMap.BeforeDestruction;
begin
  FreeAndNil(fPaths);
  inherited BeforeDestruction;
end;

procedure TFluxStateToPropsMap.Map(const AProps: IProps);
var
  mProp: IProp;
  mPath: TPropPath;
  mID: string;
  mFinder: IPropFinder;
begin
  for mPath in fPaths do
  begin
    mFinder := State as IPropFinder;
    for mID in mPath do
    begin
      if mFinder = nil then
        raise Exception.Create('todo -- finder error');
      mProp := mFinder.Find(mID);
      if mProp = nil then
        raise Exception.Create('todo -- path error');
      if mProp.PropType = TPropType.ptInterface then
        mFinder := mProp.AsInterface as IPropFinder
      else
        mFinder := nil;
    end;
    AProps.SetProp(mProp.Name, mProp);
  end;
end;

procedure TFluxStateToPropsMap.SetAddPath(const APath: string);
begin
  fPaths.Add(APath.Split('.'));
end;

end.


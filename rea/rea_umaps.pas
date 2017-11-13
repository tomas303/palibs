unit rea_umaps;

{$mode objfpc}{$H+}

interface

uses
  rea_imaps, fgl, sysutils, classes, trl_iprops, flu_iflux;

type

  { TMapStateToProps }

  TMapStateToProps = class(TInterfacedObject, IMapStateToProps)
  protected type
    TItem = class(TObject)
    protected
      fPath: string;
      fKeys: TStringArray;
      function GetKey(AIndex: integer): string;
      function GetKeyCount: integer;
    public
      constructor Create(const APath: string; AKeys: TStringArray);
      property Path: string read fPath;
      property KeyCount: integer read GetKeyCount;
      property Key[AIndex: integer]: string read GetKey;
    end;
  protected type
    TItems = specialize TFPGObjectList<TItem>;
  protected
    fItems: TItems;
    fKeys: TStringList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    // IMapStateToProps
    function Map(const AProps: IProps): IProps;
  protected
    fRdxState: IFluxState;
    procedure SetAddKey(const AKey: string);
  published
    property State: IFluxState read fRdxState write fRdxState;
    property AddKey: string write SetAddKey;
  end;

implementation

{ TMapStateToProps.TItem }

function TMapStateToProps.TItem.GetKey(AIndex: integer): string;
begin
  Result := fKeys[AIndex];
end;

function TMapStateToProps.TItem.GetKeyCount: integer;
begin
  Result := Length(fKeys);
end;

constructor TMapStateToProps.TItem.Create(const APath: string;
  AKeys: TStringArray);
begin
  inherited Create;
  fPath := APath;
  fKeys := AKeys;
end;

{ TMapStateToProps }

procedure TMapStateToProps.AfterConstruction;
begin
  inherited AfterConstruction;
  fItems := TItems.Create;
  fKeys := TStringList.Create;
end;

procedure TMapStateToProps.BeforeDestruction;
begin
  FreeAndNil(fItems);
  FreeAndNil(fKeys);
  inherited BeforeDestruction;
end;

function TMapStateToProps.Map(const AProps: IProps): IProps;
var
  mProp, mKeyProp: IProp;
  mItem: TItem;
  mKey: string;
  i: integer;
begin
  Result := AProps.Clone;
  for mKey in fKeys do
  begin
   mProp := (State as IPropFinder).Find(mKey);
   if mProp <> nil then
     Result.SetProp(mProp.Name, mProp);
  end;
end;

procedure TMapStateToProps.SetAddKey(const AKey: string);
begin
  if fKeys.IndexOf(AKey) <> -1 then
    raise Exception.CreateFmt('Key %s for map state already added', [AKey]);
  fKeys.Add(AKey);
end;

end.


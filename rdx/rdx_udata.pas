unit rdx_udata;

{$mode objfpc}{$H+}

interface

uses
  flu_iflux, trl_iprops, trl_igenericaccess, trl_idifactory, fgl, sysutils;

type

  { TRdxData }

  TRdxData = class(TInterfacedObject, IFluxData)
  private type

    TData = specialize TFPGMapInterfacedObjectData<string, IGenericAccess>;

  private
    fData: TData;
    function DataExists(const AKey: string): Boolean;
  protected
    // IFluxData
    function New(const AKey: string): IGenericAccess;
    function GetData(const AKey: string): IGenericAccess;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fFactory: IDIFactory;
  published
    property Factory: IDIFactory read fFactory write fFactory;
  end;

implementation

{ TRdxData }

function TRdxData.DataExists(const AKey: string): Boolean;
var
  mIndex: integer;
begin
  Result := fData.Find(AKey, mIndex);
end;

function TRdxData.New(const AKey: string): IGenericAccess;
begin
  if DataExists(AKey) then
    raise Exception.CreateFmt('Data with key %s already exists', [AKey]);
  Result := IProps(Factory.Locate(IProps)) as IGenericAccess;
  fData.Add(AKey, Result);
end;

function TRdxData.GetData(const AKey: string): IGenericAccess;
begin
  if not DataExists(AKey) then
    raise Exception.CreateFmt('Data with key %s don''t exists', [AKey]);
  Result := fData.KeyData[AKey];
end;

procedure TRdxData.BeforeDestruction;
begin
  FreeAndNil(fData);
  inherited BeforeDestruction;
end;

procedure TRdxData.AfterConstruction;
begin
  inherited AfterConstruction;
  fData := TData.Create;
end;

end.


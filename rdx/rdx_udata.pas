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
    function New(const AKey: string): IGenericAccess;
  protected
    // IFluxData
    function Exists(const AKey: string): Boolean;
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

function TRdxData.New(const AKey: string): IGenericAccess;
begin
  Result := IFluxState(Factory.Locate(IFluxState)) as IGenericAccess;
  fData.Add(AKey, Result);
end;

function TRdxData.Exists(const AKey: string): Boolean;
var
  mIndex: integer;
begin
  Result := fData.Find(AKey, mIndex);
end;

function TRdxData.GetData(const AKey: string): IGenericAccess;
var
  mIndex: integer;
begin
  if fData.Find(AKey, mIndex) then
    Result := fData.Data[mIndex]
  else
    Result := New(AKey);
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
  fData.Sorted := True;
end;

end.


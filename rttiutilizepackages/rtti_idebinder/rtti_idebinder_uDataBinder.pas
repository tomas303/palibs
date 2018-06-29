unit rtti_idebinder_uDataBinder;

interface

uses
  Classes, SysUtils, Controls,
  rtti_broker_iBroker, rtti_idebinder_uDataSlots, rtti_idebinder_iBindings;

type

  { TRBDataBinder }

  TRBDataBinder = class(TInterfacedObject, IRBDataBinder)
  private
    fDataSlots: TDataSlots;
  protected
    // IRBDataBinder
    procedure Bind(AContainer: TWinControl; const AData: IRBData; const ADataQuery: IRBDataQuery);
    procedure DataChange;
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
    property AData: IRBData read GetData write SetData;
  public
    destructor Destroy; override;
  end;

implementation

{ TRBDataBinder }

destructor TRBDataBinder.Destroy;
begin
  FreeAndNil(fDataSlots);
  inherited Destroy;
end;

procedure TRBDataBinder.Bind(AContainer: TWinControl; const AData: IRBData;
  const ADataQuery: IRBDataQuery);
begin
  FreeAndNil(fDataSlots);
  fDataSlots := TDataSlots.Create;
  fDataSlots.Bind(AContainer, AData, ADataQuery);
end;

procedure TRBDataBinder.DataChange;
begin
  fDataSlots.DataChange;
end;

function TRBDataBinder.GetData: IRBData;
begin
  Result := fDataSlots.Data;
end;

procedure TRBDataBinder.SetData(AValue: IRBData);
begin
  fDataSlots.Data := AValue;
end;

end.


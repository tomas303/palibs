unit tvl_udatabinder;

interface

uses
  Classes, SysUtils, trl_irttibroker, Controls, StdCtrls, ExtCtrls, fgl,
  Graphics, tvl_udatabinders, Grids, SynEdit, trl_ipersist, tvl_ibindings,
  trl_ipersiststore;

type

  { TDataSlotsItem }

  TDataSlotsItem = class
    fBinder: TEditBinder;
    fDataItem: IRBDataItem;
  private
    function NewBinder(AContainer: TWinControl): TEditBinder;
    function FindControl(AContainer: TWinControl): TWinControl;
  public
    destructor Destroy; override;
    procedure Bind(const AContainer: TWinControl; const ADataItem: IRBDataItem);
    procedure DataChange;
  end;

  { TDataSlots }

  TDataSlots = class
  private type
   TDataEditItems = specialize TFPGObjectList<TDataSlotsItem>;
  private
    fItems: TDataEditItems;
    fData: IRBData;
    fContainer: TWinControl;
    procedure ActualizeItems;
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
  private
    function GetItems(AIndex: Integer{; APropIndex: integer}): TDataSlotsItem;
    procedure SetItems(AIndex: Integer{; APropIndex: integer}; AValue: TDataSlotsItem);
    function GetItemsCount({AIndex: Integer}): integer;
    procedure SetItemsCount({AIndex: Integer;} AValue: integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Bind(const AContainer: TWinControl; const AData: IRBData);
    procedure DataChange;
    property Data: IRBData read GetData write SetData;
  public  //for now not persist
    property Items[AIndex: integer]: TDataSlotsItem {index crbList + crbObject} read GetItems write SetItems; default;
    property ItemsCount: integer {index crbListCounter} read GetItemsCount write SetItemsCount;
  end;

  { TRBDataBinder }

  TRBDataBinder = class(TInterfacedObject, IRBDataBinder)
  private
    fDataSlots: TDataSlots;
  protected
    // IRBDataBinder
    procedure Bind(AContainer: TWinControl; const AData: IRBData);
    procedure Unbind;
    procedure DataChange;
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
    property AData: IRBData read GetData write SetData;
  public
    destructor Destroy; override;
  end;

implementation

{ TDataSlotsItem }

function TDataSlotsItem.NewBinder(AContainer: TWinControl): TEditBinder;
var
  mControl: TWinControl;
begin
  Result := nil;
  mControl := FindControl(AContainer);
  if Assigned(mControl) then
  begin
    if mControl is TCustomEdit then
      Result := TTextBinder.Create
    else
    if mControl is TCustomComboBox then
    begin
      if (fDataItem.IsInterface) and Supports(fDataItem.AsInterface, IPersistRef) then
      begin
        Result := TOfferRefBinder.Create;
      end
      else
      if fDataItem.EnumNameCount > 0 then
      begin
        Result := TOfferEnumBinder.Create;
      end;
    end
    else
    if mControl is TCustomStringGrid then
      Result := TListBinder.Create
    else
    if mControl is TCustomCheckBox then
      Result := TBoolBinder.Create
    else
    if mControl is TCustomSynEdit then
      Result := TMemoBinder.Create;
  end;
  if Assigned(Result) then
    Result.Bind(mControl, fDataItem);
end;

function TDataSlotsItem.FindControl(AContainer: TWinControl): TWinControl;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to AContainer.ControlCount - 1 do
  begin
    if not (AContainer.Controls[i] is TWinControl) then
      Continue;
    Result := FindControl(AContainer.Controls[i] as TWinControl);
    if Assigned(Result) then
      Exit;
    if SameText(fDataItem.Name + '_bind', AContainer.Controls[i].Name) then
    begin
      Result := AContainer.Controls[i] as TWinControl;
      Exit;
    end;
  end;
end;

destructor TDataSlotsItem.Destroy;
begin
  FreeAndNil(fBinder);
  inherited Destroy;
end;

procedure TDataSlotsItem.DataChange;
begin
  if Assigned(fBinder) then
    fBinder.DataToControl;
end;

procedure TDataSlotsItem.Bind(const AContainer: TWinControl;
  const ADataItem: IRBDataItem);
begin
  FreeAndNil(fBinder);
  fDataItem := ADataItem;
  fBinder := NewBinder(AContainer);
  DataChange;
end;

{ TDataSlots }

function TDataSlots.GetItems(AIndex: Integer{; APropIndex: integer}): TDataSlotsItem;
begin
  Result := fItems[AIndex];
end;

procedure TDataSlots.SetData(AValue: IRBData);
begin
  fData := AValue;
  DataChange;
end;

function TDataSlots.GetItemsCount({AIndex: Integer}): integer;
begin
  Result := fItems.Count;
end;

procedure TDataSlots.ActualizeItems;
var
  i: integer;
begin
  fItems.Clear;
  ItemsCount := fData.Count;
  for i := 0 to fData.Count - 1 do
  begin
    Items[i] := TDataSlotsItem.Create;
    Items[i].Bind(fContainer, fData[i]);
  end;
end;

function TDataSlots.GetData: IRBData;
begin
  Result := fData;
end;

procedure TDataSlots.SetItems(AIndex: Integer{; APropIndex: integer}; AValue: TDataSlotsItem);
begin
  fItems[AIndex] := AValue;
end;

procedure TDataSlots.SetItemsCount({AIndex: Integer;} AValue: integer);
begin
  fItems.Count := AValue;
end;

procedure TDataSlots.AfterConstruction;
begin
  inherited AfterConstruction;
  fItems := TDataEditItems.Create;
end;

procedure TDataSlots.BeforeDestruction;
begin
  FreeAndNil(fItems);
  inherited BeforeDestruction;
end;

procedure TDataSlots.Bind(const AContainer: TWinControl; const AData: IRBData);
begin
  fContainer := AContainer;
  fData := AData;
  ActualizeItems;
end;

procedure TDataSlots.DataChange;
var
  i: Integer;
begin
  for i := 0 to ItemsCount - 1 do
    Items[i].DataChange;
end;

{ TRBDataBinder }

destructor TRBDataBinder.Destroy;
begin
  FreeAndNil(fDataSlots);
  inherited Destroy;
end;

procedure TRBDataBinder.Bind(AContainer: TWinControl; const AData: IRBData);
begin
  FreeAndNil(fDataSlots);
  fDataSlots := TDataSlots.Create;
  fDataSlots.Bind(AContainer, AData);
end;

procedure TRBDataBinder.Unbind;
begin
  FreeAndNil(fDataSlots);
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


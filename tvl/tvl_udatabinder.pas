unit tvl_udatabinder;

interface

uses
  Classes, SysUtils, trl_irttibroker, Controls, StdCtrls, ExtCtrls, fgl,
  Graphics, tvl_udatabinders, Grids, SynEdit, trl_ipersist, tvl_ibindings,
  EditBtn, trl_urttibroker, ComCtrls;

type
  { TCustomDataSlotsItem }

  TCustomDataSlotsItem = class
  private
    fDataItem: IRBDataItem;
  protected
    procedure DoBind(const AControl: TWinControl); virtual; abstract;
    procedure DoDataChange; virtual; abstract;
    procedure DoFlush(AControl: TControl = nil); virtual; abstract;
  public
    constructor Create(const ADataItem: IRBDataItem); virtual;
    procedure Bind(const AControl: TWinControl);
    procedure DataChange;
    procedure Flush(AControl: TControl = nil);
    procedure RegisterChangeEvent(AEvent: TBinderChangeEvent); virtual; abstract;
    procedure UnregisterChangeEvent(AEvent: TBinderChangeEvent); virtual; abstract;
    property DataItem: IRBDataItem read fDataItem;
  end;

  { TDataSlotsItem }

  TDataSlotsItem = class(TCustomDataSlotsItem)
  protected type
    TBinderItems = specialize TFPGObjectList<TEditBinder>;
  private
    fBinders: TBinderItems;
    fRecallDataChangeEvents: TBinderChangeEvents;
    function CreateBinder(const AControl: TWinControl): TEditBinder;
    function FindBinder(const AControl: TWinControl): TEditBinder;
  protected
    procedure PushDataChange(const ADataItem: IRBDataItem; AControl: TWinControl);
    procedure RecallDataChange(const ADataItem: IRBDataItem; AControl: TWinControl);
  protected
    procedure DoBind(const AControl: TWinControl); override;
    procedure DoDataChange; override;
    procedure DoFlush(AControl: TControl = nil); override;
  public
    constructor Create(const ADataItem: IRBDataItem); override;
    destructor Destroy; override;
    procedure RegisterChangeEvent(AEvent: TBinderChangeEvent); override;
    procedure UnregisterChangeEvent(AEvent: TBinderChangeEvent); override;
  end;

  { TObjectSlotsItem }

  TObjectSlotsItem = class(TCustomDataSlotsItem)
  protected type
    TBinderItems = specialize TFPGMapInterfacedObjectData<Pointer, IRBDataBinder>;
  private
    fBinders: TBinderItems;
  protected
    procedure DoBind(const AControl: TWinControl); override;
    procedure DoDataChange; override;
    procedure DoFlush(AControl: TControl = nil); override;
  public
    constructor Create(const ADataItem: IRBDataItem); override;
    destructor Destroy; override;
    procedure RegisterChangeEvent(AEvent: TBinderChangeEvent); override;
    procedure UnregisterChangeEvent(AEvent: TBinderChangeEvent); override;
  end;

  { TDataSlots }

  TDataSlots = class
  private type
   TDataEditItems = specialize TFPGMapObject<string, TCustomDataSlotsItem>;
  private
    fItems: TDataEditItems;
    fData: IRBData;
    fContainer: TWinControl;
    function FindControl(const AName: string; AContainer: TWinControl): TWinControl;
    procedure ActualizeItems;
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
  private
    function GetItems(AIndex: Integer): TCustomDataSlotsItem;
    procedure SetItems(AIndex: Integer; AValue: TCustomDataSlotsItem);
    function GetItemsCount: integer;
    procedure SetItemsCount(AValue: integer);
    function GetItemByName(const AName: string): TCustomDataSlotsItem;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure BindArea(const AContainer: TWinControl; const AData: IRBData);
    procedure BindControl(AControl: TWinControl; const AName: string);
    procedure BindControl(AControl: TWinControl; const AItem: IRBDataItem);
    procedure DataChange;
    procedure Flush(AControl: TControl = nil);
    property Data: IRBData read GetData write SetData;
  public
    property Items[AIndex: integer]: TCustomDataSlotsItem read GetItems write SetItems; default;
    property ItemsCount: integer read GetItemsCount write SetItemsCount;
    property ItemByName[const AName: string]: TCustomDataSlotsItem read GetItemByName;
  end;

  { TRBDataBinder }

  TRBDataBinder = class(TInterfacedObject, IRBDataBinder)
  protected
    fDataSlots: TDataSlots;
    function GetDataSlots: TDataSlots;
    property DataSlots: TDataSlots read GetDataSlots;
  protected
    // IRBDataBinder
    procedure BindArea(AContainer: TWinControl; const AData: IRBData);
    procedure BindControl(AControl: TWinControl; const AName: string);
    procedure Unbind;
    procedure DataChange;
    procedure Flush(AControl: TControl = nil);
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
    property AData: IRBData read GetData write SetData;
    procedure RegisterChangeEvent(const AItemName: string; AEvent: TBinderChangeEvent);
    procedure UnregisterChangeEvent(const AItemName: string; AEvent: TBinderChangeEvent);
  public
    destructor Destroy; override;
  end;

  EDataBinder = class(Exception);

implementation

{ TObjectSlotsItem }

procedure TObjectSlotsItem.DoBind(const AControl: TWinControl);
var
  mInd: integer;
  mBinder: IRBDataBinder;
  mData: IRBData;
begin
  mInd := fBinders.IndexOf(AControl);
  if mInd = -1 then begin
    mBinder := TRBDataBinder.Create;
    mInd := fBinders.Add(AControl, mBinder);
  end;
  fBinders.Data[mInd].Unbind;
  mData := TRBData.Create(fDataItem.AsObject);
  fBinders.Data[mInd].BindArea(AControl, mData);
  DataChange;
end;

procedure TObjectSlotsItem.DoDataChange;
var
  i: integer;
begin
  for i := 0 to fBinders.Count - 1 do
    fBinders.Data[i].DataChange;
end;

procedure TObjectSlotsItem.DoFlush(AControl: TControl);
var
  i: integer;
begin
  for i := 0 to fBinders.Count - 1 do
    fBinders.Data[i].Flush(AControl);
end;

constructor TObjectSlotsItem.Create(const ADataItem: IRBDataItem);
begin
  inherited;
  fBinders := TBinderItems.Create;
end;

destructor TObjectSlotsItem.Destroy;
begin
  FreeAndNil(fBinders);
  inherited Destroy;
end;

procedure TObjectSlotsItem.RegisterChangeEvent(AEvent: TBinderChangeEvent);
var
  i, j: integer;
begin
  for i := 0 to fBinders.Count - 1 do
    for j := 0 to fBinders.Data[i].Data.Count - 1 do
      fBinders.Data[i].RegisterChangeEvent(fBinders.Data[i].Data[j].Name, AEvent);
end;

procedure TObjectSlotsItem.UnregisterChangeEvent(AEvent: TBinderChangeEvent);
var
  i, j: integer;
begin
  for i := 0 to fBinders.Count - 1 do
    for j := 0 to fBinders.Data[i].Data.Count - 1 do
      fBinders.Data[i].UnregisterChangeEvent(fBinders.Data[i].Data[j].Name, AEvent);
end;

{ TDataSlotsItem }

function TDataSlotsItem.CreateBinder(const AControl: TWinControl): TEditBinder;
begin
  if AControl is TCustomEdit then
    Result := TTextBinder.Create
  else
  if AControl is TCustomEditButton then
    Result := TTextBtnBinder.Create
  else
  if AControl is TCustomComboBox then
  begin
    if (fDataItem.IsInterface) and Supports(fDataItem.AsInterface, IPersistRef)
      then
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
  if AControl is TCustomStringGrid then
    Result := TListBinder.Create
  else
  if AControl is TCustomCheckBox then
    Result := TBoolBinder.Create
  else
  if AControl is TCustomSynEdit then
    Result := TMemoBinder.Create
  else
  if AControl is TCustomPage then
     Result := TTabSheetBinder.Create
end;

function TDataSlotsItem.FindBinder(const AControl: TWinControl): TEditBinder;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to fBinders.Count - 1 do
    if fBinders[i].Control = AControl then begin
      Result := fBinders[i];
      Break;
    end;
end;

procedure TDataSlotsItem.PushDataChange(const ADataItem: IRBDataItem; AControl: TWinControl);
var
 mBinder: TEditBinder;
begin
  // when one control change, this will tell others control on this same DataItem
  // to refresh
  for mBinder in fBinders do
    if mBinder.Control <> AControl then
      mBinder.DataToControl;
end;

procedure TDataSlotsItem.RecallDataChange(const ADataItem: IRBDataItem;
  AControl: TWinControl);
var
  mEvent: TBinderChangeEvent;
begin
  for mEvent in fRecallDataChangeEvents do
    mEvent(ADataItem, AControl);
end;

procedure TDataSlotsItem.DoBind(const AControl: TWinControl);
var
  mBinder: TEditBinder;
begin
  mBinder := FindBinder(AControl);
  if mBinder = nil then begin
    mBinder := CreateBinder(AControl);
    mBinder.RegisterChangeEvent(@PushDataChange);
    mBinder.RegisterChangeEvent(@RecallDataChange);
    fBinders.Add(mBinder);
  end;
  mBinder.Unbind;
  mBinder.Bind(AControl, fDataItem);
  DataChange;
end;

procedure TDataSlotsItem.DoDataChange;
var
  mBinder: TEditBinder;
begin
  for mBinder in fBinders do
    mBinder.DataToControl;
end;

procedure TDataSlotsItem.DoFlush(AControl: TControl);
var
  mBinder: TEditBinder;
begin
  for mBinder in fBinders do
    if (AControl = nil) or (AControl = mBinder.Control) then
       mBinder.ControlToData;
end;

constructor TDataSlotsItem.Create(const ADataItem: IRBDataItem);
begin
  inherited;
  fBinders := TBinderItems.Create;
  fRecallDataChangeEvents := TBinderChangeEvents.Create;
end;

destructor TDataSlotsItem.Destroy;
begin
  FreeAndNil(fRecallDataChangeEvents);
  FreeAndNil(fBinders);
  inherited Destroy;
end;

procedure TDataSlotsItem.RegisterChangeEvent(AEvent: TBinderChangeEvent);
var
  mIndex: integer;
begin
  mIndex := fRecallDataChangeEvents.IndexOf(AEvent);
  if mIndex = -1 then
    fRecallDataChangeEvents.Add(AEvent);
end;

procedure TDataSlotsItem.UnregisterChangeEvent(AEvent: TBinderChangeEvent);
var
  mIndex: integer;
begin
  mIndex := fRecallDataChangeEvents.IndexOf(AEvent);
  if mIndex <> -1 then
    fRecallDataChangeEvents.Delete(mIndex);
end;

{ TCustomDataSlotsItem }

constructor TCustomDataSlotsItem.Create(const ADataItem: IRBDataItem);
begin
  fDataItem := ADataItem;
end;

procedure TCustomDataSlotsItem.DataChange;
begin
  DoDataChange;
end;

procedure TCustomDataSlotsItem.Flush(AControl: TControl = nil);
begin
  DoFlush(AControl);
end;

procedure TCustomDataSlotsItem.Bind(const AControl: TWinControl);
begin
  DoBind(AControl);
end;

{ TDataSlots }

function TDataSlots.GetItems(AIndex: Integer): TCustomDataSlotsItem;
begin
  Result := fItems.Data[AIndex];
end;

procedure TDataSlots.SetData(AValue: IRBData);
begin
  fData := AValue;
  ActualizeItems;
end;

function TDataSlots.GetItemByName(const AName: string): TCustomDataSlotsItem;
var
  mInd: Integer;
begin
  mInd := fItems.IndexOf(AName);
  if mInd = -1 then
    raise EDataBinder.CreateFmt('GetItemByName - data slot item %s not exists', [AName]);
  Result := fItems.Data[mInd];
  if Result = nil then
    raise EDataBinder.CreateFmt('GetItemByName - data slot item %s point to nil', [AName]);
end;

function TDataSlots.GetItemsCount: integer;
begin
  Result := fItems.Count;
end;

function TDataSlots.FindControl(const AName: string; AContainer: TWinControl): TWinControl;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to AContainer.ControlCount - 1 do
  begin
    if not (AContainer.Controls[i] is TWinControl) then
      Continue;
    Result := FindControl(AName, AContainer.Controls[i] as TWinControl);
    if Assigned(Result) then
      Exit;
    if SameText(AName + '_bind', AContainer.Controls[i].Name) then
    begin
      Result := AContainer.Controls[i] as TWinControl;
      Exit;
    end;
  end;
end;

procedure TDataSlots.ActualizeItems;
var
  i: integer;
  mControl: TWinControl;
begin
  fItems.Clear;
  for i := 0 to fData.Count - 1 do
  begin
    // for now create only items with control
    mControl := FindControl(fData[i].Name, fContainer);
    if mControl = nil then
      Continue;
    BindControl(mControl, fData[i]);
  end;
end;

procedure TDataSlots.BindControl(AControl: TWinControl; const AName: string);
var
  mItem: IRBDataItem;
begin
  if fData = nil then
    raise EDataBinder.Create('Data not set');
  mItem := fData.FindItem(AName);
  if mItem = nil then
    raise EDataBinder.CreateFmt('Data item %s not found', [AName]);
  BindControl(AControl, mItem);
end;

procedure TDataSlots.BindControl(AControl: TWinControl; const AItem: IRBDataItem);
var
  mInd: integer;
begin
  if AItem = nil then
    raise EDataBinder.Create('Cannot add empty item');
  mInd := fItems.IndexOf(AItem.Name);
  if mInd = -1 then begin
    if AItem.IsObject then
      mInd := fItems.Add(AItem.Name, TObjectSlotsItem.Create(AItem))
    else
      mInd := fItems.Add(AItem.Name, TDataSlotsItem.Create(AItem));
  end;
  Items[mInd].Bind(AControl);
end;

function TDataSlots.GetData: IRBData;
begin
  Result := fData;
end;

procedure TDataSlots.SetItems(AIndex: Integer; AValue: TCustomDataSlotsItem);
begin
  fItems.Data[AIndex] := AValue;
end;

procedure TDataSlots.SetItemsCount(AValue: integer);
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

procedure TDataSlots.BindArea(const AContainer: TWinControl; const AData: IRBData);
begin
  fContainer := AContainer;
  Data := AData;
end;

procedure TDataSlots.DataChange;
var
  i: Integer;
begin
  for i := 0 to ItemsCount - 1 do
    Items[i].DataChange;
end;

procedure TDataSlots.Flush(AControl: TControl = nil);
var
  i: Integer;
begin
  for i := 0 to ItemsCount - 1 do
    Items[i].Flush(AControl);
end;

{ TRBDataBinder }

destructor TRBDataBinder.Destroy;
begin
  FreeAndNil(fDataSlots);
  inherited Destroy;
end;

function TRBDataBinder.GetDataSlots: TDataSlots;
begin
  if fDataSlots = nil then
    fDataSlots := TDataSlots.Create;
  Result := fDataSlots;
end;

procedure TRBDataBinder.BindArea(AContainer: TWinControl; const AData: IRBData);
begin
  DataSlots.BindArea(AContainer, AData);
end;

procedure TRBDataBinder.BindControl(AControl: TWinControl; const AName: string);
begin
  DataSlots.BindControl(AControl, AName);
end;

procedure TRBDataBinder.Unbind;
begin
  FreeAndNil(fDataSlots);
end;

procedure TRBDataBinder.DataChange;
begin
  fDataSlots.DataChange;
end;

procedure TRBDataBinder.Flush(AControl: TControl = nil);
begin
  fDataSlots.Flush(AControl);
end;

function TRBDataBinder.GetData: IRBData;
begin
  Result := fDataSlots.Data;
end;

procedure TRBDataBinder.SetData(AValue: IRBData);
begin
  fDataSlots.Data := AValue;
end;

procedure TRBDataBinder.RegisterChangeEvent(const AItemName: string;
  AEvent: TBinderChangeEvent);
var
  mSlotItem: TCustomDataSlotsItem;
begin
  mSlotItem := fDataSlots.ItemByName[AItemName];
  mSlotItem.RegisterChangeEvent(AEvent)
end;

procedure TRBDataBinder.UnregisterChangeEvent(const AItemName: string;
  AEvent: TBinderChangeEvent);
var
  mSlotItem: TCustomDataSlotsItem;
begin
  mSlotItem := fDataSlots.ItemByName[AItemName];
  mSlotItem.UnregisterChangeEvent(AEvent)
end;

end.


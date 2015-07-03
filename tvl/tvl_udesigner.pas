unit tvl_udesigner;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Menus, StdCtrls,
  tvl_ibindings, trl_irttibroker,
  tvl_ubehavebinders, fgl, ExtCtrls, forms,
  ActnList, ValEdit, variants, TypInfo, grids;

type

  { CFxDesignerLib }

  CFxDesignerLib = class
  public
    class function AddMenuItem(AMenu: TMenu; const ACaption: string;
      AHandler: TNotifyEvent; ATag: PtrInt = 0): TMenuItem;
  end;

  { TDesignControlMgr }

  TDesignControlMgr = class(TObject)
  private type
     TBinders = TFPGObjectList<TBehaveBinder>;
  private
    fContainer: TWinControl;
    fCtlClasses: IRBPersistClassRegister;
    fBinders: TBinders;
  private
    function IsContainer(AControl: TControl): Boolean;
    function FindParent(AScreenPos: TPoint): TWinControl;
    procedure ResolveParent(AControl: TControl);
    function CreateControl(AClass: TControlClass): TControl;
    procedure AddBinders(AControl: TControl);
  private
    procedure AddControlEvent(Sender: TObject);
    procedure DropControlEvent(Sender: TObject);
    procedure PropertiesEvent(Sender: TObject);
    procedure BuildControlMenuEvent(AMenu: TMenu; AControl: TControl);
  private
    procedure BindControls(const AParent: TControl);
    procedure BindContainer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Bind(AContainer: TWinControl; ACtlClasses: IRBPersistClassRegister);
    procedure Unbind;
    procedure AddCreateControls(AMenu: TMenu);
  end;

  { TDesignStoreMgr }

  TDesignStoreMgr = class(TObject)
  private type

    { TStoreValue }

    TStoreValue = class(TRBCustomObject)
    private
      fID: string;
      fValue: string;
    published
      property ID: string read fID write fID;
      property Value: string read fValue write fValue;
    end;

    { TStoreCmp }

    TStoreCmp = class(TRBCustomObject)
    private type
      TChilds = TFPGObjectList<TStoreCmp>;
      TValues = TFPGObjectList<TStoreValue>;
    private
      fParent: TStoreCmp;
      fChilds: TChilds;
      fValues: TValues;
      fSClassName: string;
      fSName: string;
      function GetChilds(AIndex: Integer; APropIndex: integer): TStoreCmp;
      function GetChildsCount(AIndex: Integer): integer;
      function GetParent(AIndex: Integer): TStoreCmp;
      function GetValues(AIndex: Integer; APropIndex: integer): TStoreValue;
      function GetValuesCount(AIndex: Integer): integer;
      procedure SetChilds(AIndex: Integer; APropIndex: integer; AValue: TStoreCmp);
      procedure SetChildsCount(AIndex: Integer; AValue: integer);
      procedure SetParent(AIndex: Integer; AValue: TStoreCmp);
      procedure SetValues(AIndex: Integer; APropIndex: integer; AValue: TStoreValue
        );
      procedure SetValuesCount(AIndex: Integer; AValue: integer);
    public
      procedure AfterConstruction; override;
      procedure BeforeDestruction; override;
    published
      property SClassName: string read fSClassName write fSClassName;
      property SName: string read fSName write fSName;
      property Parent: TStoreCmp index crbObject + crbNotStored read GetParent write SetParent;
      property Childs[AIndex: integer]: TStoreCmp index crbList + crbObject read GetChilds write SetChilds; default;
      property ChildsCount: integer index crbListCounter read GetChildsCount write SetChildsCount;
      property Values[AIndex: integer]: TStoreValue index crbList + crbObject read GetValues write SetValues;
      property ValuesCount: integer index crbListCounter read GetValuesCount write SetValuesCount;
    end;

    { TStoreHeader }

    TStoreHeader = class(TRBCustomObject)
    private
      fRoot: TStoreCmp;
      fID: string;
      function GetRoot(AIndex: Integer): TStoreCmp;
      procedure SetRoot(AIndex: Integer; AValue: TStoreCmp);
    public
      destructor Destroy; override;
    published
      property Root: TStoreCmp index crbObject read GetRoot write SetRoot;
      property ID: string read fID write fID;
    end;

  private
    fContainer: TWinControl;
    fStore: IRBStore;
    fFactory: IRBFactory;
    fDataQuery: IRBDataQuery;
    fRegistered: Boolean;
    function FindClass(const AClassName: string): TPersistentClass;
    procedure RegisterClasses;
    function CreateComponent(const AClassName: string): TComponent;
    function FindComponent(const AOwner: TComponent; const AName: string): TComponent;
    function FindControl(const AParent: TWinControl; const AName: string): TControl;
    function FindStoreContainter(const AID: string): TStoreHeader;
    function GetContainerID: string;

    procedure DoSave(AStoreCmp: TStoreCmp; ACmp: TComponent);

    procedure DoSaveMenuItem(AStoreCmp: TStoreCmp; ACmp: TMenuItem);
    procedure DoSaveMenu(AStoreCmp: TStoreCmp; ACmp: TMenu);

    procedure DoSaveAction(AStoreCmp: TStoreCmp; ACmp: TContainedAction);
    procedure DoSaveActionList(AStoreCmp: TStoreCmp; ACmp: TCustomActionList);
    procedure DoSaveControl(AStoreCmp: TStoreCmp; ACmp: TControl);

    procedure DoSaveValues(AStoreCmp: TStoreCmp; ACmp: TComponent);
    procedure DoSaveValue(const AName: string; AControlData: IRBData; AValue: TStoreValue);
    procedure DoLoad(AStoreCmp: TStoreCmp; ACmp: TComponent);
    procedure DoLoadValues(AStoreCmp: TStoreCmp; ACmp: TComponent);
    procedure DoLoadValue(AControlData: IRBData; AValue: TStoreValue);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Bind(AContainer: TWinControl; const AStore: IRBStore;
      const AFactory: IRBFactory; const ADataQuery: IRBDataQuery);
    procedure Unbind;
    procedure Save;
    procedure Load;
  end;

  { TRBDesigner }

  TRBDesigner = class(TInterfacedObject, IRBDesigner)
  private
    fControlMgr: TDesignControlMgr;
    fStoreMgr: TDesignStoreMgr;
    //
    fContainer: TWinControl;
    fStore: IRBStore;
    fFactory: IRBFactory;
    fDataQuery: IRBDataQuery;
    //
    fCtlClasses: IRBPersistClassRegister;
    fMenuBinder: TPopupMenuBinder;
    fDesignOn: Boolean;
    procedure BindMenu;
    procedure UnbindMenu;
    procedure SwitchDesignOn;
    procedure SwitchDesignOff;
  private
    procedure SwitchModeEvent(Sender: TObject);
    procedure SaveLayoutEvent(Sender: TObject);
    procedure LoadLayoutEvent(Sender: TObject);
    procedure BuildMenuEvent(AMenu: TMenu; AControl: TControl);
  protected
    procedure Bind(AContainer: TWinControl; const AStore: IRBStore;
      const AFactory: IRBFactory; const ADataQuery: IRBDataQuery;
      const ACtlClasses: IRBPersistClassRegister);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

type
PFieldClassTable = ^TFieldClassTable;
TFieldClassTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
record
  Count: Word;
  Entries: array[{$ifdef cpu16}0..16384 div sizeof(TPersistentClass){$else}Word{$endif}] of TPersistentClass;
end;

PFieldTable = ^TFieldTable;
TFieldTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
record
  FieldCount: Word;
  ClassTable: PFieldClassTable;
end;

{ TDesignStoreMgr }

function TDesignStoreMgr.FindClass(const AClassName: string): TPersistentClass;
var
  ShortClassName: shortstring;
  ClassType: TClass;
  ClassTable: PFieldClassTable;
  i: Integer;
  FieldTable: PFieldTable;
begin
  // At first, try to locate the class in the class tables
  ShortClassName := AClassName;
  ClassType := fContainer.ClassType;
  while ClassType <> TPersistent do
  begin
    FieldTable := PFieldTable(PVmt(ClassType)^.vFieldTable);
    if Assigned(FieldTable) then
    begin
      ClassTable := FieldTable^.ClassTable;
      for i := 0 to ClassTable^.Count - 1 do
      begin
        Result := ClassTable^.Entries[i];
        writeln(result.ClassName);
        if Result.ClassNameIs(ShortClassName) then
          exit;
      end;
    end;
    // Try again with the parent class type
    ClassType := ClassType.ClassParent;
  end;
  Result := Classes.GetClass(ClassName);
end;

procedure TDesignStoreMgr.RegisterClasses;
begin
  if fRegistered then
    Exit;
  fRegistered := True;
  fFactory.RegisterClass(TDesignStoreMgr.TStoreHeader);
  fFactory.RegisterClass(TDesignStoreMgr.TStoreCmp);
  fFactory.RegisterClass(TDesignStoreMgr.TStoreValue);
end;

procedure TDesignStoreMgr.DoSave(AStoreCmp: TStoreCmp; ACmp: TComponent);
var
  i: integer;
begin
  DoSaveValues(AStoreCmp, ACmp);
  for i := 0 to ACmp.ComponentCount - 1 do
  begin
    if ACmp.Components[i] is TMenuItem then
      Continue
    else
    if ACmp.Components[i] is TMenu then
    begin
      AStoreCmp.ChildsCount := AStoreCmp.ChildsCount + 1;
      DoSaveMenu(AStoreCmp.Childs[AStoreCmp.ChildsCount - 1], ACmp.Components[i] as TMenu);
    end
    else
    if ACmp.Components[i] is TContainedAction then
      Continue
    else
    if ACmp.Components[i] is TCustomActionList then
    begin
      AStoreCmp.ChildsCount := AStoreCmp.ChildsCount + 1;
      DoSaveActionList(AStoreCmp.Childs[AStoreCmp.ChildsCount - 1], ACmp.Components[i] as TCustomActionList);
    end
    else
    if (ACmp.Components[i] is TControl)
       and ((ACmp.Components[i] as TControl).Parent <> ACmp) then
      Continue
    else
    if (ACmp.Components[i] is TControl) then
    begin
      AStoreCmp.ChildsCount := AStoreCmp.ChildsCount + 1;
      DoSaveControl(AStoreCmp.Childs[AStoreCmp.ChildsCount - 1], ACmp.Components[i] as TControl);
    end
    else
    begin
      AStoreCmp.ChildsCount := AStoreCmp.ChildsCount + 1;
      DoSave(AStoreCmp.Childs[AStoreCmp.ChildsCount - 1], ACmp.Components[i]);
    end;
  end;
end;

procedure TDesignStoreMgr.DoSaveMenuItem(AStoreCmp: TStoreCmp; ACmp: TMenuItem);
var
  i: integer;
begin
  DoSaveValues(AStoreCmp, ACmp);
  AStoreCmp.ChildsCount := ACmp.Count;
  for i := 0 to ACmp.Count - 1 do
  begin
    DoSaveMenuItem(AStoreCmp.Childs[i], ACmp.Items[i]);
  end;
end;

procedure TDesignStoreMgr.DoSaveMenu(AStoreCmp: TStoreCmp; ACmp: TMenu);
begin
  DoSaveValues(AStoreCmp, ACmp);
  AStoreCmp.ChildsCount := 1;
  DoSaveMenuItem(AStoreCmp.Childs[0], ACmp.Items);
end;

procedure TDesignStoreMgr.DoSaveAction(AStoreCmp: TStoreCmp;
  ACmp: TContainedAction);
begin
  DoSaveValues(AStoreCmp, ACmp);
end;

procedure TDesignStoreMgr.DoSaveActionList(AStoreCmp: TStoreCmp;
  ACmp: TCustomActionList);
var
  i: integer;
begin
  DoSaveValues(AStoreCmp, ACmp);
  AStoreCmp.ChildsCount := ACmp.ActionCount;
  for i := 0 to ACmp.ActionCount - 1 do
    DoSaveAction(AStoreCmp.Childs[i], ACmp.Actions[i]);
end;

procedure TDesignStoreMgr.DoSaveControl(AStoreCmp: TStoreCmp;
  ACmp: TControl);
var
  i: integer;
  mWinControl: TWinControl;
begin
  DoSaveValues(AStoreCmp, ACmp);
  if ACmp is TWinControl then
  begin
    mWinControl := ACmp as TWinControl;
    AStoreCmp.ChildsCount := mWinControl.ControlCount;
    for i := 0 to mWinControl.ControlCount - 1 do
      DoSaveControl(AStoreCmp.Childs[i], mWinControl.Controls[i]);
  end;
end;

procedure TDesignStoreMgr.DoSaveValues(AStoreCmp: TStoreCmp; ACmp: TComponent);
var
  mData: IRBData;
begin
  AStoreCmp.SClassName := ACmp.ClassName;
  AStoreCmp.SName := ACmp.Name;
  AStoreCmp.ValuesCount := 4;
  mData := TRBData.Create(ACmp, True);
  DoSaveValue('Left', mData, AStoreCmp.Values[0]);
  DoSaveValue('Top', mData, AStoreCmp.Values[1]);
  DoSaveValue('Width', mData, AStoreCmp.Values[2]);
  DoSaveValue('Height', mData, AStoreCmp.Values[3]);
end;

procedure TDesignStoreMgr.DoSaveValue(const AName: string;
  AControlData: IRBData; AValue: TStoreValue);
var
  mItem: IRBDataItem;
begin
  mItem := AControlData.FindItem(AName);
  if mItem = nil then
    Exit;
  AValue.ID := AName;
  AValue.Value := mItem.AsPersist;
end;

procedure TDesignStoreMgr.DoLoad(AStoreCmp: TStoreCmp; ACmp: TComponent);
var
  i: integer;
  mChild: TComponent;
begin
  DoLoadValues(AStoreCmp, ACmp);
  for i := 0 to AStoreCmp.ChildsCount - 1 do
  begin
    // find child component
    mChild := nil;
    if (ACmp is TWinControl) then
      mChild := (ACmp as TWinControl).FindChildControl(AStoreCmp[i].SName);
    if mChild = nil then
      mChild := FindComponent(fContainer, AStoreCmp[i].SName);
    if mChild = nil then
      mChild := FindControl(fContainer, AStoreCmp[i].SName);
    if mChild = nil then
      mChild := CreateComponent(AStoreCmp[i].SClassName);
    if mChild = nil then
      Exit;
    // change parent
    if (mChild is TControl) then
    begin
      if ACmp is TWinControl then
         (mChild as TControl).Parent := ACmp as TWinControl
       else
         (mChild as TControl).Parent := fContainer;
    end;
    // set values
    mChild.Name := AStoreCmp[i].SName;
    DoLoad(AStoreCmp[i], mChild);
  end;
end;

procedure TDesignStoreMgr.DoLoadValues(AStoreCmp: TStoreCmp; ACmp: TComponent);
var
  mData: IRBData;
  i: integer;
begin
  if ACmp.ClassName <> AStoreCmp.SClassName  then
    raise Exception.Create('necompatible classes');
  if ACmp.Name <> AStoreCmp.SName  then
    raise Exception.Create('necompatible names');
  mData := TRBData.Create(ACmp, True);
  for i := 0 to AStoreCmp.ValuesCount - 1 do
  begin
    DoLoadValue(mData, AStoreCmp.Values[i]);
  end;
end;

procedure TDesignStoreMgr.DoLoadValue(AControlData: IRBData; AValue: TStoreValue);
var
  mItem: IRBDataItem;
begin
  mItem := AControlData.FindItem(AValue.ID);
  if mItem = nil then
    Exit;
  AControlData.ItemByName[AValue.ID].AsPersist := AValue.Value;
end;

function TDesignStoreMgr.CreateComponent(const AClassName: string): TComponent;
var
  mClass: TPersistentClass;
begin
  mClass := FindClass(AClassName);
  Result := mClass.NewInstance as TComponent;
  Result.Create(fContainer);
end;

function TDesignStoreMgr.FindComponent(const AOwner: TComponent;
  const AName: string): TComponent;
var
  i: integer;
  mC: TComponent;
begin
  Result := AOwner.FindComponent(AName);
  if Result = nil then
  begin
    for i := 0 to AOwner.ComponentCount - 1 do
    begin
      mC := AOwner.Components[i];
      Result := FindComponent(mC, AName);
      if Result <> nil then
        Break;
    end;
  end;
end;

function TDesignStoreMgr.FindControl(const AParent: TWinControl;
  const AName: string): TControl;
var
  i: integer;
  mC: TControl;
begin
  Result := AParent.FindChildControl(AName);
  if Result = nil then
  begin
    for i := 0 to AParent.ControlCount - 1 do
    begin
      mC := AParent.Controls[i];
      if not (mC is TWinControl) then
        Continue;
      Result := (mC as TWinControl).FindChildControl(AName);
      if Result <> nil then
        Break;
    end;
  end;
end;

function TDesignStoreMgr.FindStoreContainter(const AID: string): TStoreHeader;
var
  i: integer;
  mStoredContainers: IRBDataList;
begin
  mStoredContainers := fStore.LoadList(TDesignStoreMgr.TStoreHeader.ClassName);
  for i := 0 to mStoredContainers.Count - 1 do
  begin
    Result := mStoredContainers[i] as TDesignStoreMgr.TStoreHeader;
    if (Result.ID = AID) then
      Exit;
  end;
  Result := nil;
end;

function TDesignStoreMgr.GetContainerID: string;
begin
  Result := fContainer.ClassName + '.' + fContainer.Name;
end;

constructor TDesignStoreMgr.Create;
begin

end;

destructor TDesignStoreMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TDesignStoreMgr.Bind(AContainer: TWinControl; const AStore: IRBStore;
  const AFactory: IRBFactory; const ADataQuery: IRBDataQuery);
begin
  fContainer := AContainer;
  fStore := AStore;
  fFactory := AFactory;
  fDataQuery := ADataQuery;
  RegisterClasses;
end;

procedure TDesignStoreMgr.Unbind;
begin

end;

procedure TDesignStoreMgr.Save;
var
  mC: TStoreHeader;
  mID: string;
begin
  mID := GetContainerID;
  mC := FindStoreContainter(mID);
  if mC = nil then
  begin
    mC := TStoreHeader.Create;
    mC.ID := mID;
  end
  else
  begin
    mC.Root := nil;
  end;
  DoSave(mC.Root, fContainer);
  fStore.Save(mC);
end;

procedure TDesignStoreMgr.Load;
var
  mC: TStoreHeader;
begin
  mC := FindStoreContainter(GetContainerID);
  if mC = nil then
    Exit;
  DoLoad(mC.Root, fContainer);
end;

{ TDesignStoreMgr.TStoreHeader }

function TDesignStoreMgr.TStoreHeader.GetRoot(AIndex: Integer): TStoreCmp;
begin
  if fRoot = nil then
    fRoot := TStoreCmp.Create;
  Result := fRoot;
end;

procedure TDesignStoreMgr.TStoreHeader.SetRoot(AIndex: Integer; AValue: TStoreCmp);
begin
  if fRoot <> nil then
    fRoot.Free;
  fRoot := AValue;
end;

destructor TDesignStoreMgr.TStoreHeader.Destroy;
begin
  FreeAndNil(fRoot);
  inherited Destroy;
end;

{ TDesignStoreMgr.TStoreCmp }

function TDesignStoreMgr.TStoreCmp.GetChilds(AIndex: Integer; APropIndex: integer
  ): TStoreCmp;
begin
  if fChilds[AIndex] = nil then
  begin
    fChilds[AIndex] := TStoreCmp.Create;
    fChilds[AIndex].Parent := Self;
  end;
  Result := fChilds[AIndex];
end;

function TDesignStoreMgr.TStoreCmp.GetChildsCount(AIndex: Integer): integer;
begin
  Result := fChilds.Count;
end;

function TDesignStoreMgr.TStoreCmp.GetParent(AIndex: Integer): TStoreCmp;
begin
  Result := fParent;
end;

function TDesignStoreMgr.TStoreCmp.GetValues(AIndex: Integer; APropIndex: integer
  ): TStoreValue;
begin
  if fValues[AIndex] = nil then
  begin
    fValues[AIndex] := TStoreValue.Create;
  end;
  Result := fValues[AIndex];
end;

function TDesignStoreMgr.TStoreCmp.GetValuesCount(AIndex: Integer): integer;
begin
  Result := fValues.Count;
end;

procedure TDesignStoreMgr.TStoreCmp.SetChilds(AIndex: Integer; APropIndex: integer;
  AValue: TStoreCmp);
begin
  if fChilds[AIndex] <> nil then
    fChilds[AIndex].Free;
  fChilds[AIndex] := AValue;
end;

procedure TDesignStoreMgr.TStoreCmp.SetChildsCount(AIndex: Integer; AValue: integer);
begin
  fChilds.Count := AValue;
end;

procedure TDesignStoreMgr.TStoreCmp.SetParent(AIndex: Integer; AValue: TStoreCmp);
begin
  fParent := AValue;
end;

procedure TDesignStoreMgr.TStoreCmp.SetValues(AIndex: Integer; APropIndex: integer;
  AValue: TStoreValue);
begin
  if fValues[AIndex] <> nil then
    fValues[AIndex].Free;
  fValues[AIndex] := AValue;
end;

procedure TDesignStoreMgr.TStoreCmp.SetValuesCount(AIndex: Integer;
  AValue: integer);
begin
  fValues.Count := AValue;
end;

procedure TDesignStoreMgr.TStoreCmp.AfterConstruction;
begin
  inherited AfterConstruction;
  fChilds := TChilds.Create(True);
  fValues := TValues.Create(True);
end;

procedure TDesignStoreMgr.TStoreCmp.BeforeDestruction;
begin
  FreeAndNil(fValues);
  FreeAndNil(fChilds);
  inherited BeforeDestruction;
end;

{ CFxDesignerLib }

class function CFxDesignerLib.AddMenuItem(AMenu: TMenu; const ACaption: string;
  AHandler: TNotifyEvent; ATag: PtrInt): TMenuItem;
begin
  Result := TMenuItem.Create(AMenu);
  AMenu.Items.Add(Result);
  Result.OnClick := AHandler;
  Result.Caption := ACaption;
  Result.Tag := ATag;
end;

{ TRBDesigner }

procedure TRBDesigner.BindMenu;
begin
  FreeAndNil(fMenuBinder);
  fMenuBinder := TPopupMenuBinder.Create(BuildMenuEvent);
  fMenuBinder.Bind(fContainer);
end;

procedure TRBDesigner.UnbindMenu;
begin
  FreeAndNil(fMenuBinder);
end;

procedure TRBDesigner.BuildMenuEvent(AMenu: TMenu; AControl: TControl);
begin
  AMenu.Items.Clear;
  case fDesignOn of
    True:
      begin
        fControlMgr.AddCreateControls(AMenu);
        CFxDesignerLib.AddMenuItem(AMenu, 'Switch to Edit mode', SwitchModeEvent);
        CFxDesignerLib.AddMenuItem(AMenu, 'Save', SaveLayoutEvent);
        CFxDesignerLib.AddMenuItem(AMenu, 'Load', LoadLayoutEvent);
      end;
    False:
      begin
        CFxDesignerLib.AddMenuItem(AMenu, 'Switch to Desing mode', SwitchModeEvent);
      end;
  end;
end;

procedure TRBDesigner.SwitchModeEvent(Sender: TObject);
begin
  if fDesignOn then
    SwitchDesignOff
  else
    SwitchDesignOn;
  fDesignOn := not fDesignOn;
end;

procedure TRBDesigner.SaveLayoutEvent(Sender: TObject);
begin
  fStoreMgr.Save;
end;

procedure TRBDesigner.LoadLayoutEvent(Sender: TObject);
begin
  fStoreMgr.Load;
end;

procedure TRBDesigner.SwitchDesignOn;
begin
  UnbindMenu;
  BindMenu;
  fControlMgr.Bind(fContainer, fCtlClasses);
  fStoreMgr.Bind(fContainer, fStore, fFactory, fDataQuery);
end;

procedure TRBDesigner.SwitchDesignOff;
begin
  fStoreMgr.Unbind;
  fControlMgr.Unbind;
  UnbindMenu;
  BindMenu;
end;

constructor TRBDesigner.Create;
begin
  fControlMgr := TDesignControlMgr.Create;
  fStoreMgr := TDesignStoreMgr.Create;
end;

destructor TRBDesigner.Destroy;
begin
  FreeAndNil(fStoreMgr);
  FreeAndNil(fControlMgr);
  inherited Destroy;
end;

procedure TRBDesigner.Bind(AContainer: TWinControl; const AStore: IRBStore;
  const AFactory: IRBFactory; const ADataQuery: IRBDataQuery;
  const ACtlClasses: IRBPersistClassRegister);
var
  mClass: TControlClass;
begin
  fContainer := AContainer;
  fStore := AStore;
  fFactory := AFactory;
  fDataQuery := ADataQuery;
  fCtlClasses := ACtlClasses;
  BindMenu;
end;

{ TDesignControlMgr }

function TDesignControlMgr.FindParent(AScreenPos: TPoint): TWinControl;
var
  mPos: TPoint;
begin
  mPos := fContainer.ScreenToClient(AScreenPos);
  Result := fContainer.ControlAtPos(mPos, [capfAllowWinControls, capfRecursive]) as TWinControl;
  while (Result <> nil) and not IsContainer(Result) do
  begin
    Result := Result.Parent;
  end;
  if Result = nil then
    Result := fContainer;
end;

function TDesignControlMgr.CreateControl(AClass: TControlClass): TControl;
begin
  Result := AClass.Create(fContainer);
  Result.Caption := AClass.ClassName;
end;

procedure TDesignControlMgr.ResolveParent(AControl: TControl);
var
  mPos: TPoint;
begin
  mPos := Mouse.CursorPos;
  AControl.Parent := FindParent(mPos);
  mPos := AControl.Parent.ScreenToClient(mPos);
  AControl.Left := mPos.X;
  AControl.Top := mPos.Y;
end;

procedure TDesignControlMgr.AddBinders(AControl: TControl);
var
  mBB: TBehaveBinder;
begin
  // move
  mBB := TMoveControlBinder.Create;
  mBB.Bind(AControl);
  fBinders.Add(mBB);
  // popup menu
  mBB := TPopupMenuBinder.Create(BuildControlMenuEvent);
  mBB.Bind(AControl);
  fBinders.Add(mBB);
end;

function TDesignControlMgr.IsContainer(AControl: TControl): Boolean;
begin
  Result :=
    (AControl is TCustomPanel)
    or (AControl is TCustomForm)
    or (AControl is TCustomGroupBox);
end;

procedure TDesignControlMgr.AddCreateControls(AMenu: TMenu);
var
  i: integer;
begin
  for i := 0 to fCtlClasses.Count - 1 do
  begin
    CFxDesignerLib.AddMenuItem(AMenu, 'Add ' + fCtlClasses[i].ClassName, AddControlEvent, i);
  end;
end;

procedure TDesignControlMgr.AddControlEvent(Sender: TObject);
var
  mClass: TControlClass;
  mControl: TControl;
begin
  mClass := TControlClass(fCtlClasses[(Sender as TMenuItem).Tag]);
  mControl := CreateControl(mClass);
  ResolveParent(mControl);
  AddBinders(mControl);
end;

procedure TDesignControlMgr.DropControlEvent(Sender: TObject);
var
  mControl: TWinControl;
begin
  mControl := TWinControl((Sender as TMenuItem).Tag);
  if mControl <> nil then
    mControl.Free;
end;

procedure TDesignControlMgr.PropertiesEvent(Sender: TObject);
begin
  // future use
end;

procedure TDesignControlMgr.BuildControlMenuEvent(AMenu: TMenu; AControl: TControl);
begin
  AMenu.Items.Clear;
  CFxDesignerLib.AddMenuItem(AMenu, 'Drop control', DropControlEvent, PtrInt(AControl));
  if IsContainer(AControl) then
    AddCreateControls(AMenu);
  CFxDesignerLib.AddMenuItem(AMenu, 'Properties', PropertiesEvent, PtrInt(AControl));
end;


procedure TDesignControlMgr.BindControls(const AParent: TControl);
var
  i: integer;
  mWControl: TWinControl;
begin
  if not (AParent is TWinControl) then
    Exit;
  mWControl := AParent as TWinControl;
  for i := 0 to mWControl.ControlCount - 1 do
  begin
    AddBinders(mWControl.Controls[i]);
    BindControls(mWControl.Controls[i]);
  end;
end;

procedure TDesignControlMgr.BindContainer;
var
  mBB: TBehaveBinder;
begin
  // repeator for non TWinControl controls
  mBB := TMoveFormBinder.Create;
  mBB.Bind(fContainer);
  fBinders.Add(mBB);
end;

constructor TDesignControlMgr.Create;
begin
  fBinders := TBinders.Create;
end;

destructor TDesignControlMgr.Destroy;
begin
  FreeAndNil(fBinders);
  inherited Destroy;
end;

procedure TDesignControlMgr.Bind(AContainer: TWinControl; ACtlClasses: IRBPersistClassRegister);
begin
  Unbind;
  fContainer := AContainer;
  fCtlClasses := ACtlClasses;
  BindControls(fContainer);
  BindContainer;
end;

procedure TDesignControlMgr.Unbind;
begin
  fBinders.Clear;
  fContainer := nil;
end;

end.


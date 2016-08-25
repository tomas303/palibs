unit trl_dicontainer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fgl, typinfo, trl_irttibroker, trl_urttibroker, trl_ifactory,
  variants;

type

  { TDIObject }

  TDIObject = class(TObject)
  public
    constructor Create; virtual;
  end;

  TDIClass = class of TDIObject;

  TDICustomContainer = class(TDIObject)
  public
    function Locate(AClass: TClass; const AID: string = ''): pointer; virtual; overload; abstract;
    function Locate(AInterface: TGUID; const AID: string = ''): pointer; virtual; overload; abstract;
    function Locate(const AClass: string; const AID: string = ''): pointer; virtual; overload; abstract;
  end;

  TDIRegCreateKind = (ckTransient, ckSingle);

  { TDIReg }

  TDIReg = class
  public type

    TOnInjectEvent = procedure(const AItem: IRBDataItem) of object;

    { TInjectProp }

    TInjectProp = record
      Name: string;
      Value: variant;
      ValueClass: TClass;
      VGuid: TGuid;
      Container: TDICustomContainer;
      ID: string;
      OnInject: TOnInjectEvent;
    public
      procedure Clear;
      function Create(const AName: string; AValue: variant): TInjectProp; overload;
      function Create(const AName: string; AValue: TClass; const AID: string = ''; AContainer: TDICustomContainer = nil): TInjectProp; overload;
      function Create(const AName: string; const AValue: TGuid; const AID: string = ''; AContainer: TDICustomContainer = nil): TInjectProp; overload;
      function Create(const AName: string; AOnInject: TOnInjectEvent): TInjectProp; overload;
    public
      class operator =(a, b: TInjectProp): Boolean;
    end;

  private type

    { TImplementation }

    TImplementation = record
      Guid: TGuid;
    public
      function Create(const AGuid: TGuid): TImplementation;
      class operator =(a, b: TImplementation): Boolean;
    end;

  private type

    { TImplementations }

    TImplementations = class(TFPGList<TImplementation>)
    end;

    { TInjectProps }

    TInjectProps = class(TFPGList<TInjectProp>)
    end;

  private
    fDIC: TDICustomContainer;
    fID: string;
    fImplementations: TImplementations;
    fInjectProps: TInjectProps;
    fCreateKind: TDIRegCreateKind;
    fSingleObject: TObject;
    fSingleHold: Boolean;
    procedure SetCreateKind(AValue: TDIRegCreateKind);
  protected
    function InstantiatedClass: TClass; virtual; abstract;
    function Implements(AIntf: TGuid; const AID: string): Boolean;
    function Instantiate(AClass: TClass; const AID: string): Boolean; overload;
    function Instantiate(const AClass: string; const AID: string): Boolean; overload;
    procedure Inject(AInstance: TObject);
    procedure AddSupportedInterfaces(AInterfaces: array of TGuid);
    function NewObject: TObject; virtual; abstract;
    procedure FreeSingleObject;
  public
    constructor Create;
    destructor Destroy; override;
    function Make: pointer; overload;
    function Make(AInterface: TGUID; const AID: string): pointer; overload;
    procedure InjectProps(AProps: array of TInjectProp);
    procedure InjectProp(const AName: string; const AValue: string); overload;
    procedure InjectProp(const AName: string; const AValue: Boolean); overload;
    procedure InjectProp(const AName: string; AValue: TClass; const AID: string = ''; AContainer: TDICustomContainer = nil); overload;
    procedure InjectProp(const AName: string; const AValue: TGuid; const AID: string = ''; AContainer: TDICustomContainer = nil); overload;
    procedure InjectProp(const AName: string; const AOnInject: TOnInjectEvent); overload;
    property CreateKind: TDIRegCreateKind read fCreateKind write SetCreateKind;
    property ID: string read fID write fID;
  end;

  { TDI_TObjectReg }

  TDI_TObjectReg = class(TDIReg)
  private
    fClass: TClass;
  protected
    function NewObject: TObject; override;
    function InstantiatedClass: TClass; override;
  public
    constructor Create(AClass: TClass; AInterfaces: array of TGuid);
  end;

  { TDI_TDIObjectReg }

  TDI_TDIObjectReg = class(TDIReg)
  private
    fClass: TDIClass;
  protected
    function NewObject: TObject; override;
    function InstantiatedClass: TClass; override;
  public
    constructor Create(AClass: TDIClass; AInterfaces: array of TGuid);
  end;

  { TDI_TComponentReg }

  TDI_TComponentReg = class(TDIReg)
  private
    fClass: TComponentClass;
    fOwner: TComponent;
  protected
    function NewObject: TObject; override;
    function InstantiatedClass: TClass; override;
  public
    constructor Create(AClass: TComponentClass; AOwner: TComponent;
      AInterfaces: array of TGuid);
  end;



  { TDIContainer }

  TDIContainer = class(TDICustomContainer)
  private type
    TDIRegs = class(specialize TFPGObjectList<TDIReg>)
    end;
  private
    fRegs: TDIRegs;
  protected
    function Find(AClass: TClass; const AID: string): TDIReg; overload;
    function Find(AInterface: TGUID; const AID: string): TDIReg; overload;
    function Find(const AClass: string; const AID: string): TDIReg; overload;
    procedure CheckRegNotExists(AClass: TClass; const AID: string);
    function RegisterReg(AReg: TDIReg): TDIReg;
    function NewReg(AClass: TClass; const AID: string; AInterfaces: array of TGuid; ACreateKind: TDIRegCreateKind): TDIReg; overload;
    function NewReg(AClass: TComponentClass; AOwner: TComponent; const AID: string; AInterfaces: array of TGuid; ACreateKind: TDIRegCreateKind): TDIReg; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Locate(AClass: TClass; const AID: string = ''): pointer; override; overload;
    function Locate(AInterface: TGUID; const AID: string = ''): pointer; override; overload;
    function Locate(const AClass: string; const AID: string = ''): pointer; override; overload;
    // TObject
    function Add(const AClass: TClass; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TClass; AInterface: TGUID; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TClass; AInterfaces: array of TGuid; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    // TComponent
    function Add(const AClass: TComponentClass; AOwner: TComponent; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TComponentClass; AOwner: TComponent; AInterface: TGUID;  const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TComponentClass; AOwner: TComponent; AInterfaces: array of TGuid; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
  end;

  { TCustomDIFactory }

  TCustomDIFactory = class(TInterfacedObject)
  private
    fContainer: TDIContainer;
    procedure SetAddClass(AValue: TClass);
    procedure SetContainer(AValue: TDIContainer);
  published
    property Container: TDIContainer read fContainer write SetContainer;
    property AddClass: TClass write SetAddClass;
  end;

  { TDIOwner }

  TDIOwner = class(TComponent)
  {
  solve problem of interfaces on TComponent - which is freed manually, but
  link from other place are not zeroed ... and crash when hit inc/dec code
  so before free such owner will clear all published interfaces on childs
  }
  protected
    procedure Clear(const AComp: TComponent; AFreedInstance: TObject = nil);
    procedure ClearChilds(AFreedInstance: TObject = nil);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  published
    destructor Destroy; override;
  end;

implementation

{ TDIOwner }

procedure TDIOwner.Clear(const AComp: TComponent; AFreedInstance: TObject = nil);
var
  mRB: IRBData;
  mRBItem: IRBDataItem;
  i: integer;
begin
  mRB := TRBData.Create(AComp, True);
  for i := 0 to mRB.Count - 1 do begin
    mRBItem := mRB.Items[i];
    case mRBItem.TypeKind of
      tkInterface:
      begin
        if mRBItem.AsInterface <> nil then begin
          if (AFreedInstance = nil) or ((mRBItem.AsInterface as TObject) = AFreedInstance) then
            mRBItem.AsInterface := nil;
        end;
      end;
    end;
  end;
end;

procedure TDIOwner.ClearChilds(AFreedInstance: TObject = nil);
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    Clear(Components[i], AFreedInstance);
end;

procedure TDIOwner.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
    ClearChilds(AComponent);
  inherited Notification(AComponent, Operation);
end;

destructor TDIOwner.Destroy;
begin
  ClearChilds;
  inherited Destroy;
end;

{ TDIReg.TImplementation }

function TDIReg.TImplementation.Create(const AGuid: TGuid): TImplementation;
begin
  Result.Guid := AGuid;
end;

class operator TDIReg.TImplementation. = (a, b: TImplementation): Boolean;
begin
  Result := CompareMem(@a.Guid, @b.Guid, SizeOf(TGuid));
end;

{ TCustomDIFactory }

procedure TCustomDIFactory.SetAddClass(AValue: TClass);
begin
  fContainer.Add(AValue);
end;

procedure TCustomDIFactory.SetContainer(AValue: TDIContainer);
begin
  fContainer := AValue;
end;

{ TDI_TDIObjectReg }

function TDI_TDIObjectReg.NewObject: TObject;
begin
  Result := fClass.Create;
end;

function TDI_TDIObjectReg.InstantiatedClass: TClass;
begin
  Result := fClass;
end;

constructor TDI_TDIObjectReg.Create(AClass: TDIClass;
  AInterfaces: array of TGuid);
begin
  inherited Create;
  fClass := AClass;
  AddSupportedInterfaces(AInterfaces);
end;

{ TDI_TObjectReg }

function TDI_TObjectReg.NewObject: TObject;
begin
  Result := fClass.Create;
end;

function TDI_TObjectReg.InstantiatedClass: TClass;
begin
  Result := fClass;
end;

constructor TDI_TObjectReg.Create(AClass: TClass; AInterfaces: array of TGuid);
begin
  inherited Create;
  fClass := AClass;
  AddSupportedInterfaces(AInterfaces);
end;

{ TDI_TComponentReg }

function TDI_TComponentReg.NewObject: TObject;
begin
  Result := fClass.Create(fOwner);
end;

function TDI_TComponentReg.InstantiatedClass: TClass;
begin
  Result := fClass;
end;

constructor TDI_TComponentReg.Create(AClass: TComponentClass; AOwner: TComponent;
  AInterfaces: array of TGuid);
begin
  inherited Create;
  fClass := AClass;
  fOwner := AOwner;
  AddSupportedInterfaces(AInterfaces);
end;

{ TDIObject }

constructor TDIObject.Create;
begin
end;

{ TDIReg.TInjectProp }

procedure TDIReg.TInjectProp.Clear;
begin
  Name := '';
  Value := Null;
  ValueClass := nil;
  VGuid := GUID_NULL;
  Container := nil;
  ID := '';
  OnInject := nil;
end;

function TDIReg.TInjectProp.Create(const AName: string; AValue: variant): TInjectProp;
begin
  Result.Clear;
  Result.Name := AName;
  Result.Value := AValue;
end;

function TDIReg.TInjectProp.Create(const AName: string; AValue: TClass;
  const AID: string = ''; AContainer: TDICustomContainer = nil): TInjectProp;
begin
  Result.Clear;
  Result.Name := AName;
  Result.ValueCLass := AValue;
  Result.Container := AContainer;
  Result.ID := AID;
end;

function TDIReg.TInjectProp.Create(const AName: string; const AValue: TGuid;
  const AID: string = ''; AContainer: TDICustomContainer = nil): TInjectProp;
begin
  Result.Clear;
  Result.Name := AName;
  Result.VGuid := AValue;
  Result.Container := AContainer;
  Result.ID := AID;
end;

function TDIReg.TInjectProp.Create(const AName: string;
  AOnInject: TOnInjectEvent): TInjectProp;
begin
  Result.Clear;
  Result.Name := AName;
  Result.OnInject := AOnInject;
end;

class operator TDIReg.TInjectProp. = (a, b: TInjectProp): Boolean;
begin
  Result := (a.Name = b.Name) and (a.Value = b.Value) and (a.ValueClass = b.ValueClass);
end;

{ TDIReg }

procedure TDIReg.SetCreateKind(AValue: TDIRegCreateKind);
begin
  if fCreateKind = AValue then
    Exit;
  if fCreateKind = ckSingle then
    FreeSingleObject;
  fCreateKind := AValue;
end;

function TDIReg.Implements(AIntf: TGuid; const AID: string): Boolean;
begin
  Result := (AID = ID) and (fImplementations.IndexOf(TImplementation.Create(AIntf)) > -1);
end;

function TDIReg.Instantiate(AClass: TClass; const AID: string): Boolean;
begin
  Result := (ID = AID) and (InstantiatedClass = AClass);
end;

function TDIReg.Instantiate(const AClass: string; const AID: string): Boolean;
begin
  Result := (ID = AID) and (InstantiatedClass.ClassName = AClass);
end;

procedure TDIReg.Inject(AInstance: TObject);
var
  mRB: IRBData;
  mRBItem: IRBDataItem;
  mIP: TInjectProp;
  mObject: TObject;
  mInterface: IUnknown;
  i: integer;
  mcname: string;
  miname:string;
begin
  mcname := ainstance.ClassName;
  mRB := TRBData.Create(AInstance, True);
  for mIP in fInjectProps do
  begin
    if (mIP.Name <> '') and (mRB.FindItem(mIP.Name) = nil) then
      raise Exception.CreateFmt('Class %s do not have property %s', [mRB.ClassName, mIP.Name]);
    for i := 0 to mRB.Count - 1 do begin
      mRBItem := mRB.Items[i];
      miname:=mRBItem.Name;
      try
        if mIP.Name = mRBItem.Name then begin
          case mRBItem.TypeKind of
            tkClassRef:
              //if mIP.ValueClass <> nil then
                mRBItem.AsPtrInt := PtrInt(mIP.ValueClass);
            tkClass:
              //if mIP.ValueClass <> nil then
              begin
                if mIP.Container <> nil then
                  mObject := mIP.Container.Locate(mIP.ValueClass, mIP.ID)
                else
                  mObject := fDIC.Locate(mIP.ValueClass, mIP.ID);
                mRBItem.AsObject := mObject;
              end;
            tkInterface:
              //if not CompareMem(@mIP.VGuid, @GUID_NULL, SizeOf(mIP.VGuid)) then
              begin
                if mIP.Container <> nil then
                  mInterface := mIP.Container.Locate(mIP.VGuid, mIP.ID)
                else
                  mInterface := fDIC.Locate(mIP.VGuid, mIP.ID);
                mRBItem.AsInterface := mInterface;
              end;
          else
            //if not VarIsNull(mIP.Value) then
              mRBItem.AsVariant := mIP.Value;
          end;
        end;
        // event
        if ((mIP.Name = mRBItem.Name) or (mIP.Name = '')) and Assigned(mIP.OnInject) then
          mIP.OnInject(mRBItem);

      except
        on E: Exception do begin
          raise Exception.Create(E.Message + ' on property ' + mRBItem.Name);
        end;
      end;
    end;
  end;
end;

procedure TDIReg.AddSupportedInterfaces(AInterfaces: array of TGuid);
var
  mImpl: TGuid;
begin
  for mImpl in AInterfaces do
  begin
    fImplementations.Add(TImplementation.Create(mImpl));
  end;
end;

procedure TDIReg.FreeSingleObject;
begin
  if fSingleObject <> nil then
  begin
    if fSingleHold then
      (fSingleObject as IUnknown)._Release
    else
      FreeAndNil(fSingleObject);
  end;
end;

constructor TDIReg.Create;
begin
  fImplementations := TImplementations.Create;
  fInjectProps := TInjectProps.Create;
  fCreateKind := ckTransient;
end;

destructor TDIReg.Destroy;
begin
  FreeSingleObject;
  FreeAndNil(fImplementations);
  FreeAndNil(fInjectProps);
  inherited Destroy;
end;

function TDIReg.Make: pointer;
begin
  if (fCreateKind = ckSingle) and (fSingleObject <> nil) then
    Result := fSingleObject
  else
  begin
    Result := NewObject;
    Inject(Result);
    if (fCreateKind = ckSingle) then
      fSingleObject := Result;
  end;
end;

function TDIReg.Make(AInterface: TGUID; const AID: string): pointer;
var
  m: TObject;
  mClassName: string;
begin
  Result := nil;
  if not Implements(AInterface, AID) then
    raise Exception.Create('not registered to implement interface ' + GUIDToString(AInterface));
  if (fCreateKind = ckSingle) and (fSingleObject <> nil) then
  begin
    fSingleObject.GetInterfaceWeak(AInterface, Result);
  end
  else
  begin
    m := Make;
    try
      mClassName := m.ClassName;
      if not m.GetInterfaceWeak(AInterface, Result) then
      begin
        if fCreateKind = ckTransient then
          m.Free;
        raise Exception.Create(mClassName + ' do not support interface ' + GUIDToString(AInterface));
      end;
      if fCreateKind = ckSingle then
      begin
        (fSingleObject as IUnknown)._AddRef;
        fSingleHold := True;
      end;
    except
      fSingleObject := nil;
      m.Free;
      raise;
    end;
  end;
end;

procedure TDIReg.InjectProps(AProps: array of TInjectProp);
var
  mIProp: TInjectProp;
begin
  fInjectProps.Clear;
  for mIProp in AProps do
    fInjectProps.Add(mIProp);
end;

procedure TDIReg.InjectProp(const AName: string; const AValue: string);
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue));
end;

procedure TDIReg.InjectProp(const AName: string; const AValue: Boolean);
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue));
end;

procedure TDIReg.InjectProp(const AName: string; AValue: TClass;
  const AID: string = ''; AContainer: TDICustomContainer = nil);
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue, AID, AContainer));
end;

procedure TDIReg.InjectProp(const AName: string; const AValue: TGuid;
    const AID: string = ''; AContainer: TDICustomContainer = nil);
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue, AID, AContainer));
end;

procedure TDIReg.InjectProp(const AName: string; const AOnInject: TOnInjectEvent
  );
begin
  fInjectProps.Add(TInjectProp.Create(AName, AOnInject));
end;

{ TDIContainer }

function TDIContainer.Find(AClass: TClass; const AID: string): TDIReg;
var
  mReg: TDIReg;
begin
  Result := nil;
  for mReg in fRegs do
  begin
    if mReg.Instantiate(AClass, AID) then
    begin
      Result := mReg;
      Break;
    end;
  end;
end;

function TDIContainer.Find(AInterface: TGUID; const AID: string): TDIReg;
var
  mReg: TDIReg;
begin
  Result := nil;
  for mReg in fRegs do
  begin
    if mReg.Implements(AInterface, AID) then
    begin
      Result := mReg;
      Break;
    end;
  end;
end;

function TDIContainer.Find(const AClass: string; const AID: string): TDIReg;
var
  mReg: TDIReg;
begin
  Result := nil;
  for mReg in fRegs do
  begin
    if mReg.Instantiate(AClass, AID) then
    begin
      Result := mReg;
      Break;
    end;
  end;
end;

procedure TDIContainer.CheckRegNotExists(AClass: TClass; const AID: string);
begin
  if Find(AClass, AID) <> nil then
    raise Exception.Create('already registered');
end;

function TDIContainer.RegisterReg(AReg: TDIReg): TDIReg;
begin
  Result := AReg;
  fRegs.Add(Result);
  Result.fDIC := Self;
end;

function TDIContainer.NewReg(AClass: TClass; const AID: string; AInterfaces: array of TGuid; ACreateKind: TDIRegCreateKind
  ): TDIReg;
begin
  CheckRegNotExists(AClass, AID);
  AClass.ClassType;
  if AClass.InheritsFrom(TDIObject) then
    Result := RegisterReg(TDI_TDIObjectReg.Create(TDIClass(AClass), AInterfaces))
  else
    Result := RegisterReg(TDI_TObjectReg.Create(AClass, AInterfaces));
  Result.ID := AID;
  Result.CreateKind := ACreateKind;
end;

function TDIContainer.NewReg(AClass: TComponentClass; AOwner: TComponent; const AID: string;
  AInterfaces: array of TGuid; ACreateKind: TDIRegCreateKind): TDIReg;
begin
  CheckRegNotExists(AClass, AID);
  Result := RegisterReg(TDI_TComponentReg.Create(AClass, AOwner, AInterfaces));
  Result.ID := AID;
  Result.CreateKind := ACreateKind;
end;

constructor TDIContainer.Create;
begin
   inherited;
   fRegs := TDIRegs.Create;
end;

destructor TDIContainer.Destroy;
begin
  FreeAndNil(fRegs);
  inherited Destroy;
end;

function TDIContainer.Locate(AClass: TClass; const AID: string = ''): pointer;
var
  mReg: TDIReg;
begin
  mReg := Find(AClass, AID);
  if mReg = nil then
    raise Exception.CreateFmt('registration not found(class:%s id:%s)', [AClass.ClassName, AID]);
  Result := mReg.Make;
end;

function TDIContainer.Locate(AInterface: TGUID; const AID: string = ''): pointer;
var
  mReg: TDIReg;
begin
  mReg := Find(AInterface, AID);
  if mReg = nil then
    raise Exception.CreateFmt('Registration for interface: %s ID:%s not found', [GUIDToString(AInterface), AID]);
  Result := mReg.Make(AInterface, AID);
end;

function TDIContainer.Locate(const AClass: string; const AID: string = ''): pointer;
var
  mReg: TDIReg;
begin
  mReg := Find(AClass, AID);
  if mReg = nil then
    raise Exception.CreateFmt('registration for class %s not found', [AClass]);
  Result := mReg.Make;
end;

function TDIContainer.Add(const AClass: TClass; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AID, [], ACreateKind);
end;

function TDIContainer.Add(const AClass: TClass; AInterface: TGUID; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AID, [AInterface], ACreateKind);
end;

function TDIContainer.Add(const AClass: TClass; AInterfaces: array of TGuid; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AID, AInterfaces, ACreateKind);
end;

function TDIContainer.Add(const AClass: TComponentClass; AOwner: TComponent; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient
  ): TDIReg;
begin
  Result := NewReg(AClass, AOwner, AID, [], ACreateKind);
end;

function TDIContainer.Add(const AClass: TComponentClass; AOwner: TComponent;
  AInterface: TGUID;  const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AOwner, AID, [AInterface], ACreateKind);
end;

function TDIContainer.Add(const AClass: TComponentClass; AOwner: TComponent;
  AInterfaces: array of TGuid; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AOwner, AID, AInterfaces, ACreateKind);
end;

end.


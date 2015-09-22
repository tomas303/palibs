unit trl_dicontainer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fgl, typinfo, trl_irttibroker, trl_urttibroker, trl_ifactory;

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

    { TInjectProp }

    TInjectProp = record
      Name: string;
      Value: variant;
      ValueClass: TClass;
      VGuid: TGuid;
      ID: string;
    public
      function Create(const AName: string; AValue: variant): TInjectProp; overload;
      function Create(const AName: string; AValue: TClass; const AID: string = ''): TInjectProp; overload;
      function Create(const AName: string; const AValue: TGuid; const AID: string = ''): TInjectProp; overload;
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
    procedure AddSupportedInterfaces(AInterfaces: array of TImplementation);
    function NewObject: TObject; virtual; abstract;
    procedure FreeSingleObject;
  public
    constructor Create;
    destructor Destroy; override;
    function Make: pointer; overload;
    function Make(AInterface: TGUID; const AID: string): pointer; overload;
    procedure InjectProps(AProps: array of TInjectProp);
    procedure InjectProp(const AName: string; const AValue: string); overload;
    procedure InjectProp(const AName: string; AValue: TClass; const AID: string = ''); overload;
    procedure InjectProp(const AName: string; const AValue: TGuid; const AID: string = ''); overload;
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
    constructor Create(AClass: TClass; AInterfaces: array of TImplementation);
  end;

  { TDI_TDIObjectReg }

  TDI_TDIObjectReg = class(TDIReg)
  private
    fClass: TDIClass;
  protected
    function NewObject: TObject; override;
    function InstantiatedClass: TClass; override;
  public
    constructor Create(AClass: TDIClass; AInterfaces: array of TImplementation);
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
      AInterfaces: array of TImplementation);
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
    function NewReg(AClass: TClass; const AID: string; AInterfaces: array of TDIReg.TImplementation; ACreateKind: TDIRegCreateKind): TDIReg; overload;
    function NewReg(AClass: TComponentClass; AOwner: TComponent; const AID: string; AInterfaces: array of TDIReg.TImplementation; ACreateKind: TDIRegCreateKind): TDIReg; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Locate(AClass: TClass; const AID: string = ''): pointer; override; overload;
    function Locate(AInterface: TGUID; const AID: string = ''): pointer; override; overload;
    function Locate(const AClass: string; const AID: string = ''): pointer; override; overload;
    // TObject
    function Add(const AClass: TClass; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TClass; AInterface: TGUID; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TClass; AInterfaces: array of TDIReg.TImplementation; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    // TComponent
    function Add(const AClass: TComponentClass; AOwner: TComponent; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TComponentClass; AOwner: TComponent; AInterface: TGUID;  const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
    function Add(const AClass: TComponentClass; AOwner: TComponent; AInterfaces: array of TDIReg.TImplementation; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg; overload;
  end;

  { TDIFactory }

  TDIFactory = class(TInterfacedObject)
  private
    fContainer: TDIContainer;
    procedure SetAddClass(AValue: TClass);
    procedure SetContainer(AValue: TDIContainer);
  published
    property Container: TDIContainer read fContainer write SetContainer;
    property AddClass: TClass write SetAddClass;
  end;

implementation

{ TDIReg.TImplementation }

function TDIReg.TImplementation.Create(const AGuid: TGuid): TImplementation;
begin
  Result.Guid := AGuid;
end;

class operator TDIReg.TImplementation. = (a, b: TImplementation): Boolean;
begin
  Result := CompareMem(@a.Guid, @b.Guid, SizeOf(TGuid));
end;

{ TDIFactory }

procedure TDIFactory.SetAddClass(AValue: TClass);
begin
  fContainer.Add(AValue);
end;

procedure TDIFactory.SetContainer(AValue: TDIContainer);
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
  AInterfaces: array of TImplementation);
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

constructor TDI_TObjectReg.Create(AClass: TClass; AInterfaces: array of TImplementation);
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
  AInterfaces: array of TImplementation);
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

function TDIReg.TInjectProp.Create(const AName: string; AValue: variant): TInjectProp;
begin
  Result.Name := AName;
  Result.Value := AValue;
end;

function TDIReg.TInjectProp.Create(const AName: string; AValue: TClass;
  const AID: string = ''): TInjectProp;
begin
  Result.Name := AName;
  Result.ValueCLass := AValue;
  Result.ID := AID;
end;

function TDIReg.TInjectProp.Create(const AName: string; const AValue: TGuid;
  const AID: string = ''): TInjectProp;
begin
  Result.Name := AName;
  Result.VGuid := AValue;
  Result.ID := AID;
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
begin
  mRB := TRBData.Create(AInstance, True);
  for mIP in fInjectProps do
  begin
    mRBItem := mRB.FindItem(mIP.Name);
    if mRBItem = nil then
      raise Exception.Create('property not found');
    case mRBItem.TypeKind of
      tkClassRef:
        mRBItem.AsPtrInt := PtrInt(mIP.ValueClass);
      tkClass:
        begin
          mObject := fDIC.Locate(mIP.ValueClass, mIP.ID);
          mRBItem.AsObject := mObject;
        end;
      tkInterface:
        begin
          mInterface := fDIC.Locate(mIP.VGuid, mIP.ID);
          mRBItem.AsInterface := mInterface;
        end;
    else
      mRBItem.AsVariant := mIP.Value;
    end;

  end;
end;

procedure TDIReg.AddSupportedInterfaces(AInterfaces: array of TImplementation);
var
  mImpl: TImplementation;
begin
  for mImpl in AInterfaces do
  begin
    fImplementations.Add(mImpl);
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
begin
  Result := nil;
  if not Implements(AInterface, AID) then
    raise Exception.Create('not registered to implement interface');
  if (fCreateKind = ckSingle) and (fSingleObject <> nil) then
  begin
    fSingleObject.GetInterfaceWeak(AInterface, Result);
  end
  else
  begin
    m := Make;
    try
      if not m.GetInterfaceWeak(AInterface, Result) then
      begin
        if fCreateKind = ckTransient then
          m.Free;
        raise Exception.Create('do not support interface');
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

procedure TDIReg.InjectProp(const AName: string; AValue: TClass;
  const AID: string = '');
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue, AID));
end;

procedure TDIReg.InjectProp(const AName: string; const AValue: TGuid; const AID: string = '');
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue, AID));
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

function TDIContainer.NewReg(AClass: TClass; const AID: string; AInterfaces: array of TDIReg.TImplementation; ACreateKind: TDIRegCreateKind
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
  AInterfaces: array of TDIReg.TImplementation; ACreateKind: TDIRegCreateKind): TDIReg;
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
    raise Exception.Create('registration not found');
  Result := mReg.Make(AInterface, AID);
end;

function TDIContainer.Locate(const AClass: string; const AID: string = ''): pointer;
var
  mReg: TDIReg;
begin
  mReg := Find(AClass, AID);
  if mReg = nil then
    raise Exception.Create('registration not found');
  Result := mReg.Make;
end;

function TDIContainer.Add(const AClass: TClass; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AID, [], ACreateKind);
end;

function TDIContainer.Add(const AClass: TClass; AInterface: TGUID; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AID, [TDIReg.TImplementation.Create(AInterface)], ACreateKind);
end;

function TDIContainer.Add(const AClass: TClass; AInterfaces: array of TDIReg.TImplementation; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
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
  Result := NewReg(AClass, AOwner, AID, [TDIReg.TImplementation.Create(AInterface)], ACreateKind);
end;

function TDIContainer.Add(const AClass: TComponentClass; AOwner: TComponent;
  AInterfaces: array of TDIReg.TImplementation; const AID: string = ''; ACreateKind: TDIRegCreateKind = ckTransient): TDIReg;
begin
  Result := NewReg(AClass, AOwner, AID, AInterfaces, ACreateKind);
end;

end.


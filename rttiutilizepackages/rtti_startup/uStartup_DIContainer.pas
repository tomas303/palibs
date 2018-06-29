unit uStartup_DIContainer;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, fgl, typinfo, rtti_broker_iBroker, rtti_broker_uData;

type

  III = interface
  ['{6DBFC907-A46F-4D1B-83B3-E2B683821C73}']
  end;

  { TDIObject }

  TDIObject = class(TObject)
  public
    constructor Create; virtual;
  end;

  TDIClass = class of TDIObject;

  TDICustomContainer = class
  public
    function Locate(AClass: TClass): pointer; virtual; overload; abstract;
    function Locate(AInterface: TGUID): pointer; virtual; overload; abstract;
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
    public
      function Create(const AName: string; AValue: variant): TInjectProp; overload;
      function Create(const AName: string; AValue: TClass): TInjectProp; overload;
      function Create(const AName: string; const AValue: TGuid): TInjectProp; overload;
      class operator =(a, b: TInjectProp): Boolean;
    end;

  private type

    TImplementations = class(TFPGList<string>)
    end;

    TInjectProps = class(TFPGList<TInjectProp>)
    end;

  private
    fDIC: TDICustomContainer;
    fImplementations: TImplementations;
    fInjectProps: TInjectProps;
    fCreateKind: TDIRegCreateKind;
    fSingleObject: TObject;
    fSingleHold: Boolean;
  protected
    function Implements(AIntf: TGuid): Boolean;
    function Instantiate(AClass: TClass): Boolean; virtual; abstract;
    procedure Inject(AInstance: TObject);
    procedure AddSupportedInterfaces(AInterfaces: array of TGUID);
    function NewObject: TObject; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function Make: pointer; overload;
    function Make(AInterface: TGUID): pointer; overload;
    procedure InjectProps(AProps: array of TInjectProp);
    procedure InjectProp(const AName: string; const AValue: string); overload;
    procedure InjectProp(const AName: string; AValue: TClass); overload;
    procedure InjectProp(const AName: string; const AValue: TGuid); overload;
  end;

  { TDI_TObjectReg }

  TDI_TObjectReg = class(TDIReg)
  private
    fClass: TClass;
  protected
    function NewObject: TObject; override;
    function Instantiate(AClass: TClass): Boolean; override;
  public
    constructor Create(AClass: TClass; AInterfaces: array of TGUID);
  end;

  { TDI_TDIObjectReg }

  TDI_TDIObjectReg = class(TDIReg)
  private
    fClass: TDIClass;
  protected
    function NewObject: TObject; override;
    function Instantiate(AClass: TClass): Boolean; override;
  public
    constructor Create(AClass: TDIClass; AInterfaces: array of TGUID);
  end;

  { TDI_TComponentReg }

  TDI_TComponentReg = class(TDIReg)
  private
    fClass: TComponentClass;
    fOwner: TComponent;
  protected
    function NewObject: TObject; override;
    function Instantiate(AClass: TClass): Boolean; override;
  public
    constructor Create(AClass: TComponentClass; AOwner: TComponent;
      AInterfaces: array of TGUID);
  end;



  { TDIContainer }

  TDIContainer = class(TDICustomContainer)
  private type
    TDIRegs = class(specialize TFPGObjectList<TDIReg>)
    end;
  private
    fRegs: TDIRegs;
  protected
    function Find(AClass: TClass): TDIReg; overload;
    function Find(AInterface: TGUID): TDIReg; overload;
    procedure CheckRegNotExists(AClass: TClass);
    function RegisterReg(AReg: TDIReg): TDIReg;
    function NewReg(AClass: TClass; AInterfaces: array of TGUID): TDIReg; overload;
    function NewReg(AClass: TDIClass; AInterfaces: array of TGUID): TDIReg; overload;
    function NewReg(AClass: TComponentClass; AOwner: TComponent; AInterfaces: array of TGUID): TDIReg; overload;
  public
    constructor Create;
    destructor Destroy; override;
    function Locate(AClass: TClass): pointer; override; overload;
    function Locate(AInterface: TGUID): pointer; override; overload;
    // TObject
    function Add(const AClass: TClass): TDIReg; overload;
    function Add(const AClass: TClass; AInterface: TGUID): TDIReg; overload;
    function Add(const AClass: TClass; AInterfaces: array of TGUID): TDIReg; overload;
    // TDIObject
    function Add(const AClass: TDIClass): TDIReg; overload;
    function Add(const AClass: TDIClass; AInterface: TGUID): TDIReg; overload;
    function Add(const AClass: TDIClass; AInterfaces: array of TGUID): TDIReg; overload;
    // TComponent
    function Add(const AClass: TComponentClass; AOwner: TComponent): TDIReg; overload;
    function Add(const AClass: TComponentClass; AOwner: TComponent; AInterface: TGUID): TDIReg; overload;
    function Add(const AClass: TComponentClass; AOwner: TComponent; AInterfaces: array of TGUID): TDIReg; overload;
  end;

  TReg<T> = class(TDIReg)
  public
    class function Locate(AContainer: TDIContainer): T;
  end;

implementation

type
  ITestik = interface
  ['{95162F78-28CF-465F-ACD1-47C71EF24297}']
  end;

  TTestO = class
  end;

procedure test;
var
  mO: TTestO;
  mi: ITestik;
  mdi: TDIContainer;
begin
  mO := TReg<TTestO>.Locate(mdi);
  mi := TReg<ITestik>.Locate(mdi);

  mO := mdi.Locate(TTestO);
  mi := mdi.Locate(ITestik);
end;

{ TReg<T> }

class function TReg<T>.Locate(AContainer: TDIContainer): T;
begin
end;

{ TDI_TDIObjectReg }

function TDI_TDIObjectReg.NewObject: TObject;
begin
  Result := fClass.Create;
end;

function TDI_TDIObjectReg.Instantiate(AClass: TClass): Boolean;
begin
  Result := AClass = fClass;
end;

constructor TDI_TDIObjectReg.Create(AClass: TDIClass;
  AInterfaces: array of TGUID);
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

function TDI_TObjectReg.Instantiate(AClass: TClass): Boolean;
begin
  Result := fClass = AClass;
end;

constructor TDI_TObjectReg.Create(AClass: TClass; AInterfaces: array of TGUID);
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

function TDI_TComponentReg.Instantiate(AClass: TClass): Boolean;
begin
  Result := fClass = AClass;
end;

constructor TDI_TComponentReg.Create(AClass: TComponentClass; AOwner: TComponent;
  AInterfaces: array of TGUID);
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

function TDIReg.TInjectProp.Create(const AName: string; AValue: TClass
  ): TInjectProp;
begin
  Result.Name := AName;
  Result.ValueCLass := AValue;
end;

function TDIReg.TInjectProp.Create(const AName: string; const AValue: TGuid
  ): TInjectProp;
begin
  Result.Name := AName;
  Result.VGuid := AValue;
end;

class operator TDIReg.TInjectProp. = (a, b: TInjectProp): Boolean;
begin
  Result := (a.Name = b.Name) and (a.Value = b.Value) and (a.ValueClass = b.ValueClass);
end;

{ TDIReg }

function TDIReg.Implements(AIntf: TGuid): Boolean;
begin
  Result := fImplementations.IndexOf(GUIDToString(AIntf)) > -1;
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
          mObject := fDIC.Locate(mIP.ValueClass);
          mRBItem.AsObject := mObject;
        end;
      tkInterface:
        begin
          mInterface := fDIC.Locate(mIP.VGuid);
          mRBItem.AsInterface := mInterface;
        end;
    else
      mRBItem.AsVariant := mIP.Value;
    end;

  end;
end;

procedure TDIReg.AddSupportedInterfaces(AInterfaces: array of TGUID);
var
  mGuid: TGUID;
begin
  for mGuid in AInterfaces do
  begin
    fImplementations.Add(GUIDToString(mGuid));
  end;
end;

constructor TDIReg.Create;
begin
  fImplementations := TImplementations.Create;
  fInjectProps := TInjectProps.Create;
end;

destructor TDIReg.Destroy;
begin
  if fSingleObject <> nil then
  begin
    if fSingleHold then
      (fSingleObject as IUnknown)._Release
    else
      FreeAndNil(fSingleObject);
  end;
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
  end;
end;

function TDIReg.Make(AInterface: TGUID): pointer;
var
  m: TObject;
begin
  if not Implements(AInterface) then
    raise Exception.Create('not registered to implement interface');
  Result := nil;

  if (fCreateKind = ckSingle) and (fSingleObject <> nil) then
  begin

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

procedure TDIReg.InjectProp(const AName: string; AValue: TClass);
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue));
end;

procedure TDIReg.InjectProp(const AName: string; const AValue: TGuid);
begin
  fInjectProps.Add(TInjectProp.Create(AName, AValue));
end;

{ TDIContainer }

function TDIContainer.Find(AClass: TClass): TDIReg;
var
  mReg: TDIReg;
begin
  Result := nil;
  for mReg in fRegs do
  begin
    if mReg.Instantiate(AClass) then
    begin
      Result := mReg;
      Break;
    end;
  end;
end;

function TDIContainer.Find(AInterface: TGUID): TDIReg;
var
  mReg: TDIReg;
begin
  Result := nil;
  for mReg in fRegs do
  begin
    if mReg.Implements(AInterface) then
    begin
      Result := mReg;
      Break;
    end;
  end;
end;

procedure TDIContainer.CheckRegNotExists(AClass: TClass);
begin
  if Find(AClass) <> nil then
    raise Exception.Create('already registered');
end;

function TDIContainer.RegisterReg(AReg: TDIReg): TDIReg;
begin
  Result := AReg;
  fRegs.Add(Result);
  Result.fDIC := Self;
end;

function TDIContainer.NewReg(AClass: TClass; AInterfaces: array of TGUID
  ): TDIReg;
begin
  CheckRegNotExists(AClass);
  Result := RegisterReg(TDI_TObjectReg.Create(AClass, AInterfaces));
end;

function TDIContainer.NewReg(AClass: TDIClass; AInterfaces: array of TGUID
  ): TDIReg;
begin
  CheckRegNotExists(AClass);
  Result := RegisterReg(TDI_TDIObjectReg.Create(AClass, AInterfaces));
end;

function TDIContainer.NewReg(AClass: TComponentClass; AOwner: TComponent;
  AInterfaces: array of TGUID): TDIReg;
begin
  CheckRegNotExists(AClass);
  Result := RegisterReg(TDI_TComponentReg.Create(AClass, AOwner, AInterfaces));
end;

constructor TDIContainer.Create;
begin
  fRegs := TDIRegs.Create;
end;

destructor TDIContainer.Destroy;
begin
  FreeAndNil(fRegs);
  inherited Destroy;
end;

function TDIContainer.Locate(AClass: TClass): pointer;
var
  mReg: TDIReg;
begin
  mReg := Find(AClass);
  if mReg = nil then
    raise Exception.Create('registration not found');
  Result := mReg.Make;
end;

function TDIContainer.Locate(AInterface: TGUID): pointer;
var
  mReg: TDIReg;
begin
  mReg := Find(AInterface);
  if mReg = nil then
    raise Exception.Create('registration not found');
  Result := mReg.Make(AInterface);
end;

function TDIContainer.Add(const AClass: TClass): TDIReg;
begin
  Result := NewReg(AClass, []);
end;

function TDIContainer.Add(const AClass: TClass; AInterface: TGUID): TDIReg;
begin
  Result := NewReg(AClass, [AInterface]);
end;

function TDIContainer.Add(const AClass: TClass; AInterfaces: array of TGUID): TDIReg;
begin
  Result := NewReg(AClass, AInterfaces);
end;

function TDIContainer.Add(const AClass: TDIClass): TDIReg;
begin
  Result := NewReg(AClass, []);
end;

function TDIContainer.Add(const AClass: TDIClass; AInterface: TGUID): TDIReg;
begin
  Result := NewReg(AClass, [AInterface]);
end;

function TDIContainer.Add(const AClass: TDIClass; AInterfaces: array of TGUID
  ): TDIReg;
begin
  Result := NewReg(AClass, AInterfaces);
end;

function TDIContainer.Add(const AClass: TComponentClass; AOwner: TComponent
  ): TDIReg;
begin
  Result := NewReg(AClass, AOwner, []);
end;

function TDIContainer.Add(const AClass: TComponentClass; AOwner: TComponent;
  AInterface: TGUID): TDIReg;
begin
  Result := NewReg(AClass, AOwner, [AInterface]);
end;

function TDIContainer.Add(const AClass: TComponentClass; AOwner: TComponent;
  AInterfaces: array of TGUID): TDIReg;
begin
  Result := NewReg(AClass, AOwner, AInterfaces);
end;

end.


unit rtti_serializer_uFactory;

interface

uses
  Classes, SysUtils, rtti_broker_iBroker, fgl, typinfo;

type

  { ECacheException }

  ECacheException = class(Exception)
  public
    class procedure ClassAlreadyRegistered(const AClassName: string);
    class procedure ClassNotRegistered(const AClassName: string);
    class procedure ObjectInstanceHasNoIDProperty(const AObject: TObject);
    class procedure ObjectWithIDAlreadyExists(AObject: TObject; AID: integer);
  end;


  { TClassCache }

  TClassCache = class
  private type
    TCache = specialize TFPGMap<string, TClass>;
  private
    fCache: TCache;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AClass: TClass);
    function Find(const AClass: string): TClass;
  end;

  { TObjectCache }

  TObjectCache = class
  private type
    TCache = specialize TFPGMap<integer, TObject>;
  private
    fCache: TCache;
  protected
     function GetID(AObject: TObject): integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AObject: TObject);
    function Find(AID: integer): TObject;
  end;

 { TSerialFactory }

  TSerialFactory = class(TInterfacedObject, IRBFactory)
  private
    fClassCache: TClassCache;
    procedure SetAddClass(AValue: TClass);
  protected
    function GetClass(const AClass: string): TClass;
    // IRBFactory
    procedure RegisterClass(const AClass: TClass);
    function CreateObject(const AClass: string): TObject;
    function FindClass(const AClass: String): TClass;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  published
    property AddClass: TClass write SetAddClass;
  end;


implementation

{ TObjectCache }

function TObjectCache.GetID(AObject: TObject): integer;
var
  mIDProp: PPropInfo;
begin
  mIDProp := GetPropInfo(AObject, 'ID');
  if mIDProp <> nil then
    Result := GetPropValue(AObject, 'ID')
  else
    ECacheException.ObjectInstanceHasNoIDProperty(AObject);
end;

constructor TObjectCache.Create;
begin
  fCache := TCache.Create;
end;

destructor TObjectCache.Destroy;
begin
  FreeAndNil(fCache);
  inherited Destroy;
end;

procedure TObjectCache.Add(AObject: TObject);
var
  mID: integer;
begin
  mID := GetID(AObject);
  if Find(mID) <> nil then
    ECacheException.ObjectWithIDAlreadyExists(AObject, mID);
  fCache.Add(mID, AObject);
end;

function TObjectCache.Find(AID: integer): TObject;
var
  mIndex: integer;
begin
  mIndex := fCache.IndexOf(AID);
  if mIndex > -1 then
    Result := fCache.Data[mIndex]
  else
    Result := nil;
end;

{ ECacheException }

class procedure ECacheException.ClassAlreadyRegistered(
  const AClassName: string);
begin
  raise CreateFmt('Class %s is already registered', [AClassName]);
end;

class procedure ECacheException.ClassNotRegistered(const AClassName: string);
begin
  raise CreateFmt('Cannot create object - class %s is not registered', [AClassName]);
end;

class procedure ECacheException.ObjectInstanceHasNoIDProperty(
  const AObject: TObject);
begin
  raise CreateFmt('Cannot set ID - class %s do not have implemented ID property', [AObject.ClassName]);
end;

class procedure ECacheException.ObjectWithIDAlreadyExists(AObject: TObject;
  AID: integer);
begin
  raise CreateFmt('Object %s with ID=%d already exists', [AObject.ClassName, AID]);
end;

{ TSerialFactory }

procedure TSerialFactory.SetAddClass(AValue: TClass);
begin
  RegisterClass(AValue);
end;

function TSerialFactory.GetClass(const AClass: string): TClass;
begin
  Result := FindClass(AClass);
  if Result = nil then
    ECacheException.ClassNotRegistered(AClass);
end;

procedure TSerialFactory.RegisterClass(const AClass: TClass);
begin
  fClassCache.Add(AClass);
end;

function TSerialFactory.CreateObject(const AClass: string): TObject;
var
  m: TClass;
begin
  m := GetClass(AClass);
  Result := m.Create
end;

function TSerialFactory.FindClass(const AClass: String): TClass;
begin
  Result := fClassCache.Find(AClass);
end;

constructor TSerialFactory.Create;
begin
end;

destructor TSerialFactory.Destroy;
begin
  inherited Destroy;
end;

procedure TSerialFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  fClassCache := TClassCache.Create;
end;

procedure TSerialFactory.BeforeDestruction;
begin
  FreeAndNil(fClassCache);
  inherited BeforeDestruction;
end;

{ TClassCache }

constructor TClassCache.Create;
begin
  fCache := TCache.Create;
end;

destructor TClassCache.Destroy;
begin
  FreeAndNil(fCache);
  inherited Destroy;
end;

procedure TClassCache.Add(AClass: TClass);
begin
  if Find(AClass.ClassName) <> nil then
    ECacheException.ClassAlreadyRegistered(AClass.ClassName);
  fCache.Add(AClass.ClassName, AClass);
end;

function TClassCache.Find(const AClass: string): TClass;
var
  mIndex: integer;
begin
  mIndex := fCache.IndexOf(AClass);
  if mIndex > -1 then
    Result := fCache.Data[mIndex]
  else
    Result := nil;
end;

end.


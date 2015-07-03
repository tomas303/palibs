unit rtti_serializer_uManager;

interface

uses
  SysUtils, rtti_broker_iBroker, rtti_broker_uData,
  fgl, rtti_serializer_uFactory, rtti_serializer_uSerialObject;

//type

  { TSerialManager }

  //TSerialManager = class(TInterfacedObject, ISerialManager)
  //private
  //  const
  //    cCounterID = 0;
  //private type
  //
  //  { TCounter }
  //
  //  TCounter = class(TRBCustomIDObject)
  //  private
  //    fIDCounter: integer;
  //    function GetIDCounter: integer;
  //    procedure SetIDCounter(AValue: integer);
  //  public
  //    constructor Create; override;
  //    function NewID: integer;
  //  published
  //    property IDCounter: integer read GetIDCounter write SetIDCounter;
  //  end;
  //
  //private
  //  fFactory: ISerialFactory;
  //  fStore: ISerialStore;
  //  fCounter: TCounter;
  //protected
  //  procedure Save(AData: TObject);
  //  function Load(const AClass: string; const AID: integer; ACanCreate: Boolean = True): TObject; overload;
  //  function Load(const AClass: TClass; const AID: integer; ACanCreate: Boolean = True): TObject; overload;
  //  function New(const AClass: string): TObject; overload;
  //  function New(const AClass: TClass): TObject; overload;
  //  function New(const AClass: string; const AID: integer): TObject; overload;
  //  function New(const AClass: TClass; const AID: integer): TObject; overload;
  //  function Exists(const AID: integer): Boolean;
  //  function LoadList(const AClass: string): ISerialList;
  //  function NewID: integer;
  //  procedure Flush;
  //  procedure RegisterClass(const AClass: TClass);
  //public
  //  constructor Create(const AConnection: string);
  //  destructor Destroy; override;
  //end;

implementation

type

  { ESerialException }

  ESerialException = class(Exception)
  public
    class procedure ClassNotRegistered(const AClassName: string);
    class procedure ClassAlreadyRegistered(const AClassName: string);
    class procedure ObjectAlreadyExistsInCache(const AClass, AName: string; AID: integer);
    class procedure TryLoadObjectWithNotExistsID(const AClass: string; AID: integer);
    class procedure TryCreateObjectWhichDoNotHaveID(const AClass: string; AID: integer);
  end;

{ ESerialException }

class procedure ESerialException.ClassNotRegistered(const AClassName: string);
begin
  raise ESerialException.CreateFmt('Cannot create object - class %s is not registered', [AClassName]);
end;

class procedure ESerialException.ClassAlreadyRegistered(const AClassName: string);
begin
  raise ESerialException.CreateFmt('Class %s is already registered', [AClassName]);
end;

class procedure ESerialException.ObjectAlreadyExistsInCache(const AClass,
  AName: string; AID: integer);
begin
  raise ESerialException.CreateFmt('Object %s.%s (ID=%d) already exists in cache', [AClass, AName, AID]);
end;

class procedure ESerialException.TryLoadObjectWithNotExistsID(const AClass: string;
  AID: integer);
begin
  raise ESerialException.CreateFmt('Try load not exists object of %s with ID=%d', [AClass, AID]);
end;

class procedure ESerialException.TryCreateObjectWhichDoNotHaveID(
  const AClass: string; AID: integer);
begin
  raise ESerialException.CreateFmt('Try create object of %s with ID=%d, but %s do not have ID property', [AClass, AID, AClass]);
end;


//{ TSerialManager.TCounter }
//
//function TSerialManager.TCounter.GetIDCounter: integer;
//begin
//  Result := fIDCounter;
//end;
//
//procedure TSerialManager.TCounter.SetIDCounter(AValue: integer);
//begin
//  fIDCounter := AValue;
//end;
//
//constructor TSerialManager.TCounter.Create;
//begin
//  fIDCounter := 0;
//  ID := cCounterID;
//end;
//
//function TSerialManager.TCounter.NewID: integer;
//begin
//  inc(fIDCounter);
//  Result := fIDCounter;
//end;
//
//{ TSerialManager }
//
//procedure TSerialManager.Save(AData: TObject);
//begin
//  fStore.Save(AData as IRBData);
//end;
//
//function TSerialManager.Load(const AClass: string; const AID: integer;
//  ACanCreate: Boolean = True): TObject;
//begin
//  if fFactory.Exists(AID) then begin
//    Result := fFactory.Get(AID);
//  end else if fStore.Exists(AID) then begin
//    Result := fFactory.New(AClass, AID);
//    fStore.Load(Result as IRBData);
//  end else if ACanCreate then begin
//    Result := fFactory.New(AClass, AID);
//  end else begin
//    ESerialException.TryLoadObjectWithNotExistsID(AClass, AID);
//  end;
//end;
//
//function TSerialManager.Load(const AClass: TClass; const AID: integer;
//  ACanCreate: Boolean = True): TObject;
//begin
//  Result := Load(AClass.ClassName, AID, ACanCreate);
//end;
//
//function TSerialManager.New(const AClass: string): TObject;
//begin
//  if fFactory.IsIDClass(AClass) then
//    Result := fFactory.New(AClass, NewID)
//  else
//    Result := fFactory.New(AClass);
//end;
//
//function TSerialManager.New(const AClass: TClass): TObject;
//begin
//  Result := New(AClass.ClassName);
//end;
//
//function TSerialManager.New(const AClass: string; const AID: integer): TObject;
//begin
//  if not fFactory.IsIDClass(AClass) then
//    ESerialException.TryCreateObjectWhichDoNotHaveID(AClass, AID);
//  Result := fFactory.New(AClass, AID);
//end;
//
//function TSerialManager.New(const AClass: TClass; const AID: integer): TObject;
//begin
//  Result := New(AClass.ClassName, AID);
//end;
//
//function TSerialManager.Exists(const AID: integer): Boolean;
//begin
//  Result := fFactory.Exists(AID);
//  if not Result then
//    Result := fStore.Exists(AID);
//end;
//
//function TSerialManager.LoadList(const AClass: string): ISerialList;
//begin
//  Result := TSerialList.Create(AClass);
//  fStore.LoadList(Result);
//end;
//
//function TSerialManager.NewID: integer;
//begin
//  if fCounter = nil then
//    Result := -1
//  else
//    Result := fCounter.NewID;
//end;
//
//procedure TSerialManager.Flush;
//begin
//  fStore.Flush;
//end;
//
//procedure TSerialManager.RegisterClass(const AClass: TClass);
//begin
//  fFactory.RegisterClass(AClass);
//end;
//
//constructor TSerialManager.Create(const AConnection: string);
//begin
//  fFactory := TSerialFactory.Create;
//  fStore := TXmlStore.Create(fFactory, AConnection);
//  //counter will not registry
//  fCounter := TCounter.Create;
//  fCounter.ID := cCounterID;
//  fStore.Load(fCounter as IRBData);
//end;
//
//destructor TSerialManager.Destroy;
//begin
//  fStore.Save(fCounter as IRBData);
//  fStore := nil;
//  FreeAndNil(fCounter);  // not in cache
//  inherited Destroy;
//end;

end.

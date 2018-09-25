unit trl_dicontainer_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, trl_dicontainer, sysutils, trl_iprops, trl_uprops,
  trl_uinterfacedownedobject;

type

  { IDummy }

  IDummy = interface
  ['{07BB96F1-787B-4621-9945-DA87F3B5634D}']
    function GetRefCount: LongInt;
    function GetDummy1: IDummy;
  end;

  { IDummy1 }

  IDummy1 = interface
  ['{F0EED38F-2CFF-4E1E-81FA-5D6FFD051898}']
  end;

  { IDummy1 }

  IDummy2 = interface
  ['{93849999-B113-41EB-B7BD-264FEF5126A8}']
  end;

  { ISomeService }

  ISomeService = interface
  ['{D9F904D0-F809-4868-8215-C6C14579034D}']
  end;

  { TSomeService }

  TSomeService = class(TInterfacedOwnedObject, ISomeService)
  end;


  { TDummy }

  TDummy = class(TInterfacedObject, IDummy, IDummy1, ISomeService)
  protected class var
    fInstances: integer;
  protected
    function GetRefCount: LongInt;
    function GetDummy1: IDummy;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
  protected
    fDummy1: IDummy;
    fSomeService: ISomeService;
    property SomeServiceIMPL: ISomeService read fSomeService implements ISomeService;
  published
    property Dummy1: IDummy read fDummy1 write fDummy1;
    property SomeService: ISomeService read fSomeService write fSomeService;
  end;

  { TDIRegTests }

  TDIRegTests = class(TTestCase)
  private type

    { TTestingDIReg }

    TTestingDIReg = class(TDIReg)
    protected
      function InstantiatedClass: TClass; override;
    end;

    { TTestingDIContainer }

    TTestingDIContainer = class(TDIContainer)
    end;

  private
    fDIReg: TTestingDIReg;
    fDIContainer: TTestingDIContainer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInject_InterfaceOwned;
  end;

  { TDIContainerTests }

  TDIContainerTests = class(TTestCase)
  private const
    cID = 'testid';
  protected
    fDIContainer: TDIContainer;
    procedure SetUp; override;
    procedure TearDown; override;
    function MakeDummy1: IDummy;
    function MakeDummy2: IDummy;
    function MakeDummy3: IDummy;
    function MakeDummy4: IDummy;
    function MakeSetIntf: IDummy;
  published
    procedure TestLocate1;
    procedure TestLocate2;
    procedure TestLocate3;
    procedure TestLocate4;
    procedure TestSetIntf;
    procedure TestLocate_InterfaceObject;
    procedure TestLocate_InterfaceObjectWithProps;
    procedure TestCanLocateAs1;
    procedure TestCanLocateAs2;
    procedure TestCanLocateAs3;
    procedure TestCanLocateAs4;
    procedure TestCanLocateAs5;
    procedure TestCanLocateAs6;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TDIRegTests.Suite);
  TestFramework.RegisterTest(TDIContainerTests.Suite);
end;

{ TDIRegTests.TTestingDIReg }

function TDIRegTests.TTestingDIReg.InstantiatedClass: TClass;
begin
  // when add registration, it controls if not exists already with same class
  // so returning nil will effectively make control succeed
  Result := nil;
end;

{ TDIRegTests }

procedure TDIRegTests.SetUp;
begin
  inherited;
  fDIContainer := TTestingDIContainer.Create;
  fDIReg := TTestingDIReg.Create;
  fDIContainer.RegisterReg(fDIReg);
end;

procedure TDIRegTests.TearDown;
begin
  FreeAndNil(fDIReg);
  inherited;
end;

procedure TDIRegTests.TestInject_InterfaceOwned;
var
  mDummy: IDummy;
  mSomeService: ISomeService;
begin
  fDIContainer.Add(TSomeService, ISomeService, '', ckSingle);
  fDIReg.InjectProp('SomeService', ISomeService);
  mDummy := TDummy.Create;
  fDIReg.Inject(mDummy as TObject);
  // 3 is beacause type ... as TObject(aswell as TInterfacedObject) create one temporal reference
  CheckEquals(3, (mDummy as TInterfacedObject).RefCount, 'reference count problem');
  // check that OwningObject was set
  mSomeService := ISomeService(fDIContainer.Locate(ISomeService));
  CheckSame(mDummy as TInterfacedObject, (mSomeService as TInterfacedOwnedObject).OwningObject);
end;

{ TDummy }

function TDummy.GetRefCount: LongInt;
begin
  Result := RefCount;
end;

function TDummy.GetDummy1: IDummy;
begin
  Result := fDummy1;
end;


procedure TDummy.AfterConstruction;
begin
  inherited;
  inc(fInstances);
end;

procedure TDummy.BeforeDestruction;
begin
  dec(fInstances);
  inherited;
end;


destructor TDummy.Destroy;
begin
  inherited Destroy;
end;

{ TDIContainerTests }

procedure TDIContainerTests.SetUp;
begin
  inherited SetUp;
  fDIContainer := TDIContainer.Create;
  fDIContainer.Add(TDummy, IDummy);
  fDIContainer.Add(TDummy, IDummy, cID);
end;

procedure TDIContainerTests.TearDown;
begin
  FreeAndNil(fDIContainer);
  //CheckEquals(0, TDummy.fInstances);
  inherited TearDown;
end;

function TDIContainerTests.MakeDummy1: IDummy;
begin
  Result := IUnknown(fDIContainer.Locate(IDummy)) as IDummy;
end;

function TDIContainerTests.MakeDummy2: IDummy;
begin
  Result := IDummy(fDIContainer.Locate(IDummy));
end;

function TDIContainerTests.MakeDummy3: IDummy;
var
  mUnk: IUnknown;
begin
  mUnk := IUnknown(fDIContainer.Locate(IDummy));
  Result := mUnk as IDummy;
end;

function TDIContainerTests.MakeDummy4: IDummy;
var
  mDummy1: IDummy;
begin
  mDummy1 := TDummy.Create;
  Result := IUnknown(fDIContainer.Locate(IDummy, '', TProps.New.SetIntf('Dummy1', mDummy1))) as IDummy;
end;

function TDIContainerTests.MakeSetIntf: IDummy;
var
  mProps: IProps;
begin
  Result := TDummy.Create;
  mProps := TProps.New;
  mProps.SetIntf('Dummy1', Result);
  mProps := nil;
end;

procedure TDIContainerTests.TestLocate1;
var
  mDummy: IDummy;
begin
  mDummy := MakeDummy1;
  CheckEquals(1, mDummy.GetRefCount);
end;

procedure TDIContainerTests.TestLocate2;
var
  mDummy: IDummy;
begin
  mDummy := MakeDummy2;
  CheckEquals(1, mDummy.GetRefCount);
end;

procedure TDIContainerTests.TestLocate3;
var
  mDummy: IDummy;
begin
  mDummy := MakeDummy3;
  CheckEquals(1, mDummy.GetRefCount);
end;

procedure TDIContainerTests.TestLocate4;
var
  mDummy: IDummy;
begin
  mDummy := MakeDummy4;
  CheckEquals(1, mDummy.GetRefCount);
  CheckEquals(2, mDummy.GetDummy1.GetRefCount);
end;

procedure TDIContainerTests.TestSetIntf;
var
  mDummy: IDummy;
begin
  mDummy := MakeSetIntf;
  CheckEquals(1, mDummy.GetRefCount);
end;

procedure TDIContainerTests.TestLocate_InterfaceObject;
var
  mUnk: IUnknown;
  mDummy: IDummy;
begin
  //mDummy := IDummy(fDIContainer.Locate(IDummy));
  //mDummy := IUnknown(fDIContainer.Locate(IDummy)) as IDummy;
  mUnk := IUnknown(fDIContainer.Locate(IDummy));
  mDummy := mUnk as IDummy;
  //CheckEquals(2, mDummy.GetRefCount);
  //mUnk := nil;
  //CheckEquals(1, mDummy.GetRefCount);
  //mDummy := nil;
end;

procedure TDIContainerTests.TestLocate_InterfaceObjectWithProps;
var
  mDummy, mDummy1: IDummy;

begin
  {
  fMachinery := IUnknown(Factory.Locate(IReactComponentMachineryMiddle, '',
        TProps.New.SetIntf('Component', mComponent)))
        as IReactComponentMachinery;
  }
  mDummy1 := TDummy.Create;
  mDummy := IUnknown(fDIContainer.Locate(IDummy, '', TProps.New.SetIntf('Dummy1', mDummy1))) as IDummy;
  //CheckEquals(1, mDummy.GetRefCount);
  mDummy := nil;
end;

procedure TDIContainerTests.TestCanLocateAs1;
begin
  CheckTrue(fDIContainer.CanLocateAs(TDummy, '', IDummy1));
  CheckFalse(fDIContainer.CanLocateAs(TDummy, '', IDummy2));
end;

procedure TDIContainerTests.TestCanLocateAs2;
begin
  CheckTrue(fDIContainer.CanLocateAs(TDummy, cID, IDummy1));
  CheckFalse(fDIContainer.CanLocateAs(TDummy, cID, IDummy2));
end;

procedure TDIContainerTests.TestCanLocateAs3;
begin
  CheckTrue(fDIContainer.CanLocateAs(IDummy, '', IDummy1));
  CheckFalse(fDIContainer.CanLocateAs(IDummy, '', IDummy2));
end;

procedure TDIContainerTests.TestCanLocateAs4;
begin
  CheckTrue(fDIContainer.CanLocateAs(IDummy, cID, IDummy1));
  CheckFalse(fDIContainer.CanLocateAs(IDummy, cID, IDummy2));
end;

procedure TDIContainerTests.TestCanLocateAs5;
begin
  CheckTrue(fDIContainer.CanLocateAs('TDummy', '', IDummy1));
  CheckFalse(fDIContainer.CanLocateAs('TDummy', '', IDummy2));
end;

procedure TDIContainerTests.TestCanLocateAs6;
begin
  CheckTrue(fDIContainer.CanLocateAs('TDummy', cID, IDummy1));
  CheckFalse(fDIContainer.CanLocateAs('TDummy', cID, IDummy2));
end;

end.


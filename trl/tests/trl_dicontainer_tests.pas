unit trl_dicontainer_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, trl_dicontainer, sysutils, trl_iprops, trl_uprops;

type

  { IDummy }

  IDummy = interface
  ['{07BB96F1-787B-4621-9945-DA87F3B5634D}']
    function GetRefCount: LongInt;
    function GetDummy1: IDummy;
  end;

  { TDummy }

  TDummy = class(TInterfacedObject, IDummy)
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
  published
    property Dummy1: IDummy read fDummy1 write fDummy1;
  end;

  { TDIContainerTests }

  TDIContainerTests = class(TTestCase)
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
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TDIContainerTests.Suite);
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

end.


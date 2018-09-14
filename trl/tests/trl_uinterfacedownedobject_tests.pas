unit trl_uinterfacedownedobject_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, uDICTestCase, trl_uinterfacedownedobject, sysutils,
  trl_iprops, trl_uprops;

type

  ITesting = interface
  ['{7E0875D6-00A9-4122-9BC9-C828535C7807}']
  end;

  ITesting2 = interface
  ['{9F74DDF1-902B-4B00-BBBD-A6702B43D4B5}']
  end;

  ITesting3 = interface
  ['{D29B9741-2343-46DC-A06D-6E462C49007D}']
  end;

  { TTestingObject }

  TTestingObject = class(TInterfacedOwnedObject, ITesting, ITesting2)
  public
    destructor Destroy; override;
    procedure FreeInstance; override;
  end;

  { IOwner }

  IOwner = interface
  ['{3D4DFEF8-CC8A-4F29-B8F3-A6378EE7A9C4}']
  end;

  { TOwner }

  TOwner = class(TInterfacedObject, IOwner)
  protected
    fTesting: ITesting;
    procedure SetTesting(const AValue: ITesting);
  public
    property Testing: ITesting read fTesting write fTesting;
    destructor Destroy; override;
  end;

  { TInterfacedOwnedObjectTests }

  TInterfacedOwnedObjectTests = class(TDICTestCase)
  public class var
    fOwnedDestructorCount: integer;
    fOwningDestructorCount: integer;
  private
    fOwner: IOwner;
    fInterfacedOwnedObject: ITesting;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure ConnectOnwer;
    procedure CheckQueryInterface;
    procedure CheckRefCount(AOwnerCount, AOwnedCount: integer);
  published
    procedure TestQueryInterface_Owned;
    procedure TestQueryInterface_Standalone;
    procedure TestAddRef_Owned;
    procedure TestAddRef_Standalone;
    procedure TestRelease_Owned;
    procedure TestRelease_Standalone;
    procedure TestFreeOwnedObject;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TInterfacedOwnedObjectTests.Suite);
end;

{ TTestingObject }

destructor TTestingObject.Destroy;
begin
  inherited Destroy;
  inc(TInterfacedOwnedObjectTests.fOwningDestructorCount);
end;

procedure TTestingObject.FreeInstance;
begin
  inherited FreeInstance;
end;

{ TOwner }

procedure TOwner.SetTesting(const AValue: ITesting);
begin
  fTesting := AValue;
  // must decrement, because AValue is aggregate about reference counting
  fTesting._Release;
end;

destructor TOwner.Destroy;
begin
  inherited Destroy;
  inc(TInterfacedOwnedObjectTests.fOwnedDestructorCount);
end;

{ TInterfacedOwnedObjectTests }

procedure TInterfacedOwnedObjectTests.SetUp;
begin
  inherited SetUp;
  fOwnedDestructorCount := 0;
  fOwningDestructorCount := 0;
  fOwner := TOwner.Create;
  fInterfacedOwnedObject := TTestingObject.Create;
end;

procedure TInterfacedOwnedObjectTests.TearDown;
begin
  fInterfacedOwnedObject := nil;
  fOwner := nil;
  inherited TearDown;
end;

procedure TInterfacedOwnedObjectTests.ConnectOnwer;
begin
  (fInterfacedOwnedObject as TInterfacedOwnedObject).OwningObject := fOwner as TInterfacedObject;
end;

procedure TInterfacedOwnedObjectTests.CheckQueryInterface;
var
  m: IUnknown;
begin
  CheckEquals(E_NOINTERFACE, fInterfacedOwnedObject.QueryInterface(IOwner, m));
  CheckNull(m);
  CheckEquals(S_OK, fInterfacedOwnedObject.QueryInterface(ITesting2, m));
  CheckNotNull(m);
end;

procedure TInterfacedOwnedObjectTests.CheckRefCount(AOwnerCount,
  AOwnedCount: integer);
begin
  if AOwnerCount <> -1 then
    CheckEquals(AOwnerCount, (fOwner as TInterfacedObject).RefCount, 'owner refcount');
  if AOwnedCount <> -1 then
    CheckEquals(AOwnedCount, (fInterfacedOwnedObject as TInterfacedObject).RefCount, 'owned refcount');
end;

procedure TInterfacedOwnedObjectTests.TestQueryInterface_Owned;
begin
  ConnectOnwer;
  CheckQueryInterface;
end;

procedure TInterfacedOwnedObjectTests.TestQueryInterface_Standalone;
begin
  CheckQueryInterface;
end;

procedure TInterfacedOwnedObjectTests.TestAddRef_Owned;
begin
  CheckRefCount(1, 1);
  ConnectOnwer;
  CheckRefCount(2, 0);
  fInterfacedOwnedObject._AddRef;
  CheckRefCount(3, 0);
end;

procedure TInterfacedOwnedObjectTests.TestAddRef_Standalone;
begin
  CheckRefCount(1, 1);
  fInterfacedOwnedObject._AddRef;
  CheckRefCount(1, 2);
end;

procedure TInterfacedOwnedObjectTests.TestRelease_Owned;
begin
  CheckRefCount(1, 1);
  ConnectOnwer;
  CheckRefCount(2, 0);
  fInterfacedOwnedObject._AddRef;
  CheckRefCount(3, 0);
  fInterfacedOwnedObject._Release;
  CheckRefCount(2, 0);
end;

procedure TInterfacedOwnedObjectTests.TestRelease_Standalone;
begin
  CheckRefCount(1, 1);
  fInterfacedOwnedObject._AddRef;
  CheckRefCount(1, 2);
  fInterfacedOwnedObject._Release;
  CheckRefCount(1, 1);
end;

procedure TInterfacedOwnedObjectTests.TestFreeOwnedObject;
begin
  CheckRefCount(1, 1);
  ConnectOnwer;
  CheckRefCount(2, 0);
  (fOwner as TOwner).Testing := fInterfacedOwnedObject;
  // must decrement to protect agains lock interfaces, fOwner and fInterfacedOwnedObject
  // use now one counter and fInterfacedOwnedObject destroy iteself when fOwner is destroyed
  fInterfacedOwnedObject._Release;
  //
  CheckRefCount(2, 0);
  fInterfacedOwnedObject := nil;
  CheckRefCount(1, -1);
  fOwner := nil;
  CheckEquals(1, fOwnedDestructorCount, 'owner count');
  CheckEquals(1, fOwningDestructorCount, 'owned count');
end;

end.


unit trl_umetaelement_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, uDICTestCase, SysUtils, trl_imetaelement, trl_umetaelement, trl_ilog,
  trl_dicontainer,
  trl_itree, trl_utree,
  trl_iinjector, trl_uinjector,
  trl_iprops, trl_uprops;

type

  { TMetaElementTests }

  TMetaElementTests = class(TDICTestCase)
  protected
    fMetaElement: IMetaElement;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLogInfo;
    procedure TestIsMetaElementProvider;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TMetaElementTests.Suite);
end;

{ TMetaElementTests }

procedure TMetaElementTests.SetUp;
var
  mReg: TDIReg;
begin
  inherited SetUp;
  DIC.Add(TParentNode, INode, 'parent');
  mReg := DIC.Add(TMetaElement, IMetaElement);
  mReg.InjectProp('Node', INode, 'parent');
  fMetaElement := IMetaElement(DIC.Locate(IMetaElement));
end;

procedure TMetaElementTests.TearDown;
begin
  fMetaElement := nil;
  inherited TearDown;
end;

procedure TMetaElementTests.TestLogInfo;
begin
  CheckEquals('()', (fMetaElement as ILogSupport).LogInfo);
end;

procedure TMetaElementTests.TestIsMetaElementProvider;
begin
  CheckFalse(fMetaElement.IsMetaElementProvider);
  WriteProp(fMetaElement, 'IsMetaElementProvider', True);
  CheckTrue(fMetaElement.IsMetaElementProvider);
end;

end.


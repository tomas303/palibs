unit trl_umetaelement_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, uDICTestCase, SysUtils, trl_imetaelement, trl_umetaelement, trl_ilog,
  trl_itree, trl_utree, trl_dicontainer;

type

  { TMetaElementTests }

  TMetaElementTests = class(TDICTestCase)
  protected
    fMetaElement: IMetaElement;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLogInfo;
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

end.


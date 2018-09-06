unit trl_umetaelementfactory_tests;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, uDICTestCase, SysUtils,
  trl_imetaelementfactory, trl_umetaelementfactory,
  trl_imetaelement, trl_umetaelement,
  trl_itree, trl_utree,
  trl_dicontainer,
  trl_iprops, trl_uprops;

  //trl_ilog,
  //trl_dicontainer,
  //trl_itree, trl_utree,
  //trl_iinjector, trl_uinjector,
  //;

type

  IDummyObject = interface
  ['{70D5A48E-1E40-450E-B410-F170B05B83A0}']
  end;

  TDummyObject = class(TInterfacedObject, IDummyObject)
  end;

  { TMetaElementFactoryTests }

  TMetaElementFactoryTests = class(TDICTestCase)
  protected const
    cTypeID = 'DummyID';
  protected
    fMetaElementFactory: IMetaElementFactory;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckMetaElement(const AElement: IMetaElement; AGuid: TGuid;
      const AID: string; const AProps: IProps; const AChildren: array of IMetaElement;
      AIsMetaElementProvider: Boolean);
  published
    procedure TestCreateElement_1;
    procedure TestCreateElement_2;
    procedure TestCreateElement_3;
    procedure TestCreateElement_4;
    procedure TestCreateElement_5;
    procedure TestCreateElement_6;
    procedure TestCreateElement_7;
    procedure TestCreateElement_8;
  end;

procedure RegisterTests;

implementation

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TMetaElementFactoryTests.Suite);
end;

{ TMetaElementFactoryTests }

procedure TMetaElementFactoryTests.SetUp;
var
  mReg: TDIReg;
begin
  inherited SetUp;
  DIC.Add(TParentNode, INode, 'parent');
  mReg := DIC.Add(TMetaElement, IMetaElement);
  mReg.InjectProp('Node', INode, 'parent');
  DIC.Add(TDummyObject, IDummyObject);
  mReg := DIC.Add(TMetaElementFactory, IMetaElementFactory);
  mReg.InjectProp('Container', TDIContainer, '', DIC);
  fMetaElementFactory := IMetaElementFactory(DIC.Locate(IMetaElementFactory));
end;

procedure TMetaElementFactoryTests.TearDown;
begin
  fMetaElementFactory := nil;
  inherited TearDown;
end;

procedure TMetaElementFactoryTests.CheckMetaElement(const AElement: IMetaElement;
  AGuid: TGuid; const AID: string; const AProps: IProps;
  const AChildren: array of IMetaElement; AIsMetaElementProvider: Boolean);
var
  mEl: IMetaElement;
  i: integer;
begin
  CheckNotNull(AElement, 'Element is null');
  CheckEquals(AGuid, AElement.Guid, 'GUID of Element is different');
  CheckEquals(GUIDToString(AGuid), AElement.TypeGuid, 'Type GUID of Element is different');
  if AProps = nil then
    CheckNotNull(AElement.Props, 'Element.Props shouldn''t be nil')
  else
    CheckSame(AProps, AElement.Props, 'Element.Props unexpected value');
  i := 0;
  for mEl in AElement do
  begin
    if i <= High(AChildren) then
    begin
      CheckSame(mEl, AChildren[i], 'expeceted child''s element differ from enumerated one');
    end;
    inc(i);
  end;
  CheckEquals(Length(AChildren), i, 'expected number of children differs from enumerated ones');
  CheckEquals(AIsMetaElementProvider, AElement.IsMetaElementProvider, 'property IsMetaElementProvider');
end;

procedure TMetaElementFactoryTests.TestCreateElement_1;
var
  mElement: IMetaElement;
begin
  mElement := fMetaElementFactory.CreateElement(IDummyObject, True);
  CheckMetaElement(mElement, IDummyObject, '', nil, [], True);
end;

procedure TMetaElementFactoryTests.TestCreateElement_2;
var
  mElement: IMetaElement;
  mProps: IProps;
begin
  mProps := TProps.New;
  mElement := fMetaElementFactory.CreateElement(IDummyObject, mProps, True);
  CheckMetaElement(mElement, IDummyObject, '', mProps, [], True);
end;

procedure TMetaElementFactoryTests.TestCreateElement_3;
var
  mElement, mEl1, mEl2: IMetaElement;
begin
  mElement := fMetaElementFactory.CreateElement(IDummyObject, [], True);
  CheckMetaElement(mElement, IDummyObject, '', nil, [], True);
  //
  mEl1 := IMetaElement(DIC.Locate(IMetaElement));
  mEl2 := IMetaElement(DIC.Locate(IMetaElement));
  mElement := fMetaElementFactory.CreateElement(IDummyObject, [mEl1, mEl2], True);
  CheckMetaElement(mElement, IDummyObject, '', nil, [mEl1, mEl2], True);
end;

procedure TMetaElementFactoryTests.TestCreateElement_4;
var
  mElement, mEl1, mEl2: IMetaElement;
  mProps: IProps;
begin
  mProps := TProps.New;
  mElement := fMetaElementFactory.CreateElement(IDummyObject, mProps, [], True);
  CheckMetaElement(mElement, IDummyObject, '', mProps, [], True);
  //
  mEl1 := IMetaElement(DIC.Locate(IMetaElement));
  mEl2 := IMetaElement(DIC.Locate(IMetaElement));
  mElement := fMetaElementFactory.CreateElement(IDummyObject, mProps, [mEl1, mEl2], True);
  CheckMetaElement(mElement, IDummyObject, '', mProps, [mEl1, mEl2], True);
end;

procedure TMetaElementFactoryTests.TestCreateElement_5;
var
  mElement: IMetaElement;
begin
  mElement := fMetaElementFactory.CreateElement(IDummyObject, cTypeID, True);
  CheckMetaElement(mElement, IDummyObject, cTypeID, nil, [], True);
end;

procedure TMetaElementFactoryTests.TestCreateElement_6;
var
  mElement: IMetaElement;
  mProps: IProps;
begin
  mProps := TProps.New;
  mElement := fMetaElementFactory.CreateElement(IDummyObject, cTypeID, mProps, True);
  CheckMetaElement(mElement, IDummyObject, cTypeID, mProps, [], True);
end;

procedure TMetaElementFactoryTests.TestCreateElement_7;
var
  mElement, mEl1, mEl2: IMetaElement;
begin
  mElement := fMetaElementFactory.CreateElement(IDummyObject, cTypeID, [], True);
  CheckMetaElement(mElement, IDummyObject, cTypeID, nil, [], True);
  //
  mEl1 := IMetaElement(DIC.Locate(IMetaElement));
  mEl2 := IMetaElement(DIC.Locate(IMetaElement));
  mElement := fMetaElementFactory.CreateElement(IDummyObject, cTypeID, [mEl1, mEl2], True);
  CheckMetaElement(mElement, IDummyObject, cTypeID, nil, [mEl1, mEl2], True);
end;

procedure TMetaElementFactoryTests.TestCreateElement_8;
var
  mElement, mEl1, mEl2: IMetaElement;
  mProps: IProps;
begin
  mProps := TProps.New;
  mElement := fMetaElementFactory.CreateElement(IDummyObject, cTypeID, mProps, [], True);
  CheckMetaElement(mElement, IDummyObject, cTypeID, mProps, [], True);
  //
  mEl1 := IMetaElement(DIC.Locate(IMetaElement));
  mEl2 := IMetaElement(DIC.Locate(IMetaElement));
  mElement := fMetaElementFactory.CreateElement(IDummyObject, cTypeID, mProps, [mEl1, mEl2], True);
  CheckMetaElement(mElement, IDummyObject, cTypeID, mProps, [mEl1, mEl2], True);
end;

end.


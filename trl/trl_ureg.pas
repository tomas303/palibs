unit trl_ureg;

{$mode objfpc}{$H+}

interface

uses
  trl_ireg, trl_dicontainer,
  trl_iinjector, trl_uinjector,
  trl_iprops, trl_uprops,
  trl_itree, trl_utree,
  trl_idifactory, trl_udifactory,
  trl_isysutils, trl_usysutils,
  trl_iExecutor, trl_uExecutor,
  trl_imetaelement, trl_umetaelement,
  trl_imetaelementfactory, trl_umetaelementfactory,
  trl_ilink, trl_ulink,
  trl_ilog,
  trl_ireconciler, trl_ureconciler,
  trl_inexus, trl_unexus,
  trl_isequence, trl_usequence;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterSysUtils: TDIReg;
    function RegisterDIOwner: TDIReg;
    function RegisterDIFactory: TDIReg;
    function RegisterInjector: TDIReg;
    function RegisterProps: TDIReg;
    procedure RegisterTreeNodes;
    procedure RegisterLink;
    function RegisterExecutor(const AID: string = ''): TDIReg;
    function RegisterElement: TDIReg;
    function RegisterElementFactory: TDIReg;
    function RegisterReconciler: TDIReg;
    function RegisterNexus: TDIReg;
    function RegisterSequence(const AID: string; ACreateKind: TDIRegCreateKind = ckSingle): TDIReg;
    procedure RegisterCommon;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write fDIC;
  end;

implementation

{ TReg }

function TReg.RegisterDIOwner: TDIReg;
begin
  Result := DIC.Add(TDIOwner, '', ckSingle);
end;

function TReg.RegisterDIFactory: TDIReg;
begin
  Result := DIC.Add(TDIFactory, IDIFactory);
  Result.InjectProp('Container', TDIContainer, '', DIC);
end;

function TReg.RegisterInjector: TDIReg;
begin
  Result := DIC.Add(TInjector, IInjector);
end;

function TReg.RegisterProps: TDIReg;
begin
  Result := DIC.Add(TProps, IProps);
end;

function TReg.RegisterSysUtils: TDIReg;
begin
  Result := DIC.Add(TSysUtils, ISysUtils);
end;

procedure TReg.RegisterTreeNodes;
begin
  //todo: instead constants differ by interfaces ... if possible, try to make empty descendant's interfaces
  DIC.Add(TParentNode, INode, 'parent');
  DIC.Add(TLeafNode, INode, 'leaf');
end;

procedure TReg.RegisterLink;
begin
  DIC.Add(TLink, ILink);
end;

function TReg.RegisterExecutor(const AID: string = ''): TDIReg;
begin
  Result := DIC.Add(TExecutor, IExecutor, AID, ckSingle);
  Result.InjectProp('Log', ILog);
end;

function TReg.RegisterElement: TDIReg;
begin
  Result := DIC.Add(TMetaElement, IMetaElement);
  Result.InjectProp('Node', INode, 'parent');
end;

function TReg.RegisterElementFactory: TDIReg;
begin
  Result := DIC.Add(TMetaElementFactory, IMetaElementFactory);
  Result.InjectProp('Container', TDIContainer, '', DIC);
  Result.InjectProp('Log', ILog);
end;

function TReg.RegisterReconciler: TDIReg;
begin
  Result := DIC.Add(TReconciler, IReconciler);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Injector', IInjector);
end;

function TReg.RegisterNexus: TDIReg;
begin
  Result := DIC.Add(TNexus, INexus);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Reconciler', IReconciler);
end;

function TReg.RegisterSequence(const AID: string; ACreateKind: TDIRegCreateKind = ckSingle): TDIReg;
begin
  Result := DIC.Add(TSequence, ISequence, AID, ACreateKind);
end;

procedure TReg.RegisterCommon;
begin
  RegisterSysUtils;
  RegisterDIOwner;
  RegisterDIFactory;
  RegisterInjector;
  RegisterProps;
  RegisterTreeNodes;
  RegisterLink;
  RegisterExecutor;
  RegisterElement;
  RegisterElementFactory;
  RegisterReconciler;
  RegisterNexus;
end;

end.


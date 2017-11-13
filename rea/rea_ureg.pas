unit rea_ureg;

{$mode objfpc}{$H+}

interface

uses
  rea_ireg, Classes, SysUtils, trl_dicontainer, trl_ilog, trl_idifactory,
  trl_itree, trl_iinjector,
  rea_ilayout, rea_ulayout,
  rea_ibits, rea_ubits,
  rea_ireact, rea_ureact,
  rdx_uredux, // todo: temporarily
  flu_iflux,
  Forms, StdCtrls;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  private
    procedure SetDIC(AValue: TDIContainer);
  protected
    // IReg
    function RegisterTerminus(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string): TDIReg;
    function RegisterContainer(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string; const ATilerID: string): TDIReg;
    function RegisterContainer(ABitClass: TClass; ABitInterface: TGuid; const ATilerID: string): TDIReg;
    function RegisterTiler(ATilerClass: TClass; ATilerInterface: TGuid; const ATilerID: string;
      AScaleClass: TClass): TDIReg;
    function RegisterElement: TDIReg;
    function RegisterElementFactory: TDIReg;
    function RegisterReact: TDIReg;
    function RegisterReactFactory: TDIReg;
    function RegisterReactComponent: TDIReg;
    function RegisterReconciliator: TDIReg;
    function RegisterComposite(ACompositeClass: TClass; ACompositeInterface: TGuid;
      AMapStateKeys: array of string): TDIReg;
    procedure RegisterCommon;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write SetDIC;
  end;

implementation

{ TReg }

procedure TReg.SetDIC(AValue: TDIContainer);
begin
  if fDIC = AValue then Exit;
  fDIC := AValue;
end;

function TReg.RegisterTerminus(ABitClass: TClass; ABitInterface: TGuid;
  AControlClass: TComponentClass; const AControlID: string): TDIReg;
begin
  DIC.Add(AControlClass, nil, AControlID);
  Result := DIC.Add(ABitClass, ABitInterface);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Control', AControlClass, AControlID);
  Result.InjectProp('Node', INode, 'leaf');
end;

function TReg.RegisterContainer(ABitClass: TClass; ABitInterface: TGuid;
  AControlClass: TComponentClass; const AControlID: string; const ATilerID: string): TDIReg;
begin
  if AControlClass <> nil then
    DIC.Add(AControlClass, nil, AControlID);
  Result := DIC.Add(ABitClass, ABitInterface);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  if AControlClass <> nil then
    Result.InjectProp('Control', AControlClass, AControlID);
  Result.InjectProp('Node', INode, 'parent');
  Result.InjectProp('Tiler', ITiler, ATilerID);
end;

function TReg.RegisterContainer(ABitClass: TClass; ABitInterface: TGuid;
  const ATilerID: string): TDIReg;
begin
  Result := RegisterContainer(ABitClass, ABitInterface, nil, '', ATilerID);
end;

function TReg.RegisterTiler(ATilerClass: TClass; ATilerInterface: TGuid;
  const ATilerID: string; AScaleClass: TClass): TDIReg;
var
  mReg: TDIReg;
begin
  // for recount size to pixel(for now nothing)
  mReg := DIC.Add(AScaleClass, IScale, ATilerID+'horizontal');
  mReg.InjectProp('Multiplicator', 1);
  mReg.InjectProp('Divider', 1);
  mReg := DIC.Add(AScaleClass, IScale, ATilerID+'vertical');
  mReg.InjectProp('Multiplicator', 1);
  mReg.InjectProp('Divider', 1);
  // layout
  mReg := DIC.Add(ATilerClass, ATilerInterface, ATilerID);
  mReg.InjectProp('HScale', IScale, ATilerID+'horizontal');
  mReg.InjectProp('VScale', IScale, ATilerID+'vertical');
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

function TReg.RegisterReact: TDIReg;
begin
  Result := DIC.Add(TReact, IReact);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('ReactFactory', IReactFactory);
  Result.InjectProp('Injector', IInjector);
  Result.InjectProp('Reconciliator', IReconciliator);
  Result.InjectProp('RootComponent', IReactComponent);
end;

function TReg.RegisterReactFactory: TDIReg;
begin
  Result := DIC.Add(TReactFactory, IReactFactory);
  Result.InjectProp('Container', TDIContainer, '', DIC);
  Result.InjectProp('Log', ILog);
end;

function TReg.RegisterReactComponent: TDIReg;
begin
  Result := DIC.Add(TReactComponent, IReactComponent);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Node', INode, 'parent');
  Result.InjectProp('Reconciliator', IReconciliator);
  Result.InjectProp('ReactFactory', IReactFactory);
end;

function TReg.RegisterReconciliator: TDIReg;
begin
  Result := DIC.Add(TReconciliator, IReconciliator);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('ReactFactory', IReactFactory);
  Result.InjectProp('Injector', IInjector);
end;

function TReg.RegisterComposite(ACompositeClass: TClass;
  ACompositeInterface: TGuid; AMapStateKeys: array of string): TDIReg;
var
  mReg: TDIReg;
  mKey: string;
begin
  {
  mReg := DIC.Add(TMapStateToProps, IMapStateToProps, 'appcomposite');
  mReg.InjectProp('RdxState', IRdxState);
  mReg.InjectProp('AddKey', Layout.Perspective.Path);
  mReg := DIC.Add(TAppComposite, IAppComposite);
  mReg.InjectProp('Factory', IDIFactory);
  mReg.InjectProp('ElementFactory', IMetaElementFactory);
  mReg.InjectProp('MapStateToProps', IMapStateToProps, 'appcomposite');
  mReg.InjectProp('Log', ILog);
  }

  // todo: temporarily rea depends on rdx, but later TMapStateToProps will be moved
  // to rea and IRdxState move to flu ... and then broke dependency again
  Result := DIC.Add(ACompositeClass, ACompositeInterface);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('ElementFactory', IMetaElementFactory);
  Result.InjectProp('Log', ILog);
  //
  mReg := DIC.Add(TMapStateToProps, IMapStateToProps, ACompositeClass.ClassName);
  mReg.InjectProp('RdxState', IFluxState);
  for mKey in AMapStateKeys do
    mReg.InjectProp('AddKey', mKey);
  //
  Result.InjectProp('MapStateToProps', IMapStateToProps, ACompositeClass.ClassName);
end;

procedure TReg.RegisterCommon;
begin
  RegisterTiler(TDesktopTiler, ITiler, cR_DesktopTiler, TScale);
  RegisterElement;
  RegisterElementFactory;
  RegisterReact;
  RegisterReactFactory;
  RegisterReactComponent;
  RegisterReconciliator;
  RegisterContainer(TFormBit, IFormBit, TForm, 'uiform', cR_DesktopTiler);
  RegisterContainer(TStripBit, IStripBit, cR_DesktopTiler);
  RegisterTerminus(TEditBit, IEditBit, TEdit, 'uiedit');
  RegisterTerminus(TTextBit, ITextBit, TLabel, 'uitext');
  RegisterTerminus(TButtonBit, IButtonBit, TButton, 'uibutton');
end;

end.


unit rea_ureg;

{$mode objfpc}{$H+}

interface

uses
  rea_ireg, Classes, SysUtils, trl_dicontainer, trl_ilog, trl_idifactory,
  trl_itree, trl_iinjector,
  rea_ilayout, rea_ulayout,
  rea_ibits, rea_ubits,
  rea_irenderer, rea_urenderer,
  rea_iflux,
  Forms, StdCtrls,
  trl_iExecutor,
  trl_iprops,
  trl_imetaelementfactory,
  trl_inexus,
  trl_ireconciler,
  rea_idesigncomponent, rea_udesigncomponent,
  trl_ireg, trl_isequence,
  rea_udesigncomponentfactory,
  trl_udifactory,
  rea_istyles, rea_ustyles,
  rea_ufuncdispatcher, rea_uflux,
  rea_idataconnector, rea_udataconnector;

type

  { TReg }

  TReg = class(TInterfacedObject, rea_ireg.IReg)
  private const
    creafuncseq = 'reafuncseq';
  private
    procedure SetDIC(AValue: TDIContainer);
  protected
    // IReg
    function RegisterBitTerminus(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string): TDIReg;
    function RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid; AControlClass: TComponentClass;
      const AControlID: string; const ATilerID: string): TDIReg;
    function RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid; const ATilerID: string): TDIReg;
    function RegisterBitTiler(ATilerClass: TClass; ATilerInterface: TGuid; const ATilerID: string;
      AScaleClass: TClass): TDIReg;
    function RegisterMessageNotifierBinder: TDIReg;
    function RegisterReactComponent(ACompositeClass: TClass; ACompositeInterface: TGuid;
      const APaths: array of string): TDIReg;
    procedure RegisterScales;
    procedure RegisterCommon;
    function RegisterDesignComponent(AComponentClass: TClass; AComponentInterface: TGuid): TDIReg;
    function RegisterDesignComponentFactory(AClass: TClass; AInterface: TGuid): TDIReg;
    function RegisterRenderer: TDIReg;
    function RegisterFuncSequence: TDIReg;
    function RegisterStyle: TDIReg;
    function RegisterDispatcher: TDIReg;
    function RegisterAction: TDIReg;
    function RegisterNotifier(const ADispatcher: TGuid; const AID: string = ''): TDIReg;
    function RegisterDataConnector: TDIReg;
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

function TReg.RegisterBitTerminus(ABitClass: TClass; ABitInterface: TGuid;
  AControlClass: TComponentClass; const AControlID: string): TDIReg;
begin
  DIC.Add(AControlClass, nil, AControlID);
  Result := DIC.Add(ABitClass, ABitInterface);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('HScale', IScale, 'horizontal');
  Result.InjectProp('VScale', IScale, 'vertical');
  Result.InjectProp('Control', AControlClass, AControlID);
  Result.InjectProp('Node', INode, 'leaf');
end;

function TReg.RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid;
  AControlClass: TComponentClass; const AControlID: string; const ATilerID: string): TDIReg;
begin
  if AControlClass <> nil then
    DIC.Add(AControlClass, nil, AControlID);
  Result := DIC.Add(ABitClass, ABitInterface);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('HScale', IScale, 'horizontal');
  Result.InjectProp('VScale', IScale, 'vertical');
  if AControlClass <> nil then
    Result.InjectProp('Control', AControlClass, AControlID);
  Result.InjectProp('Node', INode, 'parent');
  Result.InjectProp('Tiler', ITiler, ATilerID);
end;

function TReg.RegisterBitContainer(ABitClass: TClass; ABitInterface: TGuid;
  const ATilerID: string): TDIReg;
begin
  Result := RegisterBitContainer(ABitClass, ABitInterface, nil, '', ATilerID);
end;

function TReg.RegisterBitTiler(ATilerClass: TClass; ATilerInterface: TGuid;
  const ATilerID: string; AScaleClass: TClass): TDIReg;
begin
  Result := DIC.Add(ATilerClass, ATilerInterface, ATilerID);
end;

function TReg.RegisterMessageNotifierBinder: TDIReg;
begin
  Result := DIC.Add(TMessageNotifierBinder, IMessageNotifierBinder);
end;

function TReg.RegisterReactComponent(ACompositeClass: TClass;
  ACompositeInterface: TGuid; const APaths: array of string): TDIReg;
var
  mPath: string;
begin
  Result := DIC.Add(ACompositeClass, ACompositeInterface);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('ElementFactory', IMetaElementFactory);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('SelfPropsMap', IPropsMap, ACompositeClass.ClassName);
end;

procedure TReg.RegisterScales;
var
  mReg: TDIReg;
begin
  // for recount size to pixel(for now nothing)
  mReg := DIC.Add(TScale, IScale, 'horizontal', ckSingle);
  mReg.InjectProp('Multiplicator', 1);
  mReg.InjectProp('Divider', 1);
  mReg := DIC.Add(TScale, IScale, 'vertical', ckSingle);
  mReg.InjectProp('Multiplicator', 1);
  mReg.InjectProp('Divider', 1);
end;

procedure TReg.RegisterCommon;
begin
  RegisterDesignComponent(TDesignComponentForm, IDesignComponentForm);
  RegisterDesignComponent(TDesignComponentEdit, IDesignComponentEdit);
  RegisterDesignComponent(TDesignComponentText, IDesignComponentText);
  RegisterDesignComponent(TDesignComponentButton, IDesignComponentButton);
  RegisterDesignComponent(TDesignComponentStrip, IDesignComponentStrip);
  RegisterDesignComponent(TDesignComponentHBox, IDesignComponentHBox);
  RegisterDesignComponent(TDesignComponentVBox, IDesignComponentVBox);
  RegisterDesignComponent(TDesignComponentGrid, IDesignComponentGrid);
  RegisterDesignComponent(TDesignComponentPager, IDesignComponentPager);
  RegisterDesignComponent(TDesignComponentFrame, IDesignComponentFrame);
  RegisterDesignComponentFactory(TDesignComponentFormFactory, IDesignComponentFormFactory);
  RegisterDesignComponentFactory(TDesignComponentButtonFactory, IDesignComponentButtonFactory);
  RegisterDesignComponentFactory(TDesignComponentEditFactory, IDesignComponentEditFactory);
  RegisterDesignComponentFactory(TDesignComponentTextFactory, IDesignComponentTextFactory);
  RegisterDesignComponentFactory(TDesignComponentStripFactory, IDesignComponentStripFactory);
  RegisterDesignComponentFactory(TDesignComponentPagerFactory, IDesignComponentPagerFactory);
  RegisterDesignComponentFactory(TDesignComponentPagerSwitchFactory, IDesignComponentPagerSwitchFactory);
  RegisterDesignComponentFactory(TDesignComponentGridFactory, IDesignComponentGridFactory);
  RegisterDesignComponentFactory(TDesignComponentLabelEditFactory, IDesignComponentLabelEditFactory);
  RegisterBitTiler(TDesktopTiler, ITiler, cR_DesktopTiler, TScale);
  RegisterBitContainer(TFormBit, IFormBit, TForm, 'uiform', cR_DesktopTiler);
  RegisterBitContainer(TStripBit, IStripBit, cR_DesktopTiler);
  RegisterBitTerminus(TEditBit, IEditBit, TEdit, 'uiedit');
  RegisterBitTerminus(TTextBit, ITextBit, TLabel, 'uitext');
  RegisterBitTerminus(TButtonBit, IButtonBit, TLabel, 'uibutton');
  RegisterScales;
  RegisterMessageNotifierBinder;
  RegisterRenderer;
  RegisterFuncSequence;
  RegisterStyle;
  RegisterDispatcher;
  RegisterAction;
  RegisterNotifier(IFluxDispatcher);
  RegisterDataConnector;
end;

function TReg.RegisterDesignComponent(AComponentClass: TClass;
  AComponentInterface: TGuid): TDIReg;
var
  mReg: TDIReg;
  mPath: string;
begin
  Result := DIC.Add(AComponentClass, AComponentInterface);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Factory2', TDIFactory2);
  Result.InjectProp('ElementFactory', IMetaElementFactory);
  Result.InjectProp('Node', INode, 'parent');
  Result.InjectProp('Style', IStyle);
end;

function TReg.RegisterDesignComponentFactory(AClass: TClass; AInterface: TGuid): TDIReg;
begin
  Result := DIC.Add(AClass, AInterface);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Factory2', TDIFactory2);
  Result.InjectProp('FluxDispatcher', IFluxDispatcher);
  Result.InjectProp('ActionIDSequence', ISequence, 'ActionID');
end;

function TReg.RegisterRenderer: TDIReg;
begin
  Result := DIC.Add(TRenderer, IRenderer);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('ElementFactory', IMetaElementFactory);
  Result.InjectProp('Reconciler', IReconciler);
end;

function TReg.RegisterFuncSequence: TDIReg;
var
  mReg: trl_ireg.IReg;
begin
  mReg := trl_ireg.IReg(DIC.Locate(trl_ireg.IReg));
  Result := mReg.RegisterSequence(creafuncseq, ckSingle);
end;

function TReg.RegisterStyle: TDIReg;
begin
  Result := DIC.Add(TStyle, IStyle);
end;

function TReg.RegisterDispatcher: TDIReg;
begin
  Result := DIC.Add(TRdxFuncDispatcher, IFluxDispatcher, '', ckSingle);
  Result.InjectProp('Executor', IExecutor);
end;

function TReg.RegisterAction: TDIReg;
begin
  Result := DIC.Add(TFluxAction, IFluxAction);
  Result.InjectProp('Props', IProps);
end;

function TReg.RegisterNotifier(const ADispatcher: TGuid; const AID: string
  ): TDIReg;
begin
  Result := DIC.Add(TFluxNotifier, IFluxNotifier);
  // asi az pri reactu mozna    mReg.InjectProp('ActionID', cResizeFunc);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Dispatcher', ADispatcher, AID);
end;

function TReg.RegisterDataConnector: TDIReg;
begin
  Result := DIC.Add(TDataConnector, IDataConnector);
  Result.InjectProp('FluxDispatcher', IFluxDispatcher);
end;

end.


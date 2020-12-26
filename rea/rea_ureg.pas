unit rea_ureg;

{$mode objfpc}{$H+}

interface

uses
  rea_ireg, Classes, SysUtils, trl_dicontainer, trl_ilog, trl_idifactory,
  trl_itree, trl_iinjector,
  rea_ilayout, rea_ulayout,
  rea_ibits, rea_ubits,
  rea_irenderer, rea_urenderer,
  flu_iflux,
  Forms, StdCtrls,
  trl_iExecutor,
  trl_iprops,
  trl_imetaelementfactory,
  trl_inexus,
  trl_ireconciler,
  rea_idesigncomponent, rea_udesigncomponent,
  trl_ireg, trl_isequence,
  flu_ireg;

type

  { TReg }

  TReg = class(TInterfacedObject, rea_ireg.IReg)
  private const
    creafuncseq = 'reafuncseq';
  private
    fRegFlux: flu_ireg.IReg;
    function RegFlux: flu_ireg.IReg;
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
    function RegisterRenderer: TDIReg;
    function RegisterRendererFunc: TDIReg;
    function RegisterFuncSequence: TDIReg;
    procedure RegisterFuncs;
  protected
    fDIC: TDIContainer;
  published
    property DIC: TDIContainer read fDIC write SetDIC;
  end;

implementation

{ TReg }

function TReg.RegFlux: flu_ireg.IReg;
begin
  if fRegFlux = nil then
    fRegFlux := flu_ireg.IReg(DIC.Locate(flu_ireg.IReg));
  Result := fRegFlux;
end;

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
  RegisterDesignComponent(TDesignComponentButton, IDesignComponentButton);
  RegisterDesignComponent(TDesignComponentHeader, IDesignComponentHeader);
  RegisterDesignComponent(TDesignComponentGrid, IDesignComponentGrid);
  RegisterDesignComponent(TDesignComponentPager, IDesignComponentPager);
  RegisterBitTiler(TDesktopTiler, ITiler, cR_DesktopTiler, TScale);
  RegisterBitContainer(TFormBit, IFormBit, TForm, 'uiform', cR_DesktopTiler);
  RegisterBitContainer(TStripBit, IStripBit, cR_DesktopTiler);
  RegisterBitTerminus(TEditBit, IEditBit, TEdit, 'uiedit');
  RegisterBitTerminus(TTextBit, ITextBit, TLabel, 'uitext');
  RegisterBitTerminus(TButtonBit, IButtonBit, TButton, 'uibutton');
  RegisterScales;
  RegisterMessageNotifierBinder;
  RegisterRenderer;
  RegisterRendererFunc;
  RegisterFuncSequence;
  RegisterFuncs;
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
  Result.InjectProp('ElementFactory', IMetaElementFactory);
  Result.InjectProp('Node', INode, 'parent');
  Result.InjectProp('StoreConnector', IFluxStoreConnector);
  Result.InjectProp('FluxFuncReg', IFluxDispatcher);
  Result.InjectProp('FuncSequence', ISequence, creafuncseq);
end;

function TReg.RegisterRenderer: TDIReg;
begin
  Result := DIC.Add(TRenderer, IRenderer);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
end;

function TReg.RegisterRendererFunc: TDIReg;
begin
  Result := DIC.Add(TRenderFunc, IFluxFunc, TRenderFunc.ClassName);
  Result.InjectProp('Reconciler', IReconciler);
  Result.InjectProp('AppComponent', IDesignComponentApp);
  Result.InjectProp('Renderer', IRenderer);
end;

function TReg.RegisterFuncSequence: TDIReg;
var
  mReg: trl_ireg.IReg;
begin
  mReg := trl_ireg.IReg(DIC.Locate(trl_ireg.IReg));
  Result := mReg.RegisterSequence(creafuncseq, ckSingle);
end;

procedure TReg.RegisterFuncs;
begin
  RegFlux.RegisterFunc(TTextChangedFunc);
end;

end.


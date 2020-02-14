unit rea_ureg;

{$mode objfpc}{$H+}

interface

uses
  rea_ireg, Classes, SysUtils, trl_dicontainer, trl_ilog, trl_idifactory,
  trl_itree, trl_iinjector,
  rea_ilayout, rea_ulayout,
  rea_ibits, rea_ubits,
  rea_ireact, rea_ureact,
  rea_ibrace, rea_ubrace,
  rea_irenderer, rea_urenderer,
  flu_iflux, flu_umap,
  Forms, StdCtrls,
  rea_mainform,
  trl_iExecutor,
  trl_iprops,
  trl_imetaelementfactory,
  trl_inexus;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
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
    function RegisterMachinery(AMachineryCompositeClass: TClass; AMachineryInterface: TGuid): TDIReg;
    function RegisterReact(const AID: string = ''): TDIReg;
    procedure RegisterBrace;
    procedure RegisterScales;
    procedure RegisterCommon;
    function RegisterDesignComponent(AComponentClass: TClass; AComponentInterface: TGuid): TDIReg;
    function RegisterRenderer: TDIReg;
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
  mReg: TDIReg;
  mPath: string;
begin
  Result := DIC.Add(ACompositeClass, ACompositeInterface);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('ElementFactory', IMetaElementFactory);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('React', IReact, '');
  //
  mReg := DIC.Add(TFluxStateToPropsMap, IPropsMap, ACompositeClass.ClassName);
  mReg.InjectProp('State', IFluxState);
  for mPath in APaths do
    mReg.InjectProp('AddPath', mPath);
  //
  Result.InjectProp('SelfPropsMap', IPropsMap, ACompositeClass.ClassName);
end;

function TReg.RegisterMachinery(AMachineryCompositeClass: TClass; AMachineryInterface: TGuid): TDIReg;
begin
  Result := DIC.Add(AMachineryCompositeClass, AMachineryInterface);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('Injector', IInjector);
  Result.InjectProp('ElementFactory', IMetaElementFactory);
end;

function TReg.RegisterReact(const AID: string): TDIReg;
begin
  DIC.Add(TRenderExecute, IExecute, 'TRenderExecute');
  Result := DIC.Add(TReact, IReact, AID, ckSingle);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Executor', IExecutor);
  Result.InjectProp('ElFactory', IMetaElementFactory);
  Result.InjectProp('Factory', IDIFactory);
end;

procedure TReg.RegisterBrace;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TBrace, IBrace);
  mReg.InjectProp('Log', ILog);
  mReg.InjectProp('Node', INode, 'parent');
  mReg.InjectProp('Factory', IDIFactory);
  mReg.InjectProp('Nexus', INexus);
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
  RegisterBrace;
  RegisterBitTiler(TDesktopTiler, ITiler, cR_DesktopTiler, TScale);
  RegisterMachinery(TReactComponentMachineryMiddle, IReactComponentMachineryMiddle);
  RegisterMachinery(TReactComponentMachineryLeaf, IReactComponentMachineryLeaf);
  RegisterBitContainer(TFormBit, IFormBit, TForm, 'uiform', cR_DesktopTiler);
  RegisterBitContainer(TMainFormBit, IMainFormBit, TMainForm, '', cR_DesktopTiler);
  RegisterBitContainer(TStripBit, IStripBit, cR_DesktopTiler);
  RegisterBitTerminus(TEditBit, IEditBit, TEdit, 'uiedit');
  RegisterBitTerminus(TTextBit, ITextBit, TLabel, 'uitext');
  RegisterBitTerminus(TButtonBit, IButtonBit, TButton, 'uibutton');
  RegisterScales;
  RegisterMessageNotifierBinder;
  RegisterReact;
  RegisterRenderer;
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
end;

function TReg.RegisterRenderer: TDIReg;
begin
  Result := DIC.Add(TRenderer, IRenderer);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
end;

end.


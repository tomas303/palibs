(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
unit rea_ureg;

{$mode objfpc}{$H+}

interface

uses
  rea_ireg, Classes, SysUtils, trl_dicontainer, trl_ilog, trl_idifactory,
  trl_itree, trl_iinjector,
  rea_ilayout, rea_ulayout,
  rea_ibits, rea_ubits,
  rea_irenderer, rea_urenderer,
  Forms, StdCtrls, ExtCtrls,
  trl_iprops,
  trl_imetaelementfactory,
  trl_ireconciler,
  rea_idesigncomponent, rea_udesigncomponent,
  trl_ireg, trl_isequence,
  trl_udifactory,
  trl_ilauncher, trl_pubsub, rea_upubsublauncher,
  trl_ipersist, rea_idata, rea_ustoreconnector;

type

  { TReg }

  TReg = class(TInterfacedObject, rea_ireg.IReg)
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
    function RegisterMessageObservable: TDIReg;
    procedure RegisterScales;
    procedure RegisterCommon;
    function RegisterDesignComponent(AComponentClass: TClass; AComponentInterface: TGuid): TDIReg;
    function RegisterRenderer: TDIReg;
    function RegisterPubSubLauncher: TDIReg;
    function RegisterStoreConnector: TDIReg;
    function RegisterMorph: TDIReg;
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

function TReg.RegisterMessageObservable: TDIReg;
begin
  Result := DIC.Add(TMessageObservable, IMessageObservable);
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
  RegisterDesignComponent(TDesignComponentMemo, IDesignComponentMemo);
  RegisterDesignComponent(TDesignComponentText, IDesignComponentText);
  RegisterDesignComponent(TDesignComponentButton, IDesignComponentButton);
  RegisterDesignComponent(TDesignComponentStrip, IDesignComponentStrip);
  RegisterDesignComponent(TDesignComponentHBox, IDesignComponentHBox);
  RegisterDesignComponent(TDesignComponentVBox, IDesignComponentVBox);
  RegisterDesignComponent(TDesignComponentGrid, IDesignComponentGrid);
  RegisterDesignComponent(TDesignComponentPager, IDesignComponentPager);
  RegisterDesignComponent(TDesignComponentFilter, IDesignComponentFilter);
  RegisterBitTiler(TDesktopTiler, ITiler, cR_DesktopTiler, TScale);
  RegisterBitContainer(TFormBit, IFormBit, TForm, 'uiform', cR_DesktopTiler);
  RegisterBitContainer(TStripBit, IStripBit, cR_DesktopTiler);
  RegisterBitTerminus(TEditBit, IEditBit, TEdit, 'uiedit');
  RegisterBitTerminus(TMemoBit, IMemoBit, TMemo, 'uimemo');
  RegisterBitTerminus(TTextBit, ITextBit, TLabel, 'uitext');
  RegisterBitTerminus(TButtonBit, IButtonBit, TPanel, 'uibutton');
  RegisterScales;
  RegisterMessageObservable;
  RegisterRenderer;
  RegisterStoreConnector;
  RegisterMorph;
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
  Result.InjectProp('PubSub', IPubSub);
  Result.InjectProp('Morph', IMorph);
end;

function TReg.RegisterRenderer: TDIReg;
begin
  Result := DIC.Add(TRenderer, IRenderer);
  Result.InjectProp('Log', ILog);
  Result.InjectProp('Factory', IDIFactory);
  Result.InjectProp('ElementFactory', IMetaElementFactory);
  Result.InjectProp('Reconciler', IReconciler);
end;

function TReg.RegisterPubSubLauncher: TDIReg;
begin
  Result := DIC.Add(TPubSubLauncher, ILauncher);
  Result.InjectProp('Factory2', TDIFactory2);
  Result.InjectProp('PubSub', IPubSub);
  Result.InjectProp('Renderer', IRenderer);
end;

function TReg.RegisterStoreConnector: TDIReg;
begin
  Result := DIC.Add(TStoreConnector, IDataConnector, TStoreConnector.ClassName);
  Result.InjectProp('Factory2', TDIFactory2);
  Result.InjectProp('PubSub', IPubSub);
  //Result.InjectProp('Store', IPersistStore);
end;

function TReg.RegisterMorph: TDIReg;
begin
  Result := DIC.Add(TMorph, IMorph);
  Result.InjectProp('Factory2', TDIFactory2);
end;

end.


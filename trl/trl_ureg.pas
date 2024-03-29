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
  trl_imetaelement, trl_umetaelement,
  trl_imetaelementfactory, trl_umetaelementfactory,
  trl_ilink, trl_ulink,
  trl_ilog,
  trl_ireconciler, trl_ureconciler,
  trl_pubsub,
  trl_isequence, trl_usequence;

type

  { TReg }

  TReg = class(TInterfacedObject, IReg)
  protected
    // IReg
    function RegisterSysUtils: TDIReg;
    function RegisterDIOwner: TDIReg;
    function RegisterDIFactory: TDIReg;
    function RegisterDIFactory2: TDIReg;
    function RegisterInjector: TDIReg;
    function RegisterProps: TDIReg;
    procedure RegisterTreeNodes;
    procedure RegisterLink;
    function RegisterElement: TDIReg;
    function RegisterElementFactory: TDIReg;
    function RegisterReconciler: TDIReg;
    function RegisterSequence(const AID: string; ACreateKind: TDIRegCreateKind = ckSingle): TDIReg;
    function RegisterPubSub: TDIReg;
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

function TReg.RegisterDIFactory2: TDIReg;
begin
  Result := DIC.Add(TDIFactory2);
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

function TReg.RegisterSequence(const AID: string; ACreateKind: TDIRegCreateKind = ckSingle): TDIReg;
begin
  Result := DIC.Add(TSequence, ISequence, AID, ACreateKind);
end;

function TReg.RegisterPubSub: TDIReg;
begin
  Result := DIC.Add(TPubSub, IPubSub, '', ckSingle);
  Result.InjectProp('Log', ILog);
end;

procedure TReg.RegisterCommon;
begin
  RegisterSysUtils;
  RegisterDIOwner;
  RegisterDIFactory;
  RegisterDIFactory2;
  RegisterInjector;
  RegisterProps;
  RegisterTreeNodes;
  RegisterLink;
  RegisterElement;
  RegisterElementFactory;
  RegisterReconciler;
  RegisterSequence('', ckSingle);
  RegisterPubSub;
end;

end.


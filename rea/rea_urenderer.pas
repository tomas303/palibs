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
unit rea_urenderer;

{$mode objfpc}{$H+}

interface

uses
  rea_irenderer, rea_idesigncomponent, trl_ilog, trl_imetaelement, trl_idifactory,
  trl_itree, trl_iprops, rea_ibits, trl_ireconciler, sysutils,
  trl_imetaelementfactory, classes, strutils;

type

  { TRenderer }

  TRenderer = class(TInterfacedObject, IRenderer)
  private
    fHeadBit: IBit;
    fHeadEl: IMetaElement;
    procedure LogNode(const ANode: INode; ALevel: Integer = 0);
  protected
    function EmptyChildren: TMetaElementArray;
    function ExpandElement(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function ExpandElement2(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function RenderChain(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
    function Expand(const AElement: IMetaElement): IMetaElement;
    procedure Info(const AElement: IMetaElement; AInfo: TStrings; ALevel: Integer);
    procedure InfoNode(const ANode: INode; AInfo: TStrings; ALevel: Integer);
  protected
    // IRenderer
    procedure Render(const AElement: IMetaElement);
  protected
    fLog: ILog;
    fFactory: IDIFactory;
    fElementFactory: IMetaElementFactory;
    fReconciler: IReconciler;
  published
    property Log: ILog read fLog write fLog;
    property Factory: IDIFactory read fFactory write fFactory;
    property ElementFactory: IMetaElementFactory read fElementFactory write fElementFactory;
    property Reconciler: IReconciler read fReconciler write fReconciler;
  end;

implementation

{ TRenderer }

procedure TRenderer.LogNode(const ANode: INode; ALevel: Integer);
var
  mChild: INode;
begin
{$IfDef GUIDEBUG}
  if ANode = nil then begin
    Log.DebugLn(DupeString('--', ALevel) + ' nil');
  end else begin
    Log.DebugLn(DupeString('--', ALevel) + ' ' + (ANode as TObject).ClassName);
    for mChild in ANode do begin
      LogNode(mChild, ALevel + 1);
    end;
  end;
{$EndIf GUIDEBUG}
end;

function TRenderer.EmptyChildren: TMetaElementArray;
begin
  Result := nil;
  SetLength(Result, 0);
end;

function TRenderer.ExpandElement(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
var
  mEl, mEndEl, mNewEl: IMetaElement;
  mo: tobject;
  mcn: string;
begin
  Result := RenderChain(AElement, AParentProps);
  mcn := (Result as tobject).classname;
  for mEl in Result do
  begin
    //mEndEl := RenderChain(Result, mEl);
    //(Result as INode).ExchangeChild(mEl as INode, mEndEl as INode);
    mNewEl := ExpandElement(mEl, Result.Props.Clone);
    (Result as INode).ExchangeChild(mEl as INode, mNewEl as INode);
  end;
end;

function TRenderer.ExpandElement2(const AElement: IMetaElement;
  const AParentProps: IProps): IMetaElement;
var
  mEl, mNewEl: IMetaElement;
  mChildren: TMetaElementArray;
  mComponent: IDesignComponent;
begin
  for mEl in AElement do
  begin
    //mEndEl := RenderChain(Result, mEl);
    //(Result as INode).ExchangeChild(mEl as INode, mEndEl as INode);
    mNewEl := ExpandElement2(mEl, AElement.Props.Clone);
    SetLength(mChildren, Length(mChildren) + 1);
    mChildren[High(mChildren)] := mNewEl;
  end;
  Result := ElementFactory.CreateElement(AElement.Guid, AElement.Props, mChildren);
  while Factory.CanLocateAs(Result.Guid, IDesignComponent) do
  begin
    mComponent := IUnknown(Factory.Locate(Result.Guid, Result.TypeID, Result.Props)) as IDesignComponent;
    mNewEl := mComponent.Compose;
    Result := mNewEl;
  end;
end;

function TRenderer.RenderChain(const AElement: IMetaElement; const AParentProps: IProps): IMetaElement;
var
  mComponent: IDesignComponent;
  mNewEl: IMetaElement;
  mc:string;
begin
  Result := AElement;
  while Factory.CanLocateAs(Result.Guid, IDesignComponent) do
  begin
    mComponent := IUnknown(Factory.Locate(Result.Guid, Result.TypeID, Result.Props)) as IDesignComponent;
    mNewEl := mComponent.Compose;
    Result := mNewEl;
  end;

  mc := (result as tobject).ClassName;
    if pos('Design', mc) > 0 then
    Factory.CanLocateAs(Result.Guid, IDesignComponent);

end;

function TRenderer.Expand(const AElement: IMetaElement): IMetaElement;
var
  m: string;
begin
  Result := ExpandElement(AElement, AElement.Props);
  m := (Result as ILogSupport).LogInfo;
  Log.DebugLn(m);
end;

procedure TRenderer.Info(const AElement: IMetaElement; AInfo: TStrings; ALevel: Integer);
var
  mO: TObject;
  mEl: IMetaElement;
begin
  mO := IUnknown(Factory.Locate(AElement.Guid, AElement.TypeID, AElement.Props)) as tobject;
  AInfo.Add(DupeString('-', ALevel) +  mO.ClassName);
  for mEl in AElement do begin
    Info(mEl, AInfo, ALevel + 2);
  end;
end;

procedure TRenderer.InfoNode(const ANode: INode; AInfo: TStrings;
  ALevel: Integer);
var
  mN: INode;
begin
  AInfo.Add(DupeString('-', ALevel) + (ANode as TObject).ClassName);
  for mN in ANode do begin
    InfoNode(mN, AInfo, ALevel + 2);
  end;
end;

procedure TRenderer.Render(const AElement: IMetaElement);
var
  mNewEl: IMetaElement;
  mNew: IUnknown;
  mNewBit: IBit;
begin
  //mNewEl := Expand(AElement);
  mNewEl := AElement;
  mNew := Reconciler.Reconcile(fHeadEl, fHeadBit, mNewEl);
  mNewBit := mNew as IBit;
  fHeadEl := mNewEl;
  {$IfDef GUIDEBUG}
  Log.DebugLnEnter('--- NEW BIT ---');
  LogNode(mNewBit as INode);
  Log.DebugLnExit('--- NEW BIT ---');
  Log.DebugLnEnter('--- OLD BIT ---');
  LogNode(fHeadBit as INode);
  Log.DebugLnExit('--- OLD BIT ---');
  {$EndIf GUIDEBUG}
  fHeadBit := mNewBit;
  fHeadBit.Render;
end;

end.


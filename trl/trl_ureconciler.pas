unit trl_ureconciler;

{$mode objfpc}{$H+}

interface

uses
  trl_ireconciler, trl_ilog, trl_idifactory, trl_iinjector, trl_imetaelement,
  trl_iprops, trl_itree, sysutils, Math, StrUtils;

type

  { TReconciler }

  TReconciler = class(TInterfacedObject, IReconciler)
  private
    procedure LogElement(const ANode: INode; ALevel: Integer = 0);
  protected
    procedure RemoveChild(const AParentBit: INode; AIndex: integer);
    procedure InsertChild(const AParentBit: INode; const AChildElement: IMetaElement; AIndex: integer);
    procedure AddChild(const AParentBit: INode; const AChildElement: IMetaElement);
    procedure ProcessDiff(const ABit: INode; const ANewChildElement, AOldChildElement: IMetaElement);
    procedure ProcessChildren(const ABit: INode; const AOldElement, ANewElement: IMetaElement);
  protected
    // IReconciler
    function Reconcile(const AOldElement: IMetaElement; const AOldEntity: IUnknown; const ANewElement: IMetaElement): IUnknown;
  protected
    fLog: ILog;
    fFactory: IDIFactory;
    fInjector: IInjector;
  published
    property Log: ILog read fLog write fLog;
    property Factory: IDIFactory read fFactory write fFactory;
    property Injector: IInjector read fInjector write fInjector;
  end;

implementation

{ TReconciler }

procedure TReconciler.LogElement(const ANode: INode; ALevel: Integer);
var
  mChild: INode;
begin
{$IfDef GUIDEBUG}
  if ANode = nil then begin
    Log.DebugLn(DupeString('--', ALevel) + ' nil');
  end else begin
    Log.DebugLn(DupeString('--', ALevel) + ' ' + (ANode as IMetaElement).TypeGuid + '/' + (ANode as IMetaElement).TypeID + '     ' + (ANode as IMetaElement).Props.Info);
    for mChild in ANode do begin
      LogElement(mChild, ALevel + 1);
    end;
  end;
{$EndIf GUIDEBUG}
end;

procedure TReconciler.RemoveChild(const AParentBit: INode; AIndex: integer);
var
  mChildBit: INode;
begin
  mChildBit := (AParentBit as INode).Child[AIndex] as INode;
  (AParentBit as INode).RemoveChild(mChildBit as INode);
end;

procedure TReconciler.InsertChild(const AParentBit: INode;
  const AChildElement: IMetaElement; AIndex: integer);
var
  mNew: IUnknown;
  mChildBit: INode;
  i: integer;
begin
  mNew := IUnknown(Factory.Locate(AChildElement.Guid, AChildElement.TypeID, AChildElement.Props.Clone));
  if Supports(mNew, INode, mChildBit) then
  begin
    // what render to INode will be use directly
    (AParentBit as INode).Insert(AIndex, mChildBit as INode);
    for i := 0 to (AChildElement as INode).Count - 1 do
      AddChild(mChildBit, (AChildElement as INode).Child[i] as IMetaElement);
  end
  else
  begin
    raise exception.create('todo');
  end;
end;

procedure TReconciler.AddChild(const AParentBit: INode;
  const AChildElement: IMetaElement);
var
  mNew: IUnknown;
  mChildBit: INode;
  i: integer;
begin
  mNew := IUnknown(Factory.Locate(AChildElement.Guid, AChildElement.TypeID, AChildElement.Props.Clone));
  if Supports(mNew, INode, mChildBit) then
  begin
    // what render to INode will be use directly
    (AParentBit as INode).AddChild(mChildBit as INode);
    for i := 0 to (AChildElement as INode).Count - 1 do
      AddChild(mChildBit, (AChildElement as INode).Child[i] as IMetaElement);
  end
  else
  begin
    raise exception.create('todo');
  end;
end;

procedure TReconciler.ProcessDiff(const ABit: INode; const ANewChildElement,
  AOldChildElement: IMetaElement);
var
  mDiffProps: IProps;
begin
  mDiffProps := ANewChildElement.Props.Diff(AOldChildElement.Props, pdmDifferent);
  if mDiffProps.Count > 0 then
  begin
    Injector.Write(ABit as TObject, mDiffProps);
  end;
  ProcessChildren(ABit, AOldChildElement, ANewChildElement);
end;

procedure TReconciler.ProcessChildren(const ABit: INode; const AOldElement,
  ANewElement: IMetaElement);
var
  mOldChildEl: IMetaElement;
  mNewChildEl: IMetaElement;
  i: Integer;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  for i := 0 to Min((AOldElement as INode).Count, (ANewElement as INode).Count) - 1 do
  begin
    mOldChildEl := (AOldElement as INode).Child[i] as IMetaElement;
    mNewChildEl := (ANewElement as INode).Child[i] as IMetaElement;
    if (mOldChildEl.TypeGuid <> mNewChildEl.TypeGuid) or (mOldChildEl.TypeID <> mNewChildEl.TypeID) then
    begin
      RemoveChild(ABit, i);
      InsertChild(ABit, mNewChildEl, i);
    end
    else
    begin
      ProcessDiff(ABit.Child[i], mNewChildEl, mOldChildEl);
    end;
  end;
  if (ANewElement as INode).Count > (AOldElement as INode).Count then
  begin
    for i := (AOldElement as INode).Count to (ANewElement as INode).Count - 1 do
    begin
      mNewChildEl := (ANewElement as INode).Child[i] as IMetaElement;
      AddChild(ABit, mNewChildEl);
    end;
  end
  else
  begin
    for i := (ANewElement as INode).Count to (AOldElement as INode).Count - 1 do
    begin
      RemoveChild(ABit, (ANewElement as INode).Count);
    end;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

function TReconciler.Reconcile(const AOldElement: IMetaElement;
  const AOldEntity: IUnknown; const ANewElement: IMetaElement): IUnknown;
var
  i: integer;
  mb: INode;
  mch: INode;
  mchE: IMetaElement;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  if (AOldElement = nil) and (ANewElement = nil) then begin
    Log.DebugLn('both nil');
    Result := AOldEntity;
  end else
  if (AOldElement <> nil) and (ANewElement = nil) then begin
    Log.DebugLn(AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to nil');
    Result := nil;
  end else
  if (AOldElement = nil) and (ANewElement <> nil) then begin
    Log.DebugLn('from nil to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := IUnknown(Factory.Locate(ANewElement.Guid, ANewElement.TypeID, ANewElement.Props.Clone));
    for i := 0 to (ANewElement as INode).Count - 1 do
    begin
      //AddChild(Result as INode, (ANewElement as INode).Child[i] as IMetaElement);
      mb := Result as INode;
      mch := (ANewElement as INode).Child[i];
      mchE := mch as IMetaElement;
      AddChild(mb, mchE);
    end;
  end else
  if (AOldElement.TypeGuid <> ANewElement.TypeGuid) or (AOldElement.TypeID <> ANewElement.TypeID) then
  begin
    Log.DebugLn('from ' + AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := IUnknown(Factory.Locate(ANewElement.Guid, ANewElement.TypeID, ANewElement.Props.Clone));
    for i := 0 to (ANewElement as INode).Count - 1 do
      AddChild(Result as INode, (ANewElement as INode).Child[i] as IMetaElement);
  end
  else
  begin
    Log.DebugLn('equalize props');
    Result := AOldEntity;
    ProcessDiff(Result as INode, ANewElement, AOldElement);
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
  {$IfDef GUIDEBUG}
  Log.DebugLnEnter('--- OLD ELEMENT ---');
  LogElement(AOldElement as INode);
  Log.DebugLnExit('--- OLD ELEMENT ---');
  Log.DebugLnEnter('--- NEW ELEMENT ---');
  LogElement(ANewElement as INode);
  Log.DebugLnExit('--- NEW ELEMENT ---');
  {$EndIf}

end;

end.


unit trl_ureconciler;

{$mode objfpc}{$H+}

interface

uses
  trl_ireconciler, trl_ilog, trl_idifactory, trl_iinjector, trl_imetaelement,
  trl_iprops;

type

  { TReconciler }

  TReconciler = class(TInterfacedObject, IReconciler)
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

function TReconciler.Reconcile(const AOldElement: IMetaElement;
  const AOldEntity: IUnknown; const ANewElement: IMetaElement): IUnknown;
var
  mDiffProps: IProps;
begin
  Log.DebugLnEnter({$I %CURRENTROUTINE%});
  if (AOldElement = nil) and (ANewElement = nil) then begin
    Log.DebugLn('both nil');
    Result := nil;
  end else
  if (AOldElement <> nil) and (ANewElement = nil) then begin
    Log.DebugLn(AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to nil');
    Result := nil;
  end else
  if (AOldElement = nil) and (ANewElement <> nil) then begin
    Log.DebugLn('from nil to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := IUnknown(Factory.Locate(ANewElement.TypeGuid, ANewElement.TypeID));
  end else
  if (AOldElement.TypeGuid <> ANewElement.TypeGuid) or (AOldElement.TypeID <> ANewElement.TypeID) then begin
    Log.DebugLn('from ' + AOldElement.TypeGuid + '.' + AOldElement.TypeID + ' to ' + ANewElement.TypeGuid + '.' + ANewElement.TypeID);
    Result := IUnknown(Factory.Locate(ANewElement.TypeGuid, ANewElement.TypeID));
  end else begin
    Log.DebugLn('equalize props');
    mDiffProps := ANewElement.Props.Diff(AOldElement.Props);
    Injector.Write(AOldEntity as TObject, mDiffProps);
    Result := AOldEntity;
  end;
  Log.DebugLnExit({$I %CURRENTROUTINE%});
end;

end.


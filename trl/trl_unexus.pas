unit trl_unexus;

{$mode objfpc}{$H+}

interface

uses
  trl_inexus, trl_ireconciler, trl_imetaelement, trl_ilog;

type

  { TNexus }

  TNexus = class(TInterfacedObject, INexus)
  protected
   fElement: IMetaElement;
   fInstance: IUnknown;
  protected
   // INexus
   function Renew(const AElement: IMetaElement): IUnknown;
   function Instance: IUnknown;
  protected
    fLog: ILog;
    fReconciler: IReconciler;
  published
    property Log: ILog read fLog write fLog;
    property Reconciler: IReconciler read fReconciler write fReconciler;
  end;

implementation

{ TNexus }

function TNexus.Renew(const AElement: IMetaElement): IUnknown;
begin
  fInstance := Reconciler.Reconcile(fElement, fInstance, AElement);
end;

function TNexus.Instance: IUnknown;
begin
  Result := fInstance;
end;


end.


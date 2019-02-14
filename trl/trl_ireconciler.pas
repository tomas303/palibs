unit trl_ireconciler;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement;

type
  IReconciler = interface
  ['{88708DEB-D28C-4D68-9390-64E0E1A09362}']
    function Reconcile(const AOldElement: IMetaElement; const AOldEntity: IUnknown; const ANewElement: IMetaElement): IUnknown;
  end;

implementation

end.


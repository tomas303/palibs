unit rea_ibrace;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement, trl_itree, rea_ibits;

type

  { IBrace }

  IBrace = interface
  ['{278C5A0E-8BE1-48E9-BB20-9E2759B17799}']
    function Refresh(const AElement: IMetaElement): IBit;
    function Remove(const AParent: IBit): IBit;
  end;

implementation

end.


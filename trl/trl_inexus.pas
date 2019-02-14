unit trl_inexus;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement;

type
  INexus = interface
  ['{DE2BD10D-CE83-4748-AE69-C303AEEFB1DF}']
    function Renew(const AElement: IMetaElement): IUnknown;
  end;

implementation

end.


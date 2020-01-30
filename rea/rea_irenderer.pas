unit rea_irenderer;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement, rea_idesigncomponent;

type
  IRenderer = interface
  ['{B6DA13A6-8D9A-4FB6-9A7C-62944E491810}']
    function Render(const AComponent: IDesignComponent): IMetaElement;
  end;

implementation

end.


unit rea_irenderer;

{$mode objfpc}{$H+}

interface

uses
  trl_imetaelement, rea_idesigncomponent;

type
  IRenderer = interface
  ['{B6DA13A6-8D9A-4FB6-9A7C-62944E491810}']
    procedure Render(const AElement: IMetaElement);
  end;

const
  cNotifyRender = -400;
  cNotifyCloseGUI = -401;

implementation

end.


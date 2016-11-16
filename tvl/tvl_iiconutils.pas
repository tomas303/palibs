unit tvl_iiconutils;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

type
  IIconUtils = interface
  ['{160FC291-39FA-4977-914F-9033ED120A6E}']
    procedure RenderAppIcon(const AApplication: string; ABitmap: TBitmap;
      AIconHeight: Integer);
  end;

const
  cIconUtilsTransparentColor = $CCCCCC;

implementation

end.


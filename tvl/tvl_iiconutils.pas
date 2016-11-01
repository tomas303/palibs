unit tvl_iiconutils;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

type
  IIconUtils = interface
  ['{160FC291-39FA-4977-914F-9033ED120A6E}']
    function FindAppIconFile(const AApplication: string): string;
    procedure RenderAppIcon(const AFile: string; ABitmap: TBitmap);
  end;

implementation

end.


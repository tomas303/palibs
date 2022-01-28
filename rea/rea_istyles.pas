unit rea_istyles;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

type

  cStyleKind = class
  public const
    Common = 0;
    None = 1;
    Important = 2;
    Focused = 3;
  end;

  TStyleData = record
    ForeColor: TColor;
    BackColor: TColor;
    SuppleColor: TColor;
  end;

  IStyle = interface
  ['{19CF28DD-EF1D-4437-BC47-294986B28B31}']
    function Get(const AStyle: Integer): TStyleData;
  end;

implementation

end.


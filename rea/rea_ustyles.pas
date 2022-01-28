unit rea_ustyles;

{$mode objfpc}{$H+}

interface

uses
  rea_istyles, Graphics;

type

  { TStyle }

  TStyle = class(TInterfacedObject, IStyle)
  protected
    function Get(const AStyle: Integer): TStyleData;
  end;

implementation

{ TStyle }

function TStyle.Get(const AStyle: Integer): TStyleData;
begin
  case AStyle of
    cStyleKind.Common:
      begin
        Result.ForeColor := clWhite;
        Result.BackColor := clBlack;
        Result.SuppleColor := clAqua;
      end;
    cStyleKind.None:
      begin
        Result.ForeColor := clBlack;
        Result.BackColor := clBlack;
        Result.SuppleColor := clBlack;
      end;
    cStyleKind.Important:
      begin
        Result.ForeColor := clBlack;
        Result.BackColor := clBlack;
        Result.SuppleColor := clRed;
      end;
    cStyleKind.Focused:
      begin
        Result.ForeColor := clBlack;
        Result.BackColor := clSilver;
        Result.SuppleColor := clYellow;
      end;
  end;
end;

end.


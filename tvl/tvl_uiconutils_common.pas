unit tvl_uiconutils_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, tvl_iiconutils;

type

  { TIconUtilsCommon }

  TIconUtilsCommon = class(TInterfacedObject)
  protected
    procedure PrepareBitmap(const ABitmap: TBitmap; AHeight: integer);
    procedure RenderBitmap(const ABitmap: TBitmap; AGraphic: TGraphic);
  end;

implementation

{ TIconUtilsCommon }

procedure TIconUtilsCommon.PrepareBitmap(const ABitmap: TBitmap;
  AHeight: integer);
begin
  ABitmap.Width := AHeight;
  ABitmap.Height := AHeight;
  ABitmap.Canvas.Brush.Color := cIconUtilsTransparentColor;
  ABitmap.Canvas.FillRect(0, 0, ABitmap.Width, ABitmap.Height);
  ABitmap.Mask(cIconUtilsTransparentColor);
end;

procedure TIconUtilsCommon.RenderBitmap(const ABitmap: TBitmap;
  AGraphic: TGraphic);
var
  mHalf: integer;
begin
  if AGraphic.Height <= ABitmap.Height then begin
   mHalf := (ABitmap.Height - AGraphic.Height) div 2;
   ABitmap.Canvas.Draw(mHalf, mHalf, AGraphic);
  end
  else
   ABitmap.Canvas.StretchDraw(TRect.Create(0, 0, ABitmap.Width - 1, ABitmap.Height - 1), AGraphic);
end;

end.


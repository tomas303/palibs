(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
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


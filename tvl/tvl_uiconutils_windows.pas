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
unit tvl_uiconutils_windows;

{$mode objfpc}{$H+}

interface

{$IFDEF WINDOWS}

uses
  Classes, SysUtils, Graphics, LCLIntf, LCLType, shellapi, tvl_uiconutils_common;

type

  { TIconUtilsWindows }

  TIconUtilsWindows = class(TIconUtilsCommon)
  protected
    procedure RenderAppIcon(const AApplication: string; ABitmap: TBitmap;
      AIconHeight: Integer);
  end;

{$ENDIF WINDOWS}

implementation

{$IFDEF WINDOWS}

{ TIconUtilsWindows }

procedure TIconUtilsWindows.RenderAppIcon(const AApplication: string; ABitmap: TBitmap;
  AIconHeight: Integer);
var
  mIcon: TIcon;
  mApp: UnicodeString;
  mhIcon: HICON = 0;
  mhIconLarge: HICON = 0;
  mhIconSmall: HICON = 0;
  mHalf: Integer;
begin
  PrepareBitmap(ABitmap, AIconHeight);
  mIcon := TIcon.Create;
  try
    mApp := AApplication;
    if ExtractIconEx(PWideChar(mApp), 0, mhIconLarge, mhIconSmall, 1) = 2 then
    begin
      if AIconHeight >= GetSystemMetrics(SM_CXSMICON) + 8 then
        mhIcon := mhIconLarge
      else
        mhIcon := mhIconSmall;
      mIcon.Handle := mhIcon;
      RenderBitmap(ABitmap, mIcon);
    end;
  finally
    mIcon.Free;
  end;
end;

{$ENDIF WINDOWS}

end.


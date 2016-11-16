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


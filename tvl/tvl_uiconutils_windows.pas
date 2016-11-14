unit tvl_uiconutils_windows;

{$mode objfpc}{$H+}

interface

{$IFDEF WINDOWS}

uses
  Classes, SysUtils, Graphics, LCLIntf, LCLType, shellapi;

type

  { TIconUtilsWindows }

  TIconUtilsWindows = class(TInterfacedObject)
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
const
  // by test looks the best
  cTrColor = TColor($CCCCCC);
var
  mIcon: TIcon;
  mApp: UnicodeString;
  mhIcon: HICON = 0;
  mhIconLarge: HICON = 0;
  mhIconSmall: HICON = 0;
  mHalf: Integer;
begin
  ABitmap.Transparent := False;
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
      ABitmap.Width := AIconHeight;
      ABitmap.Height := AIconHeight;
      ABitmap.Canvas.Brush.Color := cTrColor;
      ABitmap.Canvas.FillRect(0, 0, ABitmap.Width, ABitmap.Height);
      ABitmap.TransparentColor := cTrColor;
      ABitmap.Transparent := True;
      if mIcon.Height <= ABitmap.Height then begin
        mHalf := (AIconHeight - mIcon.Height) div 2;
        ABitmap.Canvas.Draw(mHalf, mHalf, mIcon);
      end
      else
        ABitmap.Canvas.StretchDraw(TRect.Create(0, 0, ABitmap.Width - 1, ABitmap.Height - 1), mIcon);
    end;
  finally
    mIcon.Free;
  end;
end;

{$ENDIF WINDOWS}

end.


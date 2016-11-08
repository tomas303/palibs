unit tvl_uiconutils;

{$mode objfpc}{$H+}

interface

uses
  tvl_iiconutils,
{$IFDEF UNIX}
  tvl_uiconutils_unix;
{$ENDIF UNIX}
{$IFDEF WINDOWS}
  tvl_uiconutils_windows;
{$ENDIF WINDOWS}

type

  { TIconUtils }

{$IFDEF UNIX}
  TIconUtils = class(TIconUtilsUnix, IIconUtils)
{$ENDIF UNIX}
{$IFDEF WINDOWS}
  TIconUtils = class(TIconUtilsWindows, IIconUtils)
{$ENDIF WINDOWS}
  end;

implementation

end.


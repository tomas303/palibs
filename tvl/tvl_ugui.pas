unit tvl_ugui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tvl_igui, LCLIntf, LCLType;

type

  { TGUI }

  TGUI = class(TInterfacedObject, IGUI)
  protected
    // IGUI
    function MenuHeight: integer;
    function ScreenWidth: integer;
    function ScrenHeight: integer;
  end;

implementation

{ TGUI }

function TGUI.MenuHeight: integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
end;

function TGUI.ScreenWidth: integer;
begin
  Result := GetSystemMetrics(SM_CXSCREEN);
end;

function TGUI.ScrenHeight: integer;
begin
  Result := GetSystemMetrics(SM_CYSCREEN);
end;

end.


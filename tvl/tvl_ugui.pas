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
  end;

implementation

{ TGUI }

function TGUI.MenuHeight: integer;
begin
  Result := GetSystemMetrics(SM_CYMENU);
end;

end.


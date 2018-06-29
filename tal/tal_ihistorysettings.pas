unit tal_ihistorysettings;

{$mode delphi}{$H+}

interface

uses
  Controls;

type
  IHistorySettings = interface
  ['{55CDFF2B-1AFA-453D-A4AC-0BE498683341}']
    procedure Load(const ATopControl: TWinControl; AInside: Boolean = True); overload;
    procedure Load(const ATopControl: TWinControl; const AID: string; AInside: Boolean = True); overload;
    //procedure Save(const AID; const AControls: array of TControl);
    procedure Save(const ATopControl: TWinControl; AInside: Boolean = True); overload;
    procedure Save(const ATopControl: TWinControl; const AID: string; AInside: Boolean = True); overload;
  end;

implementation

end.


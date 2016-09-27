unit tvl_ibindings;

interface

uses
  trl_irttibroker, trl_ipersist, Controls, fgl;

type

  TBinderChangeEvent = procedure(const ADataItem: IRBDataItem; AControl: TWinControl) of object;

  TBinderChangeEvents = specialize TFPGList<TBinderChangeEvent>;


  { IRBDataBinder }

  IRBDataBinder = interface
  ['{015ECE02-6F9E-4071-BDCE-63BD11F6FAD9}']
    procedure BindArea(AContainer: TWinControl; const AData: IRBData);
    procedure BindControl(AControl: TWinControl; const AName: string);
    procedure Unbind;
    procedure DataChange;
    procedure Flush(AControl: TControl = nil);
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
    property Data: IRBData read GetData write SetData;
    procedure RegisterChangeEvent(const AItemName: string; AEvent: TBinderChangeEvent);
    procedure UnregisterChangeEvent(const AItemName: string; AEvent: TBinderChangeEvent);
  end;

  IRBTallyBinder = interface
  ['{15291FE4-A0AC-11E3-82F7-08002721C44F}']
    procedure Bind(const AListControl: TWinControl; const AClassName: string);
    procedure Unbind;
    procedure Reload;
    function GetCurrentData: IRBData;
    property CurrentData: IRBData read GetCurrentData;
  end;

  IRBBehavioralBinder = interface
  ['{8FA26A2F-57D6-44FA-B824-EACAC24E5FA3}']
    procedure Bind(AContainer: TWinControl);
    procedure Unbind;
  end;


  IRBDesigner = interface
  ['{4E3E03A4-B21F-4194-B7A4-063B31038D4B}']
    //procedure Bind(AContainer: TWinControl; const AStore: IPersistStore; const AFactory: IRBFactory;
    //  const ADataQuery: IPersistQuery; const ACtlClasses: IRBPersistClassRegister);
  end;

implementation

end.


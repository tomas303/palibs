unit tvl_ibindings;

interface

uses
  trl_irttibroker, trl_ipersist, trl_ipersiststore, Controls;

type

  { IRBDataBinder }

  IRBDataBinder = interface
  ['{015ECE02-6F9E-4071-BDCE-63BD11F6FAD9}']
    procedure Bind(AContainer: TWinControl; const AData: IRBData);
    procedure Unbind;
    procedure DataChange;
    function GetData: IRBData;
    procedure SetData(AValue: IRBData);
    property AData: IRBData read GetData write SetData;
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


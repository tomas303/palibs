unit tvl_iedit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker, Forms;

type
  IEditData = interface
  ['{85B5EB62-8686-4BEC-90F2-7D216E513C40}']
     function Edit(const AData: IRBData): Boolean;
  end;

  IListData = interface
  ['{D2100F1E-5141-4935-98E5-19D2F6BE4BA4}']
     procedure List;
  end;

  IMainForm = interface
  ['{C322BECE-AE6A-4A1B-9794-BCE551D0A82D}']
    procedure StartUp;
    procedure ShutDown;
    procedure ConnectCloseHandler(OnCloseHandler: TCloseEvent);
  end;

implementation

end.


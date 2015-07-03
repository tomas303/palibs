unit iStart;

{$mode objfpc}{$H+}

interface

uses
  rtti_broker_iBroker, rtti_idebinder_iBindings, Controls, Forms;

type

  { IAppFactory }

  IAppFactory = interface
  ['{F73EFD7D-6F3F-4ED9-BFE2-F02DF1690E87}']
    function CreateObject(const AClass: string): TObject;
    function FindClass(const AClass: String): TClass;
  end;

  { IAppStoreList }

  IAppStoreList = interface
  ['{5CA63112-A45E-4D54-BF26-9CEECB37878E}']
  end;

  { IAppStore }

  IAppStore = interface
  ['{B621A696-FAC1-4652-95DB-FB365A989ADF}']
    procedure Save(AData: TObject);
    function Load(const AClass: string; const AProperty, AValue: string): TObject;
    function LoadList(const AClass: string): IAppStoreList;
    procedure Flush;
    procedure Delete(AData: TObject);
  end;

  IAppContext = interface
  ['{3452F867-A06C-4BC7-98FC-0E7E830EAFED}']
    function GetConfig: IAppStore;
    function GetFactory: IAppFactory;
    property Config: IAppStore read GetConfig;
    property Factory: IAppFactory read GetFactory;
  end;

  { IApp }

  IApp = interface
  ['{98A3DEC9-BEFC-4EC0-BE7D-67A8E307977D}']
    procedure RegisterDataClasses(const AClasses: array of TClass);
    procedure RegisterControlClasses(const ACtlClasses: array of TControlClass);
    procedure Run(const AFormClass: TFormClass);
  end;

  { IAppContext_kvyhozeni }

  IAppContext_kvyhozeni = interface
  ['{44D90982-3C93-4FAC-A1E3-48C91A901288}']
    function GetConfig: IRBStore;
    function GetData: IRBStore;
    function GetClassFactory: IRBFactory;
    property Config: IRBStore read GetConfig;
    property Data: IRBStore read GetData;
    property ClassFactory: IRBFactory read GetClassFactory;
  end;

  IStartContext = interface
  ['{44D9302B-B326-45FD-B659-9B1A28E274C5}']
    function GetBinderContext: IRBBinderContext;
    function GetDataStore: IRBStore;
    function GetSerialFactory: IRBFactory;
    function GetDataQuery: IRBDataQuery;
    function GetDesigner: IRBDesigner;
    property SerialFactory: IRBFactory read GetSerialFactory;
    property DataStore: IRBStore read GetDataStore;
    property DataQuery: IRBDataQuery read GetDataQuery;
    property Designer: IRBDesigner read GetDesigner;
    property BinderContext: IRBBinderContext read GetBinderContext;
  end;

  IStartContextConnectable = interface
  ['{E8F2A90A-20DE-446A-8F19-FBD6A9DD0833}']
    procedure Connect(const AContext: IStartContext);
  end;

implementation

end.


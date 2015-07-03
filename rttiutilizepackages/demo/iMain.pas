unit iMain;

{$mode objfpc}{$H+}

interface

uses
  rtti_serializer_iManager;

type

  { IMainContext }

  IMainContext = interface
  ['{671CFABE-2B03-4AB2-9441-6581CBB95BC6}']
    function GetDataStore: ISerialStore;
    function GetSerialFactory: ISerialFactory;
    property SerialFactory: ISerialFactory read GetSerialFactory;
    property DataStore: ISerialStore read GetDataStore;
  end;

implementation

end.


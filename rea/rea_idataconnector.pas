unit rea_idataconnector;

{$mode objfpc}{$H+}

interface

uses
  rea_iflux, rea_idesigncomponent, rea_udesigncomponentdata;

type
  IDataConnector = interface
  ['{ADEBA04F-AC04-47BA-A3B4-912C69E33BB2}']
    procedure Connect(const AProvider: IGridDataProvider; AData: TGridData; AIndexes: array of Integer);
    procedure Connect(const AProvider: IGridDataProvider; AData: TEditData; AIndex: Integer);
  end;

implementation

end.


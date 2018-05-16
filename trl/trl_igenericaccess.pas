unit trl_igenericaccess;

{$mode objfpc}{$H+}

interface

type
  TGenericAccessType = (gatUndefined, gatInt, gatStr, gatBool, gatGuid, gatInterface);

  IGenericAccess = interface
  ['{F4195F07-5EF9-4916-B660-4327F8237B3B}']
    function AsStr(const AName: string): string;
    function AsInt(const AName: string): integer;
    function AsBool(const AName: string): Boolean;
    function AsGuid(const AName: string): TGUID;
    function AsIntf(const AName: string): IUnknown;
    function AsStr(const AIndex: integer): string;
    function AsInt(const AIndex: integer): integer;
    function AsBool(const AIndex: integer): Boolean;
    function AsGuid(const AIndex: integer): TGUID;
    function AsIntf(const AIndex: integer): IUnknown;
    function SetStr(const AName: string; const AValue: string): IGenericAccess;
    function SetInt(const AName: string; const AValue: integer): IGenericAccess;
    function SetBool(const AName: string; const AValue: Boolean): IGenericAccess;
    function SetGuid(const AName: string; const AValue: TGUID): IGenericAccess;
    function SetIntf(const AName: string; const AValue: IUnknown): IGenericAccess;
    function SetStr(const AIndex: integer; const AValue: string): IGenericAccess;
    function SetInt(const AIndex: integer; const AValue: integer): IGenericAccess;
    function SetBool(const AIndex: integer; const AValue: Boolean): IGenericAccess;
    function SetGuid(const AIndex: integer; const AValue: TGUID): IGenericAccess;
    function SetIntf(const AIndex: integer; const AValue: IUnknown): IGenericAccess;
    function Count: integer;
    function GAType(const AName: string): TGenericAccessType;
    function GAType(const AIndex: integer): TGenericAccessType;
    function Name(const AIndex: integer): string;
  end;

implementation

end.


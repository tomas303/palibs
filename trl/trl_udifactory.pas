unit trl_udifactory;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_idifactory, trl_dicontainer;

type

  { TDIFactory }

  TDIFactory = class(TCustomDIFactory, IDIFactory)
  protected
    //IDIFactory
    function Locate(AClass: TClass; const AID: string = ''): pointer; overload;
    function Locate(AInterface: TGUID; const AID: string = ''): pointer; overload;
    function Locate(const AClass: string; const AID: string = ''): pointer; overload;
  end;

implementation

{ TDIFactory }

function TDIFactory.Locate(AClass: TClass; const AID: string): pointer;
begin
  Result := Container.Locate(AClass, AID);
end;

function TDIFactory.Locate(AInterface: TGUID; const AID: string): pointer;
begin
  Result := Container.Locate(AInterface, AID);
end;

function TDIFactory.Locate(const AClass: string; const AID: string): pointer;
begin
  Result := Container.Locate(AClass, AID);
end;

end.


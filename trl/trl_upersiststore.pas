unit trl_upersiststore;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_ifactory, trl_irttibroker,
  trl_dicontainer, fgl, trl_upersist;

type

  { TPersistFactory }

  TPersistFactory = class(TCustomDIFactory, IPersistFactory)
  protected
    // IPersistFactory
    function CreateObject(const AClass: string): IRBData;
    function Create(const AClass: string; const AID: string = ''): TObject; overload;
    function Create(AInterface: TGUID; const AID: string = ''): IUnknown; overload;
  end;

implementation

{ TPersistFactory }

function TPersistFactory.CreateObject(const AClass: string): IRBData;
begin
  if AClass = '' then
    raise Exception.Create('Try create IRBData object with empty class');
  Result := Container.Locate(IRBData, AClass);
end;

function TPersistFactory.Create(const AClass: string; const AID: string
  ): TObject;
begin
  Result := Container.Locate(AClass, AID);
end;

function TPersistFactory.Create(AInterface: TGUID; const AID: string): IUnknown;
begin
  Result := Container.Locate(AInterface, AID);
end;

end.


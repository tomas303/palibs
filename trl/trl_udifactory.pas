unit trl_udifactory;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_idifactory, trl_dicontainer, trl_iprops;

type

  { TDIFactory }

  TDIFactory = class(TCustomDIFactory, IDIFactory)
  protected
    //IDIFactory
    function Locate(AClass: TClass; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function Locate(AInterface: TGUID; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
    function Locate(const AClass: string; const AID: string = ''; const AProps: IProps = nil): pointer; overload;
  end;

implementation

{ TDIFactory }

function TDIFactory.Locate(AClass: TClass; const AID: string; const AProps: IProps = nil): pointer;
begin
  Result := Container.Locate(AClass, AID, AProps);
end;

function TDIFactory.Locate(AInterface: TGUID; const AID: string; const AProps: IProps = nil): pointer;
begin
  Result := Container.Locate(AInterface, AID, AProps);
end;

function TDIFactory.Locate(const AClass: string; const AID: string; const AProps: IProps = nil): pointer;
begin
  Result := Container.Locate(AClass, AID, AProps);
end;

end.


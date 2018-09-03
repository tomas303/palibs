unit trl_imetaelement;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops;

type

  { IMetaElement }

  IMetaElement = interface;

  IMetaElementEnumerator = interface
  ['{986E6CB6-E8A2-42AA-819E-7BB1F1A2C1A7}']
    function MoveNext: Boolean;
    function GetCurrent: IMetaElement;
    property Current: IMetaElement read GetCurrent;
  end;

  IMetaElement = interface
  ['{13BBE7DA-46FC-4DC1-97AD-73913576EC12}']
    function Guid: TGuid;
    function GetTypeGuid: string;
    function GetTypeID: string;
    function GetProps: IProps;
    property TypeGuid: string read GetTypeGuid;
    property TypeID: string read GetTypeID;
    property Props: IProps read GetProps;
    function GetEnumerator: IMetaElementEnumerator;
  end;

  TMetaElementArray = array of IMetaElement;

implementation

end.


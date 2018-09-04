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
    function GetIsMetaElementProvider: Boolean;
    property TypeGuid: string read GetTypeGuid;
    property TypeID: string read GetTypeID;
    property Props: IProps read GetProps;
    property IsMetaElementProvider: Boolean read GetIsMetaElementProvider;
    function GetEnumerator: IMetaElementEnumerator;
  end;

  TMetaElementArray = array of IMetaElement;

  { IMetaElementProvider }

  IMetaElementProvider = interface
  ['{C6217A1B-069F-4E21-A798-18C007D84008}']
    function ProvideMetaElement: IMetaElement;
  end;


implementation

end.


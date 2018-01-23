unit rea_ireact;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, rea_ibits;

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
    function Info: string;
    function GetEnumerator: IMetaElementEnumerator;
  end;

  TMetaElementArray = array of IMetaElement;

  IMetaElementFactory = interface
  ['{64895959-43CF-43E3-A3CE-1EF69608BEBE}']
    function CreateElement(const ATypeGuid: TGuid): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AChildren: array of IMetaElement): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const AProps: IProps;
      const AChildren: array of IMetaElement): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AProps: IProps): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AChildren: array of IMetaElement): IMetaElement;
    function CreateElement(const ATypeGuid: TGuid; const ATypeID: string; const AProps: IProps;
      const AChildren: array of IMetaElement): IMetaElement;
  end;

  IReactComponentMachinery = interface
  ['{B4CFF5F0-7493-414C-B540-F5CF9F2AA74E}']
    procedure RenderChildren(const AElement: IMetaElement);
    function Bit: IBit;
  end;

  IReactComponentMachineryMiddle = interface(IReactComponentMachinery)
  ['{24628866-7C1F-4699-BEC1-2D40818B2789}']
  end;

  IReactComponentMachineryLeaf = interface(IReactComponentMachinery)
  ['{5769E225-1F08-4876-8007-B61D4E21D294}']
  end;

  { IReactComponent }

  IReactComponent = interface
  ['{FB2D2C72-1E52-40C0-BE52-63AFA7448590}']
    procedure Render;
    procedure Render(const AProps: IProps; const AParentElement: IMetaElement);
    function GetBit: IBit;
    property Bit: IBit read GetBit;
    function IsDirty: Boolean;
  end;

  IReactComponentApp = interface(IReactComponent)
  ['{CAD729C0-F921-4932-9583-921B177142D2}']
  end;

  IReactComponentMainForm = interface(IReactComponent)
  ['{FA7A6E42-52EC-44DB-B522-E840DC9F34A3}']
  end;

  IReactComponentForm = interface(IReactComponent)
  ['{E549424C-6C94-48BC-934D-B100341207C9}']
  end;

  IReactComponentEdit = interface(IReactComponent)
  ['{E649F83B-D785-4209-B2F1-B072ECFDC680}']
  end;

  IReactComponentEdits = interface(IReactComponent)
  ['{B5B85863-29E5-4444-A65C-890A713E51C6}']
  end;

  IReactComponentButton = interface(IReactComponent)
  ['{F4526301-63C9-4270-B61F-AD8BFAC60220}']
  end;

  IReactComponentButtons = interface(IReactComponent)
  ['{F350FB28-34B2-4626-ABD9-ABA2AE87B760}']
  end;

  IReactComponentHeader = interface(IReactComponent)
  ['{4F6C423B-D002-4717-B455-67232370A145}']
  end;

  IReconciliator = interface
  ['{066DDE74-0738-4636-B8DD-E3E1BA873D2E}']
    function Reconciliate(const AComponent: IReactComponent; var ABit: IBit; const AOldElement, ANewElement: IMetaElement): Boolean;
  end;

  IReact = interface
  ['{21EAABB4-22F1-461B-A2A7-2B6CD05B04DB}']
    procedure Render(const AComponent: IReactComponent);
    procedure RenderAsync(const AComponent: IReactComponent);
  end;

implementation

end.


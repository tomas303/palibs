unit rea_ireact;

{$mode objfpc}{$H+}

interface

uses
  trl_iprops, rea_ibits, trl_imetaelement;

type

  IReactComponentMachinery = interface
  ['{B4CFF5F0-7493-414C-B540-F5CF9F2AA74E}']
    procedure RenderChildren(const AElement: IMetaElement);
    function Bit: IBit;
    procedure ChangeProps(const AProps: IProps);
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
    procedure Render(const AParentElement: IMetaElement);
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

  IReactComponentButton = interface(IReactComponent)
  ['{F4526301-63C9-4270-B61F-AD8BFAC60220}']
  end;

  IReactComponentHeader = interface(IReactComponent)
  ['{4F6C423B-D002-4717-B455-67232370A145}']
  end;

  IReact = interface
  ['{21EAABB4-22F1-461B-A2A7-2B6CD05B04DB}']
    procedure Render(const AComponent: IReactComponent);
    procedure RenderAsync(const AComponent: IReactComponent);
  end;

  cProps = class
  public const
    Children = 'Children';
    Layout = 'Layout';
    Place = 'Place';
    Title = 'Title';
    Color = 'Color';
    Width = 'Width';
    Height = 'Height';
    MMLeft = 'MMLeft';
    MMTop = 'MMTop';
    MMWidth = 'MMWidth';
    MMHeight = 'MMHeight';
    Value = 'Value';
    Text = 'Text';
    Border = 'Border';
    BorderColor = 'BorderColor';
    FontColor = 'FontColor';
    Transparent = 'Transparent';
    SizeNotifier = 'SizeNotifier';
    MoveNotifier = 'MoveNotifier';
    ActivateNotifier = 'ActivateNotifier';
    ClickNotifier = 'ClickNotifier';
    OnTextNotifier = 'OnTextNotifier';
  end;

implementation

end.


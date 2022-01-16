unit rea_idesigncomponent;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$modeswitch multihelpers}

interface

uses
  trl_imetaelement, trl_iprops;

type

  cProps = class
    public const
      Children = 'Children';
      Layout = 'Layout';
      Place = 'Place';
      Title = 'Title';
      Color = 'Color';
      Left = 'Left';
      Top = 'Top';
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
      CloseQueryNotifier = 'CloseQueryNotifier';
      ClickNotifier = 'ClickNotifier';
      ClickFunc = 'ClickFunc';
      OnTextNotifier = 'OnTextNotifier';
      AskNotifier = 'AskNotifier';
      RowMMHeight = 'RowMMHeight';
      ColMMWidth = 'ColMMWidth';
      RowEvenColor = 'RowEvenColor';
      RowOddColor = 'RowOddColor';
      ColEvenColor = 'ColEvenColor';
      ColOddColor = 'ColOddColor';
      Caption = 'Caption';
      CaptionWidth = 'CaptionWidth';
      PairWidth = 'PairWidth';
      CaptionEdge = 'CaptionEdge';
      SwitchEdge = 'SwitchEdge';
      SwitchSize = 'SwitchSize';
      ID = 'ID';
      DataPath = 'DataPath';
    end;

  cEdge = class
    public const
       Left = 0;
       Top = 1;
       Right = 2;
       Bottom = 3;
    end;

  IGridDataProvider = interface
  ['{308CF052-70BC-46D9-8B76-C565B3920261}']
    function Prev: Boolean;
    function Next: Boolean;
    function IsEmpty: Boolean;
    function GetValue(Ind: integer): string;
    procedure SetValue(Ind: integer; AValue: string);
    property Value[Ind: integer]: string read GetValue write SetValue; default;
  end;

  IDesignComponent = interface
  ['{AD83F143-4C0A-4703-A38A-E3F175036FE6}']
    function Compose(const AParentProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
  end;

  IDesignComponentApp = interface(IDesignComponent)
  ['{4035F57F-CA74-4D0E-8972-3A2162FAB714}']
  end;

  IDesignComponentForm = interface(IDesignComponent)
  ['{AE80667B-FA5E-432F-9667-402165481946}']
  end;

  IDesignComponentEdit = interface(IDesignComponent)
  ['{BBF9AEB9-28A5-4E15-840D-3483885936E1}']
  end;

  IDesignComponentButton = interface(IDesignComponent)
  ['{9EFB4F6C-F20C-4234-BE3C-C6F1F89A9314}']
  end;

  IDesignComponentHeader = interface(IDesignComponent)
  ['{B16AAF18-076F-449D-82B1-BB6AE4962B4B}']
  end;

  IDesignComponentGrid = interface(IDesignComponent)
  ['{72646A21-0264-4798-A9DE-C0B3E843806B}']
  end;

  IDesignComponentPager = interface(IDesignComponent)
  ['{D3ED4153-75BB-493B-90FD-3DC074149FD6}']
  end;

  IDesignComponentPagerHelper = type helper for IDesignComponentPager
  public const
    SwitchEdge = cProps.SwitchEdge;
    SwitchEdgeLeft = cEdge.Left;
    SwitchEdgeTop = cEdge.Top;
    SwitchEdgeRight = cEdge.Right;
    SwitchEdgeBottom = cEdge.Bottom;
    SwitchSize = cProps.SwitchSize;
  end;

  IDesignComponentLabelEdit = interface(IDesignComponent)
  ['{EAA33F6C-C4B8-40A2-B37E-82C7EE013EF7}']
  end;

  IDesignComponentLabelEditHelper = type helper for IDesignComponentLabelEdit
  public const
    CaptionEdge = cProps.CaptionEdge;
    CaptionEdgeLeft = cEdge.Left;
    CaptionEdgeTop = cEdge.Top;
    CaptionEdgeRight = cEdge.Right;
    CaptionEdgeBottom = cEdge.Bottom;
    Caption = cProps.Caption;
  end;

  IDesignComponentFactory = interface
  ['{C271A13A-4306-4C17-8A21-F1E66B94F570}']
    function New(const AProps: IProps): IDesignComponent;
  end;

  IDesignComponentFormFactory = interface(IDesignComponentFactory)
  ['{24916508-17A0-4577-A1F0-55E6605F5039}']
  end;

  IDesignComponentButtonFactory = interface(IDesignComponentFactory)
  ['{F09A5425-2065-45AA-A11B-9A58E4DC7AF2}']
  end;

  IDesignComponentPagerFactory = interface(IDesignComponentFactory)
  ['{CFFD386A-150F-455A-8CC7-78917625F7B1}']
  end;

  IDesignComponentPagerSwitchFactory = interface(IDesignComponentFactory)
  ['{7117E5CD-78AC-4747-9735-C76A2226424D}']
  end;

  IDesignComponentGridFactory = interface(IDesignComponentFactory)
  ['{79082132-2BCB-4996-9C3D-556D25172557}']
  end;

const
  cEditHeight = 25;
  cCaptionWidth = 100;

implementation

end.


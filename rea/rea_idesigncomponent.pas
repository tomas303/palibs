unit rea_idesigncomponent;

{$mode delphi}{$H+}
{$modeswitch typehelpers}
{$modeswitch multihelpers}
{$modeswitch advancedrecords}

interface

uses
  trl_imetaelement, trl_iprops, trl_pubsub, rea_ibits, nullable, trl_funcp;

type

  cProps = class
    public const
      Children = 'Children';
      Layout = 'Layout';
      Place = 'Place';
      Title = 'Title';
      Color = 'Color';
      TextColor = 'TextColor';
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
      ClickFunc = 'ClickFunc';
      TextChangedFunc = 'TextChangedFunc';
      KeyDownFunc = 'KeyDownFunc';
      Caption = 'Caption';
      CaptionWidth = 'CaptionWidth';
      CaptionHeight = 'CaptionHeight';
      PairWidth = 'PairWidth';
      CaptionEdge = 'CaptionEdge';
      CaptionEditBorder = 'CaptionEditBorder';
      CaptionEditBorderColor = 'CaptionEditBorderColor';
      SwitchEdge = 'SwitchEdge';
      SwitchSize = 'SwitchSize';
      BoxLaticeSize = 'BoxLaticeSize';
      FontDirection = 'FontDirection';
      StyleKind = 'StyleKind';
      Focused = 'Focused';
      Flat = 'Flat';
      ID = 'ID';
      Data = 'Data';
    end;

  cPager = class(cProps)
  public const
    PageIndex = 'PageIndex';
    PagerData = 'PagerData';
  end;

  cEdit = class(cProps)
  public const
    PSTextChannel = 'PSTextChannel';
    PSKeyDownChannel = 'PSKeyDownChannel';
  end;

  cForm = class(cProps)
  public const
    PSCloseChannel = 'PSCloseChannel';
    PSSizeChannel = 'PSSizeChannel';
    PSPositionChannel = 'PSPositionChannel';
    PSActivateChannel = 'PSActivateChannel';
  end;

  cButton = class(cProps)
  public const
    PSClickChannel = 'PSClickChannel';
  end;

  cGrid = class(cProps)
  public const
    RowCount = 'RowCount';
    ColCount = 'ColCount';
    RowMMHeight = 'RowMMHeight';
    ColMMWidth = 'ColMMWidth';
    LaticeColColor = 'LaticeColColor';
    LaticeRowColor = 'LaticeRowColor';
    LaticeColSize = 'LaticeColSize';
    LaticeRowSize = 'LaticeRowSize';
    RowEvenColor = 'RowEvenColor';
    RowOddColor = 'RowOddColor';
    ColEvenColor = 'ColEvenColor';
    ColOddColor = 'ColOddColor';
  end;

  cAction = class(cProps)
  public const
    ActionID = 'ActionID';
    Dispatcher = 'Dispatcher';
  end;

  cEdge = class
  public const
    Left = 0;
    Top = 1;
    Right = 2;
    Bottom = 3;
  end;

  { TGUIData }

  TGUIData = record
  public type
    TGUIAction = (gaRender);
  private
    fAction: TGUIAction;
  public
    constructor Create(Action: TGUIAction);
    class operator equal(a,b: TGUIData): Boolean;
    property Action: TGUIAction read fAction;
  end;

  {

  insertrecord
  deleterecord
  askforrelativedata
  receiverelativedata
  recivemovementcurrent

  }

  { TGridCmdMove }

  TGridCmdMove = record
  private
    fDelta: Integer;
  public
    constructor Create(ADelta: Integer);
    class operator equal(a,b: TGridCmdMove): Boolean;
    property Delta: Integer read fDelta;
  end;

  { TGridCmdInfo }

  TGridCmdInfo = record
  private
    fFromPos: Integer;
    fToPos: Integer;
  public
    constructor Create(AFromPos, AToPos: Integer);
    class operator equal(a,b: TGridCmdInfo): Boolean;
    property FromPos: Integer read fFromPos;
    property ToPos: Integer read fToPos;
  end;

  { TGridRecord }

  TGridRecord = record
  private
    fPos: Integer;
    fData: TArray<String>;
  public
    constructor Create(APos: Integer; const AData: TArray<String>);
    class operator equal(a,b: TGridRecord): Boolean;
    property Pos: Integer read fPos;
    property Data: TArray<String> read fData;
  end;

  { TGridMover }

  TGridMover = record
  private
    fDelta: TOptional<Integer>;
  public
    class function New: TGridMover; static; overload;
    class function New(ADelta: Integer): TGridMover; static; overload;
    class operator Initialize(var a: TGridMover);
    class operator equal(a,b: TGridMover): Boolean;
    property Delta: TOptional<Integer> read fDelta;
  end;


  IPSGUIChannel = IPubSubDataChannel<TGUIData>;

  IPSGridCmdMoveChannel = IPubSubDataChannel<TGridCmdMove>;
  IPSGridCmdInfoChannel = IPubSubDataChannel<TGridCmdInfo>;
  IPSGridRecordChannel = IPubSubDataChannel<TGridRecord>;
  IPSGridMoverChannel = IPubSubDataChannel<TGridMover>;

  { IGridDataProvider }

  IGridDataProvider = interface
  ['{308CF052-70BC-46D9-8B76-C565B3920261}']
    function Prev: Boolean;
    function Next: Boolean;
    function IsEmpty: Boolean;
    function GetValue(Ind: integer): string;
    procedure SetValue(Ind: integer; AValue: string);
    function NewBookmark: IInterface;
    procedure GotoBookmark(ABookmark: IInterface);
    function GetSilent: Boolean;
    procedure SetSilent(AValue: Boolean);
    function GetMoveActionID: Integer;
    property Value[Ind: integer]: string read GetValue write SetValue; default;
    property Silent: Boolean read GetSilent write SetSilent;
    property MoveActionID: Integer read GetMoveActionID;
  end;

  IDesignComponent = interface
  ['{AD83F143-4C0A-4703-A38A-E3F175036FE6}']
    function Compose(const AParentProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
  end;

  IDesignComponentApp = interface(IDesignComponent)
  ['{4035F57F-CA74-4D0E-8972-3A2162FAB714}']
    function PSGUIChannel: IPSGUIChannel;
  end;

  IDesignComponentForm = interface(IDesignComponent)
  ['{AE80667B-FA5E-432F-9667-402165481946}']
    function PSCloseChannel: IPSCloseChannel;
    function PSSizeChannel: IPSSizeChannel;
    function PSPositionChannel: IPSPositionChannel;
    function PSActivateChannel: IPSActivateChannel;
  end;

  IDesignComponentEdit = interface(IDesignComponent)
  ['{BBF9AEB9-28A5-4E15-840D-3483885936E1}']
    function PSTextChannel: IPSTextChannel;
    function PSKeyDownChannel: IPSKeyChannel;
  end;

  IDesignComponentText = interface(IDesignComponent)
  ['{E5A64708-B673-4284-96B7-F7ACAE44BE0C}']
  end;

  IDesignComponentButton = interface(IDesignComponent)
  ['{9EFB4F6C-F20C-4234-BE3C-C6F1F89A9314}']
    function PSClickChannel: IPSClickChannel;
  end;

  IDesignComponentStrip = interface(IDesignComponent)
  ['{B16AAF18-076F-449D-82B1-BB6AE4962B4B}']
  end;

  IDesignComponentHBox = interface(IDesignComponent)
  ['{020BB807-2D13-4C66-98A5-CC99AC9ED0C9}']
  end;

  IDesignComponentVBox = interface(IDesignComponent)
  ['{FD920A3B-D673-4C70-8FD1-8A65B1D4CB61}']
  end;

  IDesignComponentGrid = interface(IDesignComponent)
  ['{72646A21-0264-4798-A9DE-C0B3E843806B}']
    function PSGridCmdMoveChannel: IPSGridCmdMoveChannel;
    function PSGridCmdInfoChannel: IPSGridCmdInfoChannel;
    function PSGridRecordChannel: IPSGridRecordChannel;
    function PSGridMoverChannel: IPSGridMoverChannel;
  end;

  IPSPagerChannel = IPubSubDataChannel<Integer>;

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

  IDesignComponentFrame = interface(IDesignComponent)
  ['{33417F52-E1E5-46E3-9E4B-7733FD2CA1D0}']
  end;

const
  cEditHeight = 25;
  cCaptionWidth = 100;

implementation

{ TGridMover }

class function TGridMover.New: TGridMover;
begin
end;

class function TGridMover.New(ADelta: Integer): TGridMover;
begin
  Result.fDelta := TOptional<Integer>.New(ADelta);
end;

class operator TGridMover.Initialize(var a: TGridMover);
begin
  a.fDelta := TOptional<Integer>.New;
end;

class operator TGridMover.equal(a, b: TGridMover): Boolean;
begin
  Result := a.Delta = b.Delta;
end;

{ TGridRecord }

constructor TGridRecord.Create(APos: Integer; const AData: TArray<String>);
begin
  fPos := APos;
  fData := AData;
end;

class operator TGridRecord.equal(a, b: TGridRecord): Boolean;
begin
  Result := (a.Pos = b.Pos) and (a.Data = b.Data);
end;

{ TGridCmdInfo }

constructor TGridCmdInfo.Create(AFromPos, AToPos: Integer);
begin
  fFromPos := AFromPos;
  fToPos := AToPos;
end;

class operator TGridCmdInfo.equal(a, b: TGridCmdInfo): Boolean;
begin
  Result := (a.FromPos = b.FromPos) and (a.ToPos = b.ToPos);
end;

{ TGridCmdMove }

constructor TGridCmdMove.Create(ADelta: Integer);
begin
  fDelta := ADelta;
end;

class operator TGridCmdMove.equal(a, b: TGridCmdMove): Boolean;
begin
  Result := a.Delta = b.Delta;
end;

{ TGUIData }

constructor TGUIData.Create(Action: TGUIAction);
begin
  fAction := Action;
end;

class operator TGUIData.equal(a, b: TGUIData): Boolean;
begin
  Result := Ord(a.Action) = Ord(b.Action);
end;

end.


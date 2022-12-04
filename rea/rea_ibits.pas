unit rea_ibits;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Controls, Graphics, LMessages, LCLType, trl_pubsub;

type
  IMessageNotifierBinder = interface
  ['{639A647A-916B-4906-A4F8-89CD4C0E129D}']
    procedure Bind(AControl: TControl);
    procedure Unbind;
    function Message: TLMessage;
  end;

  //TMessageObserverCallback = reference to procedure(AMessage: TLMessage);
  TMessageObserverCallback = procedure(AMessage: TLMessage) of object;

  IMessageObservable = interface
  ['{C3A8F182-7E03-4FA5-891B-0F41743EC400}']
    procedure Subscribe(ACallback: TMessageObserverCallback);
    procedure Unsubscribe(ACallback: TMessageObserverCallback);
    procedure Bind(AControl: TControl);
    procedure Unbind;
    function GetEnabled: Boolean;
    procedure SetEnabled(Enabled: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TSizeData }

  TSizeData = record
    Source: TObject;
    Width, Height: Integer;
    constructor Create(Source: TObject; Width, Height: Integer);
    class operator equal(a,b: TSizeData): Boolean;
    class operator notequal(a,b: TSizeData): Boolean;
  end;

  { TPositionData }

  TPositionData = record
    Source: TObject;
    Top, Left: Integer;
    constructor Create(Source: TObject; Top, Left: Integer);
    class operator equal(a,b: TPositionData): Boolean;
    class operator notequal(a,b: TPositionData): Boolean;
  end;


  TControlKey = (ckUnknown, ckEnter, ckLeft, ckRight, ckUp, ckDown, ckTab, ckEsc, ckPgUp, ckPgDown);

  { TKeyData }

  TKeyData = record
    ControlKey: TControlKey;
    constructor Create(ControlKey: TControlKey); overload;
    constructor Create(AMsg: TLMKey); overload;
    class operator equal(a,b: TKeyData): Boolean;
    class operator notequal(a,b: TKeyData): Boolean;
  end;

  IPSTextChannel = IPubSubDataChannel<String>;
  IPSSizeChannel = IPubSubDataChannel<TSizeData>;
  IPSPositionChannel = IPubSubDataChannel<TPositionData>;
  IPSKeyChannel = IPubSubDataChannel<TKeyData>;
  IPSCloseChannel = IPubSubChannel;
  IPSActivateChannel = IPubSubChannel;

  // wrapper for real control and its binder
  IBit = interface
  ['{479784FA-9E6B-4826-BCFE-92A676B2F7DD}']
    procedure Render;
    procedure RenderPaint(const ACanvas: TCanvas);
    procedure HookParent(const AParent: TWinControl);
  end;

  IFormBit = interface
  ['{64AAE7AE-AC24-4CBA-8430-2885CFC396AC}']
  end;

  IStripBit = interface
  ['{354C99E0-E2B2-43CF-89E7-95057DF390F0}']
  end;

  IEditBit = interface
  ['{0392A25E-1D90-4F37-9279-09F32D6F7D03}']
  end;

  ITextBit = interface
   ['{4178C1EC-AA39-4429-B48C-7058676ABA7B}']
  end;

  IButtonBit = interface
   ['{7FB3194B-62AB-44B7-8317-603D10706C71}']
  end;

implementation

{ TKeyData }

constructor TKeyData.Create(ControlKey: TControlKey);
begin
  Self.ControlKey := ControlKey;
end;

constructor TKeyData.Create(AMsg: TLMKey);
begin
  case AMsg.CharCode of
    VK_RETURN: ControlKey := ckEnter;
    VK_LEFT: ControlKey := ckLeft;
    VK_RIGHT: ControlKey := ckRight;
    VK_UP: ControlKey := ckUp;
    VK_DOWN: ControlKey := ckDown;
    VK_TAB: ControlKey := ckTab;
    VK_ESCAPE: ControlKey := ckEsc;
    VK_PRIOR: ControlKey := ckPgUp;
    VK_NEXT: ControlKey := ckPgDown;
  else
    ControlKey := ckUnknown;
  end;
end;

class operator TKeyData.equal(a, b: TKeyData): Boolean;
begin
  Result := Ord(a.ControlKey) = Ord(b.ControlKey);
end;

class operator TKeyData.notequal(a, b: TKeyData): Boolean;
begin
  Result := not(a = b);
end;

{ TPositionData }

constructor TPositionData.Create(Source: TObject; Top, Left: Integer);
begin
  Self.Source := Source;
  Self.Top := Top;
  Self.Left := Left;
end;

class operator TPositionData.equal(a, b: TPositionData): Boolean;
begin
  Result := (a.Left = b.Left) and (a.Top = b.Top);
end;

class operator TPositionData.notequal(a, b: TPositionData): Boolean;
begin
  Result := not(a = b);
end;

{ TSizeData }

constructor TSizeData.Create(Source: TObject; Width, Height: Integer);
begin
  Self.Source := Source;
  Self.Width := Width;
  Self.Height := Height;
end;

class operator TSizeData.equal(a, b: TSizeData): Boolean;
begin
  Result := (a.Width = b.Width) and (a.Height = b.Height);
end;

class operator TSizeData.notequal(a, b: TSizeData): Boolean;
begin
  Result := not(a = b);
end;

end.


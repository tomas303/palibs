unit rea_ibits;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Controls, Graphics, LMessages, trl_pubsub;

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

  IPSTextChannel = IPubSubDataChannel<String>;

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

  IPSSizeChannel = IPubSubDataChannel<TSizeData>;
  IPSPositionChannel = IPubSubDataChannel<TPositionData>;
  IPSCloseChannel = IPubSubChannel;

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


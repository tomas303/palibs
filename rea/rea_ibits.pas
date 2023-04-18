(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
unit rea_ibits;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, Controls, Graphics, LMessages, LCLType, LCLIntf, trl_pubsub;

type
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
    constructor Create(Source: TObject; Left, Top: Integer);
    class operator equal(a,b: TPositionData): Boolean;
    class operator notequal(a,b: TPositionData): Boolean;
  end;


  TControlKey = (ckUnknown, ckEnter, ckLeft, ckRight, ckUp, ckDown, ckTab, ckEsc, ckPgUp, ckPgDown, ckInsert, ckDelete,
    ckF1, ckF2, ckF3, ckF4, ckF5, ckF6, ckF7, ckF8, ckF9, ckF10, ckF11, ckF12);

  { TKeyData }

  TKeyData = record
  strict private
    fControlKey: TControlKey;
    fAlt: Boolean;
    fShift: Boolean;
    fCtrl: Boolean;
    fMeta: Boolean;
    function DecodeControlKey(Key: Word): TControlKey;
  public
    constructor Create(ControlKey: TControlKey); overload;
    constructor Create(ControlKey: TControlKey; ShiftState: TShiftState); overload;
    constructor Create(Key: Word; ShiftState: TShiftState); overload;
    constructor Create(AMsg: TLMKey); overload;
    function ControlKey: TControlKey;
    function Alt: Boolean;
    function Shift: Boolean;
    function Ctrl: Boolean;
    function Meta: Boolean;
    function NoModifier: Boolean;
    class operator equal(a,b: TKeyData): Boolean;
    class operator notequal(a,b: TKeyData): Boolean;
  end;

  { TFocusData }

  TFocusData = record
  strict private
    fSource: TObject;
    fFocused: Boolean;
  public
    constructor Create(ASource: TObject; AFocused: Boolean);
    class operator equal(a,b: TFocusData): Boolean;
    function Source: TObject;
    function Focused: Boolean;
  end;

  { TLayoutData }

  TLayoutData = record
  strict private
    fLeft: Integer;
    fTop: Integer;
    fWidth: Integer;
    fHeight: Integer;
  public
    constructor Create(ALeft, ATop, AWidth, AHeight: Integer);
    class operator equal(a,b: TLayoutData): Boolean;
    function Left: Integer;
    function Top: Integer;
    function Width: Integer;
    function Height: Integer;
  end;

  IPSTextChannel = IPubSubDataChannel<String>;
  IPSSizeChannel = IPubSubDataChannel<TSizeData>;
  IPSPositionChannel = IPubSubDataChannel<TPositionData>;
  IPSKeyChannel = IPubSubDataChannel<TKeyData>;
  IPSCloseChannel = IPubSubChannel;
  IPSActivateChannel = IPubSubChannel;
  IPSClickChannel = IPubSubChannel;
  IPSFocusChannel = IPubSubDataChannel<TFocusData>;
  IPSLayoutChannel = IPubSubDataChannel<TLayoutData>;

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

  IMemoBit = interface
  ['{38D03281-94A8-466B-AFC3-FE1DC58BE8F9}']
  end;

  ITextBit = interface
   ['{4178C1EC-AA39-4429-B48C-7058676ABA7B}']
  end;

  IButtonBit = interface
   ['{7FB3194B-62AB-44B7-8317-603D10706C71}']
  end;

implementation

{ TLayoutData }

constructor TLayoutData.Create(ALeft, ATop, AWidth, AHeight: Integer);
begin
  fLeft := ALeft;
  fTop := ATop;
  fWidth := AWidth;
  fHeight := AHeight;
end;

class operator TLayoutData.equal(a, b: TLayoutData): Boolean;
begin
  Result := (a.Left = b.Left) and (a.Top = b.Top) and (a.Width = b.Width) and (a.Height = b.Height);
end;

function TLayoutData.Left: Integer;
begin
  Result := fLeft;
end;

function TLayoutData.Top: Integer;
begin
  Result := fTop;
end;

function TLayoutData.Width: Integer;
begin
  Result := fWidth;
end;

function TLayoutData.Height: Integer;
begin
  Result := fHeight;
end;

{ TFocusData }

constructor TFocusData.Create(ASource: TObject; AFocused: Boolean);
begin
  fSource := ASource;
  fFocused := AFocused;
end;

class operator TFocusData.equal(a, b: TFocusData): Boolean;
begin
  Result := (a.fSource = b.fSource) and (a.fFocused = b.fFocused);
end;

function TFocusData.Source: TObject;
begin
  Result := fSource;
end;

function TFocusData.Focused: Boolean;
begin
  Result := fFocused;
end;

{ TKeyData }

function TKeyData.DecodeControlKey(Key: Word): TControlKey;
begin
  case Key of
    VK_RETURN: Result := ckEnter;
    VK_LEFT: Result := ckLeft;
    VK_RIGHT: Result := ckRight;
    VK_UP: Result := ckUp;
    VK_DOWN: Result := ckDown;
    VK_TAB: Result := ckTab;
    VK_ESCAPE: Result := ckEsc;
    VK_PRIOR: Result := ckPgUp;
    VK_NEXT: Result := ckPgDown;
    VK_INSERT: Result := ckInsert;
    VK_DELETE: Result := ckDelete;
    VK_F1: Result := ckF1;
    VK_F2: Result := ckF2;
    VK_F3: Result := ckF3;
    VK_F4: Result := ckF4;
    VK_F5: Result := ckF5;
    VK_F6: Result := ckF6;
    VK_F7: Result := ckF7;
    VK_F8: Result := ckF8;
    VK_F9: Result := ckF9;
    VK_F10: Result := ckF10;
    VK_F11: Result := ckF11;
    VK_F12: Result := ckF12;
  else
    Result := ckUnknown;
  end;
end;

constructor TKeyData.Create(ControlKey: TControlKey);
begin
  Create(ControlKey, []);
end;

constructor TKeyData.Create(ControlKey: TControlKey; ShiftState: TShiftState);
begin
  Self.fControlKey := ControlKey;
  Self.fAlt := ssAlt in ShiftState;
  Self.fCtrl := ssCtrl in ShiftState;
  Self.fShift := ssShift in ShiftState;
  Self.fMeta := ssMeta in ShiftState;
end;

constructor TKeyData.Create(Key: Word; ShiftState: TShiftState);
begin
  Create(DecodeControlKey(Key));
  Self.fAlt := ssAlt in ShiftState;
  Self.fCtrl := ssCtrl in ShiftState;
  Self.fShift := ssShift in ShiftState;
  Self.fMeta := ssMeta in ShiftState;
end;

constructor TKeyData.Create(AMsg: TLMKey);
begin
  Create(DecodeControlKey(AMsg.CharCode), MsgKeyDataToShiftState(AMsg.KeyData));
end;

function TKeyData.ControlKey: TControlKey;
begin
  Result := fControlKey;
end;

function TKeyData.Alt: Boolean;
begin
  Result := fAlt;
end;

function TKeyData.Shift: Boolean;
begin
  Result := fShift;
end;

function TKeyData.Ctrl: Boolean;
begin
  Result := fCtrl;
end;

function TKeyData.Meta: Boolean;
begin
  Result := fMeta;
end;

function TKeyData.NoModifier: Boolean;
begin
  Result := not Ctrl
    and not Alt
    and not Shift
    and not Meta;
end;

class operator TKeyData.equal(a, b: TKeyData): Boolean;
begin
  Result := Ord(a.fControlKey) = Ord(b.fControlKey);
end;

class operator TKeyData.notequal(a, b: TKeyData): Boolean;
begin
  Result := not(a = b);
end;

{ TPositionData }

constructor TPositionData.Create(Source: TObject; Left, Top: Integer);
begin
  Self.Source := Source;
  Self.Left := Left;
  Self.Top := Top;
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


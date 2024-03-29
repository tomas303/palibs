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
unit rea_ubits;

{$mode delphi}{$H+}
{$modeswitch functionreferences}

interface

uses
  SysUtils, rea_ibits, Controls, trl_idifactory, forms, trl_itree,
  StdCtrls, ExtCtrls, LCLType,
  trl_iprops, Graphics, trl_ilog,
  rea_ilayout, tvl_ucontrolbinder,
  LMessages, Classes, fgl;

type

  { TMessageObservable }

  TMessageObservable = class(TControlBinder, IMessageObservable)
  private
    fObservers: TFPGList<TMessageObserverCallback>;
    fEnabled: Boolean;
  protected
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Subscribe(ACallback: TMessageObserverCallback);
    procedure Unsubscribe(ACallback: TMessageObserverCallback);
    function GetEnabled: Boolean;
    procedure SetEnabled(Enabled: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;
  protected
    fMsg: Cardinal;
  published
    property Msg: Cardinal read fMsg write fMsg;
  end;


  { TBit }

  TBit = class(TInterfacedObject, IBit, INode, IBitPosition)
  private
    procedure SetColor(AValue: TColor);
    function NewMessageObservable(AMsg: Integer): IMessageObservable;
  protected
    // INode
    procedure AddChild(const ANode: INode);
    procedure RemoveChild(const ANode: INode);
    procedure ExchangeChild(const AFromNode, AToNode: INode);
    procedure Insert(const AIndex: integer; const ANode: INode);
    procedure Delete(const AIndex: integer);
    function Count: integer;
    function GetChild(const AIndex: integer): INode;
    function GetNodeEnumerator: INodeEnumerator;
    function INode.GetEnumerator = GetNodeEnumerator;
  protected
    // IBit
    procedure Render;
    procedure RenderPaint(const ACanvas: TCanvas);
    procedure HookParent(const AParent: TWinControl);
  protected
    fColor: TColor;
    fTextColor: TColor;
    function AsControl: TControl;
    procedure SetControl(AValue: TControl);
    function NewProps: IProps;
  protected
    procedure DoRender; virtual;
    procedure DoRenderPaint(const ACanvas: TCanvas); virtual;
    procedure DoHookParent(const AParent: TWinControl); virtual;
    procedure EnableNotifiers; virtual;
    procedure DisableNotifiers; virtual;
  public
    destructor Destroy; override;
  protected
    fLayout: integer;
    fPlace: integer;
    fPlaceSize: integer;
    function GetLayout: integer;
    function GetPlace: integer;
    function GetPlaceSize: integer;
    procedure SetLayout(AValue: integer);
    procedure SetPlace(AValue: integer);
    procedure SetPlaceSize(AValue: integer);
  protected
    // IPlace
    fLeft: integer;
    fTop: integer;
    fWidth: integer;
    fHeight: integer;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetLeft(AValue: integer);
    procedure SetTop(AValue: integer);
    procedure SetWidth(AValue: integer);
    procedure SetHeight(AValue: integer);
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  protected
    fID: string;
    fLog: ILog;
    fNode: INode;
    fFactory: IDIFactory;
    fHScale: IScale;
    fVScale: IScale;
    fControl: TControl;
  published
    property ID: String read fID write fID;
    property Log: ILog read fLog write fLog;
    property Node: INode read fNode write fNode;
    property Factory: IDIFactory read fFactory write fFactory;
    property HScale: IScale read fHScale write fHScale;
    property VScale: IScale read fVScale write fVScale;
    property Control: TControl read fControl write SetControl;
  published
    property Color: TColor read fColor write SetColor;
    property TextColor: TColor read fTextColor write fTextColor;
  published
    property Layout: integer read GetLayout write SetLayout;
    property Place: integer read GetPlace write SetPlace;
    property PlaceSize: integer read GetPlaceSize write SetPlaceSize;
  end;

  { TFormBit }

  TFormBit = class(TBit, IFormBit)
  private
    fLMCloseQuery: IMessageObservable;
    procedure LMCloseQueryObserver(AMessage: TLMessage);
    procedure PSCloseChannelObserver;
    procedure SetPSCloseChannel(AValue: IPSCloseChannel);
    procedure PSCloseChannelConnect;
    procedure PSCloseChannelDisconnect;
  private
    fLMSize: IMessageObservable;
    procedure LMSizeObserver(AMessage: TLMessage);
    procedure PSSizeChannelObserver(const AData: TSizeData);
    procedure SetPSSizeChannel(AValue: IPSSizeChannel);
    procedure PSSizeChannelConnect;
    procedure PSSizeChannelDisonnect;
  private
    fLMMove: IMessageObservable;
    procedure LMMoveObserver(AMessage: TLMessage);
    procedure PSPositionChannelObserver(const AData: TPositionData);
    procedure SetPSPositionChannel(AValue: IPSPositionChannel);
    procedure PSPositionChannelConnect;
    procedure PSPositionChannelDisconnect;
  private
    fLMActivate: IMessageObservable;
    procedure LMActivateObserver(AMessage: TLMessage);
    procedure PSActivateChannelObserver;
    procedure SetPSActivateChannel(AValue: IPSActivateChannel);
    procedure PSActivateChannelConnect;
    procedure PSActivateChannelDisconnect;
  private
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PSKeyDownChannelObserver(const AValue: TKeyData);
    procedure SetPSKeyDownChannel(AValue: IPSKeyChannel);
    procedure PSKeyDownChannelConnect;
    procedure PSKeyDownChannelDisconnect;
  protected
    function AsForm: TCustomForm;
    procedure ResetScroll;
    procedure OnPaint(Sender: TObject);
  protected
    procedure DoRender; override;
    procedure EnableNotifiers; override;
    procedure DisableNotifiers; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fTiler: ITiler;
    fTitle: string;
    fPSCloseChannel: IPSCloseChannel;
    fPSSizeChannel: IPSSizeChannel;
    fPSPositionChannel: IPSPositionChannel;
    fPSActivateChannel: IPSActivateChannel;
    fPSKeyDownChannel: IPSKeyChannel;
  published
    property Tiler: ITiler read fTiler write fTiler;
    property Title: string read fTitle write fTitle;
    property PSCloseChannel: IPSCloseChannel read fPSCloseChannel write SetPSCloseChannel;
    property PSSizeChannel: IPSSizeChannel read fPSSizeChannel write SetPSSizeChannel;
    property PSPositionChannel: IPSPositionChannel read fPSPositionChannel write SetPSPositionChannel;
    property PSActivateChannel: IPSActivateChannel read fPSActivateChannel write SetPSActivateChannel;
    property PSKeyDownChannel: IPSKeyChannel read fPSKeyDownChannel write SetPSKeyDownChannel;
  end;

  { TStripBit }

  TStripBit = class(TBit, IStripBit)
  protected
    procedure PaintBackground(const ACanvas: TCanvas);
    procedure PaintBorder(const ACanvas: TCanvas);
    procedure PaintTitle(const ACanvas: TCanvas);
    procedure DoRender; override;
    procedure DoRenderPaint(const ACanvas: TCanvas); override;
    procedure DoHookParent(const AParent: TWinControl); override;
  protected
    fTiler: ITiler;
    fTransparent: Boolean;
    fTitle: string;
    fFontColor: TColor;
    fBorder: integer;
    fBorderColor: TColor;
    fPSLayoutChannel: IPSLayoutChannel;
  public
    procedure AfterConstruction; override;
  published
    property Tiler: ITiler read fTiler write fTiler;
    property Transparent: Boolean read fTransparent write fTransparent default True;
    property Title: string read fTitle write fTitle;
    property FontColor: TColor read fFontColor write fFontColor;
    property Border: integer read fBorder write fBorder;
    property BorderColor: TColor read fBorderColor write fBorderColor;
    property PSLayoutChannel: IPSLayoutChannel read fPSLayoutChannel write fPSLayoutChannel;
  end;

  { TEditBit }

  TEditBit = class(TBit, IEditBit)
  private
    fCMTextChanged: IMessageObservable;
    procedure CMTextChangedObserver(AMessage: TLMessage);
    procedure PSTextChannelObserver(const AValue: String);
    procedure SetPSTextChannel(AValue: IPSTextChannel);
    procedure PSTextChannelConnect;
    procedure PSTextChannelDisconnect;
  private
    fCNKeyDown: IMessageObservable;
    procedure CNKeyDownObserver(AMessage: TLMessage);
    procedure PSKeyDownChannelObserver(const AValue: TKeyData);
    procedure SetPSKeyDownChannel(AValue: IPSKeyChannel);
    procedure PSKeyDownChannelConnect;
    procedure PSKeyDownChannelDisconnect;
  private
    fCMEnter: IMessageObservable;
    fCMExit: IMessageObservable;
    procedure CMEnterObserver(AMessage: TLMessage);
    procedure CMExitObserver(AMessage: TLMessage);
    procedure PSFocusChannelObserver(const AValue: TFocusData);
    procedure SetPSFocusChannel(AValue: IPSFocusChannel);
    procedure PSFocusChannelConnect;
    procedure PSFocusChannelDisconnect;
  protected
    function AsEdit: TCustomEdit;
    procedure DoRender; override;
    procedure EnableNotifiers; override;
    procedure DisableNotifiers; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fText: string;
    fFocused: Boolean;
    fFlat: Boolean;
    fTabStop: Boolean;
    fPasswordChar: String;
    fPSTextChannel: IPSTextChannel;
    fPSKeyDownChannel: IPSKeyChannel;
    fPSFocusChannel: IPSFocusChannel;
  published
    property Text: string read fText write fText;
    property Focused: Boolean read fFocused write fFocused;
    property Flat: Boolean read fFlat write fFlat;
    property TabStop: Boolean read fTabStop write fTabStop;
    property PasswordChar: String read fPasswordChar write fPasswordChar;
    property PSTextChannel: IPSTextChannel read fPSTextChannel write SetPSTextChannel;
    property PSKeyDownChannel: IPSKeyChannel read fPSKeyDownChannel write SetPSKeyDownChannel;
    property PSFocusChannel: IPSFocusChannel read fPSFocusChannel write SetPSFocusChannel;
  end;

  { TMemoBit }

  TMemoBit = class(TBit, IMemoBit)
  private
    fCMTextChanged: IMessageObservable;
    procedure CMTextChangedObserver(AMessage: TLMessage);
    procedure PSTextChannelObserver(const AValue: String);
    procedure SetPSTextChannel(AValue: IPSTextChannel);
    procedure PSTextChannelConnect;
    procedure PSTextChannelDisconnect;
  private
    fCNKeyDown: IMessageObservable;
    procedure CNKeyDownObserver(AMessage: TLMessage);
    procedure PSKeyDownChannelObserver(const AValue: TKeyData);
    procedure SetPSKeyDownChannel(AValue: IPSKeyChannel);
    procedure PSKeyDownChannelConnect;
    procedure PSKeyDownChannelDisconnect;
  private
    fCMEnter: IMessageObservable;
    fCMExit: IMessageObservable;
    procedure CMEnterObserver(AMessage: TLMessage);
    procedure CMExitObserver(AMessage: TLMessage);
    procedure PSFocusChannelObserver(const AValue: TFocusData);
    procedure SetPSFocusChannel(AValue: IPSFocusChannel);
    procedure PSFocusChannelConnect;
    procedure PSFocusChannelDisconnect;
  protected
    function AsMemo: TCustomMemo;
    procedure DoRender; override;
    procedure EnableNotifiers; override;
    procedure DisableNotifiers; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fText: string;
    fFocused: Boolean;
    fFlat: Boolean;
    fPSTextChannel: IPSTextChannel;
    fPSKeyDownChannel: IPSKeyChannel;
    fPSFocusChannel: IPSFocusChannel;
  published
    property Text: string read fText write fText;
    property Focused: Boolean read fFocused write fFocused;
    property Flat: Boolean read fFlat write fFlat;
    property PSTextChannel: IPSTextChannel read fPSTextChannel write SetPSTextChannel;
    property PSKeyDownChannel: IPSKeyChannel read fPSKeyDownChannel write SetPSKeyDownChannel;
    property PSFocusChannel: IPSFocusChannel read fPSFocusChannel write SetPSFocusChannel;
  end;

 { TTextBit }

  TTextBit = class(TBit, ITextBit)
  protected
    function AsText: TCustomLabel;
  protected
    procedure DoRender; override;
  protected
    fText: string;
  published
    property Text: string read fText write fText;
  end;

  { TButtonBit }

  TButtonBit = class(TBit, IButtonBit)
  private
    fClickEnabled: Boolean;
    fCNKeyDown: IMessageObservable;
    fLMSetFocus: IMessageObservable;
    fLMKillFocus: IMessageObservable;
    procedure CNKeyDownObserver(AMessage: TLMessage);
    procedure LMSetFocusObserver(AMessage: TLMessage);
    procedure LMKillFocusObserver(AMessage: TLMessage);
    procedure OnClick(Sender: TObject);
    procedure PSClickChannelObserver;
    procedure SetPSClickChannel(AValue: IPSClickChannel);
    procedure PSClickChannelConnect;
    procedure PSClickChannelDisconnect;
  protected
    function AsButton: TPanel;
  protected
    procedure DoRender; override;
    procedure EnableNotifiers; override;
    procedure DisableNotifiers; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  protected
    fText: string;
    fFontDirection: Integer;
    fFontColor: TColor;
    fFocusColor: TColor;
    fPSClickChannel: IPSClickChannel;
    fTransparent: Boolean;
  published
    property Text: string read fText write fText;
    property FontDirection: integer read fFontDirection write fFontDirection;
    property FontColor: TColor read fFontColor write fFontColor;
    property FocusColor: TColor read fFocusColor write fFocusColor;
    property Transparent: Boolean read fTransparent write fTransparent;
    property PSClickChannel: IPSClickChannel read fPSClickChannel write SetPSClickChannel;
  end;

implementation

{ TMessageObservable }

procedure TMessageObservable.DoControlWndProc(var TheMessage: TLMessage);
var
  cb: TMessageObserverCallback;
begin
  inherited DoControlWndProc(TheMessage);
  if not Enabled then
    Exit;
  if TheMessage.Msg = Msg then begin
    for cb in fObservers do
      cb(TheMessage);
  end;
end;

procedure TMessageObservable.AfterConstruction;
begin
  inherited AfterConstruction;
  fObservers := TFPGList<TMessageObserverCallback>.Create;
  fEnabled := True;
end;

procedure TMessageObservable.BeforeDestruction;
begin
  FreeAndNil(fObservers);
  inherited BeforeDestruction;
end;

procedure TMessageObservable.Subscribe(ACallback: TMessageObserverCallback);
begin
  if fObservers.IndexOf(ACallback) = -1 then
    fObservers.Add(ACallback);
end;

procedure TMessageObservable.Unsubscribe(ACallback: TMessageObserverCallback);
begin
  if fObservers.IndexOf(ACallback) <> -1 then
    fObservers.Remove(ACallback);
end;

function TMessageObservable.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

procedure TMessageObservable.SetEnabled(Enabled: Boolean);
begin
  fEnabled := Enabled;
end;

{ TStripBit }

procedure TStripBit.DoRender;
var
  mChild: INode;
  mPosition: IBitPosition;
begin
  // need to shift children relatively to surface this strip is on(because strip
  // has no control to render ... intentionaly)
  Tiler.ReplaceChildren(Self, Border);
  for mChild in (Self as INode) do begin
    mPosition := mChild as IBitPosition;
    mPosition.Left := Left + mPosition.Left + Border;
    mPosition.Top := Top + mPosition.Top + Border;
  end;
  if PSLayoutChannel <> nil then
    PSLayoutChannel.Debounce(TLayoutData.Create(Left, Top, Width, Height));
end;

procedure TStripBit.DoRenderPaint(const ACanvas: TCanvas);
begin
  inherited DoRenderPaint(ACanvas);
  PaintBorder(ACanvas);
  PaintBackground(ACanvas);
  PaintTitle(ACanvas);
end;

procedure TStripBit.DoHookParent(const AParent: TWinControl);
var
  mChild: INode;
begin
  for mChild in (Self as INode) do begin
    (mChild as IBit).HookParent(AParent);
  end;
end;

procedure TStripBit.PaintBackground(const ACanvas: TCanvas);
var
  mBColor: TColor;
begin
  if not Transparent then begin
    mBColor := ACanvas.Brush.Color;
    try
      ACanvas.Brush.Color := Color;
      ACanvas.FillRect(Left + Border, Top + Border, Left + Width - Border, Top
        + Height - Border);
    finally
      ACanvas.Brush.Color := mBColor;
    end;
  end;
end;

procedure TStripBit.PaintBorder(const ACanvas: TCanvas);
var
  mBColor: TColor;
begin
  if Border > 0 then begin
    mBColor := ACanvas.Brush.Color;
    try
      ACanvas.Brush.Color := BorderColor;
      // top
      ACanvas.FillRect(Left, Top, Left + Width, Top + Border);
      // bottom
      ACanvas.FillRect(Left, Top + Height - Border, Left + Width, Top + Height);
      // left
      ACanvas.FillRect(Left, Top + Border, Left + Border, Top + Height - Border);
      // right
      ACanvas.FillRect(Left + Width - Border, Top + Border, Left + Width, Top + Height - Border);
    finally
      ACanvas.Brush.Color := mBColor;
    end;
  end;
end;

procedure TStripBit.PaintTitle(const ACanvas: TCanvas);
var
  mFColor: TColor;
  mBColor: TColor;
begin
  if Title <> '' then begin
    mBColor := ACanvas.Brush.Color;
    mFColor := ACanvas.Font.Color;
    try
      ACanvas.Font.Color := FontColor;
      ACanvas.Brush.Color := Color;
      ACanvas.TextOut(Left + Border + 1, Top + Border + 1, Title);
    finally
      ACanvas.Font.Color := mFColor;
      ACanvas.Brush.Color := mBColor;
    end;
  end;
end;

procedure TStripBit.AfterConstruction;
begin
  inherited AfterConstruction;
  fTransparent := True;
end;

{ TButtonBit }

procedure TButtonBit.SetPSClickChannel(AValue: IPSClickChannel);
begin
  PSClickChannelDisconnect;
  fPSClickChannel := AValue;
  PSClickChannelConnect;
end;

procedure TButtonBit.PSClickChannelConnect;
begin
  if fPSClickChannel <> nil then
  begin
    AsButton.OnClick := OnClick;
    fCNKeyDown := NewMessageObservable(CN_KeyDown);
    fCNKeyDown.Bind(AsButton);
    fCNKeyDown.Subscribe(CNKeyDownObserver);
    fLMSetFocus := NewMessageObservable(LM_SETFOCUS);
    fLMSetFocus.Bind(AsButton);
    fLMSetFocus.Subscribe(LMSetFocusObserver);
    fLMKillFocus := NewMessageObservable(LM_KILLFOCUS);
    fLMKillFocus.Bind(AsButton);
    fLMKillFocus.Subscribe(LMKillFocusObserver);
    fPSClickChannel.Subscribe(PSClickChannelObserver);
    fClickEnabled := True;
  end;
end;

procedure TButtonBit.PSClickChannelDisconnect;
begin
  if fPSClickChannel <> nil then
  begin
    AsButton.OnClick := nil;
    fCNKeyDown.Unbind;
    fCNKeyDown.Unsubscribe(CNKeyDownObserver);
    fCNKeyDown := nil;
    fLMSetFocus.Unbind;
    fLMSetFocus.Unsubscribe(LMSetFocusObserver);
    fLMSetFocus := nil;
    fLMKillFocus.Unbind;
    fLMKillFocus.Unsubscribe(LMKillFocusObserver);
    fLMKillFocus := nil;
    fPSClickChannel.Unsubscribe(PSClickChannelObserver);
    fClickEnabled := False;
  end;
end;

function TButtonBit.AsButton: TPanel;
begin
  Result := AsControl as TPanel;
end;

procedure TButtonBit.CNKeyDownObserver(AMessage: TLMessage);
begin
  if fClickEnabled then begin
    if TLMKeyDown(AMessage).CharCode in [VK_RETURN, VK_SPACE] then begin
     PSClickChannel.Publish;
    end;
  end;
end;

procedure TButtonBit.LMSetFocusObserver(AMessage: TLMessage);
begin
  AsButton.Color := FocusColor;
  AsButton.BevelInner := bvLowered;
  AsButton.BevelOuter := bvRaised;
  AsButton.BevelWidth := 1;
end;

procedure TButtonBit.LMKillFocusObserver(AMessage: TLMessage);
begin
  AsButton.Color := Color;
  AsButton.BevelInner := bvNone;
  AsButton.BevelOuter := bvNone;
  AsButton.BevelWidth := 0;
end;

procedure TButtonBit.OnClick(Sender: TObject);
begin
  if fClickEnabled then
    PSClickChannel.Publish;
end;

procedure TButtonBit.PSClickChannelObserver;
begin

end;

procedure TButtonBit.DoRender;
begin
  inherited DoRender;
  AsButton.Caption := Text;
  AsButton.AutoSize := False;
  AsButton.Alignment := taCenter;
  AsButton.Font.Quality := fqCleartype;
  AsButton.Font.Color := FontColor;
  AsButton.BorderStyle := bsNone;
  AsButton.TabStop := False;
  AsButton.BevelWidth := 0;
  case FontDirection of
    cFontDirection.VertLeft:
      AsButton.Font.Orientation := 900;
    cFontDirection.VertRight:
      AsButton.Font.Orientation := 2700;
  else
    AsButton.Font.Orientation := 0;
  end;
  if AsButton.Height > 6 then
    AsButton.Font.Height := (AsButton.Height div 4) * 3;
  AsButton.Show;
end;

procedure TButtonBit.EnableNotifiers;
begin
  inherited EnableNotifiers;
  fClickEnabled := True;
end;

procedure TButtonBit.DisableNotifiers;
begin
  fClickEnabled := False;
  inherited DisableNotifiers;
end;

procedure TButtonBit.AfterConstruction;
begin
  inherited AfterConstruction;
  Transparent := True;
end;

procedure TButtonBit.BeforeDestruction;
begin
  PSClickChannelDisconnect;
  inherited BeforeDestruction;
end;

{ TTextBit }

function TTextBit.AsText: TCustomLabel;
begin
  Result := AsControl as TCustomLabel;
end;

procedure TTextBit.DoRender;
begin
  inherited;
  AsText.Caption := Text;
  AsText.Font.Color := TextColor;
  AsText.Align := alCustom;
end;

{ TEditBit }

procedure TEditBit.CMTextChangedObserver(AMessage: TLMessage);
begin
  fText := AsEdit.Text;
  fPSTextChannel.Publish(AsEdit.Text);
end;

procedure TEditBit.SetPSTextChannel(AValue: IPSTextChannel);
begin
  PSTextChannelDisconnect;
  fPSTextChannel := AValue;
  PSTextChannelConnect;
end;

procedure TEditBit.PSTextChannelConnect;
begin
  if fPSTextChannel <> nil then
  begin
    fCMTextChanged := NewMessageObservable(CM_TEXTCHANGED);
    fCMTextChanged.Bind(AsEdit);
    fCMTextChanged.Subscribe(CMTextChangedObserver);
    fPSTextChannel.Subscribe(PSTextChannelObserver);
  end;
end;

procedure TEditBit.PSTextChannelDisconnect;
begin
  if fPSTextChannel <> nil then
  begin
    fCMTextChanged.Unbind;
    fCMTextChanged.Unsubscribe(CMTextChangedObserver);
    fCMTextChanged := nil;
    fPSTextChannel.Unsubscribe(PSTextChannelObserver);
  end;
end;

procedure TEditBit.CNKeyDownObserver(AMessage: TLMessage);
begin
  PSKeyDownChannel.Publish(TKeyData.Create(TLMKeyDown(AMessage)));
end;

procedure TEditBit.PSKeyDownChannelObserver(const AValue: TKeyData);
begin

end;

procedure TEditBit.SetPSKeyDownChannel(AValue: IPSKeyChannel);
begin
  PSKeyDownChannelDisconnect;
  fPSKeyDownChannel := AValue;
  PSKeyDownChannelConnect;
end;

procedure TEditBit.PSKeyDownChannelConnect;
begin
  if fPSKeyDownChannel <> nil then
  begin
    fCNKeyDown := NewMessageObservable(CN_KeyDown);
    fCNKeyDown.Bind(AsEdit);
    fCNKeyDown.Subscribe(CNKeyDownObserver);
    fPSKeyDownChannel.Subscribe(PSKeyDownChannelObserver);
  end;
end;

procedure TEditBit.PSKeyDownChannelDisconnect;
begin
  if fPSKeyDownChannel <> nil then
  begin
    fCNKeyDown.Unbind;
    fCNKeyDown.Unsubscribe(CNKeyDownObserver);
    fCNKeyDown := nil;
    fPSKeyDownChannel.Unsubscribe(PSKeyDownChannelObserver);
  end;
end;

procedure TEditBit.CMEnterObserver(AMessage: TLMessage);
begin
  PSFocusChannel.Publish(TFocusData.Create(Self, True));
end;

procedure TEditBit.CMExitObserver(AMessage: TLMessage);
begin
  PSFocusChannel.Publish(TFocusData.Create(Self, False));
end;

procedure TEditBit.PSFocusChannelObserver(const AValue: TFocusData);
begin
  if AValue.Source <> Self then
    if AValue.Focused and not AsEdit.Focused then
      AsEdit.SetFocus;
end;

procedure TEditBit.SetPSFocusChannel(AValue: IPSFocusChannel);
begin
  PSFocusChannelDisconnect;
  fPSFocusChannel := AValue;
  PSFocusChannelConnect;
end;

procedure TEditBit.PSFocusChannelConnect;
begin
  if fPSFocusChannel <> nil then
  begin
    fCMEnter := NewMessageObservable(CM_ENTER);
    fCMEnter.Bind(AsEdit);
    fCMEnter.Subscribe(CMEnterObserver());
    fCMExit := NewMessageObservable(CM_EXIT);
    fCMExit.Bind(AsEdit);
    fCMExit.Subscribe(CMExitObserver());
    fPSFocusChannel.Subscribe(PSFocusChannelObserver());
  end;
end;

procedure TEditBit.PSFocusChannelDisconnect;
begin
  if fPSFocusChannel <> nil then
  begin
    fCMEnter.Unbind;
    fCMEnter.Unsubscribe(CMEnterObserver());
    fCMEnter := nil;
    fCMExit.Unbind;
    fCMExit.Unsubscribe(CMExitObserver());
    fCMExit := nil;
    fPSFocusChannel.Unsubscribe(PSFocusChannelObserver());
  end;
end;

procedure TEditBit.PSTextChannelObserver(const AValue: String);
begin
  if fCMTextChanged = nil then
    Exit;
  if fText = AValue then
    Exit;
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := False;
  fText := AValue;
  AsEdit.Text := AValue;
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := True;
end;

function TEditBit.AsEdit: TCustomEdit;
begin
  Result := AsControl as TCustomEdit;
end;

procedure TEditBit.DoRender;
begin
  inherited;
  if Flat then
    AsEdit.BorderStyle := bsNone;
  AsEdit.Color := Color;
  AsEdit.Font.Color := TextColor;
  AsEdit.Text := Text;
  if PasswordChar <> '' then
    AsEdit.PasswordChar := PasswordChar[1]
  else
    AsEdit.PasswordChar := #0;
  AsEdit.Show;
  if Focused then begin
    Focused := False;
    AsEdit.SetFocus;
    AsEdit.SelLength := 0;
  end;
  AsEdit.TabStop := TabStop;
end;

procedure TEditBit.EnableNotifiers;
begin
  inherited EnableNotifiers;
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := True;
  if fCNKeyDown <> nil then
    fCNKeyDown.Enabled := True;
end;

procedure TEditBit.DisableNotifiers;
begin
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := False;
  if fCNKeyDown <> nil then
    fCNKeyDown.Enabled := False;
  inherited DisableNotifiers;
end;

procedure TEditBit.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TEditBit.BeforeDestruction;
begin
  PSTextChannelDisconnect;
  PSKeyDownChannelDisconnect;
  PSFocusChannelDisconnect;
  inherited BeforeDestruction;
end;

{ TMemoBit }

procedure TMemoBit.CMTextChangedObserver(AMessage: TLMessage);
begin
  fText := AsMemo.Text;
  fPSTextChannel.Publish(AsMemo.Text);
end;

procedure TMemoBit.SetPSTextChannel(AValue: IPSTextChannel);
begin
  PSTextChannelDisconnect;
  fPSTextChannel := AValue;
  PSTextChannelConnect;
end;

procedure TMemoBit.PSTextChannelConnect;
begin
  if fPSTextChannel <> nil then
  begin
    fCMTextChanged := NewMessageObservable(CM_TEXTCHANGED);
    fCMTextChanged.Bind(AsMemo);
    fCMTextChanged.Subscribe(CMTextChangedObserver);
    fPSTextChannel.Subscribe(PSTextChannelObserver);
  end;
end;

procedure TMemoBit.PSTextChannelDisconnect;
begin
  if fPSTextChannel <> nil then
  begin
    fCMTextChanged.Unbind;
    fCMTextChanged.Unsubscribe(CMTextChangedObserver);
    fCMTextChanged := nil;
    fPSTextChannel.Unsubscribe(PSTextChannelObserver);
  end;
end;

procedure TMemoBit.CNKeyDownObserver(AMessage: TLMessage);
begin
  PSKeyDownChannel.Publish(TKeyData.Create(TLMKeyDown(AMessage)));
end;

procedure TMemoBit.PSKeyDownChannelObserver(const AValue: TKeyData);
begin

end;

procedure TMemoBit.SetPSKeyDownChannel(AValue: IPSKeyChannel);
begin
  PSKeyDownChannelDisconnect;
  fPSKeyDownChannel := AValue;
  PSKeyDownChannelConnect;
end;

procedure TMemoBit.PSKeyDownChannelConnect;
begin
  if fPSKeyDownChannel <> nil then
  begin
    fCNKeyDown := NewMessageObservable(CN_KeyDown);
    fCNKeyDown.Bind(AsMemo);
    fCNKeyDown.Subscribe(CNKeyDownObserver);
    fPSKeyDownChannel.Subscribe(PSKeyDownChannelObserver);
  end;
end;

procedure TMemoBit.PSKeyDownChannelDisconnect;
begin
  if fPSKeyDownChannel <> nil then
  begin
    fCNKeyDown.Unbind;
    fCNKeyDown.Unsubscribe(CNKeyDownObserver);
    fCNKeyDown := nil;
    fPSKeyDownChannel.Unsubscribe(PSKeyDownChannelObserver);
  end;
end;

procedure TMemoBit.CMEnterObserver(AMessage: TLMessage);
begin
  PSFocusChannel.Publish(TFocusData.Create(Self, True));
end;

procedure TMemoBit.CMExitObserver(AMessage: TLMessage);
begin
  PSFocusChannel.Publish(TFocusData.Create(Self, False));
end;

procedure TMemoBit.PSFocusChannelObserver(const AValue: TFocusData);
begin
  if AValue.Source <> Self then
    if AValue.Focused and not AsMemo.Focused then
      AsMemo.SetFocus;
end;

procedure TMemoBit.SetPSFocusChannel(AValue: IPSFocusChannel);
begin
  PSFocusChannelDisconnect;
  fPSFocusChannel := AValue;
  PSFocusChannelConnect;
end;

procedure TMemoBit.PSFocusChannelConnect;
begin
  if fPSFocusChannel <> nil then
  begin
    fCMEnter := NewMessageObservable(CM_ENTER);
    fCMEnter.Bind(AsMemo);
    fCMEnter.Subscribe(CMEnterObserver());
    fCMExit := NewMessageObservable(CM_EXIT);
    fCMExit.Bind(AsMemo);
    fCMExit.Subscribe(CMExitObserver());
    fPSFocusChannel.Subscribe(PSFocusChannelObserver());
  end;
end;

procedure TMemoBit.PSFocusChannelDisconnect;
begin
  if fPSFocusChannel <> nil then
  begin
    fCMEnter.Unbind;
    fCMEnter.Unsubscribe(CMEnterObserver());
    fCMEnter := nil;
    fCMExit.Unbind;
    fCMExit.Unsubscribe(CMExitObserver());
    fCMExit := nil;
    fPSFocusChannel.Unsubscribe(PSFocusChannelObserver());
  end;
end;

procedure TMemoBit.PSTextChannelObserver(const AValue: String);
begin
  if fCMTextChanged = nil then
    Exit;
  if fText = AValue then
    Exit;
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := False;
  fText := AValue;
  AsMemo.Text := AValue;
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := True;
end;

function TMemoBit.AsMemo: TCustomMemo;
begin
  Result := AsControl as TCustomMemo;
end;

procedure TMemoBit.DoRender;
begin
  inherited;
  if Flat then
    AsMemo.BorderStyle := bsNone;
  AsMemo.Color := Color;
  AsMemo.Font.Color := TextColor;
  AsMemo.Text := Text;
  AsMemo.Show;
  if Focused then begin
    Focused := False;
    AsMemo.SetFocus;
    AsMemo.SelLength := 0;
  end;
end;

procedure TMemoBit.EnableNotifiers;
begin
  inherited EnableNotifiers;
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := True;
  if fCNKeyDown <> nil then
    fCNKeyDown.Enabled := True;
end;

procedure TMemoBit.DisableNotifiers;
begin
  if fCMTextChanged <> nil then
    fCMTextChanged.Enabled := False;
  if fCNKeyDown <> nil then
    fCNKeyDown.Enabled := False;
  inherited DisableNotifiers;
end;

procedure TMemoBit.AfterConstruction;
begin
  inherited AfterConstruction;
end;

procedure TMemoBit.BeforeDestruction;
begin
  PSTextChannelDisconnect;
  PSKeyDownChannelDisconnect;
  PSFocusChannelDisconnect;
  inherited BeforeDestruction;
end;

{ TFormBit }

procedure TFormBit.LMCloseQueryObserver(AMessage: TLMessage);
begin
  PSCloseChannel.Publish;
end;

procedure TFormBit.PSCloseChannelObserver;
begin
  AsForm.Close;
end;

procedure TFormBit.SetPSCloseChannel(AValue: IPSCloseChannel);
begin
  PSCloseChannelDisconnect;
  fPSCloseChannel := AValue;
  PSCloseChannelConnect;
end;

procedure TFormBit.PSCloseChannelConnect;
begin
  if fPSCloseChannel <> nil then
  begin
    fLMCloseQuery := NewMessageObservable(LM_CLOSEQUERY);
    fLMCloseQuery.Bind(AsForm);
    fLMCloseQuery.Subscribe(LMCloseQueryObserver);
    fPSCloseChannel.Subscribe(PSCloseChannelObserver);
  end;
end;

procedure TFormBit.PSCloseChannelDisconnect;
begin
  if fPSCloseChannel <> nil then
  begin
    fLMCloseQuery.Unbind;
    fLMCloseQuery.Unsubscribe(LMCloseQueryObserver);
    fLMCloseQuery := nil;
    fPSCloseChannel.Unsubscribe(PSCloseChannelObserver);
  end;
end;

procedure TFormBit.LMSizeObserver(AMessage: TLMessage);
begin
  if (Width <> AsForm.Width) or (Height <> AsForm.Height) then begin
    Width := AsForm.Width;
    Height := AsForm.Height;
    PSSizeChannel.Publish(TSizeData.Create(Self, HScale.Unscale(Width), VScale.Unscale(Height)));
  end;
end;

procedure TFormBit.PSSizeChannelObserver(const AData: TSizeData);
begin
  if AData.Source = Self then
    Exit;
  fLMSize.Enabled := False;
  Width := HScale.Scale(AData.Width);
  Height := VScale.Scale(AData.Height);
  AsForm.Width := Width;
  AsForm.Height := Height;
  fLMSize.Enabled := True;
end;

procedure TFormBit.SetPSSizeChannel(AValue: IPSSizeChannel);
begin
  PSSizeChannelDisonnect;
  fPSSizeChannel := AValue;
  PSSizeChannelConnect;
end;

procedure TFormBit.PSSizeChannelConnect;
begin
  if fPSSizeChannel <> nil then
  begin
    fLMSize := NewMessageObservable(LM_SIZE);
    fLMSize.Bind(AsForm);
    fLMSize.Subscribe(LMSizeObserver);
    fPSSizeChannel.Subscribe(PSSizeChannelObserver);
  end;
end;

procedure TFormBit.PSSizeChannelDisonnect;
begin
  if fPSSizeChannel <> nil then
  begin
    fLMSize.Unbind;
    fLMSize.Unsubscribe(LMSizeObserver);
    fLMSize := nil;
    fPSSizeChannel.Unsubscribe(PSSizeChannelObserver);
  end;
end;

procedure TFormBit.LMMoveObserver(AMessage: TLMessage);
begin
  if (Left <> AsForm.Left) or (Top <> AsForm.Top) then begin
    Left := AsForm.Left;
    Top := AsForm.Top;
    PSPositionChannel.Publish(TPositionData.Create(Self, HScale.Unscale(Left), VScale.Unscale(Top)));
  end;
end;

procedure TFormBit.PSPositionChannelObserver(const AData: TPositionData);
begin
  if AData.Source = Self then
    Exit;
  fLMMove.Enabled := False;
  Left := HScale.Scale(AData.Left);
  Top := HScale.Scale(AData.Top);
  AsForm.Left := Left;
  AsForm.Top := Top;
  fLMMove.Enabled := True;
end;

procedure TFormBit.SetPSPositionChannel(AValue: IPSPositionChannel);
begin
  PSPositionChannelDisconnect;
  fPSPositionChannel := AValue;
  PSPositionChannelConnect;
end;

procedure TFormBit.PSPositionChannelConnect;
begin
  if fPSPositionChannel <> nil then
  begin
    fLMMove := NewMessageObservable(LM_MOVE);
    fLMMove.Bind(AsForm);
    fLMMove.Subscribe(LMMoveObserver);
    fPSPositionChannel.Subscribe(PSPositionChannelObserver);
  end;
end;

procedure TFormBit.PSPositionChannelDisconnect;
begin
  if fPSPositionChannel <> nil then
  begin
    fLMMove.Unbind;
    fLMMove.Unsubscribe(LMMoveObserver);
    fLMMove := nil;
    fPSPositionChannel.Unsubscribe(PSPositionChannelObserver);
  end;
end;

procedure TFormBit.LMActivateObserver(AMessage: TLMessage);
begin
  PSActivateChannel.Publish;
end;

procedure TFormBit.PSActivateChannelObserver;
begin

end;

procedure TFormBit.SetPSActivateChannel(AValue: IPSActivateChannel);
begin
  PSActivateChannelDisconnect;
  fPSActivateChannel := AValue;
  PSActivateChannelConnect;
end;

procedure TFormBit.PSActivateChannelConnect;
begin
  if fPSActivateChannel <> nil then
  begin
    fLMActivate := NewMessageObservable(LM_ACTIVATE);
    fLMActivate.Bind(AsForm);
    fLMActivate.Subscribe(LMActivateObserver);
    fPSActivateChannel.Subscribe(PSActivateChannelObserver);
  end;
end;

procedure TFormBit.PSActivateChannelDisconnect;
begin
  if fPSActivateChannel <> nil then
  begin
    fLMActivate.Unbind;
    fLMActivate.Unsubscribe(LMActivateObserver);
    fLMActivate := nil;
    fPSActivateChannel.Unsubscribe(PSActivateChannelObserver);
  end;
end;

procedure TFormBit.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  PSKeyDownChannel.Publish(TKeyData.Create(Key, Shift));
end;

procedure TFormBit.PSKeyDownChannelObserver(const AValue: TKeyData);
begin

end;

procedure TFormBit.SetPSKeyDownChannel(AValue: IPSKeyChannel);
begin
  PSKeyDownChannelDisconnect;
  fPSKeyDownChannel := AValue;
  PSKeyDownChannelConnect;
end;

procedure TFormBit.PSKeyDownChannelConnect;
begin
  if fPSKeyDownChannel <> nil then
  begin
    AsForm.KeyPreview := True;
    AsForm.OnKeyDown := OnKeyDown;
    fPSKeyDownChannel.Subscribe(PSKeyDownChannelObserver);
  end;
end;

procedure TFormBit.PSKeyDownChannelDisconnect;
begin
  if fPSKeyDownChannel <> nil then
  begin
    AsForm.KeyPreview := False;
    AsForm.OnKeyDown := nil;
    fPSKeyDownChannel.Unsubscribe(PSKeyDownChannelObserver);
  end;
end;

function TFormBit.AsForm: TCustomForm;
begin
  Result := AsControl as TCustomForm;
end;

procedure TFormBit.ResetScroll;
var
  mLastChild: INode;
  mOpposite: integer;
begin
  if Node.Count > 0 then
    mLastChild := Node[Node.Count - 1];
  if mLastChild <> nil then
  begin
    mOpposite := (mLastChild as IBitPosition).Left + (mLastChild as IBitPosition).Width - 1;
    if mOpposite > Width then
       AsForm.HorzScrollBar.Range := mOpposite
     else
       AsForm.HorzScrollBar.Range := 0;
    mOpposite := (mLastChild as IBitPosition).Top + (mLastChild as IBitPosition).Height - 1;
    if mOpposite > Height then
       AsForm.VertScrollBar.Range := mOpposite
     else
       AsForm.VertScrollBar.Range := 0;
  end;
end;

procedure TFormBit.OnPaint(Sender: TObject);
var
  mChild: INode;
begin
  for mChild in Node do
    (mChild as IBit).RenderPaint(AsForm.Canvas);
end;

procedure TFormBit.DoRender;
var
  mChild: INode;
begin
  inherited;
  Tiler.ReplaceChildren(Self);
  ResetScroll;
  AsForm.OnPaint := OnPaint;
  AsForm.Caption := Title;
  AsForm.Show;
  for mChild in Node do
    (mChild as IBit).HookParent(AsForm);
end;

procedure TFormBit.EnableNotifiers;
begin
  inherited EnableNotifiers;
  if fLMSize <> nil then
    fLMSize.Enabled := True;
  if fLMMove <> nil then
    fLMMove.Enabled := True;
  if fLMActivate <> nil then
    fLMActivate.Enabled := True;
  if fLMCloseQuery <> nil then
    fLMCloseQuery.Enabled := True;
end;

procedure TFormBit.DisableNotifiers;
begin
  if fLMSize <> nil then
    fLMSize.Enabled := False;
  if fLMMove <> nil then
    fLMMove.Enabled := False;
  if fLMActivate <> nil then
    fLMActivate.Enabled := False;
  if fLMCloseQuery <> nil then
    fLMCloseQuery.Enabled := False;
  inherited DisableNotifiers;
end;

procedure TFormBit.AfterConstruction;
var
  mMon: TMonitor;
begin
  inherited AfterConstruction;
  fWidth := 400;
  fHeight := 150;
  mMon := Screen.MonitorFromPoint(point(0,0));
  if mMon <> nil then begin
    fLeft := (mMon.Width - fWidth) div 2;
    fTop := (mMon.Height - fHeight) div 2;
  end;
end;

procedure TFormBit.BeforeDestruction;
begin
  PSCloseChannelDisconnect;
  PSSizeChannelDisonnect;
  PSPositionChannelDisconnect;
  PSActivateChannelDisconnect;
  PSKeyDownChannelDisconnect;
  inherited BeforeDestruction;
end;

{ TBit }

procedure TBit.SetControl(AValue: TControl);
begin
  if fControl = AValue then
    Exit;
  fControl := AValue;
  Color := Control.Color;
end;

function TBit.NewProps: IProps;
begin
  Result := IProps(Factory.Locate(IProps));
end;

procedure TBit.SetColor(AValue: TColor);
begin
  if fColor=AValue then Exit;
  fColor:=AValue;
end;

function TBit.NewMessageObservable(AMsg: Integer): IMessageObservable;
begin
  Result := IMessageObservable(
    Factory.Locate(
      IMessageObservable,
      '',
      NewProps
      .SetInt('Msg', AMsg)
    )
  );
end;

procedure TBit.SetPlaceSize(AValue: integer);
begin
  fPlaceSize := AValue;
end;

procedure TBit.AddChild(const ANode: INode);
begin
  Node.AddChild(ANode);
end;

procedure TBit.RemoveChild(const ANode: INode);
begin
  Node.RemoveChild(ANode);
end;

procedure TBit.ExchangeChild(const AFromNode, AToNode: INode);
begin
  Node.ExchangeChild(AFromNode, AToNode);
end;

procedure TBit.Insert(const AIndex: integer; const ANode: INode);
begin
  Node.Insert(AIndex, ANode);
end;

procedure TBit.Delete(const AIndex: integer);
begin
 Node.Delete(AIndex);
end;

function TBit.Count: integer;
begin
  Result := Node.Count;
end;

function TBit.GetChild(const AIndex: integer): INode;
begin
  Result := Node[AIndex];
end;

function TBit.GetNodeEnumerator: INodeEnumerator;
begin
  Result := Node.GetEnumerator;
end;

procedure TBit.Render;
var
  mChild: INode;
begin
  //AsControl.Hide;
  DisableNotifiers;
  try
    DoRender;
    if AsControl <> nil then
      Log.DebugLn('RENDERED ' + ClassName
        + ' L:' + IntToStr(AsControl.Left)
        + ' T:' + IntToStr(AsControl.Top)
        + ' W:' + IntToStr(AsControl.Width)
        + ' H:' + IntToStr(AsControl.Height)
        + ' VIS:' + BoolToStr(AsControl.Visible)
        )
    else
      Log.DebugLn('RENDERED NO CONTROL');

    Log.DebugLn('BIT SIZE ' + ClassName
      + ' L:' + IntToStr(Left)
      + ' T:' + IntToStr(Top)
      + ' W:' + IntToStr(Width)
      + ' H:' + IntToStr(Height)
      );

    for mChild in Node do
      (mChild as IBit).Render;

  finally
    EnableNotifiers;
  end;
  //AsControl.Show;
end;

procedure TBit.RenderPaint(const ACanvas: TCanvas);
var
  mChild: INode;
begin
  DoRenderPaint(ACanvas);
  for mChild in Node do
    (mChild as IBit).RenderPaint(ACanvas);
end;

procedure TBit.HookParent(const AParent: TWinControl);
begin
  DisableNotifiers;
  try
    DoHookParent(AParent);
  finally
    EnableNotifiers;
  end;
end;

function TBit.GetLayout: integer;
begin
  Result := fLayout;
end;

function TBit.GetPlace: integer;
begin
  Result := fPlace;
end;

function TBit.GetPlaceSize: integer;
begin
  Result := fPlaceSize;
end;

function TBit.GetLeft: integer;
begin
  Result := fLeft;
end;

function TBit.GetTop: integer;
begin
  Result := fTop;
end;

function TBit.GetWidth: integer;
begin
  Result := fWidth;
end;

function TBit.GetHeight: integer;
begin
  Result := fHeight;
end;

procedure TBit.SetLayout(AValue: integer);
begin
  fLayout := AValue;
end;

procedure TBit.SetPlace(AValue: integer);
begin
  fPlace := AValue;
end;

procedure TBit.SetLeft(AValue: integer);
begin
  fLeft := AValue;
end;

procedure TBit.SetTop(AValue: integer);
begin
  fTop := AValue;
end;

procedure TBit.SetWidth(AValue: integer);
begin
  fWidth := AValue;
end;

procedure TBit.SetHeight(AValue: integer);
begin
  fHeight := AValue;
end;

function TBit.AsControl: TControl;
begin
  Result := fControl;
end;

procedure TBit.DoRender;
begin
  AsControl.AutoSize := False;

  AsControl.Left := Left;
  AsControl.Top := Top;
  AsControl.Width := Width;
  AsControl.Height := Height;
  AsControl.Color := Color;
end;

procedure TBit.DoRenderPaint(const ACanvas: TCanvas);
begin
end;

procedure TBit.DoHookParent(const AParent: TWinControl);
begin
  AsControl.Parent := AParent;
end;

procedure TBit.EnableNotifiers;
begin

end;

procedure TBit.DisableNotifiers;
begin

end;

destructor TBit.Destroy;
begin
  FreeAndNil(fControl);
  inherited Destroy;
end;

end.


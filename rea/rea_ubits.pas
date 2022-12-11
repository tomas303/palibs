unit rea_ubits;

{$mode delphi}{$H+}
{$modeswitch functionreferences}

interface

uses
  SysUtils, rea_ibits, Controls, trl_idifactory, forms, trl_itree,
  StdCtrls, trl_iprops, Graphics, trl_ilog,
  rea_ilayout, rea_iflux, tvl_ucontrolbinder,
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
    fMMLeft: integer;
    fMMTop: integer;
    fMMWidth: integer;
    fMMHeight: integer;
    function GetLayout: integer;
    function GetPlace: integer;
    function GetMMLeft: integer;
    function GetMMTop: integer;
    function GetMMWidth: integer;
    function GetMMHeight: integer;
    procedure SetLayout(AValue: integer);
    procedure SetPlace(AValue: integer);
    procedure SetMMLeft(AValue: integer);
    procedure SetMMTop(AValue: integer);
    procedure SetMMWidth(AValue: integer);
    procedure SetMMHeight(AValue: integer);
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
    property MMLeft: integer read GetMMLeft write SetMMLeft;
    property MMTop: integer read GetMMTop write SetMMTop;
    property MMWidth: integer read GetMMWidth write SetMMWidth;
    property MMHeight: integer read GetMMHeight write SetMMHeight;
  end;

  { TFormBit }

  TFormBit = class(TBit, IFormBit)
  private
    fLMCloseQuery: IMessageObservable;
    procedure LMCloseQueryObserver(AMessage: TLMessage);
    procedure PSCloseChannelObserver;
    procedure SetPSCloseChannel(AValue: IPSCloseChannel);
  private
    fLMSize: IMessageObservable;
    procedure LMSizeObserver(AMessage: TLMessage);
    procedure PSSizeChannelObserver(const AData: TSizeData);
    procedure SetPSSizeChannel(AValue: IPSSizeChannel);
  private
    fLMMove: IMessageObservable;
    procedure LMMoveObserver(AMessage: TLMessage);
    procedure PSPositionChannelObserver(const AData: TPositionData);
    procedure SetPSPositionChannel(AValue: IPSPositionChannel);
  private
    fLMActivate: IMessageObservable;
    procedure LMActivateObserver(AMessage: TLMessage);
    procedure PSActivateChannelObserver;
    procedure SetPSActivateChannel(AValue: IPSActivateChannel);
  protected
    function AsForm: TCustomForm;
    procedure ResetScroll;
    procedure OnPaint(Sender: TObject);
  protected
    procedure DoRender; override;
    procedure EnableNotifiers; override;
    procedure DisableNotifiers; override;
  protected
    fTiler: ITiler;
    fTitle: string;
    fPSCloseChannel: IPSCloseChannel;
    fPSSizeChannel: IPSSizeChannel;
    fPSPositionChannel: IPSPositionChannel;
    fPSActivateChannel: IPSActivateChannel;
  published
    property Tiler: ITiler read fTiler write fTiler;
    property Title: string read fTitle write fTitle;
    property PSCloseChannel: IPSCloseChannel read fPSCloseChannel write SetPSCloseChannel;
    property PSSizeChannel: IPSSizeChannel read fPSSizeChannel write SetPSSizeChannel;
    property PSPositionChannel: IPSPositionChannel read fPSPositionChannel write SetPSPositionChannel;
    property PSActivateChannel: IPSActivateChannel read fPSActivateChannel write SetPSActivateChannel;
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
  public
    procedure AfterConstruction; override;
  published
    property Tiler: ITiler read fTiler write fTiler;
    property Transparent: Boolean read fTransparent write fTransparent default True;
    property Title: string read fTitle write fTitle;
    property FontColor: TColor read fFontColor write fFontColor;
    property Border: integer read fBorder write fBorder;
    property BorderColor: TColor read fBorderColor write fBorderColor;
  end;

  { TEditBit }

  TEditBit = class(TBit, IEditBit)
  private
    fCMTextChanged: IMessageObservable;
    procedure CMTextChangedObserver(AMessage: TLMessage);
    procedure PSTextChannelObserver(const AValue: String);
    procedure SetPSTextChannel(AValue: IPSTextChannel);
  private
    fCNKeyDown: IMessageObservable;
    procedure CNKeyDownObserver(AMessage: TLMessage);
    procedure PSKeyDownChannelObserver(const AValue: TKeyData);
    procedure SetPSKeyDownChannel(AValue: IPSKeyChannel);
  protected
    function AsEdit: TCustomEdit;
    procedure DoRender; override;
    procedure EnableNotifiers; override;
    procedure DisableNotifiers; override;
  public
    procedure AfterConstruction; override;
  protected
    fText: string;
    fFocused: Boolean;
    fFlat: Boolean;
    fPSTextChannel: IPSTextChannel;
    fPSKeyDownChannel: IPSKeyChannel;
  published
    property Text: string read fText write fText;
    property Focused: Boolean read fFocused write fFocused;
    property Flat: Boolean read fFlat write fFlat;
    property PSTextChannel: IPSTextChannel read fPSTextChannel write SetPSTextChannel;
    property PSKeyDownChannel: IPSKeyChannel read fPSKeyDownChannel write SetPSKeyDownChannel;
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
    procedure OnClick(Sender: TObject);
    procedure PSClickChannelObserver;
    procedure SetPSClickChannel(AValue: IPSClickChannel);
  protected
    function AsButton: TLabel;
  protected
    procedure DoRender; override;
    procedure EnableNotifiers; override;
    procedure DisableNotifiers; override;
  protected
    fText: string;
    fFontDirection: Integer;
    fFontColor: TColor;
    fPSClickChannel: IPSClickChannel;
  published
    property Text: string read fText write fText;
    property FontDirection: integer read fFontDirection write fFontDirection;
    property FontColor: TColor read fFontColor write fFontColor;
    property PSClickChannel: IPSClickChannel read fPSClickChannel write SetPSClickChannel;
  end;

implementation

{ TMessageObservable }

procedure TMessageObservable.DoControlWndProc(var TheMessage: TLMessage);
var
  cb: TMessageObserverCallback;
begin
  inherited DoControlWndProc(TheMessage);
  if TheMessage.Msg = CM_TEXTCHANGED then begin
    Enabled := Enabled;
  end;
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
  mclass: string;
begin
  for mChild in (Self as INode) do begin
    mclass := (mChild as tobject).ClassName;
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
  if fPSClickChannel <> nil then
  begin
    AsButton.OnClick := nil;
    fPSClickChannel.Unsubscribe(PSClickChannelObserver);
    fClickEnabled := False;
  end;
  fPSClickChannel := AValue;
  if fPSClickChannel <> nil then
  begin
    AsButton.OnClick := OnClick;
    fPSClickChannel.Subscribe(PSClickChannelObserver);
    fClickEnabled := True;
  end;
end;

function TButtonBit.AsButton: TLabel;
begin
  Result := AsControl as TLabel;
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
  AsButton.Layout := tlCenter;
  AsButton.Font.Quality := fqCleartype;
  AsButton.Font.Color := FontColor;
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
  AsText.Show;
end;

{ TEditBit }

procedure TEditBit.CMTextChangedObserver(AMessage: TLMessage);
begin
  fText := AsEdit.Text;
  fPSTextChannel.Publish(AsEdit.Text);
end;

procedure TEditBit.SetPSTextChannel(AValue: IPSTextChannel);
begin
  if fPSTextChannel <> nil then
  begin
    fCMTextChanged.Unbind;
    fCMTextChanged.Unsubscribe(CMTextChangedObserver);
    fCMTextChanged := nil;
    fPSTextChannel.Unsubscribe(PSTextChannelObserver);
  end;
  fPSTextChannel := AValue;
  if fPSTextChannel <> nil then
  begin
    fCMTextChanged := NewMessageObservable(CM_TEXTCHANGED);
    fCMTextChanged.Bind(AsEdit);
    fCMTextChanged.Subscribe(CMTextChangedObserver);
    fPSTextChannel.Subscribe(PSTextChannelObserver);
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
  if fPSKeyDownChannel <> nil then
  begin
    fCNKeyDown.Unbind;
    fCNKeyDown.Unsubscribe(CNKeyDownObserver);
    fCNKeyDown := nil;
    fPSKeyDownChannel.Unsubscribe(PSKeyDownChannelObserver);
  end;
  fPSKeyDownChannel := AValue;
  if fPSKeyDownChannel <> nil then
  begin
    fCNKeyDown := NewMessageObservable(CN_KeyDown);
    fCNKeyDown.Bind(AsEdit);
    fCNKeyDown.Subscribe(CNKeyDownObserver);
    fPSKeyDownChannel.Subscribe(PSKeyDownChannelObserver);
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
  AsEdit.Show;
  if Focused then begin
    Focused := False;
    AsEdit.SetFocus;
    AsEdit.SelLength := 0;
  end;
  if AsEdit.Height > 6 then
    AsEdit.Font.Height := (AsEdit.Height div 4) * 3;
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
  if fPSCloseChannel <> nil then
  begin
    fLMCloseQuery.Unbind;
    fLMCloseQuery.Unsubscribe(LMCloseQueryObserver);
    fLMCloseQuery := nil;
    fPSCloseChannel.Unsubscribe(PSCloseChannelObserver);
  end;
  fPSCloseChannel := AValue;
  if fPSCloseChannel <> nil then
  begin
    fLMCloseQuery := NewMessageObservable(LM_CLOSEQUERY);
    fLMCloseQuery.Bind(AsForm);
    fLMCloseQuery.Subscribe(LMCloseQueryObserver);
    fPSCloseChannel.Subscribe(PSCloseChannelObserver);
  end;
end;

procedure TFormBit.LMSizeObserver(AMessage: TLMessage);
begin
  Width := AsForm.Width;
  Height := AsForm.Height;
  PSSizeChannel.Publish(TSizeData.Create(Self, HScale.Unscale(AsForm.Width), VScale.Unscale(AsForm.Height)));
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
  if fPSSizeChannel <> nil then
  begin
    fLMSize.Unbind;
    fLMSize.Unsubscribe(LMSizeObserver);
    fLMSize := nil;
    fPSSizeChannel.Unsubscribe(PSSizeChannelObserver);
  end;
  fPSSizeChannel := AValue;
  if fPSSizeChannel <> nil then
  begin
    fLMSize := NewMessageObservable(LM_SIZE);
    fLMSize.Bind(AsForm);
    fLMSize.Subscribe(LMSizeObserver);
    fPSSizeChannel.Subscribe(PSSizeChannelObserver);
  end;
end;

procedure TFormBit.LMMoveObserver(AMessage: TLMessage);
begin
  Left := AsForm.Left;
  Top := AsForm.Top;
  PSPositionChannel.Publish(TPositionData.Create(Self, HScale.Unscale(AsForm.Left), VScale.Unscale(AsForm.Top)));
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
  if fPSPositionChannel <> nil then
  begin
    fLMMove.Unbind;
    fLMMove.Unsubscribe(LMMoveObserver);
    fLMMove := nil;
    fPSPositionChannel.Unsubscribe(PSPositionChannelObserver);
  end;
  fPSPositionChannel := AValue;
  if fPSPositionChannel <> nil then
  begin
    fLMMove := NewMessageObservable(LM_MOVE);
    fLMMove.Bind(AsForm);
    fLMMove.Subscribe(LMMoveObserver);
    fPSPositionChannel.Subscribe(PSPositionChannelObserver);
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
  if fPSActivateChannel <> nil then
  begin
    fLMActivate.Unbind;
    fLMActivate.Unsubscribe(LMActivateObserver);
    fLMActivate := nil;
    fPSActivateChannel.Unsubscribe(PSActivateChannelObserver);
  end;
  fPSActivateChannel := AValue;
  if fPSActivateChannel <> nil then
  begin
    fLMActivate := NewMessageObservable(LM_ACTIVATE);
    fLMActivate.Bind(AsForm);
    fLMActivate.Subscribe(LMActivateObserver);
    fPSActivateChannel.Subscribe(PSActivateChannelObserver);
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
  if avalue=0 then exit;
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

function TBit.GetMMLeft: integer;
begin
  Result := fMMLeft;
end;

function TBit.GetMMTop: integer;
begin
  Result := fMMTop;
end;

function TBit.GetMMWidth: integer;
begin
  Result := fMMWidth;
end;

function TBit.GetMMHeight: integer;
begin
  Result := fMMHeight;
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

procedure TBit.SetMMLeft(AValue: integer);
begin
  fMMLeft := AValue;
  Left := HScale.Scale(MMLeft);
end;

procedure TBit.SetMMTop(AValue: integer);
begin
  fMMTop := AValue;
  Top := VScale.Scale(MMTop);
end;

procedure TBit.SetMMWidth(AValue: integer);
begin
  fMMWidth := AValue;
  Width := HScale.Scale(MMWidth);
end;

procedure TBit.SetMMHeight(AValue: integer);
begin
  fMMHeight := AValue;
  Height := VScale.Scale(MMHeight);
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


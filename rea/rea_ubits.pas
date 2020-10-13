unit rea_ubits;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rea_ibits, Controls, trl_idifactory, forms, trl_itree,
  StdCtrls, trl_iprops, Graphics, trl_ilog,
  rea_ilayout, flu_iflux, tvl_ucontrolbinder,
  LMessages;

type

  { TMessageNotifierBinder }

  TMessageNotifierBinder = class(TControlBinder, IMessageNotifierBinder)
  private
    fMessage: TLMessage;
  protected
    // IMessageNotifierBinder
    function Message: TLMessage;
  protected
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
  protected
    fNotifier: IFluxNotifier;
    fMsg: Cardinal;
  published
    property Notifier: IFluxNotifier read fNotifier write fNotifier;
    property Msg: Cardinal read fMsg write fMsg;
  end;

  { TBit }

  TBit = class(TInterfacedObject, IBit, INode, IBitPosition)
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
    function AsControl: TControl;
    procedure SetControl(AValue: TControl);
    function NewProps: IProps;
  protected
    procedure DoRender; virtual;
    procedure DoRenderPaint(const ACanvas: TCanvas); virtual;
    procedure DoHookParent(const AParent: TWinControl); virtual;
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
  protected
    fLog: ILog;
    fNode: INode;
    fFactory: IDIFactory;
    fHScale: IScale;
    fVScale: IScale;
    fControl: TControl;
  published
    property Log: ILog read fLog write fLog;
    property Node: INode read fNode write fNode;
    property Factory: IDIFactory read fFactory write fFactory;
    property HScale: IScale read fHScale write fHScale;
    property VScale: IScale read fVScale write fVScale;
    property Control: TControl read fControl write SetControl;
  published
    property Color: TColor read fColor write fColor;
  published
    property Layout: integer read GetLayout write SetLayout;
    property Place: integer read GetPlace write SetPlace;
    property MMLeft: integer read GetMMLeft write SetMMLeft;
    property MMTop: integer read GetMMTop write SetMMTop;
    property MMWidth: integer read GetMMWidth write SetMMWidth;
    property MMHeight: integer read GetMMHeight write SetMMHeight;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
  end;

  { TFormBit }

  TFormBit = class(TBit, IFormBit)
  private
    procedure SetActivateNotifier(AValue: IFluxNotifier);
    procedure SetMoveNotifier(AValue: IFluxNotifier);
    procedure SetSizeNotifier(AValue: IFluxNotifier);
    procedure SetCloseQueryNotifier(AValue: IFluxNotifier);
  protected
    function AsForm: TCustomForm;
    procedure ResetScroll;
    procedure OnPaint(Sender: TObject);
    procedure SizeNotifierData(const AProps: IProps);
    procedure MoveNotifierData(const AProps: IProps);
  protected
    fSizeMsgBinder: IMessageNotifierBinder;
    fMoveMsgBinder: IMessageNotifierBinder;
    fActivateMsgBinder: IMessageNotifierBinder;
    fCloseQueryMsgBinder: IMessageNotifierBinder;
    procedure DoRender; override;
  public
    destructor Destroy; override;
  protected
    fTiler: ITiler;
    fTitle: string;
    fSizeNotifier: IFluxNotifier;
    fMoveNotifier: IFluxNotifier;
    fActivateNotifier: IFluxNotifier;
    fCloseQueryNotifier: IFluxNotifier;
  published
    property Tiler: ITiler read fTiler write fTiler;
    property Title: string read fTitle write fTitle;
    property SizeNotifier: IFluxNotifier read fSizeNotifier write SetSizeNotifier;
    property MoveNotifier: IFluxNotifier read fMoveNotifier write SetMoveNotifier;
    property ActivateNotifier: IFluxNotifier read fActivateNotifier write SetActivateNotifier;
    property CloseQueryNotifier: IFluxNotifier read fCloseQueryNotifier write SetCloseQueryNotifier;
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
    fTextChangedMsgBinder: IMessageNotifierBinder;
    fKeyDownMsgBinder: IMessageNotifierBinder;
    procedure TextChangedNotifierData(const AProps: IProps);
    procedure SetTextChangedNotifier(AValue: IFluxNotifier);
    procedure KeyDownNotifierData(const AProps: IProps);
    procedure SetKeyDownNotifier(AValue: IFluxNotifier);
  protected
    function AsEdit: TCustomEdit;
    procedure DoRender; override;
  public
    destructor Destroy; override;
  protected
    fText: string;
    fTextChangedNotifier: IFluxNotifier;
    fKeyDownNotifier: IFluxNotifier;
    fFocused: Boolean;
  published
    property Text: string read fText write fText;
    property TextChangedNotifier: IFluxNotifier read fTextChangedNotifier write SetTextChangedNotifier;
    property KeyDownNotifier: IFluxNotifier read fKeyDownNotifier write SetKeyDownNotifier;
    property Focused: Boolean read fFocused write fFocused;
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
  protected
    function AsButton: TCustomButton;
    procedure OnClick(Sender: TObject);
  protected
    procedure DoRender; override;
  protected
    fText: string;
    fClickNotifier: IFluxNotifier;
  published
    property Text: string read fText write fText;
    property ClickNotifier: IFluxNotifier read fClickNotifier write fClickNotifier;
  end;

implementation

{ TMessageNotifierBinder }

function TMessageNotifierBinder.Message: TLMessage;
begin
  Result := fMessage;
end;

procedure TMessageNotifierBinder.DoControlWndProc(var TheMessage: TLMessage);
begin
  inherited DoControlWndProc(TheMessage);
  if TheMessage.Msg = Msg then begin
    if Notifier <> nil then begin
      fMessage := TheMessage;
      Notifier.Notify;
    end;
  end;
end;

{ TStripBit }

procedure TStripBit.DoRender;
var
  mChild: INode;
  mPosition: IBitPosition;
begin
  // need to shift children relatively to surface this strip is on(because strip
  // has no control to render ... intentionaly)
  Tiler.ReplaceChildren(Self);
  for mChild in (Self as INode) do begin
    mPosition := mChild as IBitPosition;
    mPosition.Left := Left + mPosition.Left;
    mPosition.Top := Top + mPosition.Top;
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

function TButtonBit.AsButton: TCustomButton;
begin
  Result := AsControl as TCustomButton;
end;

procedure TButtonBit.OnClick(Sender: TObject);
begin
  if ClickNotifier <> nil then
    ClickNotifier.Notify;
end;

procedure TButtonBit.DoRender;
begin
  inherited DoRender;
  AsButton.Caption := Text;
  AsButton.OnClick := @OnClick;
  if ClickNotifier <> nil then
    ClickNotifier.Enabled := True;
  AsButton.Show;
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
  AsText.Show;
end;

{ TEditBit }

procedure TEditBit.SetTextChangedNotifier(AValue: IFluxNotifier);
begin
  if fTextChangedNotifier <> nil then
  begin
    fTextChangedMsgBinder.Unbind;
    fTextChangedMsgBinder := nil;
    fTextChangedNotifier.Remove(@TextChangedNotifierData);
  end;
  fTextChangedNotifier := AValue;
  if fTextChangedNotifier <> nil then
  begin
    fTextChangedMsgBinder := IMessageNotifierBinder(Factory.Locate(IMessageNotifierBinder, '', NewProps.SetIntf('Notifier', fTextChangedNotifier).SetInt('Msg', CM_TEXTCHANGED)));
    fTextChangedMsgBinder.Bind(AsEdit);
    fTextChangedNotifier.Add(@TextChangedNotifierData);
  end;
end;

procedure TEditBit.KeyDownNotifierData(const AProps: IProps);
var
  mMessage: TLMKeyDown;
begin
  mMessage := TLMKeyDown(fKeyDownMsgBinder.Message);
  AProps
    .SetInt('CharCode', mMessage.CharCode)
    .SetInt('KeyData', mMessage.KeyData)
    .SetBool('Focused', AsEdit.Focused);
end;

procedure TEditBit.TextChangedNotifierData(const AProps: IProps);
begin
  if AsEdit <> nil then
  AProps
    .SetStr('Text', AsEdit.Text);
end;

procedure TEditBit.SetKeyDownNotifier(AValue: IFluxNotifier);
begin
  if fKeyDownNotifier <> nil then
  begin
    fKeyDownMsgBinder.Unbind;
    fKeyDownMsgBinder := nil;
    fKeyDownNotifier.Remove(@KeyDownNotifierData);
  end;
  fKeyDownNotifier := AValue;
  if fKeyDownNotifier <> nil then
  begin
    fKeyDownMsgBinder := IMessageNotifierBinder(Factory.Locate(IMessageNotifierBinder, '', NewProps.SetIntf('Notifier', fKeyDownNotifier).SetInt('Msg', CN_KEYDOWN)));
    fKeyDownMsgBinder.Bind(AsEdit);
    fKeyDownNotifier.Add(@KeyDownNotifierData);
  end;
end;

function TEditBit.AsEdit: TCustomEdit;
begin
  Result := AsControl as TCustomEdit;
end;

procedure TEditBit.DoRender;
begin
  if TextChangedNotifier <> nil then
    TextChangedNotifier.Enabled := False;
  if KeyDownNotifier <> nil then
    KeyDownNotifier.Enabled := False;
  inherited;
  AsEdit.Text := Text;
  AsEdit.Show;
  if Focused then begin
    Focused := False;
    AsEdit.SetFocus;
  end;
  if TextChangedNotifier <> nil then
    TextChangedNotifier.Enabled := True;
  if KeyDownNotifier <> nil then
    KeyDownNotifier.Enabled := True;
end;

destructor TEditBit.Destroy;
begin
  TextChangedNotifier := nil;
  KeyDownNotifier := nil;
  inherited Destroy;
end;

{ TFormBit }

procedure TFormBit.SetActivateNotifier(AValue: IFluxNotifier);
begin
  if fActivateNotifier <> nil then
  begin
    fActivateMsgBinder.Unbind;
    fActivateMsgBinder := nil;
  end;
  fActivateNotifier := AValue;
  if fActivateNotifier <> nil then
  begin
    fActivateMsgBinder := IMessageNotifierBinder(Factory.Locate(IMessageNotifierBinder, '', NewProps.SetIntf('Notifier', fActivateNotifier).SetInt('Msg', LM_ACTIVATE)));
    fActivateMsgBinder.Bind(AsForm);
  end;
end;

procedure TFormBit.SetCloseQueryNotifier(AValue: IFluxNotifier);
begin
  if fCloseQueryNotifier <> nil then
    begin
      fCloseQueryMsgBinder.Unbind;
      fCloseQueryMsgBinder := nil;
    end;
    fCloseQueryNotifier := AValue;
    if fCloseQueryNotifier <> nil then
    begin
      fCloseQueryMsgBinder := IMessageNotifierBinder(Factory.Locate(IMessageNotifierBinder, '', NewProps.SetIntf('Notifier', fCloseQueryNotifier).SetInt('Msg', LM_CloseQuery)));
      fCloseQueryMsgBinder.Bind(AsForm);
    end;
end;

procedure TFormBit.SetMoveNotifier(AValue: IFluxNotifier);
begin
  if fMoveNotifier <> nil then
  begin
    fMoveMsgBinder.Unbind;
    fMoveMsgBinder := nil;
    fMoveNotifier.Remove(@MoveNotifierData);
  end;
  fMoveNotifier := AValue;
  if fMoveNotifier <> nil then
  begin
    fMoveMsgBinder := IMessageNotifierBinder(Factory.Locate(IMessageNotifierBinder, '', NewProps.SetIntf('Notifier', fMoveNotifier).SetInt('Msg', LM_MOVE)));
    fMoveMsgBinder.Bind(AsForm);
    fMoveNotifier.Add(@MoveNotifierData);
  end;
end;

procedure TFormBit.SetSizeNotifier(AValue: IFluxNotifier);
begin
  if fSizeNotifier <> nil then
  begin
    fSizeMsgBinder.Unbind;
    fSizeMsgBinder := nil;
    fSizeNotifier.Remove(@SizeNotifierData);
  end;
  fSizeNotifier := AValue;
  if fSizeNotifier <> nil then
  begin
    fSizeMsgBinder := IMessageNotifierBinder(Factory.Locate(IMessageNotifierBinder, '', NewProps.SetIntf('Notifier', fSizeNotifier).SetInt('Msg', LM_SIZE)));
    fSizeMsgBinder.Bind(AsForm);
    fSizeNotifier.Add(@SizeNotifierData);
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

procedure TFormBit.SizeNotifierData(const AProps: IProps);
begin
  AProps
  .SetInt('MMWidth', HScale.Unscale(AsControl.Width))
  .SetInt('MMHeight', VScale.Unscale(AsControl.Height));
end;

procedure TFormBit.MoveNotifierData(const AProps: IProps);
begin
  AProps
  .SetInt('MMLeft', HScale.Unscale(AsControl.Left))
  .SetInt('MMTop', VScale.Unscale(AsControl.Top));
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
  // discard during render should be samewhat general(callbacks could result in
  // another render call .... anyway, do not want to allow whatsever call back during
  // render - because of kiss - so need to cement it somewhere - maybe notifief active
  // property and set it for all notifiers before and after render)
  if SizeNotifier <> nil then
    SizeNotifier.Enabled := False;
  if MoveNotifier <> nil then
    MoveNotifier.Enabled := False;
  if ActivateNotifier <> nil then
    ActivateNotifier.Enabled := False;
  if CloseQueryNotifier <> nil then
    CloseQueryNotifier.Enabled := False;
  inherited;
  Tiler.ReplaceChildren(Self);
  ResetScroll;
  AsForm.OnPaint := @OnPaint;
  AsForm.Caption := Title;
  AsForm.Show;
  if SizeNotifier <> nil then
    SizeNotifier.Enabled := True;
  if MoveNotifier <> nil then
    MoveNotifier.Enabled := True;
  if ActivateNotifier <> nil then
    ActivateNotifier.Enabled := True;
  if CloseQueryNotifier <> nil then
    CloseQueryNotifier.Enabled := True;
  for mChild in Node do
    (mChild as IBit).HookParent(AsForm);
end;

destructor TFormBit.Destroy;
begin
  SizeNotifier := nil;
  MoveNotifier := nil;
  ActivateNotifier := nil;
  CloseQueryNotifier := nil;
  inherited Destroy;
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
  DoHookParent(AParent);
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
var
  m:string;
begin
  m:=classname;
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

destructor TBit.Destroy;
begin
  FreeAndNil(fControl);
  inherited Destroy;
end;

end.


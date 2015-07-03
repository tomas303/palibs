unit tvl_ubehavebinders;

{$mode objfpc}{$H+}

interface

uses
  Controls, ComCtrls, LMessages, Forms, SysUtils, StdCtrls, Menus, types,
  classes, ExtCtrls;

const
  LM_BEHAVEBINDER = LM_USER + $99000;
  LM_DISCONECTWINDOWPROC = LM_BEHAVEBINDER + 01;

type

  { TBehaveBinder }

  TBehaveBinder = class
  private type

    PMethod = ^TMethod;

    TLMBBWINDOWPROC = record
      Msg: Cardinal;
    {$ifdef cpu64}
      UnusedMsg: Cardinal;
    {$endif}
      DisposedProc: PMethod;
      StoredProc: PMethod;
      Result: PtrInt;
    end;

    { TNotificator }

    TNotificator = class(TComponent)
    private
      fBinder: TBehaveBinder;
    protected
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(ABinder: TBehaveBinder); reintroduce;
    end;

  private
    fControl: TControl;
    fControlNotificator: TNotificator;
    fOldWndProc: TWndMethod;
    procedure HookWndPproc;
    procedure UnhookWndProc;
  protected
    procedure ControlWndProc(var TheMessage: TLMessage);
    procedure DoControlWndProc(var TheMessage: TLMessage); virtual;
    property Control: TControl read fControl;
    procedure BindControl; virtual;
    procedure UnbindControl; virtual;
  public
    destructor Destroy; override;
    procedure Bind(AControl: TControl);
    procedure Unbind;
  end;

  { TPageControlBehaveBinder }

  TPageControlBehaveBinder = class(TBehaveBinder)
  private
    fPageIndex: Integer;
    fFirstVisiblePassed: Boolean;
    function GetControl: TPageControl;
    function IsActiveControlOnActivePage: Boolean;
    procedure FocusActualPageControl;
    procedure SetFirstVisiblePage;
    function FindForm: TCustomForm;
  protected
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
  public
    procedure BindControl; override;
    property Control: TPageControl read GetControl;
  end;

  { TPopupMenuBinder }

  TPopupMenuBinder = class(TBehaveBinder)
  public type

    TBuildMenuEvent = procedure(AMenu: TMenu; AControl: TControl) of object;

  private
    fMenu: TPopupMenu;
    fOnBuildMenu: TBuildMenuEvent;
  private
    procedure ShowPopupMenu(const AX, AY: integer);
    function GetMenu: TPopupMenu;
    property Menu: TPopupMenu read GetMenu;
  protected
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
  public
    constructor Create(AOnBuildMenu: TBuildMenuEvent);
    destructor Destroy; override;
    procedure BindControl; override;
  end;

  { TMoveControlBinder }

  TMoveControlBinder = class(TBehaveBinder)
  private
    fOriginalParentFormWndProc: TWndMethod;
    fHold: Boolean;
    fPrevPos: TPoint;
    fControlDelta: TPoint;
    fOriginalCapture: TControl;
    fOriginalCaptureButtons: TCaptureMouseButtons;
    function GetParentControl: TWinControl;
    function GetParentForm: TCustomForm;
    procedure MoveBegin;
    procedure MoveEnd;
    procedure MoveControl;
    procedure CorrectMousePos;
    procedure CorrectParent(const AMousePos: TPoint);
    function FindBranchParent(const AParent: TWinControl; APos: TPoint): TWinControl;
    function FindUpperParent(const AParent: TWinControl; ASkip: TControl; APos: TPoint): TWinControl;
  protected
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
    procedure MouseMoveUserInputHandler(Sender: TObject; Msg: Cardinal);
   procedure BindControl; override;
  public
    property ParentControl: TWinControl read GetParentControl;
    property ParentForm: TCustomForm read GetParentForm;
  end;

  { TMoveFormBinder }

  TMoveFormBinder = class(TBehaveBinder)
  end;

implementation

type

  { TControlHelper }

  TControlHelper = class helper for TControl
  private
    function GetH_MouseCapture: Boolean;
    procedure SetH_MouseCapture(AValue: Boolean);
  public
    property H_MouseCapture: Boolean read GetH_MouseCapture write SetH_MouseCapture;
    function H_GetClientScrollOffset: TPoint;
  end;

  { TWinControlHelper }

  TWinControlHelper = class helper for TWinControl
  public
    function H_IsControlMouseMsg(var TheMessage): Boolean;
  end;

{ TWinControlHelper }

function TWinControlHelper.H_IsControlMouseMsg(var TheMessage): Boolean;
begin
  Result := IsControlMouseMsg(TheMessage);
end;

{ TControlHelper }

function TControlHelper.GetH_MouseCapture: Boolean;
begin
  Result := MouseCapture;
end;

procedure TControlHelper.SetH_MouseCapture(AValue: Boolean);
begin
  MouseCapture := AValue;
end;

function TControlHelper.H_GetClientScrollOffset: TPoint;
begin
  Result := GetClientScrollOffset;
end;

{ TBehaveBinder.TNotificator }

procedure TBehaveBinder.TNotificator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = fBinder.Control) then
  begin
    fBinder.Unbind;
  end;
end;

constructor TBehaveBinder.TNotificator.Create(ABinder: TBehaveBinder);
begin
  inherited Create(nil);
  fBinder := ABinder;
  fBinder.Control.FreeNotification(Self);
end;

{ TMoveControlBinder }

function TMoveControlBinder.GetParentControl: TWinControl;
begin
  Result := Control.Parent;
end;

function TMoveControlBinder.GetParentForm: TCustomForm;
var
  mParent: TWinControl;
begin
  Result := nil;
  mParent := ParentControl;
  while (mParent <> nil) and not (mParent is TCustomForm) do
    mParent := mParent.Parent;
  if (mParent <> nil) and (mParent is TCustomForm) then
    Result := mParent as TCustomForm;
end;

procedure TMoveControlBinder.MoveControl;
var
  mPos: TPoint;
  mNew: integer;
  mc: TControl;
begin
  mc := Control;
  mPos := ParentControl.ScreenToClient(Mouse.CursorPos);
  mNew := mPos.X - fControlDelta.X;
  if Control.Left <> mNew then
    Control.Left := mNew;
  mNew := mPos.Y - fControlDelta.Y;
  if Control.Top <> mNew then
    Control.Top := mNew;
end;

procedure TMoveControlBinder.CorrectMousePos;
var
  mPos: TPoint;
  mConPos: TPoint;
  mRect: TRect;
begin
  mRect.Left := 0;
  mRect.Right := Control.Width;
  mRect.Top := 0;
  mRect.Bottom := Control.Height;
  mRect.TopLeft := Control.ClientToScreen(mRect.TopLeft);
  mRect.BottomRight := Control.ClientToScreen(mRect.BottomRight);
  mPos := Mouse.CursorPos;
  if mPos.X < mRect.Left then
    mPos.X := mRect.Left + 1
  else if mPos.X > mRect.Right then
    mPos.X := mRect.Right - 1;
  if mPos.Y < mRect.Top then
    mPos.Y := mRect.Top + 1
  else if mPos.Y > mRect.Bottom then
    mPos.Y := mRect.Bottom - 1;
  Mouse.CursorPos := mPos;
end;

procedure TMoveControlBinder.CorrectParent(const AMousePos: TPoint);
var
  mPoint, mControlPoint: TPoint;
  mNewParent: TWinControl;
begin
  mNewParent := FindUpperParent(ParentControl, Control, AMousePos);
  if (mNewParent <> nil) and (mNewParent <> ParentControl) then
  begin
    mControlPoint := Control.ScreenToClient(AMousePos);
    Control.Parent := mNewParent;
    mPoint := mNewParent.ScreenToClient(AMousePos);
    Control.Left := mPoint.X - mControlPoint.X;
    Control.Top := mPoint.y - mControlPoint.Y;
    if Control is TWinControl then
      (Control as TWinControl).SetFocus;
  end;
end;

function TMoveControlBinder.FindBranchParent(const AParent: TWinControl;
  APos: TPoint): TWinControl;
var
  i: integer;
  mControl: TWinControl;
begin
  Result := nil;
  if AParent = nil then
    Exit;
  // serach in subcontrols
  for i := 0 to AParent.ControlCount - 1 do
  begin
    if not (AParent.Controls[i] is TWinControl) then
      Continue;
    mControl := AParent.Controls[i] as TWinControl;
    Result := FindBranchParent(mControl, APos);
    if Result <> nil then
      Exit;
  end;
  if AParent is TCustomPanel then
  begin
    if PtInRect(AParent.ClientRect, AParent.ScreenToClient(APos)) then
       Result := AParent;
  end;
end;

function TMoveControlBinder.FindUpperParent(const AParent: TWinControl;
  ASkip: TControl; APos: TPoint): TWinControl;
var
  i: integer;
  mControl: TWinControl;
begin
  Result := nil;
  if AParent = nil then
    Exit;
  // serach in subcontrols
  for i := 0 to AParent.ControlCount - 1 do
  begin
    if not (AParent.Controls[i] is TWinControl) then
      Continue;
    if AParent.Controls[i] = ASkip then
      Continue;
    mControl := AParent.Controls[i] as TWinControl;
    Result := FindBranchParent(mControl, APos);
    if Result <> nil then
      Exit;
  end;
  // test self
  if (
      (AParent is TCustomPanel)
      or (AParent is TCustomForm)
      or (AParent is TCustomGroupBox)
     )
     and PtInRect(AParent.ClientRect, AParent.ScreenToClient(APos)) then
  begin
    Result := AParent;
    Exit;
  end;
  // extend search on upper level
  Result := FindUpperParent(AParent.Parent, AParent, APos);
end;

procedure TMoveControlBinder.MoveBegin;
begin
  if fHold then
    raise Exception.Create('already moving');
  fHold := True;
  fPrevPos := Mouse.CursorPos;
  fControlDelta := Control.ScreenToClient(Mouse.CursorPos);
  // capture only control not work, ParentForm with MouseMoveUserInputHandler yes
  SetCaptureControl(ParentForm);
  Application.AddOnUserInputHandler(@MouseMoveUserInputHandler, True);
  Control.BringToFront;
end;

procedure TMoveControlBinder.MoveEnd;
begin
  if not fHold then
    raise Exception.Create('not moving');
  fHold := False;
  CorrectParent(Mouse.CursorPos);
  Application.RemoveOnUserInputHandler(@MouseMoveUserInputHandler);
  SetCaptureControl(nil);
end;

procedure TMoveControlBinder.DoControlWndProc(var TheMessage: TLMessage
  );
begin
  case TheMessage.Msg of
    LM_LBUTTONDOWN:
      begin
        MoveBegin;
      end;
    LM_LBUTTONUP:
      begin
        if fHold then
          MoveEnd
        else
          inherited;
      end;
    LM_MOUSEMOVE:
      begin
        if fHold then
          MoveControl
        else
          inherited;
      end;
    LM_MOUSELEAVE:
      begin
        if fHold then
          MoveControl
        else
          inherited;
      end;
  else
    begin
      inherited;
    end;
  end;
end;

procedure TMoveControlBinder.MouseMoveUserInputHandler(Sender: TObject; Msg: Cardinal);
begin
  case Msg of
    LM_LBUTTONUP:
      begin
        if fHold then
          MoveEnd;
      end;
    LM_MOUSEMOVE:
      begin
        if fHold then
          MoveControl;
      end;
  end;
end;

procedure TMoveControlBinder.BindControl;
begin
end;

{ TPopupMenuBinder }

function TPopupMenuBinder.GetMenu: TPopupMenu;
begin
  if fMenu = nil then
  begin
    fMenu := TPopupMenu.Create(nil);
  end;
  Result := fMenu;
end;

procedure TPopupMenuBinder.ShowPopupMenu(const AX, AY: integer);
begin
  if Assigned(fOnBuildMenu) then
    fOnBuildMenu(Menu, Control);
  Menu.PopUp(AX, AY);
end;

procedure TPopupMenuBinder.DoControlWndProc(var TheMessage: TLMessage
  );
begin
  case TheMessage.Msg of
    LM_RBUTTONDOWN:
      begin
        fOldWndProc(TheMessage);
        ShowPopupMenu(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      end;
  else
    inherited;
  end;
end;

constructor TPopupMenuBinder.Create(AOnBuildMenu: TBuildMenuEvent);
begin
  inherited Create;
  fOnBuildMenu := AOnBuildMenu;
end;

destructor TPopupMenuBinder.Destroy;
begin
  FreeAndNil(fMenu);
  inherited Destroy;
end;

procedure TPopupMenuBinder.BindControl;
begin
end;

{ TBehaveBinder }

procedure TBehaveBinder.HookWndPproc;
begin
  //if (Control is TWinControl) or (Control.Parent = nil) then
  begin
    fOldWndProc := Control.WindowProc;
    Control.WindowProc := @ControlWndProc;
  end
  //else
  //begin
  //  fOldWndProc := Control.Parent.WindowProc;
  //  Control.Parent.WindowProc := @DoControlWndProc;
  //end;
end;

procedure TBehaveBinder.UnhookWndProc;
var
  mMsgWP: TLMBBWINDOWPROC;
  mMsg: TLMessage absolute mMsgWP;
  mdWndProc: TWndMethod;
begin
  mdWndProc := @ControlWndProc;
  if (TMethod(Control.WindowProc).Code = TMethod(mdWndProc).Code)
     and (TMethod(Control.WindowProc).Data = TMethod(mdWndProc).Data) then
    // this object hook belongs to this binder
    Control.WindowProc := fOldWndProc
  else
  begin
    // something hooked after me, so need to notify it to adjust its fOldWndProc
    // (WndProc is method - so we need to compare code and data aswell)
    mMsgWP.Msg := LM_DISCONECTWINDOWPROC;
    mMsgWP.DisposedProc := @TMethod(mdWndProc);
    mMsgWP.StoredProc := @TMethod(fOldWndProc);
    mMsgWP.Result := 0;
    Control.WindowProc(mMsg);
    if mMsgWP.Result <> 1 then
      raise Exception.Create('Problem with unhook windowproc from chain - superior hook not responsed');
  end;
end;

procedure TBehaveBinder.ControlWndProc(var TheMessage: TLMessage);
begin
  if not (Control is TWinControl)
     or not (Control as TWinControl).H_IsControlMouseMsg(TheMessage) then
    DoControlWndProc(TheMessage);
end;

procedure TBehaveBinder.DoControlWndProc(var TheMessage: TLMessage);
var
  mMsg: TLMBBWINDOWPROC absolute TheMessage;
begin
  case TheMessage.Msg of
    LM_DISCONECTWINDOWPROC:
      begin
        if (mMsg.DisposedProc^.Code = TMethod(fOldWndProc).Code)
           and (mMsg.DisposedProc^.Data = TMethod(fOldWndProc).Data) then
        begin
          fOldWndProc := TWndMethod(mMsg.StoredProc^);
          mMsg.Result := 1;
        end
        else
          fOldWndProc(TheMessage);
      end;
  else
    fOldWndProc(TheMessage);
  end;
end;

procedure TBehaveBinder.BindControl;
begin
end;

procedure TBehaveBinder.UnbindControl;
begin
end;

destructor TBehaveBinder.Destroy;
begin
  Unbind;
  FreeAndNil(fControlNotificator);
  inherited Destroy;
end;

procedure TBehaveBinder.Bind(AControl: TControl);
begin
  fControl := AControl;
  HookWndPproc;
  FreeAndNil(fControlNotificator);
  fControlNotificator := TNotificator.Create(Self);
  BindControl;
end;

procedure TBehaveBinder.Unbind;
begin
  if fControl <> nil then
  begin
    UnbindControl;
    UnhookWndProc;
    fControl := nil;
  end;
end;

{ TPageControlBehaveBinder }

function TPageControlBehaveBinder.GetControl: TPageControl;
begin
  Result := inherited Control as TPageControl;
end;

procedure TPageControlBehaveBinder.DoControlWndProc(var TheMessage: TLMessage
  );
begin
  case TheMessage.Msg of
    48206:
      begin
        if TLMNotify(TheMessage).NMHdr^.code = TCN_SELCHANGE then
        begin
          fPageIndex := Control.PageIndex;
          inherited;
          if fPageIndex <> Control.PageIndex then
            FocusActualPageControl;
        end else
          inherited;
      end;
    LM_SHOWWINDOW:
      begin
        inherited;
        if Control.Visible and not fFirstVisiblePassed then
        begin
          fFirstVisiblePassed := True;
          SetFirstVisiblePage;
        end;
      end
  else
    inherited;
  end;
end;

procedure TPageControlBehaveBinder.BindControl;
begin

end;

function TPageControlBehaveBinder.IsActiveControlOnActivePage: Boolean;
var
  mControl: TWinControl;
begin
  Result := False;
  mControl := FindForm.ActiveControl.Parent;
  while mControl <> nil do begin
    if mControl = Control.ActivePage then
    begin
      Result := True;
      Break;
    end;
    mControl := mControl.Parent;
  end;
end;

procedure TPageControlBehaveBinder.FocusActualPageControl;
var
  mControl, mFocControl: TWinControl;
  mTabOrder: integer;
  i: integer;
begin
  if not IsActiveControlOnActivePage then
  begin
    mTabOrder := MaxInt;
    mFocControl := nil;
    for i := 0 to Control.ActivePage.ControlCount - 1 do
    begin
      if not (Control.ActivePage.Controls[i] is TWinControl) then
        Continue;
      mControl := Control.ActivePage.Controls[i] as TWinControl;
      if not mControl.CanFocus or not mControl.TabStop then
        Continue;
      if mControl.TabOrder < mTabOrder then
      begin
        mFocControl := mControl;
        mTabOrder := mControl.TabOrder;
      end;
    end;
    if mFocControl <> nil then
    begin
      mFocControl.SetFocus;
    end;
  end;
end;

procedure TPageControlBehaveBinder.SetFirstVisiblePage;
var
  i: integer;
begin
  for i := 0 to Control.PageCount - 1 do
  begin
    if Control.Pages[i].TabVisible then
    begin
      Control.PageIndex := Control.Pages[i].PageIndex;
      Break;
    end;
  end;
end;

function TPageControlBehaveBinder.FindForm: TCustomForm;
var
  mControl: TWinControl;
begin
  Result := nil;
  mControl := Control.Parent;
  while mControl <> nil do
  begin
    if mControl is TCustomForm then
    begin
      Result := mControl as TCustomForm;
      Break;
    end;
    mControl := mControl.Parent;
  end;
end;

end.


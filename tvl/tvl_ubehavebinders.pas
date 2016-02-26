unit tvl_ubehavebinders;

{$mode objfpc}{$H+}

interface

uses
  Controls, ComCtrls, LMessages, Forms, SysUtils, StdCtrls, Menus, types,
  classes, ExtCtrls, LCLProc, lclintf, fgl, tvl_ucontrolbinder{, Windows};

const
  LM_FOCUSCONTROLONPAGE = LM_BEHAVEBINDER + 02;

type

  { TPageControlBehaveBinder }

  TPageControlBehaveBinder = class(TControlBinder)
  private type
    TPageActiveControls = specialize TFPGMap<integer, pointer>;
  private
    fPageIndex: Integer;
    fFirstVisiblePassed: Boolean;
    fActiveControls: TPageActiveControls;
    function GetControl: TPageControl;
    function GetActiveControl: TWinControl;
    function IsActiveControlOnActivePage: Boolean;
    procedure FocusActualPageControl;
    procedure SetFirstVisiblePage;
    function FindForm: TCustomForm;
  protected
    procedure DoControlWndProc(var TheMessage: TLMessage); override;
  public
    procedure BindControl; override;
    property Control: TPageControl read GetControl;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TPopupMenuBehaveBinder = class(TControlBinder)

  end;

  { TPopupMenuBinder }

  TPopupMenuBinder = class(TControlBinder)
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

  TMoveControlBinder = class(TControlBinder)
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

  TMoveFormBinder = class(TControlBinder)
  end;

implementation

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
        inherited;
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

{ TPageControlBehaveBinder }

function TPageControlBehaveBinder.GetControl: TPageControl;
begin
  Result := inherited Control as TPageControl;
end;

function TPageControlBehaveBinder.GetActiveControl: TWinControl;
var
  mForm: TCustomForm;
begin
  Result := nil;
  mForm := FindForm;
  if mForm = nil then
    Exit;
  Result := mForm.ActiveControl;
end;

procedure TPageControlBehaveBinder.DoControlWndProc(var TheMessage: TLMessage
  );
var
  mControl: TWinControl;
  mH: THandle;
begin
  //DebugLn('msg->%d', [TheMessage.Msg]);
  case TheMessage.Msg of
    LM_SETFOCUS:
      begin
        mControl := GetActiveControl;
        if mControl <> Control then
          fActiveControls.KeyData[Control.PageIndex] := mControl;
      end;
    48206:
      // on PageIndex change will focus first or last focused control
      // on given tabsheed
      begin
        if TLMNotify(TheMessage).NMHdr^.code = TCN_SELCHANGE then
        begin
          fPageIndex := Control.PageIndex;
          inherited;
          if (Control.PageCount > 0) and (fPageIndex <> Control.PageIndex) then
          begin
            PostMessage(Control.Handle, LM_FOCUSCONTROLONPAGE, 0, 0)
          end
        end else
          inherited;
      end;
    //LM_SHOWWINDOW is received only on first show
    LM_PAINT:
      begin
        // on first show will set first control on tabsheet as active
        inherited;
        if not fFirstVisiblePassed and Control.Visible then
        begin
          fFirstVisiblePassed := True;
          SetFirstVisiblePage;
          mh := Control.Handle;
          if Control.PageCount > 0 then
            PostMessage(Control.Handle, LM_FOCUSCONTROLONPAGE, 0, 0);
        end;
      end;
    LM_FOCUSCONTROLONPAGE:
      begin
        FocusActualPageControl;
      end;
  else
    inherited;
  end;
end;

procedure TPageControlBehaveBinder.BindControl;
begin

end;

procedure TPageControlBehaveBinder.AfterConstruction;
begin
  inherited AfterConstruction;
  fActiveControls := TPageActiveControls.Create;
end;

procedure TPageControlBehaveBinder.BeforeDestruction;
begin
  FreeAndNil(fActiveControls);
  inherited BeforeDestruction;
end;

function TPageControlBehaveBinder.IsActiveControlOnActivePage: Boolean;
var
  mControl: TWinControl;
  mForm: TCustomForm;
begin
  Result := False;
  mForm := FindForm;
  if mForm = nil then
    Exit;
  if mForm.ActiveControl = nil then
    Exit;
  mControl := mForm.ActiveControl.Parent;
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
  mInd: integer;
begin
  if not IsActiveControlOnActivePage then
  begin
    mFocControl := nil;
    mInd := fActiveControls.IndexOf(Control.TabIndex);
    if (mInd <> -1) and (fActiveControls.Data[mInd] <> nil) then
      mFocControl := TWinControl(fActiveControls.Data[mInd]);
    if (mFocControl <> nil) and mFocControl.CanFocus then
      mFocControl.SetFocus
    else
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
        if mFocControl.CanFocus then
        begin
          mFocControl.SetFocus;
        end;
      end;
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


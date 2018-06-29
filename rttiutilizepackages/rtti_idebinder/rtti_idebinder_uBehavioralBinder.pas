unit rtti_idebinder_uBehavioralBinder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, rtti_idebinder_iBindings, rtti_idebinder_uBehaveBinders,
  Controls, ComCtrls, fgl;

type

  { TRBBehavioralBinder }

  TRBBehavioralBinder = class(TInterfacedObject, IRBBehavioralBinder)
  private type
    TBehaveBinderItems = specialize TFPGObjectList<TBehaveBinder>;
  private
    fBinders: TBehaveBinderItems;
    fContainer: TWinControl;
  protected
    // IRBBehavioralBinder
    procedure Bind(AContainer: TWinControl);
  protected
    procedure ActualizeItems;
    procedure MakeBinders(AContainer: TWinControl);
    procedure AddBinderForControl(AControl: TWinControl);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TRBBehavioralBinder }

procedure TRBBehavioralBinder.Bind(AContainer: TWinControl);
begin
  fContainer := AContainer;
  ActualizeItems;
end;

procedure TRBBehavioralBinder.ActualizeItems;
begin
  fBinders.Clear;
  MakeBinders(fContainer);
end;

procedure TRBBehavioralBinder.MakeBinders(AContainer: TWinControl);
var
  i: integer;
begin
  for i := 0 to AContainer.ControlCount - 1 do
  begin
    if not (AContainer.Controls[i] is TWinControl) then
      Continue;
    AddBinderForControl(AContainer.Controls[i] as TWinControl);
  end;
end;

procedure TRBBehavioralBinder.AddBinderForControl(AControl: TWinControl);
var
  mBinder: TBehaveBinder;
begin
  if AControl is TCustomTabControl then begin
    mBinder := TPageControlBehaveBinder.Create;
    fBinders.Add(mBinder);
    mBinder.Bind(AControl);
  end;
end;

constructor TRBBehavioralBinder.Create;
begin
  fBinders := TBehaveBinderItems.Create;
end;

destructor TRBBehavioralBinder.Destroy;
begin
  FreeAndNil(fBinders);
  inherited Destroy;
end;

end.


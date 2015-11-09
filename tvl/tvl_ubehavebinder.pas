unit tvl_ubehavebinder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, tvl_ibindings, tvl_ubehavebinders,
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
    procedure Unbind;
  protected
    procedure ActualizeItems;
    procedure MakeBinders(AContainer: TWinControl);
    procedure AddBinderForControl(AControl: TWinControl);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ TRBBehavioralBinder }

procedure TRBBehavioralBinder.Bind(AContainer: TWinControl);
begin
  fContainer := AContainer;
  ActualizeItems;
end;

procedure TRBBehavioralBinder.Unbind;
begin
  fBinders.Clear;
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

procedure TRBBehavioralBinder.AfterConstruction;
begin
  inherited AfterConstruction;
  fBinders := TBehaveBinderItems.Create;
end;

procedure TRBBehavioralBinder.BeforeDestruction;
begin
  FreeAndNil(fBinders);
  inherited BeforeDestruction;
end;

end.


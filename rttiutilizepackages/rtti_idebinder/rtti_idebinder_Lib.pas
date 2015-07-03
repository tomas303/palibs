unit rtti_idebinder_Lib;

interface

uses
  Classes, SysUtils, rtti_broker_iBroker, Controls, rtti_idebinder_iBindings,
  rtti_idebinder_uDataBinder, rtti_idebinder_uTallyBinders, forms,
  rtti_idebinder_uBehavioralBinder,
  rtti_idebinder_uDesigner;

type

  { TLib }

  TLib = class
  public
    class function NewDataBinder: IRBDataBinder;
    class function NewListBinder(const AListControl: TWinControl; const AContext: IRBBinderContext;
      const AClass: TClass): IRBTallyBinder;
    class function NewBehavioralBinder: IRBBehavioralBinder;
    class function NewDesigner: IRBDesigner;
  end;

  { TIDE }

  TIDE = class
  public
    class function Edit(AFormClass: TFormClass; const AData: IRBdata; const ADataQuery: IRBDataQuery): Boolean;
  end;

implementation

{ TIDE }

class function TIDE.Edit(AFormClass: TFormClass; const AData: IRBdata;
  const ADataQuery: IRBDataQuery): Boolean;
var
  mForm: TForm;
  mBinder: IRBDataBinder;
begin
  mForm := AFormClass.Create(nil);
  try
    mBinder := TLib.NewDataBinder;
    mBinder.Bind(mForm, AData, ADataQuery);
    Result := mForm.ShowModal = mrOK;
  finally
    mForm.Free;
  end;
end;

{ TLib }

class function TLib.NewDataBinder: IRBDataBinder;
begin
  Result := TRBDataBinder.Create;
end;

class function TLib.NewListBinder(const AListControl: TWinControl; const AContext: IRBBinderContext;
  const AClass: TClass): IRBTallyBinder;
begin
  Result := TTallyBinder.New(AListControl, AContext, AClass);
end;

class function TLib.NewBehavioralBinder: IRBBehavioralBinder;
begin
  Result := TRBBehavioralBinder.Create;
end;

class function TLib.NewDesigner: IRBDesigner;
begin
  Result := TRBDesigner.Create;
end;

end.

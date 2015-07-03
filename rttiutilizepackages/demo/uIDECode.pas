unit uIDECode;

interface

uses
  uPerson, sysutils, rtti_broker_iBroker, rtti_broker_uData,
  rtti_idebinder_Lib,
  rtti_serializer_iManager, Forms, Controls, rtti_idebinder_iBindings;

type

  { TIDEPersons }

  TIDEPersons = class
  private
    fStore: ISerialStore;
    fFactory: ISerialFactory;
    fActualPerson: TPerson;
    fForm: TForm;
    fLister: TWinControl;
    //fListContext: IIDEContext;
    fEdit: TWinControl;
  public
    class function NewEditPerson(AStore: ISerialStore): TPerson;
    procedure AttachStore(const AStore: ISerialStore);
    procedure AttachFactory(const AFactory: ISerialFactory);
    procedure AttachForm(const AForm: TForm);

  end;

implementation

{ TIDEPersons }

class function TIDEPersons.NewEditPerson(AStore: ISerialStore): TPerson;
var
  mPC: TIDEPersons;
  mData: IRBData;
begin
  mPC := TIDEPersons.Create;
  try
    mPC.fStore := AStore;
    mPC.fActualPerson := TPerson.Create;
    mData := TRBData.Create(mPC.fActualPerson);
    //TLib.ModalEdit(mData, mCommands);
    Result := mPC.fActualPerson;
  finally
    mPC.Free;
  end;
end;

procedure TIDEPersons.AttachStore(const AStore: ISerialStore);
begin
  fStore := AStore;
end;

procedure TIDEPersons.AttachFactory(const AFactory: ISerialFactory);
begin
  fFactory := AFactory;
end;

procedure TIDEPersons.AttachForm(const AForm: TForm);
begin
  fForm := AForm;
  //fLister := TIDEFactory.CreateDataListControl(fStore.LoadList('TPerson'), NewListCommands);
  fLister.Parent := fForm;
  fLister.Align := alClient;
end;

end.


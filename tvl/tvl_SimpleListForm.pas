unit tvl_SimpleListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  tvl_iedit, tvl_ibindings, trl_ipersist, trl_irttibroker;

type

  { TSimpleListForm }

  TSimpleListForm = class(TForm, IListData)
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    lbList: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fBinder: IRBTallyBinder;
    fEdit: IEditData;
    fDataClass: string;
  protected
    procedure List;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property Binder: IRBTallyBinder read fBinder write fBinder;
    property Edit: IEditData read fEdit write fEdit;
    property DataClass: string read fDataClass write fDataClass;
  end;

var
  SimpleListForm: TSimpleListForm;

implementation

{$R *.lfm}

{ TSimpleListForm }

procedure TSimpleListForm.btnAddClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := Factory.CreateObject(DataClass);
  if Edit.Edit(mData) then
  begin
    Store.Save(mData);
    Store.Flush;
    Binder.Reload;
  end;
end;

procedure TSimpleListForm.btnDeleteClick(Sender: TObject);
begin
  Store.Delete(Binder.CurrentData);
  Store.Flush;
  Binder.Reload;
end;

procedure TSimpleListForm.btnEditClick(Sender: TObject);
var
  mData, mNewData: IRBData;
begin
  mData := Binder.CurrentData;
  if mData = nil then
    Exit;
  mNewData := Factory.CreateObject(mData.ClassName);
  mNewData.Assign(mData);
  if Edit.Edit(mNewData) then
  begin
    mData.Assign(mNewData);
    Store.Save(mData);
    Store.Flush;
    Binder.Reload;
  end;
end;

procedure TSimpleListForm.List;
begin
  Binder.Bind(lbList, DataClass);
  ShowModal;
end;

end.


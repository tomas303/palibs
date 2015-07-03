unit fList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, iStart, rtti_broker_iBroker, rtti_idebinder_Lib, rtti_idebinder_iBindings;

type

  { TListForm }

  TListForm = class(TForm, IStartContextConnectable)
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    lbList: TListBox;
    pnRunEdit: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    fContext: IStartContext;
    fListField: string;
    fEditForm: TFormClass;
    fObjectClass: string;
    fDirty: Boolean;
    fBinder: IRBTallyBinder;
  protected
    procedure Actualize;
    procedure Connect(const AContext: IStartContext);
  public
    constructor Create(TheOwner: TComponent; const AObjectClass, AListField: string;
    AEditForm: TFormClass);
  end;


implementation

{$R *.lfm}

{ TListForm }

procedure TListForm.btnAddClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := fContext.SerialFactory.CreateObject(fObjectClass) as IRBData;
  if TIDE.Edit(fEditFOrm, mData, fContext.DataQuery) then
  begin
    fContext.DataStore.Save(mData);
    fContext.DataStore.Flush;
    fDirty := True;
    Actualize;
  end;
end;

procedure TListForm.btnEditClick(Sender: TObject);
var
  mData, mNewData: IRBData;
begin
  mData := fBinder.CurrentData;
  if mData = nil then
    Exit;
  mNewData := fContext.SerialFactory.CreateObject(mData.ClassName) as IRBData;
  mNewData.Assign(mData);
  if TIDE.Edit(fEditFOrm, mNewData, fContext.DataQuery) then
  begin
    mData.Assign(mNewData);
    fContext.DataStore.Save(mData);
    fContext.DataStore.Flush;
    fDirty := True;
    Actualize;
  end;
end;

procedure TListForm.Actualize;
begin
  fBinder.Reload;
end;

procedure TListForm.Connect(const AContext: IStartContext);
var
  mClass: TClass;
begin
  fContext := AContext;
  mClass := fContext.SerialFactory.FindClass(fObjectClass);
  lbList.Items.Add(fListField);
  fBinder := TLib.NewListBinder(lbList, fContext.BinderContext, mClass);
end;

constructor TListForm.Create(TheOwner: TComponent; const AObjectClass,
  AListField: string; AEditForm: TFormClass);
begin
  inherited Create(TheOwner);
  fObjectClass := AObjectClass;
  fListField := AListField;
  fEditForm := AEditForm;
end;

end.


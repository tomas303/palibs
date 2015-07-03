unit fGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids,
  iStart, rtti_broker_iBroker, rtti_idebinder_Lib, rtti_idebinder_iBindings;

type

  { TGridForm }

  TGridForm = class(TForm{, IStartContextConnectable})
    btnAdd: TButton;
    btnDelete: TButton;
    btnEdit: TButton;
    grdGrid: TDrawGrid;
    pnRunEdit: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    //fContext: IStartContext;
    fEditForm: TFormClass;
    fObjectClass: string;
    fDirty: Boolean;
    fBinder: IRBTallyBinder;

    fFactory: IRBFactory;
    fStore: IRBStore;

  protected
    //procedure Connect(const AContext: IStartContext);
    procedure Actualize;
  public
    constructor Create(TheOwner: TComponent; const AObjectClass: string;
      AEditForm: TFormClass);
    property Factory: IRBFactory read fFactory write fFactory;
    property Store: IRBStore read fStore write fStore;
  end;


implementation

{$R *.lfm}

{ TGridForm }

procedure TGridForm.btnAddClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := Factory.CreateObject(fObjectClass) as IRBData;
  if TIDE.Edit(fEditFOrm, mData, Store as IRBDataQuery) then
  begin
    Store.Save(mData);
    Store.Flush;
    fDirty := True;
    Actualize;
  end;
end;

procedure TGridForm.btnEditClick(Sender: TObject);
var
  mData, mNewData: IRBData;
begin
  mData := fBinder.CurrentData;
  if mData = nil then
    Exit;
  mNewData := Factory.CreateObject(mData.ClassName) as IRBData;
  mNewData.Assign(mData);
  if TIDE.Edit(fEditFOrm, mNewData, Store as IRBDataQuery) then
  begin
    mData.Assign(mNewData);
    Store.Save(mData);
    Store.Flush;
    fDirty := True;
    Actualize;
  end;
end;

//procedure TGridForm.Connect(const AContext: IStartContext);
//var
//  mClass: TClass;
//begin
//  fContext := AContext;
//  mClass := fContext.SerialFactory.FindClass(fObjectClass);
//  //lbList.Items.Add(fListField);
//  fBinder := TLib.NewListBinder(grdGrid, fContext.BinderContext, mClass);
//end;

procedure TGridForm.Actualize;
begin
  fBinder.Reload;
end;

constructor TGridForm.Create(TheOwner: TComponent; const AObjectClass: string;
  AEditForm: TFormClass);
begin
  inherited Create(TheOwner);
  fObjectClass := AObjectClass;
  fEditForm := AEditForm;
end;

end.


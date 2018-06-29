unit rtti_startup_lib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, iStart, forms, fList, fGrid, rtti_broker_iBroker;

type

  { TStartupLib }

  TStartupLib = class
  public
    class function ListObjects(const AContext: IStartContext; const AObjectClass, AListField: string;
      AEditForm: TFormClass): TForm;
    class function GridObjects(const AFactory: IRBFactory; const AStore: IRBStore;
      const AObjectClass: string; AEditForm: TFormClass): TForm;
  end;

implementation

{ TStartupLib }

class function TStartupLib.ListObjects(const AContext: IStartContext;
  const AObjectClass, AListField: string; AEditForm: TFormClass): TForm;
begin
  Result := TListForm.Create(Application, AObjectClass, AListField, AEditForm);
  (Result as IStartContextConnectable).Connect(AContext);
  Result.Show;
end;

class function TStartupLib.GridObjects(const AFactory: IRBFactory; const AStore: IRBStore;
  const AObjectClass: string; AEditForm: TFormClass): TForm;
var
  mGForm: TGridForm;
begin
  mGForm := TGridForm.Create(Application, AObjectClass, AEditForm);
  //(Result as IStartContextConnectable).Connect(AContext);
  mGForm.Factory := AFactory;
  mGForm.Store := AStore;
  Result := mGForm;
  Result.Show;
end;

end.


unit trl_persist;

{$mode delphi}{$H+}
{$modeswitch advancedrecords}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  trl_ipersist, trl_usystem, trl_udifactory, trl_irttibroker, trl_upersist;

type

  { TPersist }

  TPersist = class
  public
    function New<T: TPlainObject>: TPersistData<T>;
    function Load<T: TPlainObject>(const ASID: TSID): TPersistData<T>;
    procedure Save<T: TPlainObject>(AData: TPersistData<T>);
    procedure Delete<T: TPlainObject>(AData: TPersistData<T>);
    function Select<T: TPlainObject>: IDataList<T>;
  protected
    fFactory2: TDIFactory2;
    fDevice: IPersistStoreDevice;
  published
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property Device: IPersistStoreDevice read fDevice write fDevice;
  end;


implementation

{ TPersist }

procedure TPersist.Delete<T>;
var
  mSID: TSID;
begin
  if not AData.SID.IsClear then
    fDevice.Delete(AData.SID);
end;

function TPersist.Load<T>;
var
  mO: T;
begin
  //mO := fFactory2.LocateC<T>;
  mO := T.Create;
  fDevice.Load(ASID, mO.RB);
  Result := TPersistData<T>.Create(ASID, mO);
end;

function TPersist.New<T>;
var
  mO: T;
begin
  mO := T.Create;
  Result := TPersistData<T>.Create(mO);
end;

procedure TPersist.Save<T>;
var
  mSID: TSID;
begin
  if AData.SID.IsClear then
    AData.SID := fDevice.NewSID;
  fDevice.Save(mSID, AData.RB);
end;

function TPersist.Select<T>;
var
  mB: IPersistDataListBuilder<T>;
begin
  mB := TPersistDataListBuilder<T>.Create;
  fDevice.Select(T.ClassName, mB as IPersistDataListBuilder);
  Result := mB.Build;
end;

end.

